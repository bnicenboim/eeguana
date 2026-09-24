library(eeguana)
options(eeguana.verbose = FALSE)

## browse_ica() is a Shiny app. Its server is tested with shiny::testServer(),
## which runs it without a browser: inputs are set by the test, and the
## update*() calls that would change them in the browser have no effect, so
## the helpers they rely on are tested on their own. shiny and bslib are only
## suggested, so the tests skip, saying why, when they are missing.

skip_if_not(
  requireNamespace("shiny", quietly = TRUE) && requireNamespace("bslib", quietly = TRUE),
  "browse_ica() needs shiny and bslib, which are only suggested"
)

seg <- eeg_segment(data_faces_10_trials, .description %in% c("s70", "s71"), .lim = c(-.2, .5))
ica_seg <- suppressWarnings(
  eeg_ica(seg, -EOGH, -EOGV, -M1, -M2, .method = fast_ICA, .config = list(maxit = 10))
)
rec <- names(ica_seg$.ica)
prep_seg <- eeguana:::browse_ica_prep(ica_seg, rec)

test_that("the units agree with as_time() and as_sample_int()", {
  s <- sample_int(c(-99L, 1L, 251L), 500)
  for (unit in c("s", "ms")) {
    expect_equal(
      eeguana:::position_to_sample(as_time(s, .unit = unit), unit, 500),
      as.numeric(s)
    )
  }
  expect_equal(eeguana:::position_to_sample(as.numeric(s), "samples", 500), as.numeric(s))
  expect_equal(eeguana:::sample_to_position(s, "s", 500), as_time(s, .unit = "s"))
  expect_equal(eeguana:::sample_to_position(s, "ms", 500), as_time(s, .unit = "ms"))
  expect_equal(eeguana:::sample_to_position(s, "samples", 500), as.numeric(s))
  expect_equal(
    eeguana:::position_to_sample(c(-.2, .5), "s", 500),
    as.numeric(as_sample_int(c(-.2, .5), .sampling_rate = 500, .unit = "s"))
  )
  expect_equal(eeguana:::duration_to_samples(2, "s", 500), 1000)
  expect_equal(eeguana:::duration_to_samples(10, "ms", 500), 5)
})

test_that("windows stay inside their segment", {
  b <- prep_seg$bounds
  ## moved rather than shrunk when they fit
  expect_equal(eeguana:::clamp_window(b, 2L, -150, 49), list(id = 2L, first = -99L, last = 100L))
  expect_equal(eeguana:::clamp_window(b, 2L, 200, 399), list(id = 2L, first = 52L, last = 251L))
  ## shrunk when they do not
  expect_equal(eeguana:::clamp_window(b, 2L, -500, 500), list(id = 2L, first = -99L, last = 251L))
  ev <- prep_seg$events[.id == 3L][1]
  w <- eeguana:::event_window(b, ev, from = -50, to = 50)
  expect_equal(w$first, ev$.initial - 50L)
  expect_equal(w$last, ev$.initial + 50L)
  expect_equal(w$anchor, ev$.initial)
  ## from and to in either order
  expect_equal(eeguana:::event_window(b, ev, from = 50, to = -50), w)
})

test_that("next and previous windows cross segments and stop at the ends", {
  b <- prep_seg$bounds
  nw <- eeguana:::next_window
  expect_equal(nw(b, 1L, -99L, 100L, 1), list(id = 1L, first = 1L))
  expect_equal(nw(b, 1L, 201L, 100L, 1), list(id = 2L, first = -99L))
  expect_equal(nw(b, 2L, -99L, 100L, -1), list(id = 1L, first = 152L))
  expect_equal(nw(b, 1L, -99L, 100L, -1), list(id = 1L, first = -99L))
  last <- b$.id[nrow(b)]
  expect_equal(nw(b, last, 201L, 100L, 1), list(id = last, first = 201L))
})

test_that("the activations in a window are the ones eeg_ica_show() gives", {
  w <- list(id = 4L, first = -20L, last = 80L)
  tbl <- eeguana:::window_tbl(prep_seg, w, c("ICA1", "ICA3"), "EOGV")
  shown <- ica_seg %>%
    eeg_filter(.id == 4L, .sample >= -20L, .sample <= 80L) %>%
    eeg_ica_show(ICA1, ICA3)
  ## eeg_ica_show() multiplies the activations by 10
  expect_equal(tbl[.key == "ICA1"]$.value * 10, as.numeric(shown$.signal$ICA1))
  expect_equal(tbl[.key == "ICA3"]$.value * 10, as.numeric(shown$.signal$ICA3))
  eogv <- as.numeric(shown$.signal$EOGV)
  expect_equal(tbl[.key == "EOGV"]$.value, eogv - mean(eogv))
  expect_equal(unique(tbl$.sample), -20:80)
})

test_that("events are offered by type, with the blinks first chosen", {
  events <- data.table::data.table(
    .type = c("Stimulus", "Stimulus", "artifact", "artifact"),
    .description = c("s70", "s70", "peak_threshold=100_x", "step_threshold=30_x")
  )
  choices <- eeguana:::event_choices(events)
  expect_equal(names(choices), c("Stimulus", "artifact"))
  expect_equal(choices$artifact, c("peak (1)" = "peak_threshold=100_x", "step (1)" = "step_threshold=30_x"))
  ## descriptions that would get the same short label keep their full one
  expect_equal(
    eeguana:::short_labels(c("peak_threshold=100_x", "peak_threshold=50_x", "step_threshold=30_x", "s70")),
    c("peak_threshold=100_x", "peak_threshold=50_x", "step", "s70")
  )
  expect_equal(choices$Stimulus, c("s70 (2)" = "s70"))
  expect_equal(eeguana:::default_events(events), "peak_threshold=100_x")
  expect_equal(eeguana:::default_events(events[-3]), "step_threshold=30_x")
  expect_equal(eeguana:::default_events(events[1:2]), "s70")
  expect_equal(eeguana:::event_choices(events, ".type"), c("Stimulus (2)" = "Stimulus", "artifact (2)" = "artifact"))
  expect_equal(eeguana:::default_events(events, ".type"), "artifact")
  expect_equal(eeguana:::default_events(events[1:2], ".type"), "Stimulus")
  expect_equal(eeguana:::eog_abbreviation(c("VEOG", "EOGH", "EOG", "Fp1")), c("V", "H", "EOG", "Fp1"))
})

test_that("the code printed at the end removes the marked components", {
  code <- eeguana:::ica_keep_code("ica_seg", stats::setNames(list(c("ICA1", "ICA3")), rec))
  expect_match(code, "eeg_ica_keep(ica_seg, -c(ICA1, ICA3))", fixed = TRUE)
  cleaned <- eval(parse(text = sub("^.*\n", "", code)))
  expect_equal(component_names(cleaned), setdiff(component_names(ica_seg), c("ICA1", "ICA3")))
  expect_equal(
    eeguana:::ica_keep_code("ica_seg", stats::setNames(list(character(0)), rec)),
    "No components were marked for removal."
  )
  two <- eeguana:::ica_keep_code("x", list(a = "ICA1", b = character(0), c = c("ICA2", "ICA4")))
  expect_match(two, "eeg_ica_keep(x, `a` = -c(ICA1), `c` = -c(ICA2, ICA4))", fixed = TRUE)
})

test_that("browse_ica() checks its arguments", {
  expect_error(eeguana:::browse_ica_app(seg), "must be an eeg_ica_lst")
  expect_error(eeguana:::browse_ica_app(ica_seg, .eog = "VEOG"), "Channels not found: VEOG")
  for (freq in list(1, c(1, 2, 3), c("a", "b"))) {
    expect_error(eeguana:::browse_ica_app(ica_seg, .eog_freq = freq), "must be NULL or two cutoff")
  }
  for (freq in list(NULL, c(NA, 30), c(1, NA), c(NA, NA))) {
    expect_s3_class(eeguana:::browse_ica_app(ica_seg, .eog_freq = freq), "shiny.appobj")
  }
})

test_that("the app shows windows around events and through the recording", {
  shiny::testServer(eeguana:::browse_ica_app(ica_seg), {
    session$setInputs(
      unit = "ms", mode = "events", event_field = ".description", event_match = "exact",
      event_values = "s71", event_pattern = "", from = -100, to = 300,
      event_i = 3, components = c("ICA1", "ICA2"), channels = c("EOGV", "EOGH"),
      scale = "shared", electrodes = FALSE, order = "var", n_components = 4
    )
    ## the plots wait until the components stop changing
    session$elapse(1100)
    ev <- prep()$events[.description == "s71"]
    expect_equal(window()$id, ev$.id[3])
    expect_equal(window()$anchor, ev$.initial[3])
    expect_equal(c(window()$first, window()$last), ev$.initial[3] + c(-50L, 150L))
    expect_equal(output$n_events, "5 events")
    expect_match(output$where, "Event 3 of 5: [^ ]+ \u00b7 s71 at")
    expect_match(output$activations$src, "^data:image/png")
    expect_match(output$topographies$src, "^data:image/png")

    ## event numbers past the end show the last event, and below one the first
    session$setInputs(event_i = 99)
    expect_equal(event_i(), 5)
    expect_true(window()$at_end)
    session$setInputs(event_slider = 2)
    expect_equal(window()$anchor, ev$.initial[2])
    session$setInputs(event_i = -4)
    expect_equal(event_i(), 1)
    expect_true(window()$at_start)
    ## next and previous stop at the ends
    session$setInputs(prev = 1)
    expect_equal(event_i(), 1)
    session$setInputs(`next` = 1)
    session$setInputs(arrow_key = list(key = "ArrowRight"))
    expect_equal(event_i(), 3)

    ## the length first: the start is kept inside the segment for the length
    ## there is when it is set
    session$setInputs(mode = "continuous", unit_continuous = "ms", segment = "5", length = 200, start = 100)
    expect_equal(window()[c("id", "first", "last")], list(id = 5L, first = 51L, last = 150L))
    expect_match(output$where, "Segment 5, 100 ms to 298 ms")
    expect_match(output$activations$src, "^data:image/png")
  })
})

test_that("the window through the recording stays inside the data", {
  shiny::testServer(eeguana:::browse_ica_app(ica_seg), {
    session$setInputs(mode = "continuous", unit_continuous = "ms", segment = "2", start = 0, length = 200)
    ## a window longer than the segment is shortened to it
    session$setInputs(length = 10000)
    expect_equal(continuous(), list(id = 2L, first = -99L, length = 351L))
    ## and one that goes past the end is moved back
    session$setInputs(length = 200, start = 600)
    expect_equal(continuous(), list(id = 2L, first = 152L, length = 100L))
    ## the next window starts the next segment, the previous one ends the
    ## previous segment
    session$setInputs(`next` = 1)
    expect_equal(continuous(), list(id = 3L, first = -99L, length = 100L))
    session$setInputs(prev = 1, start_slider = 0)
    expect_equal(continuous()$id, 2L)
    ## the slider moves the start, in the unit shown
    session$setInputs(start_slider = 100)
    expect_equal(continuous()$first, 51L)
    ## changing the unit does not move the window
    session$setInputs(unit_continuous = "samples")
    expect_equal(continuous()$first, 51L)
    session$setInputs(start = 60)
    expect_equal(continuous()$first, 60L)
    ## at the edges of the recording, the buttons are grayed out
    last <- prep()$bounds[.N]
    session$setInputs(segment = as.character(last$.id), start = 1000)
    expect_true(window()$at_end)
    expect_false(window()$at_start)
  })
})

test_that("the arrow keys and buttons zoom the amplitudes", {
  shiny::testServer(eeguana:::browse_ica_app(ica_seg), {
    session$setInputs(components = "ICA1", channels = "EOGV", scale = "shared")
    session$elapse(1100)
    expect_match(output$scale_text, "^Rows span \u00b1[0-9.]+ for the channels and \u00b14 typical SDs for the components$")
    session$setInputs(arrow_key = list(key = "ArrowUp"))
    session$setInputs(zoom_in = 1)
    expect_equal(zoom(), 2)
    expect_match(output$scale_text, "\u00b12 typical SDs for the components")
    session$setInputs(scale = "each")
    expect_equal(output$scale_text, "Rows span \u00b12 times the typical SD of each trace")
    session$setInputs(arrow_key = list(key = "ArrowDown"))
    expect_equal(zoom(), sqrt(2))
    ## the multiplier can be written by hand, and stays between 1/64 and 64
    session$setInputs(zoom = 3)
    expect_equal(zoom(), 3)
    session$setInputs(zoom_in = 2)
    expect_equal(zoom(), 3 * sqrt(2))
    session$setInputs(zoom = 1000)
    expect_equal(zoom(), 64)
    session$setInputs(zoom = -1)
    expect_equal(zoom(), 64)
    session$setInputs(zoom = NA)
    expect_equal(zoom(), 64)
  })
})

test_that("amplitudes are scaled by their typical standard deviation", {
  ## the median of the standard deviations of stretches of one second (500
  ## samples here) of each segment; the segments are shorter, so each segment
  ## is one stretch. eeg_ica_show() multiplies activations by 10
  shown <- eeg_ica_show(ica_seg, ICA1)
  per_segment <- function(x) stats::median(tapply(as.numeric(x), shown$.signal$.id, stats::sd))
  expect_equal(prep_seg$sd[["ICA1"]], per_segment(shown$.signal$ICA1) / 10)
  expect_equal(prep_seg$sd[["EOGV"]], per_segment(shown$.signal$EOGV))
  ## a slow drift does not change it, and it is split into stretches
  x <- cbind(a = rep(c(-1, 1), 500), b = rep(c(-1, 1), 500) + seq(0, 20, length.out = 1000))
  sds <- eeguana:::typical_sd(x, ids = rep(1L, 1000), chunk = 10)
  expect_equal(sds[["a"]], stats::sd(rep(c(-1, 1), 5)))
  expect_lt(abs(sds[["b"]] - sds[["a"]]) / sds[["a"]], .05)
  expect_gt(stats::sd(x[, "b"]), 5 * sds[["b"]])

  each <- eeguana:::amplitude_scale(prep_seg, c("ICA1", "ICA2"), c("EOGV", "Fz"), "each", 1)
  expect_equal(each$ref, prep_seg$sd[c("ICA1", "ICA2", "EOGV", "Fz")])
  shared <- eeguana:::amplitude_scale(prep_seg, c("ICA1", "ICA2"), c("EOGV", "Fz"), "shared", 2)
  pooled <- function(x) sqrt(mean(x^2))
  expect_equal(unname(shared$ref[1:2]), rep(pooled(prep_seg$sd[c("ICA1", "ICA2")]), 2))
  expect_equal(unname(shared$ref[3:4]), rep(pooled(prep_seg$sd[c("EOGV", "Fz")]), 2))
  expect_equal(shared$half, 2)
})

test_that("clicking a topography marks and unmarks the component", {
  shiny::testServer(eeguana:::browse_ica_app(ica_seg), {
    session$setInputs(components = c("ICA1", "ICA2"), marked = NULL, electrodes = TRUE)
    session$elapse(1100)
    session$setInputs(topo_click = list(panelvar1 = "ICA2"))
    expect_equal(marks()[[rec]], "ICA2")
    session$setInputs(topo_click = list(panelvar1 = "ICA1"))
    expect_equal(marks()[[rec]], c("ICA2", "ICA1"))
    expect_match(output$topographies$src, "^data:image/png")
    session$setInputs(topo_click = list(panelvar1 = "ICA2"))
    expect_equal(marks()[[rec]], "ICA1")
    ## the field in the sidebar replaces the marks
    session$setInputs(marked = c("ICA3", "ICA4"))
    expect_equal(marks()[[rec]], c("ICA3", "ICA4"))
  })
})

test_that("each recording keeps its own marks", {
  two <- bind(seg, eeg_mutate(seg, .recording = "second"))
  ica_two <- suppressWarnings(
    eeg_ica(two, -EOGH, -EOGV, -M1, -M2, .method = fast_ICA, .config = list(maxit = 10))
  )
  shiny::testServer(eeguana:::browse_ica_app(ica_two), {
    session$setInputs(recording = rec, components = "ICA1", marked = NULL)
    session$setInputs(topo_click = list(panelvar1 = "ICA1"))
    session$setInputs(recording = "second")
    expect_equal(prep()$recording, "second")
    session$setInputs(topo_click = list(panelvar1 = "ICA2"))
    expect_equal(marks(), stats::setNames(list("ICA1", "ICA2"), c(rec, "second")))
  })
})

test_that("events are matched by their type or description", {
  ev <- prep_seg$events
  me <- eeguana:::match_events
  n_desc <- function(x) sum(ev$.description %in% x)
  expect_equal(nrow(me(ev, ".description", "exact", values = c("s70", "s71"))), n_desc(c("s70", "s71")))
  expect_equal(nrow(me(ev, ".description", "starts", pattern = "s7")), n_desc(c("s70", "s71")))
  expect_equal(nrow(me(ev, ".description", "ends", pattern = "1")), n_desc("s71"))
  expect_equal(nrow(me(ev, ".description", "contains", pattern = "7")), n_desc(c("s70", "s71")))
  expect_equal(nrow(me(ev, ".description", "regex", pattern = "^s7[01]$")), n_desc(c("s70", "s71")))
  ## contains is literal: the dot is not a wildcard
  expect_equal(nrow(me(ev, ".description", "contains", pattern = "s.0")), 0)
  expect_equal(nrow(me(ev, ".type", "exact", values = unique(ev$.type)[1])), sum(ev$.type == unique(ev$.type)[1]))
  ## the events stay in their order, so that next and previous follow it
  m <- me(ev, ".description", "starts", pattern = "s7")
  expect_equal(m, ev[startsWith(ev$.description, "s7")])
  expect_error(suppressWarnings(me(ev, ".description", "regex", pattern = "s7(")))
})

test_that("the EOG channels are filtered as asked, and only them", {
  d <- prep_seg$data
  eog <- c("EOGV", "EOGH")
  expect_identical(eeguana:::filter_eog(d, eog, NULL), d)
  expect_identical(eeguana:::filter_eog(d, eog, c(NA, NA)), d)
  band <- eeguana:::filter_eog(d, eog, c(.1, 30))
  expect_equal(band$.signal$EOGV, eeg_filt_band_pass(d, EOGV, EOGH, .freq = c(.1, 30))$.signal$EOGV)
  expect_equal(band$.signal$Fz, d$.signal$Fz)
  high <- eeguana:::filter_eog(d, eog, c(1, NA))
  expect_equal(high$.signal$EOGH, eeg_filt_high_pass(d, EOGV, EOGH, .freq = 1)$.signal$EOGH)
  low <- eeguana:::filter_eog(d, eog, c(NA, 30))
  expect_equal(low$.signal$EOGH, eeg_filt_low_pass(d, EOGV, EOGH, .freq = 30)$.signal$EOGH)
})

test_that("the labels show the correlations with the filtered EOG channels", {
  eog <- c("EOGV", "EOGH")
  unfiltered <- eeguana:::browse_ica_summaries(prep_seg, eog, NULL)
  filtered <- eeguana:::browse_ica_summaries(prep_seg, eog, c(1, NA))
  expected <- eeg_ica_cor_tbl(eeg_filt_high_pass(prep_seg$data, EOGV, EOGH, .freq = 1), EOGV, EOGH)
  expect_equal(filtered$cor, expected)
  expect_false(isTRUE(all.equal(unfiltered$cor$cor, filtered$cor$cor)))
  expect_match(filtered$labels[["ICA1"]], "^ICA1 \u00b7 [0-9.]+%\nH -?[0-9.]+ \u00b7 V -?[0-9.]+$")
  top <- filtered$order$cor[1]
  expect_equal(max(abs(expected[.ICA == top]$cor)), max(abs(expected$cor)))
  ## without EOG channels there are no correlations, and the order by
  ## correlation falls back on the variance
  none <- eeguana:::browse_ica_summaries(prep_seg, character(0), NULL)
  expect_equal(none$order$cor, prep_seg$order_var)
  expect_match(none$labels[["ICA1"]], "^ICA1 \u00b7 [0-9.]+%$")
})

test_that("the app matches events with a regular expression, and says when it is invalid", {
  shiny::testServer(eeguana:::browse_ica_app(ica_seg), {
    session$setInputs(
      mode = "events", event_field = ".description", event_match = "regex",
      event_pattern = "^s7[01]$", event_values = NULL, from = -.1, to = .1, event_i = 1
    )
    expect_equal(nrow(selected_events()), 9)
    expect_equal(output$n_events, "9 events: s71 (5), s70 (4)")
    session$setInputs(event_pattern = "s7(")
    expect_error(output$n_events, "Invalid regular expression")
    session$setInputs(event_match = "ends", event_pattern = "0")
    expect_equal(output$n_events, "4 events: s70 (4)")
    session$setInputs(event_pattern = "")
    expect_error(output$n_events, "Write the text the events should match")
  })
})

test_that("changing the filter of the EOG channels changes the correlations", {
  shiny::testServer(eeguana:::browse_ica_app(ica_seg), {
    session$setInputs(eog_filter = TRUE, eog_low = .1, eog_high = 30)
    band <- summaries()$cor
    session$setInputs(eog_filter = FALSE)
    session$elapse(1000)
    none <- summaries()$cor
    expect_false(isTRUE(all.equal(band$cor, none$cor)))
    expect_equal(none, eeg_ica_cor_tbl(ica_seg, EOGV, EOGH))
  })
})
