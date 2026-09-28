library(eeguana)
options(eeguana.verbose = FALSE)

## eeg_browse() is a Shiny app. Its server is tested with shiny::testServer(),
## which runs it without a browser: inputs are set by the test, and the
## update*() calls that would change them in the browser have no effect, so
## the helpers they rely on are tested on their own. shiny and bslib are only
## suggested, so the tests skip, saying why, when they are missing.

skip_if_not(
  requireNamespace("shiny", quietly = TRUE) && requireNamespace("bslib", quietly = TRUE),
  "eeg_browse() needs shiny and bslib, which are only suggested"
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

test_that("windows go on from one segment into the next and stay inside the recording", {
  b <- prep_seg$bounds
  ## each segment has 351 samples, from -99 to 251
  expect_equal(eeguana:::to_position(b, 1L, -99L), 0L)
  expect_equal(eeguana:::to_position(b, 2L, -99L), 351L)
  expect_equal(eeguana:::from_position(b, 360L), list(id = 2L, sample = -90L))
  ## a window that fits in a segment
  piece <- function(id, first, last) data.table::data.table(.id = id, first = first, last = last)
  expect_equal(eeguana:::window_pieces(b, list(start = 351L, length = 100L)), piece(2L, -99L, 0L))
  ## one that goes on into the next segments
  expect_equal(
    eeguana:::window_pieces(b, list(start = 300L, length = 500L)),
    piece(c(1L, 2L, 3L), c(201L, -99L, -99L), c(251L, 251L, -2L))
  )
  ## windows past the edges of the recording are moved back, not shortened
  total <- 351L * nrow(b)
  expect_equal(eeguana:::clamp_span(b, -50, 100), list(start = 0L, length = 100L))
  expect_equal(eeguana:::clamp_span(b, total - 10, 100), list(start = total - 100L, length = 100L))
  ## and shortened only when the recording is shorter
  expect_equal(eeguana:::clamp_span(b, 10, total + 5), list(start = 0L, length = total))
  ## around an event, in either order
  ev <- prep_seg$events[.id == 3L][1]
  span <- eeguana:::event_span(b, ev, from = -400, to = 50)
  expect_equal(span$start, eeguana:::to_position(b, 3L, ev$.initial) - 400L)
  expect_equal(span$length, 451L)
  expect_equal(eeguana:::event_span(b, ev, from = 50, to = -400), span)
  expect_equal(eeguana:::window_pieces(b, span)$.id, c(2L, 3L))
})

test_that("a click on a column selects its segment, and elsewhere the one of the event", {
  w <- list(focus = 7L)
  click <- list(x = 1, panelvar1 = "Fz", panelvar2 = "5", mapping = list(panelvar1 = ".key", panelvar2 = ".id"))
  expect_equal(eeguana:::clicked_segment(click, w), 5L)
  expect_equal(eeguana:::clicked_segment(list(x = 1, panelvar1 = "Fz", mapping = list(panelvar1 = ".key")), w), 7L)
  expect_equal(eeguana:::clicked_segment(list(x = 1), w), 7L)
})

test_that("the activations in a window are the ones eeg_ica_show() gives", {
  w <- list(pieces = data.table::data.table(.id = 4L, first = -20L, last = 80L))
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

test_that("the message at the end lists the components and how to keep or remove them", {
  msg <- eeguana:::ica_selection_message("ica_seg", stats::setNames(list(c("ICA1", "ICA3")), rec))
  lines <- strsplit(msg, "\n")[[1]]
  expect_equal(lines[1], "Components selected: ICA1, ICA3")
  expect_equal(lines[2], "To keep only these components: eeg_ica_keep(ica_seg, c(ICA1, ICA3))")
  expect_equal(lines[3], "To remove them: eeg_ica_keep(ica_seg, -c(ICA1, ICA3))")
  ## the calls do what they say
  run <- function(line) eval(parse(text = sub("^[^:]*: ", "", line)))
  expect_equal(component_names(run(lines[2])), c("ICA1", "ICA3"))
  expect_equal(component_names(run(lines[3])), setdiff(component_names(ica_seg), c("ICA1", "ICA3")))
  expect_equal(
    eeguana:::ica_selection_message("ica_seg", stats::setNames(list(character(0)), rec)),
    "No components were selected."
  )
  ## with several recordings, each one gets its own components
  two <- eeguana:::ica_selection_message("x", list(a = "ICA1", b = character(0), c = c("ICA2", "ICA4")))
  expect_equal(
    strsplit(two, "\n")[[1]],
    c(
      "Components selected:", "  a: ICA1", "  c: ICA2, ICA4",
      "To keep only these components: eeg_ica_keep(x, `a` = c(ICA1), `c` = c(ICA2, ICA4))",
      "To remove them: eeg_ica_keep(x, `a` = -c(ICA1), `c` = -c(ICA2, ICA4))"
    )
  )
})

test_that("eeg_browse() checks its arguments", {
  expect_error(eeguana:::browse_app(seg, .kind = "ica"), "must be an eeg_ica_lst")
  expect_error(eeguana:::browse_app(ica_seg, .kind = "ica", .eog = "VEOG"), "Channels not found: VEOG")
  for (freq in list(1, c(1, 2, 3), c("a", "b"))) {
    expect_error(eeguana:::browse_app(ica_seg, .kind = "ica", .eog_freq = freq), "must be NULL or two cutoff")
  }
  for (freq in list(NULL, c(NA, 30), c(1, NA), c(NA, NA))) {
    expect_s3_class(eeguana:::browse_app(ica_seg, .kind = "ica", .eog_freq = freq), "shiny.appobj")
  }
})

test_that("the app shows windows around events and through the recording", {
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    session$setInputs(
      unit = "ms", mode = "events", event_field = ".description", event_match = "exact",
      event_values = "s71", event_pattern = "", from = -100, to = 300,
      event_i = 3, components = c("ICA1", "ICA2"), channels = c("EOGV", "EOGH"),
      scale = "shared", electrodes = FALSE, order = "var", n_components = 4
    )
    ## the plots wait until the components stop changing
    session$elapse(1100)
    ev <- prep()$events[.description == "s71"]
    expect_equal(window()$anchor, list(id = ev$.id[3], sample = ev$.initial[3]))
    expect_equal(window()$focus, ev$.id[3])
    expect_equal(
      window()$pieces,
      data.table::data.table(.id = ev$.id[3], first = ev$.initial[3] - 50L, last = ev$.initial[3] + 150L)
    )
    expect_equal(output$n_events, "5 events")
    expect_match(output$where, "Event 3 of 5: [^ ]+ \u00b7 s71 at")
    expect_match(output$activations$src, "^data:image/png")
    expect_match(output$topographies$src, "^data:image/png")

    ## event numbers past the end show the last event, and below one the first
    session$setInputs(event_i = 99)
    expect_equal(event_i(), 5)
    expect_true(window()$at_end)
    session$setInputs(event_slider = 2)
    expect_equal(window()$anchor$sample, ev$.initial[2])
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
    expect_equal(window()$pieces, data.table::data.table(.id = 5L, first = 51L, last = 150L))
    expect_match(output$where, "Segment 5, 100 ms to 298 ms")
    expect_match(output$activations$src, "^data:image/png")
  })
})

test_that("the window through the recording goes on into the next segments", {
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    b <- prep()$bounds
    session$setInputs(mode = "continuous", unit_continuous = "ms", segment = "2", start = 0, length = 200)
    expect_equal(window()$pieces, data.table::data.table(.id = 2L, first = 1L, last = 100L))
    ## a window longer than the segment goes on into the next ones
    session$setInputs(length = 1500)
    expect_equal(window()$pieces$.id, c(2L, 3L, 4L))
    expect_equal(window()$pieces$first[1], 1L)
    expect_match(output$where, "^Segments 2 to 4, 0 ms to ")
    ## a start past the end of the segment is in the next one, and the fields
    ## then refer to that segment
    session$setInputs(length = 200, start = 800)
    expect_equal(window()$pieces, data.table::data.table(.id = 3L, first = 50L, last = 149L))
    ## the next window starts where this one ends, also in the next segment
    session$setInputs(segment = "2", start = 400)
    expect_equal(
      window()$pieces,
      data.table::data.table(.id = c(2L, 3L), first = c(201L, -99L), last = c(251L, -51L))
    )
    session$setInputs(`next` = 1)
    expect_equal(window()$pieces, data.table::data.table(.id = 3L, first = -50L, last = 49L))
    session$setInputs(prev = 1)
    expect_equal(window()$pieces$.id, c(2L, 3L))
    ## the slider places the start anywhere in the recording, counting the
    ## segments one after the other: 1 s is 500 samples, in the second segment
    session$setInputs(position_slider = 1000)
    expect_equal(continuous()$start, 500L)
    expect_equal(window()$pieces, data.table::data.table(.id = 2L, first = 50L, last = 149L))
    session$setInputs(segment = "2", start = 400)
    ## changing the unit does not move the window
    session$setInputs(unit_continuous = "samples")
    expect_equal(window()$pieces$.id, c(2L, 3L))
    ## at the edges of the recording, the window is moved back inside, and the
    ## buttons are grayed out
    last <- b[.N]
    session$setInputs(segment = as.character(last$.id), start = 1000)
    expect_equal(window()$pieces, data.table::data.table(.id = last$.id, first = 152L, last = 251L))
    expect_true(window()$at_end)
    expect_false(window()$at_start)
    session$setInputs(segment = "1", start = -99)
    expect_true(window()$at_start)
  })
})

test_that("start and length beyond the recording are replaced by the values used", {
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    b <- prep()$bounds
    last_sent <- function(id) sent[[id]][[length(sent[[id]])]]
    session$setInputs(
      mode = "continuous", unit_continuous = "samples", segment = as.character(b$.id[nrow(b)]),
      length = 100, start = 1000
    )
    ## the window ends with the recording, and the field shows where it starts
    expect_true(window()$at_end)
    expect_equal(last_sent("start"), b$last[nrow(b)] - 99)
    ## also when the window does not move
    n <- length(sent$start)
    session$setInputs(start = 2000)
    expect_length(sent$start, n + 1)
    expect_equal(last_sent("start"), b$last[nrow(b)] - 99)
    ## a length longer than the recording is the recording, and a length of
    ## zero or less is one sample
    session$setInputs(length = 1e6)
    total <- eeguana:::segment_positions(b)$total
    expect_equal(continuous()$length, total)
    expect_equal(last_sent("length"), total)
    session$setInputs(length = -3)
    expect_equal(continuous()$length, 1L)
    expect_equal(last_sent("length"), 1)
    ## an emptied field shows the current value again
    session$setInputs(start = NA)
    expect_false(is.na(last_sent("start")))
  })
})

test_that("the fields changed by the app are not taken as the user's when they come back", {
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    session$setInputs(mode = "continuous", unit_continuous = "s", length = .2)
    session$setInputs(position_slider = 1)
    expect_equal(continuous()$start, 500L)
    ## the start of the window, in its segment, that the app sent to the field
    first <- sent$start[[length(sent$start)]]
    session$setInputs(position_slider = 2)
    expect_equal(continuous()$start, 1000L)
    ## the field comes back from the browser with the first start, late
    session$setInputs(start = first)
    expect_equal(continuous()$start, 1000L)
    ## but a start written by the user moves the window
    session$setInputs(start = 0)
    expect_equal(from_position(prep()$bounds, continuous()$start)$sample, 1L)
  })
})

test_that("the arrow keys and buttons zoom the amplitudes", {
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    session$setInputs(components = "ICA1", channels = "EOGV", scale = "shared")
    session$elapse(1100)
    expect_match(output$scale_text, "^Each row spans \u00b1[0-9.]+ for the channels and \u00b14 typical SDs for the components; positive up$")
    session$setInputs(arrow_key = list(key = "ArrowUp"))
    session$setInputs(zoom_in = 1)
    expect_equal(zoom(), 2)
    expect_match(output$scale_text, "\u00b12 typical SDs for the components")
    session$setInputs(scale = "each")
    expect_equal(output$scale_text, "Each row spans \u00b12 times the typical SD of its trace; positive up")
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
  ## the median of the standard deviations, estimated with the MAD, of
  ## stretches of one second (500 samples here) of each segment; the segments
  ## are shorter, so each segment is one stretch. eeg_ica_show() multiplies
  ## activations by 10
  shown <- eeg_ica_show(ica_seg, ICA1)
  per_segment <- function(x) stats::median(tapply(as.numeric(x), shown$.signal$.id, stats::mad))
  expect_equal(prep_seg$sd[["ICA1"]], per_segment(shown$.signal$ICA1) / 10)
  expect_equal(prep_seg$sd[["EOGV"]], per_segment(shown$.signal$EOGV))
  ## a slow drift does not change it, and it is split into stretches
  x <- cbind(a = rep(c(-1, 1), 500), b = rep(c(-1, 1), 500) + seq(0, 20, length.out = 1000))
  sds <- eeguana:::typical_sd(x, ids = rep(1L, 1000), chunk = 10)
  expect_equal(sds[["a"]], stats::mad(rep(c(-1, 1), 5)))
  expect_lt(abs(sds[["b"]] - sds[["a"]]) / sds[["a"]], .05)
  expect_gt(stats::sd(x[, "b"]), 3 * sds[["b"]])
  ## and neither does a large outlier in every stretch, which the median over
  ## the stretches alone would not remove
  spiky <- rep(c(-1, 1), 500)
  spiky[seq(1, 1000, by = 10)] <- 50
  expect_equal(eeguana:::typical_sd(cbind(spiky), ids = rep(1L, 1000), chunk = 10)[[1]], sds[["a"]])

  each <- eeguana:::amplitude_scale(prep_seg, c("ICA1", "ICA2"), c("EOGV", "Fz"), "each", 1)
  expect_equal(each$ref, prep_seg$sd[c("ICA1", "ICA2", "EOGV", "Fz")])
  shared <- eeguana:::amplitude_scale(prep_seg, c("ICA1", "ICA2"), c("EOGV", "Fz"), "shared", 2)
  pooled <- function(x) sqrt(mean(x^2))
  expect_equal(unname(shared$ref[1:2]), rep(pooled(prep_seg$sd[c("ICA1", "ICA2")]), 2))
  expect_equal(unname(shared$ref[3:4]), rep(pooled(prep_seg$sd[c("EOGV", "Fz")]), 2))
  expect_equal(shared$half, 2)
})

test_that("clicking a topography selects and deselects the component", {
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    session$setInputs(components = c("ICA1", "ICA2"), selected = NULL, electrodes = TRUE)
    session$elapse(1100)
    session$setInputs(topo_click = list(panelvar1 = "ICA2"))
    expect_equal(marks()[[rec]], "ICA2")
    session$setInputs(topo_click = list(panelvar1 = "ICA1"))
    expect_equal(marks()[[rec]], c("ICA2", "ICA1"))
    expect_match(output$topographies$src, "^data:image/png")
    session$setInputs(topo_click = list(panelvar1 = "ICA2"))
    expect_equal(marks()[[rec]], "ICA1")
    ## the field in the sidebar replaces the marks
    session$setInputs(selected = c("ICA3", "ICA4"))
    expect_equal(marks()[[rec]], c("ICA3", "ICA4"))
  })
})

test_that("each recording keeps its own selection", {
  two <- bind(seg, eeg_mutate(seg, .recording = "second"))
  ica_two <- suppressWarnings(
    eeg_ica(two, -EOGH, -EOGV, -M1, -M2, .method = fast_ICA, .config = list(maxit = 10))
  )
  shiny::testServer(eeguana:::browse_app(ica_two, .kind = "ica"), {
    session$setInputs(recording = rec, components = "ICA1", selected = NULL)
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
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
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
  shiny::testServer(eeguana:::browse_app(ica_seg, .kind = "ica"), {
    session$setInputs(eog_filter = TRUE, eog_low = .1, eog_high = 30)
    band <- summaries()$cor
    session$setInputs(eog_filter = FALSE)
    session$elapse(1000)
    none <- summaries()$cor
    expect_false(isTRUE(all.equal(band$cor, none$cor)))
    expect_equal(none, eeg_ica_cor_tbl(ica_seg, EOGV, EOGH))
  })
})

#### eeg_browse() on an eeg_lst

test_that("the signal of the channels is shown, without components", {
  shiny::testServer(eeguana:::browse_app(seg), {
    session$setInputs(mode = "continuous", unit_continuous = "ms", segment = "2", start = 0, length = 200)
    session$setInputs(channels = c("Fz", "Cz", "EOGV"), scale = "shared")
    session$elapse(1100)
    expect_equal(components(), character(0))
    expect_equal(window()$focus, 2L)
    expect_match(output$activations$src, "^data:image/png")
    expect_match(output$scale_text, "^Each row spans \u00b1[0-9.,]+ for the channels; positive up$")
    ## the typical SDs are the ones of the channels
    expect_equal(prep()$sd[["EOGV"]], prep_seg$sd[["EOGV"]])
    ## a window over several segments is drawn too
    session$setInputs(length = 1500)
    expect_equal(window()$pieces$.id, c(2L, 3L, 4L))
    expect_match(output$activations$src, "^data:image/png")
  })
  w <- list(pieces = data.table::data.table(.id = c(4L, 5L), first = c(200L, -99L), last = c(251L, -50L)))
  tbl <- eeguana:::window_tbl(eeguana:::browse_eeg_prep(seg, rec), w, character(0), "Fz")
  ## each segment is centered on its own
  fz <- function(id, from, to) {
    x <- as.numeric(eeg_filter(seg, .id == id, .sample >= from, .sample <= to)$.signal$Fz)
    x - mean(x)
  }
  expect_equal(tbl$.value, c(fz(4L, 200L, 251L), fz(5L, -99L, -50L)))
  expect_equal(tbl$.id, rep(c(4L, 5L), c(52L, 50L)))
  expect_equal(as.character(unique(tbl$.key)), "Fz")
})

test_that("large amplitudes go beyond their row, or are cut, but are never flattened", {
  p <- eeguana:::browse_eeg_prep(seg, rec)
  w <- list(pieces = data.table::data.table(.id = 4L, first = -99L, last = 251L))
  ## zoomed in 16 times, many values are beyond the rows
  sc <- eeguana:::amplitude_scale(p, character(0), c("Fz", "Cz"), "shared", 16)
  plot <- eeguana:::activations_plot(p, w, character(0), c("Fz", "Cz"), character(0), "s", 500, sc)
  ## also beyond the top and the bottom of the panel
  expect_equal(plot$coordinates$clip, "off")
  free <- plot$data$.y - plot$data$.center
  expect_gt(max(abs(free)), 1.5)
  ## no value is held at the edge of the row
  expect_equal(sum(abs(free) == 1), 0)
  ## cut, the lines reach the edges of the rows and stop there
  cut <- eeguana:::cut_at_rows(plot$data)
  rel <- c(cut$.y - cut$.center, cut$.yend - cut$.center)
  expect_true(all(abs(rel) <= 1 + 1e-9))
  expect_true(any(abs(rel) > 1 - 1e-9))
  ## the parts inside the rows are the samples themselves
  inside <- abs(free) < 1
  expect_true(all(round(plot$data$.x[inside], 6) %in% round(c(cut$.x, cut$.xend), 6)))
  ## a segment that leaves a row ends on its edge, on the line between samples
  s1 <- data.table::data.table(
    .key = "a", .id = 1L, .sample = 1:2, .x = c(0, 1), .y = c(0, 3), .center = 0
  )
  expect_equal(unlist(eeguana:::cut_at_rows(s1)[, list(.x, .y, .xend, .yend)]), c(.x = 0, .y = 0, .xend = 1 / 3, .yend = 1))
  expect_s3_class(
    eeguana:::activations_plot(p, w, character(0), c("Fz", "Cz"), character(0), "s", 500, sc, cut = TRUE),
    "ggplot"
  )
})

test_that("negative can be up, and a scale bar shows a round amplitude in the unit of the channels", {
  p <- eeguana:::browse_eeg_prep(seg, rec)
  w <- list(pieces = data.table::data.table(.id = 4L, first = -99L, last = 251L))
  sc <- eeguana:::amplitude_scale(p, character(0), c("Fz", "Cz"), "shared", 1, amp_unit = "\u00b5V")
  expect_match(sc$text, "^Each row spans \u00b1[0-9.,]+ \u00b5V for the channels; positive up$")
  draw <- function(negative_up) {
    d <- eeguana:::activations_plot(p, w, character(0), c("Fz", "Cz"), character(0), "s", 500, sc,
      negative_up = negative_up
    )$data
    d$.y - d$.center
  }
  expect_equal(draw(TRUE), -draw(FALSE))
  ## the bar is 1, 2, or 5 times a power of 10, and fits in a row
  expect_equal(
    vapply(c(1, 5, 22, 99, .37), eeguana:::round_below, numeric(1)),
    c(1, 5, 20, 50, .2)
  )
  ## the unit comes from the channels table, and is shown only when known
  expect_null(eeguana:::channel_unit_label(seg))
  with_unit <- seg
  channels_tbl(with_unit) <- tidytable::mutate(channels_tbl(with_unit), unit = "microvolt")
  expect_equal(eeguana:::channel_unit_label(with_unit), "\u00b5V")
  ## the app starts with the polarity asked, which can be changed
  shiny::testServer(eeguana:::browse_app(with_unit, .negative_up = TRUE), {
    session$setInputs(mode = "continuous", channels = "Fz", scale = "shared")
    expect_match(output$scale_text, "\u00b5V for the channels; negative up$")
    expect_match(output$activations$src, "^data:image/png")
    session$setInputs(polarity = "up")
    expect_match(output$scale_text, "positive up$")
  })
})

test_that("the topography is the mean of the window, one head per segment or their mean", {
  seg_layout <- seg
  p <- eeguana:::browse_eeg_prep(seg_layout, rec)
  ## the channels without positions do not count
  expect_false(any(c("EOGV", "EOGH") %in% p$coords$.channel))
  expect_true(all(c("Fz", "Cz", "Oz") %in% p$coords$.channel))
  w <- list(pieces = data.table::data.table(.id = c(4L, 5L), first = c(200L, -99L), last = c(251L, -50L)))
  per_segment <- eeguana:::window_topo_tbl(p, w)
  expect_equal(levels(per_segment$.group), c("4", "5"))
  averaged <- eeguana:::window_topo_tbl(p, w, average = TRUE)
  expect_equal(levels(averaged$.group), "all")
  ## the value interpolated at an electrode is close to its mean over the window
  mean_fz <- function(rows) mean(as.numeric(rows$.signal$Fz))
  fz4 <- mean_fz(eeg_filter(seg, .id == 4L, .sample >= 200L))
  fz5 <- mean_fz(eeg_filter(seg, .id == 5L, .sample <= -50L))
  at_fz <- function(tbl, group) {
    pos <- p$coords[.channel == "Fz"]
    d <- data.table::as.data.table(tbl)[.group == group]
    d[which.min((.x - pos$.x)^2 + (.y - pos$.y)^2)]$.value
  }
  expect_equal(at_fz(per_segment, "4"), fz4, tolerance = .1)
  expect_equal(at_fz(per_segment, "5"), fz5, tolerance = .1)
  both <- rbind(
    eeg_filter(seg, .id == 4L, .sample >= 200L)$.signal,
    eeg_filter(seg, .id == 5L, .sample <= -50L)$.signal
  )
  expect_equal(at_fz(averaged, "all"), mean(as.numeric(both$Fz)), tolerance = .1)
  expect_s3_class(eeguana:::window_topo_plot(p, w, average = FALSE, selected = 5L, electrodes = TRUE), "ggplot")
  shiny::testServer(eeguana:::browse_app(seg), {
    session$setInputs(mode = "continuous", unit_continuous = "ms", segment = "2", start = 0, length = 1500)
    session$setInputs(channels = "Fz", scale = "shared", topo_average = FALSE, electrodes = FALSE)
    expect_equal(n_heads(), 3)
    expect_match(output$topographies$src, "^data:image/png")
    session$setInputs(topo_average = TRUE)
    expect_equal(n_heads(), 1)
    expect_match(output$topographies$src, "^data:image/png")
  })
})

test_that("segments are selected and deselected, and returned", {
  shiny::testServer(eeguana:::browse_app(seg), {
    ## a window of one whole segment
    session$setInputs(mode = "continuous", unit_continuous = "ms", segment = "1", start = -200, length = 702)
    session$setInputs(channels = "Fz", scale = "shared")
    expect_equal(window()$pieces$.id, 1L)
    ## the button selects the segment shown
    session$setInputs(select_segment = 1)
    expect_equal(selected_segments(), 1L)
    expect_match(output$where, "segment 1 selected$")
    ## a click on the signal selects the next one
    session$setInputs(`next` = 1)
    expect_equal(window()$focus, 2L)
    expect_no_match(output$where, "selected")
    session$setInputs(trace_click = list(x = 0.1, y = 0))
    expect_equal(selected_segments(), c(1L, 2L))
    ## and M deselects it
    session$setInputs(arrow_key = list(key = "m"))
    expect_equal(selected_segments(), 1L)
    ## with several segments shown, a click selects the one in its column
    session$setInputs(length = 1500)
    expect_equal(window()$pieces$.id, c(2L, 3L, 4L))
    session$setInputs(trace_click = list(
      x = 0.1, panelvar1 = "Fz", panelvar2 = "3", mapping = list(panelvar1 = ".key", panelvar2 = ".id")
    ))
    expect_equal(selected_segments(), c(1L, 3L))
    ## the field in the sidebar replaces the selection
    session$setInputs(selected_segments = c("5", "3"))
    expect_equal(selected_segments(), c(3L, 5L))
    expect_equal(result(), c(3L, 5L))
  })
})

test_that("a recording that is one segment can be browsed but not selected", {
  shiny::testServer(eeguana:::browse_app(data_faces_10_trials), {
    session$setInputs(mode = "continuous", unit_continuous = "s", segment = "1", start = 10, length = 4)
    session$setInputs(channels = "Fz", scale = "shared")
    expect_equal(window()$focus, 1L)
    session$setInputs(arrow_key = list(key = "m"))
    expect_equal(selected_segments(), integer(0))
    expect_equal(result(), integer(0))
  })
})

test_that("the recording is in the title, and the buttons move to the others", {
  two <- bind(seg, eeg_mutate(seg, .recording = "second"))
  shiny::testServer(eeguana:::browse_app(two), {
    session$setInputs(mode = "continuous", unit_continuous = "ms", start = 0, length = 200)
    session$setInputs(channels = "Fz", scale = "shared")
    expect_equal(output$recording_name, rec)
    session$setInputs(next_recording = 1)
    expect_equal(current_rec(), "second")
    expect_equal(prep()$recording, "second")
    expect_equal(output$recording_name, "second")
    ## it stops at the last one
    session$setInputs(next_recording = 2)
    expect_equal(current_rec(), "second")
    session$setInputs(prev_recording = 1)
    expect_equal(current_rec(), rec)
    session$setInputs(recording = "second")
    expect_equal(current_rec(), "second")
  })
  ## with one recording, it is in the title too
  shiny::testServer(eeguana:::browse_app(seg), {
    expect_equal(output$recording_name, rec)
  })
})

test_that("the message at the end lists the segments and how to keep or remove them", {
  msg <- eeguana:::segment_selection_message("seg", c(2L, 5L))
  lines <- strsplit(msg, "\n")[[1]]
  expect_equal(lines, c(
    "Segments selected: 2, 5",
    "To keep only these segments: eeg_filter(seg, .id %in% c(2, 5))",
    "To remove them: eeg_filter(seg, !.id %in% c(2, 5))"
  ))
  run <- function(line) eval(parse(text = sub("^[^:]*: ", "", line)))
  expect_equal(unique(run(lines[2])$.segments$.id), c(2L, 5L))
  expect_equal(nrow(run(lines[3])$.segments), nrow(seg$.segments) - 2)
  expect_equal(eeguana:::segment_selection_message("seg", integer(0)), "No segments were selected.")
})

test_that("eeg_browse() returns the selection of the app and prints how to use it", {
  ## the viewer, which would open the app in a browser, closes it right away
  ## with a selection, as clicking "Done" would
  closing_with <- function(selected) function(url) later::later(function() shiny::stopApp(selected), 0)
  expect_message(
    selected <- eeg_browse(seg, .viewer = closing_with(c(2L, 5L))),
    "Segments selected: 2, 5\nTo keep only these segments: eeg_filter(seg, .id %in% c(2, 5))",
    fixed = TRUE
  )
  expect_equal(selected, c(2L, 5L))
  expect_message(
    selected <- eeg_browse(ica_seg, .viewer = closing_with(stats::setNames(list("ICA1"), rec))),
    "Components selected: ICA1\nTo keep only these components: eeg_ica_keep(ica_seg, c(ICA1))\nTo remove them: eeg_ica_keep(ica_seg, -c(ICA1))",
    fixed = TRUE
  )
  expect_equal(selected[[1]], "ICA1")
})
