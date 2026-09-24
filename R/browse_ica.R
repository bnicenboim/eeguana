#' Browse the components of an ICA interactively
#'
#' `browse_ica()` opens a Shiny app to decide which independent components to
#' remove. It shows the activations of the components over a window of the
#' recording, together with the EOG channels or any other channels, and the
#' topography of each component, labeled with the proportion of the variance
#' of the channels it explains (see [eeg_ica_var_tbl()]) and its correlation
#' with the EOG channels (see [eeg_ica_cor_tbl()]).
#'
#' The window can be placed around events and moved from one event to the
#' next, or it can be moved through the recording. The events are chosen by
#' their type or their description: the ones that are equal to some values,
#' start with, end with, or contain some text, or match a regular expression.
#' For example, the descriptions that start with "peak" are the blinks found
#' with [eeg_artif_peak()]. The limits of the window can be set in samples,
#' milliseconds, or seconds. The left and right arrow keys move to the previous
#' and next window, and the up and down arrow keys zoom the amplitudes in and
#' out.
#'
#' The amplitudes are shown in units of their typical standard deviation, the
#' median of their standard deviations in stretches of one second of the whole
#' recording, so a window with a blink and one without are drawn at the same
#' scale, and slow drifts do not flatten the traces. By default, all the components share one scale, and all the
#' channels another, so their sizes can be compared; each trace can also be
#' drawn at its own scale, which makes the small ones visible.
#'
#' Clicking a topography marks the component for removal, and clicking it
#' again unmarks it. "Done" closes the app and returns the marked components;
#' closing the window returns them as well.
#'
#' The EOG channels are filtered before they are correlated with the
#' components, because slow drifts and offsets can hide how closely they
#' follow a component. In the intro vignette, the blink component correlates
#' 0.31 with the unfiltered VEOG and 0.91 with the filtered one. The filter
#' only affects the correlations: the channels are shown unfiltered.
#'
#' The app needs the packages shiny and bslib.
#'
#' @param data An `eeg_ica_lst` object, created with [eeg_ica()].
#' @param .eog Names of the EOG channels, used for the correlations and shown
#'   under the components when the app opens. By default, the channels whose
#'   names start or end with "eog", ignoring case.
#' @param .eog_freq Cutoff frequencies in Hz of the filter applied to the EOG
#'   channels before correlating them with the components: a band-pass filter
#'   from 0.1 to 30 Hz by default. With `NA` as the first value, it is only a
#'   low-pass filter, and with `NA` as the second, only a high-pass filter.
#'   `NULL` does not filter. It can also be changed in the app.
#' @param .n_components Number of components shown when the app opens, in the
#'   order of the variance they explain.
#' @param .viewer Where the app opens, see [shiny::viewer]. By default, in the
#'   browser.
#' @family ICA functions
#' @family plotting functions
#'
#' @return Invisibly, a list with one element per recording, holding the names
#'   of the components marked for removal. A message shows the call to
#'   [eeg_ica_keep()] that removes them.
#' @examples
#' if (interactive()) {
#'   ica <- eeg_ica(data_faces_10_trials, -EOGH, -EOGV, -M1, -M2,
#'     .method = fast_ICA
#'   )
#'   removed <- browse_ica(ica)
#'   clean <- eeg_ica_keep(ica, -tidyselect::all_of(removed[[1]]))
#' }
#' @export
browse_ica <- function(data, .eog = NULL, .eog_freq = c(.1, 30), .n_components = 16, .viewer = NULL) {
  rlang::check_installed(c("shiny", "bslib"), reason = "to use `browse_ica()`.")
  name <- rlang::as_label(rlang::enexpr(data))
  app <- browse_ica_app(data, .eog = .eog, .eog_freq = .eog_freq, .n_components = .n_components)
  if (is.null(.viewer)) .viewer <- shiny::browserViewer()
  removed <- shiny::runGadget(app, viewer = .viewer, stopOnCancel = FALSE)
  message(ica_keep_code(name, removed))
  invisible(removed)
}

#' The Shiny app behind browse_ica(), separate so that it can be tested
#' @noRd
browse_ica_app <- function(data, .eog = NULL, .eog_freq = c(.1, 30), .n_components = 16) {
  if (!inherits(data, "eeg_ica_lst")) {
    stop("`data` must be an eeg_ica_lst, created with `eeg_ica()`.", call. = FALSE)
  }
  if (!is.null(.eog_freq) &&
    (length(.eog_freq) != 2 || !(is.numeric(.eog_freq) || all(is.na(.eog_freq))))) {
    stop("`.eog_freq` must be NULL or two cutoff frequencies, one of them can be NA.", call. = FALSE)
  }
  recs <- names(data$.ica)
  if (is.null(.eog)) {
    .eog <- grep("(^eog)|(eog$)", channel_names(data), ignore.case = TRUE, value = TRUE)
  } else if (!all(.eog %in% channel_names(data))) {
    stop("Channels not found: ", toString(setdiff(.eog, channel_names(data))), call. = FALSE)
  }
  srate <- sampling_rate(data)
  units <- c("s" = "s", "ms" = "ms", "samples" = "samples")
  matches <- c(
    "is one of" = "exact", "starts with" = "starts", "ends with" = "ends",
    "contains" = "contains", "matches the regex" = "regex"
  )
  ## the inputs of a row share its width, unless they say otherwise
  row <- function(...) shiny::div(class = "d-flex gap-2 browse-ica-row", ...)
  wide <- function(...) shiny::div(style = "flex: 3 1 0;", ...)
  ## the window starts as 2 s on each side of the events, or the first 4 s
  default_from <- -2 * srate
  default_to <- 2 * srate
  default_length <- 4 * srate

  ui <- bslib::page_sidebar(
    title = shiny::div(
      class = "d-flex w-100 align-items-center gap-3",
      shiny::span("ICA components"),
      shiny::span(class = "text-muted small text-truncate", shiny::textOutput("where", inline = TRUE)),
      shiny::actionButton("done", "Done", class = "btn-primary ms-auto")
    ),
    shiny::tags$script(shiny::HTML(browse_ica_keys_js)),
    shiny::tags$style(shiny::HTML(browse_ica_css)),
    sidebar = bslib::sidebar(
      width = 360,
      if (length(recs) > 1) shiny::selectInput("recording", "Recording", recs),
      bslib::accordion(
        multiple = TRUE,
        bslib::accordion_panel(
          "Window",
          shiny::radioButtons("mode", NULL,
            c("Around events" = "events", "Through the recording" = "continuous"),
            inline = TRUE
          ),
          shiny::conditionalPanel(
            "input.mode == 'events'",
            row(
              shiny::selectInput("event_field", "Events whose",
                c("description" = ".description", "type" = ".type")
              ),
              shiny::selectInput("event_match", "", matches)
            ),
            shiny::conditionalPanel(
              "input.event_match == 'exact'",
              shiny::selectizeInput("event_values", NULL, NULL, multiple = TRUE)
            ),
            shiny::conditionalPanel(
              "input.event_match != 'exact'",
              shiny::textInput("event_pattern", NULL, placeholder = "text to match")
            ),
            shiny::div(class = "small text-muted mb-2 browse-ica-wrap", shiny::textOutput("n_events")),
            row(
              shiny::numericInput("from", "From", -2),
              shiny::numericInput("to", "To", 2),
              shiny::selectInput("unit", "Unit", units)
            ),
            row(
              wide(shiny::sliderInput("event_slider", "Event", min = 1, max = 1, value = 1, step = 1, ticks = FALSE)),
              shiny::numericInput("event_i", "Number", 1, min = 1, max = 1, step = 1)
            )
          ),
          shiny::conditionalPanel(
            "input.mode == 'continuous'",
            shiny::selectInput("segment", "Segment (.id)", NULL),
            row(
              shiny::numericInput("start", "Start", 0),
              shiny::numericInput("length", "Length", 4, min = 0),
              shiny::selectInput("unit_continuous", "Unit", units)
            ),
            shiny::sliderInput("start_slider", "Start", min = 0, max = 1, value = 0, ticks = FALSE)
          ),
          row(
            shiny::actionButton("prev", "\u2190 Previous"),
            shiny::actionButton("next", "Next \u2192")
          ),
          shiny::div(
            class = "d-flex gap-2 align-items-center mt-2",
            shiny::span("Amplitude"),
            shiny::actionButton("zoom_out", "\u2212", class = "btn-sm"),
            shiny::actionButton("zoom_in", "+", class = "btn-sm"),
            shiny::span("\u00d7"),
            shiny::div(
              class = "browse-ica-zoom",
              shiny::numericInput("zoom", NULL, 1, min = 1 / 64, max = 64, step = .1, width = "90px")
            )
          ),
          shiny::div(
            class = "small text-muted mt-2",
            "Keys: \u2190 \u2192 previous and next window; \u2191 \u2193 zoom the amplitudes in and out."
          )
        ),
        bslib::accordion_panel(
          "Components",
          shiny::radioButtons("order", "Order by",
            c("Variance explained" = "var", "Correlation with EOG" = "cor"),
            inline = TRUE
          ),
          shiny::numericInput("n_components", "Show the first", .n_components, min = 1, step = 1),
          shiny::selectizeInput("components", "Components shown", NULL, multiple = TRUE),
          shiny::selectizeInput("marked", "Marked for removal", NULL, multiple = TRUE)
        ),
        bslib::accordion_panel(
          "EOG correlations",
          shiny::checkboxInput("eog_filter", "Filter the EOG channels before correlating them",
            !is.null(.eog_freq)
          ),
          shiny::conditionalPanel(
            "input.eog_filter",
            row(
              shiny::numericInput("eog_low", "High-pass (Hz)", if (!is.null(.eog_freq)) .eog_freq[1] else NA, min = 0),
              shiny::numericInput("eog_high", "Low-pass (Hz)", if (!is.null(.eog_freq)) .eog_freq[2] else NA, min = 0)
            ),
            shiny::div(class = "small text-muted", "Leave one empty to filter only on the other side.")
          )
        ),
        bslib::accordion_panel(
          "Display",
          shiny::selectizeInput("channels", "Channels shown", channel_names(data),
            selected = .eog, multiple = TRUE
          ),
          shiny::radioButtons("scale", "Amplitude scale",
            c(
              "Shared: one for the components, one for the channels" = "shared",
              "Separate: each trace fills its row" = "each"
            )
          ),
          shiny::div(
            class = "small text-muted mb-3",
            "Shared: a larger component looks larger, so the components can be compared
            with each other, and so can the channels. Separate: each trace is scaled to
            its own typical size, so even small ones are visible. The typical size of a
            trace is its typical SD, the median of its standard deviations in stretches
            of 1 s of the whole recording, so the scale is the same in every window."
          ),
          shiny::checkboxInput("electrodes", "Electrode labels on the topographies", FALSE)
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between gap-2",
          "Activations",
          shiny::span(class = "small text-muted", shiny::textOutput("scale_text", inline = TRUE))
        ),
        bslib::card_body(
          class = "browse-ica-scroll",
          shiny::plotOutput("activations", height = "100%")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Topographies: click one to mark it for removal"),
        bslib::card_body(
          class = "browse-ica-scroll",
          shiny::plotOutput("topographies", height = "100%", click = "topo_click")
        )
      )
    )
  )

  server <- function(input, output, session) {
    ## the summaries and topographies of each recording are computed once, the
    ## first time the recording is shown, and the correlations once for each
    ## filter of the EOG channels
    cache <- new.env(parent = emptyenv())
    prep <- shiny::reactive({
      rec <- input$recording %||% recs[1]
      if (is.null(cache[[rec]])) {
        shiny::withProgress(
          message = paste0("Preparing ", rec, "..."),
          cache[[rec]] <- browse_ica_prep(data, rec)
        )
      }
      cache[[rec]]
    })
    eog_freq <- shiny::debounce(shiny::reactive({
      if (!isTRUE(input$eog_filter %||% !is.null(.eog_freq))) {
        return(NULL)
      }
      freq <- c(input$eog_low %||% .eog_freq[1], input$eog_high %||% .eog_freq[2])
      freq <- as.numeric(freq)
      if (all(is.na(freq))) NULL else freq
    }), 800)
    summaries <- shiny::reactive({
      p <- prep()
      freq <- eog_freq()
      key <- paste(p$recording, toString(freq))
      if (is.null(cache[[key]])) {
        shiny::withProgress(
          message = "Correlating the components with the EOG channels...",
          cache[[key]] <- browse_ica_summaries(p, .eog, freq)
        )
      }
      cache[[key]]
    })
    ## redrawing waits until the components or channels stop changing
    components <- shiny::debounce(shiny::reactive(input$components), 1000)
    channels <- shiny::debounce(shiny::reactive(input$channels), 1000)
    marks <- shiny::reactiveVal(stats::setNames(rep(list(character(0)), length(recs)), recs))

    ## The window is kept here, in samples, and the fields show it in the
    ## chosen unit. The fields change it, and when it had to be corrected, for
    ## example when it went past the end of a segment, the fields are updated
    ## to show what is displayed.
    unit <- shiny::reactiveVal("s")
    from <- shiny::reactiveVal(default_from)
    to <- shiny::reactiveVal(default_to)
    event_i <- shiny::reactiveVal(1)
    continuous <- shiny::reactiveVal(NULL)
    zoom <- shiny::reactiveVal(1)

    ## the unit is changed first, so that the other fields changed at the same
    ## time are read in the new unit
    set_unit <- function(new) {
      if (!is.null(new) && !identical(new, unit())) {
        unit(new)
        shiny::updateSelectInput(session, "unit", selected = new)
        shiny::updateSelectInput(session, "unit_continuous", selected = new)
      }
    }
    shiny::observeEvent(input$unit, set_unit(input$unit))
    shiny::observeEvent(input$unit_continuous, set_unit(input$unit_continuous))

    shiny::observeEvent(prep(), {
      p <- prep()
      shiny::updateSelectInput(session, "segment", choices = p$bounds$.id)
      shiny::updateRadioButtons(session, "mode",
        selected = if (nrow(p$events) > 0) "events" else "continuous"
      )
      shiny::updateSelectizeInput(session, "marked",
        choices = p$order_var,
        selected = marks()[[p$recording]]
      )
      continuous(clamp_continuous(p$bounds, p$bounds$.id[1], p$bounds$first[1], default_length))
    })
    shiny::observeEvent(list(prep(), input$event_field), {
      p <- prep()
      field <- input$event_field %||% ".description"
      shiny::updateSelectizeInput(session, "event_values",
        choices = event_choices(p$events, field),
        selected = default_events(p$events, field)
      )
    })

    ## the first components in the chosen order; they can then be edited. The
    ## order by correlation changes with the filter of the EOG channels
    shiny::observeEvent(
      list(prep(), input$order, input$n_components, if (identical(input$order, "cor")) summaries()),
      {
        shiny::req(input$order)
        ord <- summaries()$order[[input$order]]
        n <- if (is.na(input$n_components)) .n_components else input$n_components
        shiny::updateSelectizeInput(session, "components",
          choices = ord, selected = utils::head(ord, n)
        )
      }
    )

    selected_events <- shiny::reactive({
      p <- prep()
      shiny::req(input$event_field, input$event_match)
      if (input$event_match == "exact") {
        shiny::validate(shiny::need(length(input$event_values) > 0, "Choose the events to browse."))
      } else {
        shiny::validate(shiny::need(
          nzchar(input$event_pattern %||% ""), "Write the text the events should match."
        ))
      }
      ev <- tryCatch(
        suppressWarnings(match_events(p$events, input$event_field, input$event_match,
          values = input$event_values, pattern = input$event_pattern
        )),
        error = function(e) e
      )
      if (inherits(ev, "error")) {
        shiny::validate(paste("Invalid regular expression:", conditionMessage(ev)))
      }
      shiny::validate(shiny::need(nrow(ev) > 0, "No events match."))
      ev
    })
    ## what the text matched; the values chosen are already in their field
    output$n_events <- shiny::renderText({
      ev <- selected_events()
      n <- paste(nrow(ev), if (nrow(ev) == 1) "event" else "events")
      if (identical(input$event_match, "exact")) {
        return(n)
      }
      labels <- short_labels(ev[[input$event_field]])
      counts <- sort(table(labels), decreasing = TRUE)
      shown <- utils::head(counts, 4)
      paste0(
        n, ": ", paste0(names(shown), " (", shown, ")", collapse = ", "),
        if (length(counts) > length(shown)) ", ..."
      )
    })

    ## the fields of the event: a new set of events starts from the first one,
    ## and the number is kept between 1 and the number of events
    n_events <- shiny::reactive(nrow(selected_events()))
    shiny::observeEvent(selected_events(), {
      event_i(1)
      n <- n_events()
      shiny::updateSliderInput(session, "event_slider", min = 1, max = max(n, 2), value = 1)
      shiny::updateNumericInput(session, "event_i", max = n, value = 1)
    })
    set_event <- function(i) {
      if (is.null(i) || is.na(i)) {
        return()
      }
      event_i(min(max(1, round(i)), n_events()))
    }
    shiny::observeEvent(input$event_i, set_event(input$event_i))
    shiny::observeEvent(input$event_slider, set_event(input$event_slider))
    shiny::observe({
      i <- event_i()
      if (!identical(as.numeric(shiny::isolate(input$event_i)), i)) {
        shiny::updateNumericInput(session, "event_i", value = i)
      }
      if (!identical(as.numeric(shiny::isolate(input$event_slider)), i)) {
        shiny::updateSliderInput(session, "event_slider", value = i)
      }
    })

    shiny::observeEvent(input$from, {
      shiny::req(!is.na(input$from))
      from(duration_to_samples(input$from, unit(), srate))
    })
    shiny::observeEvent(input$to, {
      shiny::req(!is.na(input$to))
      to(duration_to_samples(input$to, unit(), srate))
    })

    ## the fields of the window through the recording
    set_continuous <- function(id = NULL, first = NULL, length = NULL) {
      cur <- continuous()
      shiny::req(cur)
      p <- prep()
      id <- id %||% cur$id
      shiny::req(id %in% p$bounds$.id)
      continuous(clamp_continuous(p$bounds, id, first %||% cur$first, length %||% cur$length))
    }
    shiny::observeEvent(input$segment, {
      shiny::req(nzchar(input$segment))
      set_continuous(id = as.integer(input$segment))
    })
    shiny::observeEvent(input$start, {
      shiny::req(!is.na(input$start))
      set_continuous(first = position_to_sample(input$start, unit(), srate))
    })
    shiny::observeEvent(input$start_slider, {
      set_continuous(first = position_to_sample(input$start_slider, unit(), srate))
    })
    shiny::observeEvent(input$length, {
      shiny::req(!is.na(input$length), input$length > 0)
      set_continuous(length = max(1, duration_to_samples(input$length, unit(), srate)))
    })

    ## the fields follow the window and the unit
    show <- function(id, value, samples, position = FALSE) {
      shown <- shiny::isolate(input[[id]])
      read <- if (position) position_to_sample else duration_to_samples
      if (is.null(shown) || is.na(shown) || read(shown, unit(), srate) != samples) {
        shiny::updateNumericInput(session, id, value = value)
      }
    }
    shiny::observe({
      u <- unit()
      show("from", signif(from() / scaling(srate, u), 6), from())
      show("to", signif(to() / scaling(srate, u), 6), to())
    })
    shiny::observe({
      w <- continuous()
      shiny::req(w)
      u <- unit()
      b <- prep()$bounds[.id == w$id]
      if (!identical(shiny::isolate(input$segment), as.character(w$id))) {
        shiny::updateSelectInput(session, "segment", selected = w$id)
      }
      show("start", signif(sample_to_position(w$first, u, srate), 6), w$first, position = TRUE)
      show("length", signif(w$length / scaling(srate, u), 6), w$length)
      shiny::updateSliderInput(session, "start_slider",
        min = sample_to_position(b$first, u, srate),
        max = sample_to_position(max(b$first, b$last - w$length + 1), u, srate),
        value = sample_to_position(w$first, u, srate),
        step = 1 / scaling(srate, u)
      )
    })

    window <- shiny::reactive({
      p <- prep()
      u <- unit()
      shiny::req(input$mode)
      if (input$mode == "events") {
        ev <- selected_events()
        i <- min(event_i(), nrow(ev))
        event_window(p$bounds, ev[i], from = from(), to = to()) %>%
          c(list(
            at_start = i == 1, at_end = i == nrow(ev),
            label = sprintf(
              "Event %d of %d: %s \u00b7 %s at %s",
              i, nrow(ev), ev$.type[i], ev$.description[i], format_position(ev$.initial[i], u, srate)
            )
          ))
      } else {
        w <- continuous()
        shiny::req(w)
        b <- p$bounds
        list(
          id = w$id, first = w$first, last = w$first + w$length - 1L, anchor = NA_integer_,
          at_start = w$id == b$.id[1] && w$first <= b$first[1],
          at_end = w$id == b$.id[nrow(b)] && w$first + w$length - 1L >= b$last[nrow(b)],
          label = sprintf(
            "Segment %d, %s to %s", w$id, format_position(w$first, u, srate),
            format_position(w$first + w$length - 1L, u, srate)
          )
        )
      }
    })
    ## the buttons are grayed out at the first and last windows
    shiny::observe({
      w <- window()
      shiny::updateActionButton(session, "prev", disabled = w$at_start)
      shiny::updateActionButton(session, "next", disabled = w$at_end)
    })

    move <- function(step) {
      shiny::req(input$mode)
      if (input$mode == "events") {
        event_i(min(max(1, event_i() + step), n_events()))
      } else {
        w <- continuous()
        shiny::req(w)
        new <- next_window(prep()$bounds, w$id, w$first, w$length, step)
        set_continuous(id = new$id, first = new$first)
      }
    }
    ## the buttons and keys multiply the zoom by the square root of 2, and the
    ## field sets it by hand
    set_zoom <- function(value) {
      if (!is.null(value) && !is.na(value) && value > 0) zoom(min(64, max(1 / 64, value)))
    }
    step_zoom <- function(step) set_zoom(zoom() * sqrt(2)^step)
    shiny::observeEvent(input$prev, move(-1))
    shiny::observeEvent(input$`next`, move(1))
    shiny::observeEvent(input$zoom_in, step_zoom(1))
    shiny::observeEvent(input$zoom_out, step_zoom(-1))
    shiny::observeEvent(input$zoom, {
      set_zoom(input$zoom)
      ## a value out of range shows the multiplier used instead; an empty
      ## field is left alone, it is empty while a new number is typed
      z <- zoom()
      if (!is.na(input$zoom) && abs(input$zoom - z) > 1e-3 * z) {
        shiny::updateNumericInput(session, "zoom", value = signif(z, 3))
      }
    })
    shiny::observeEvent(input$arrow_key, {
      switch(input$arrow_key$key,
        ArrowLeft = move(-1),
        ArrowRight = move(1),
        ArrowUp = step_zoom(1),
        ArrowDown = step_zoom(-1)
      )
    })
    shiny::observe({
      z <- zoom()
      shown <- shiny::isolate(input$zoom)
      if (is.null(shown) || is.na(shown) || abs(shown - z) > 1e-3 * z) {
        shiny::updateNumericInput(session, "zoom", value = signif(z, 3))
      }
    })

    output$where <- shiny::renderText({
      w <- window()
      paste0(if (length(recs) > 1) paste0(prep()$recording, " \u00b7 "), w$label)
    })

    scale <- shiny::reactive({
      amplitude_scale(prep(), components(), channels(), input$scale %||% "shared", zoom())
    })
    output$scale_text <- shiny::renderText(scale()$text)

    n_traces <- shiny::reactive(length(components()) + length(channels()))
    output$activations <- shiny::renderPlot(
      {
        p <- prep()
        w <- window()
        shiny::validate(shiny::need(n_traces() > 0, "Choose components or channels to show."))
        activations_plot(p, w,
          components = components(), channels = channels(),
          marked = marks()[[p$recording]], unit = unit(), srate = srate,
          scale = scale()
        )
      },
      ## the traces fill the card, and it scrolls when there are too many
      height = function() {
        max(session$clientData$output_activations_height %||% 400, 60 + 28 * n_traces())
      }
    )

    topo_size <- shiny::reactive({
      n <- max(1, length(components()))
      ncol <- min(4, n)
      width <- session$clientData$output_topographies_width %||% 500
      panel <- min(floor(width / ncol), 300)
      list(ncol = ncol, width = ncol * panel, height = ceiling(n / ncol) * (panel + 40))
    })
    output$topographies <- shiny::renderPlot(
      {
        p <- prep()
        shiny::validate(shiny::need(length(components()) > 0, "Choose components to show."))
        topographies_plot(p, summaries()$labels, components(),
          marked = marks()[[p$recording]], electrodes = isTRUE(input$electrodes),
          ncol = topo_size()$ncol
        )
      },
      width = function() topo_size()$width,
      height = function() topo_size()$height
    )

    ## the marks of each recording are kept while another one is shown
    shiny::observeEvent(input$marked, ignoreNULL = FALSE, ignoreInit = TRUE, {
      m <- marks()
      m[[prep()$recording]] <- as.character(input$marked)
      marks(m)
    })
    shiny::observeEvent(input$topo_click, {
      comp <- input$topo_click$panelvar1
      shiny::req(comp)
      m <- marks()
      rec <- prep()$recording
      m[[rec]] <- if (comp %in% m[[rec]]) setdiff(m[[rec]], comp) else c(m[[rec]], comp)
      marks(m)
      shiny::updateSelectizeInput(session, "marked", selected = m[[rec]])
    })

    shiny::observeEvent(input$done, shiny::stopApp(marks()))
    session$onSessionEnded(function() shiny::stopApp(shiny::isolate(marks())))
  }

  shiny::shinyApp(ui, server)
}

## Sends the arrow keys to the server, unless the focus is in a field, where
## they move the cursor or change the number
browse_ica_keys_js <- "
document.addEventListener('keydown', function(e) {
  if (['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown'].indexOf(e.key) < 0) return;
  if (e.target.closest('input, textarea, select, [contenteditable], .irs')) return;
  e.preventDefault();
  Shiny.setInputValue('arrow_key', {key: e.key}, {priority: 'event'});
});
"

browse_ica_css <- "
.browse-ica-row > * { flex: 1 1 0; min-width: 0; }
.browse-ica-scroll { overflow-y: auto; }
.browse-ica-wrap { overflow-wrap: anywhere; }
.browse-ica-zoom .form-group { margin-bottom: 0; }
"

#' Everything browse_ica() needs from one recording that depends neither on
#' the window nor on the filter of the EOG channels
#' @noRd
browse_ica_prep <- function(data, rec) {
  one <- eeg_filter(data, .recording == !!rec)
  ica <- one$.ica[[rec]]
  var_tbl <- eeg_ica_var_tbl(one)
  signal <- one$.signal
  chs <- channel_names(one)
  ica_chs <- rownames(ica$unmixing_matrix)
  activations <- scale(as.matrix(signal[, ica_chs, with = FALSE]), scale = FALSE) %*%
    ica$unmixing_matrix
  list(
    recording = rec,
    data = one,
    ica = ica,
    ica_channels = ica_chs,
    signal = signal,
    ids = signal$.id,
    samples = as.integer(signal$.sample),
    bounds = signal[, list(first = as.integer(min(.sample)), last = as.integer(max(.sample))), by = .id],
    ## a copy: as.data.table() returns the same table, and := would change
    ## the events of `data`
    events = data.table::copy(data.table::as.data.table(one$.events))[
      , `:=`(.initial = as.integer(.initial), .final = as.integer(.final))
    ][order(.id, .initial)],
    var = var_tbl,
    order_var = var_tbl$.ICA,
    sd = typical_sd(
      cbind(activations, as.matrix(signal[, chs, with = FALSE])),
      ids = signal$.id, chunk = round(sampling_rate(one))
    ),
    topo = data.table::as.data.table(components_topo_tbl(one))
  )
}

#' The labels of the topographies and the order of the components by their
#' correlation with the EOG channels, filtered with the cutoffs `freq`
#' @noRd
browse_ica_summaries <- function(p, eog, freq) {
  EOG <- NULL
  cor_tbl <- if (length(eog) > 0) {
    p$data %>%
      filter_eog(eog, freq) %>%
      eeg_ica_cor_tbl(tidyselect::all_of(eog))
  } else {
    data.table::data.table(EOG = character(0), .ICA = character(0), cor = numeric(0))
  }
  comps <- colnames(p$ica$unmixing_matrix)
  max_cor <- vapply(comps, function(comp) {
    x <- abs(cor_tbl[as.character(.ICA) == comp]$cor)
    if (length(x) == 0) NA_real_ else max(x)
  }, numeric(1))
  labels <- vapply(comps, function(comp) {
    cors <- cor_tbl[as.character(.ICA) == comp][order(EOG)]
    cors_text <- if (nrow(cors) > 0) {
      paste0(eog_abbreviation(cors$EOG), " ", sprintf("%.2f", cors$cor), collapse = " \u00b7 ")
    }
    paste0(
      comp, " \u00b7 ", sprintf("%.1f%%", 100 * p$var[.ICA == comp]$var),
      if (!is.null(cors_text)) paste0("\n", cors_text)
    )
  }, character(1))
  list(
    cor = cor_tbl,
    labels = labels,
    order = list(
      var = p$order_var,
      cor = if (all(is.na(max_cor))) p$order_var else comps[order(-max_cor)]
    )
  )
}

#' Filters the EOG channels: a band-pass filter with two cutoffs, a high-pass
#' filter when the second is NA, a low-pass filter when the first is NA, and
#' no filter when `freq` is NULL
#' @noRd
filter_eog <- function(data, eog, freq) {
  if (is.null(freq) || all(is.na(freq))) {
    return(data)
  }
  if (!anyNA(freq)) {
    eeg_filt_band_pass(data, tidyselect::all_of(!!eog), .freq = freq)
  } else if (is.na(freq[2])) {
    eeg_filt_high_pass(data, tidyselect::all_of(!!eog), .freq = freq[1])
  } else {
    eeg_filt_low_pass(data, tidyselect::all_of(!!eog), .freq = freq[2])
  }
}

#' The events whose `field` (".description" or ".type") is one of `values`,
#' or starts with, ends with, contains, or matches the regular expression
#' `pattern`
#' @noRd
match_events <- function(events, field, how, values = NULL, pattern = "") {
  x <- events[[field]]
  keep <- switch(how,
    exact = x %in% values,
    starts = startsWith(x, pattern),
    ends = endsWith(x, pattern),
    contains = grepl(pattern, x, fixed = TRUE),
    regex = grepl(pattern, x),
    stop("Unknown way of matching events: ", how, call. = FALSE)
  )
  events[keep %in% TRUE]
}

## Units: as in as_time() and as_sample_int(), time 0 is sample 1, and samples
## are not shifted
duration_to_samples <- function(x, unit, srate) round(x * scaling(srate, unit))
position_to_sample <- function(x, unit, srate) {
  round(x * scaling(srate, unit) + if (unit == "samples") 0 else 1)
}
sample_to_position <- function(s, unit, srate) {
  (as.numeric(s) - if (unit == "samples") 0 else 1) / scaling(srate, unit)
}
format_position <- function(s, unit, srate) {
  paste(signif(sample_to_position(s, unit, srate), 6), unit)
}

#' Keeps a window inside its segment, moving it rather than shrinking it
#' when it fits
#' @noRd
clamp_window <- function(bounds, id, first, last) {
  b <- bounds[bounds$.id == id]
  len <- last - first
  if (first < b$first) {
    first <- b$first
    last <- first + len
  }
  if (last > b$last) {
    last <- b$last
    first <- max(b$first, last - len)
  }
  list(id = id, first = as.integer(first), last = as.integer(last))
}

#' The window around one event; `from` and `to` are in samples relative to its
#' onset
#' @noRd
event_window <- function(bounds, event, from, to) {
  w <- clamp_window(bounds, event$.id, event$.initial + min(from, to), event$.initial + max(from, to))
  c(w, list(anchor = event$.initial))
}

#' The first sample and segment of the next (step = 1) or previous (step = -1)
#' window; past the edge of a segment, it goes to the next or previous
#' segment, and it stops at the edges of the recording
#' @noRd
next_window <- function(bounds, id, first, length, step) {
  i <- match(id, bounds$.id)
  new_first <- first + step * length
  if (new_first > bounds$last[i]) {
    if (i == nrow(bounds)) {
      return(list(id = id, first = first))
    }
    return(list(id = bounds$.id[i + 1], first = bounds$first[i + 1]))
  }
  if (new_first + length - 1 < bounds$first[i]) {
    if (i == 1) {
      return(list(id = id, first = bounds$first[i]))
    }
    return(list(id = bounds$.id[i - 1], first = max(bounds$first[i - 1], bounds$last[i - 1] - length + 1)))
  }
  list(id = id, first = max(bounds$first[i], new_first))
}

#' Rows of the signal table and the activations of the components in a window
#' @noRd
window_tbl <- function(p, w, components, channels) {
  rows <- which(p$ids == w$id & p$samples >= w$first & p$samples <= w$last)
  center <- function(m) sweep(m, 2, colMeans(m, na.rm = TRUE))
  X <- center(as.matrix(p$signal[rows, p$ica_channels, with = FALSE]))
  S <- X %*% p$ica$unmixing_matrix[, components, drop = FALSE]
  Y <- center(as.matrix(p$signal[rows, channels, with = FALSE]))
  values <- cbind(S, Y)
  data.table::data.table(
    .sample = rep(p$samples[rows], ncol(values)),
    .key = factor(rep(colnames(values), each = length(rows)), levels = colnames(values)),
    .kind = rep(c("component", "channel"), c(ncol(S), ncol(Y)) * length(rows)),
    .value = c(values)
  )
}

#' A window through the recording that fits in its segment: no longer than
#' the segment, and moved back inside it when it goes past an edge
#' @noRd
clamp_continuous <- function(bounds, id, first, length) {
  b <- bounds[bounds$.id == id]
  length <- min(max(1, round(length)), b$last - b$first + 1)
  first <- min(max(round(first), b$first), b$last - length + 1)
  list(id = as.integer(id), first = as.integer(first), length = as.integer(length))
}

#' The typical standard deviation of each column of `x`: the median of its
#' standard deviations in stretches of `chunk` samples of each segment. Unlike
#' the standard deviation of the whole recording, it ignores slow drifts, which
#' would flatten unfiltered channels, and rare large events such as blinks
#' @noRd
typical_sd <- function(x, ids, chunk) {
  pos <- stats::ave(seq_along(ids), ids, FUN = seq_along)
  groups <- interaction(ids, (pos - 1) %/% chunk, drop = TRUE)
  apply(x, 2, function(col) {
    sds <- tapply(col, groups, stats::sd, na.rm = TRUE)
    stats::median(sds, na.rm = TRUE)
  })
}

#' How the amplitudes are scaled: each trace is divided by `half` times its
#' reference, so that the panel of each trace spans -1 to 1. The reference is
#' the typical standard deviation over the recording, of each trace or pooled
#' over the components and over the channels
#' @noRd
amplitude_scale <- function(p, components, channels, scale, zoom) {
  keys <- c(components, channels)
  sd <- p$sd[keys]
  sd[!is.finite(sd) | sd == 0] <- 1
  kind <- rep(c("component", "channel"), c(length(components), length(channels)))
  pooled <- tapply(sd, kind, function(x) sqrt(mean(x^2)))
  ref <- if (scale == "shared") as.vector(pooled[kind]) else unname(sd)
  names(ref) <- keys
  half <- 4 / zoom
  fmt <- function(x) format(signif(x, 2), big.mark = ",", scientific = FALSE, drop0trailing = TRUE)
  text <- if (scale == "shared") {
    parts <- c(
      if (length(channels) > 0) paste0("\u00b1", fmt(half * pooled[["channel"]]), " for the channels"),
      if (length(components) > 0) paste0("\u00b1", fmt(half), " typical SDs for the components")
    )
    paste0("Rows span ", paste(parts, collapse = " and "))
  } else {
    paste0("Rows span \u00b1", fmt(half), " times the typical SD of each trace")
  }
  list(ref = ref, half = half, text = text)
}

activations_plot <- function(p, w, components, channels, marked, unit, srate, scale) {
  .x <- .y <- .value <- .key <- .kind <- .marked <- xmin <- xmax <- label <- NULL
  tbl <- window_tbl(p, w, components, channels)
  tbl[, .x := sample_to_position(.sample, unit, srate)]
  tbl[, .y := .value / (scale$ref[as.character(.key)] * scale$half)]
  tbl[, .marked := .key %in% marked]
  keys <- levels(tbl$.key)

  plot <- ggplot2::ggplot(tbl, ggplot2::aes(x = .x, y = .y)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray85")
  ev <- p$events[.id == w$id & .final >= w$first & .initial <= w$last]
  ## events on one channel are drawn on that channel only, and dropped when
  ## the channel is not shown
  ev <- ev[is.na(.channel) | .channel %in% keys]
  types <- sort(unique(ev$.type))
  type_colors <- stats::setNames(rep_len(event_palette, length(types)), types)
  if (nrow(ev) > 0) {
    ev[, `:=`(
      xmin = sample_to_position(pmax(.initial, w$first), unit, srate),
      xmax = sample_to_position(pmin(.final, w$last), unit, srate),
      label = short_description(.description)
    )]
    add_events <- function(plot, ev, layer) {
      everywhere <- ev[is.na(.channel)]
      channel <- ev[!is.na(.channel)][, .key := factor(.channel, levels = keys)]
      if (nrow(everywhere) > 0) plot <- plot + layer(everywhere[, !".channel"])
      if (nrow(channel) > 0) plot <- plot + layer(channel)
      plot
    }
    plot <- add_events(plot, ev[.final > .initial], function(d) {
      ggplot2::geom_rect(
        data = d, ggplot2::aes(xmin = xmin, xmax = xmax, fill = .type),
        ymin = -Inf, ymax = Inf, alpha = .3, inherit.aes = FALSE
      )
    })
    plot <- add_events(plot, ev[.final == .initial], function(d) {
      ggplot2::geom_vline(data = d, ggplot2::aes(xintercept = xmin, color = .type), linewidth = .8)
    })
    ## one label per event, on the top trace, while they are few enough to read
    if (nrow(ev) <= 30) {
      labels <- unique(ev[, list(xmin, label)])[, .key := factor(keys[1], levels = keys)]
      plot <- plot + ggplot2::geom_text(
        data = labels, ggplot2::aes(x = xmin, y = Inf, label = label),
        hjust = -.05, vjust = 1.2, size = 4, color = "gray20", inherit.aes = FALSE
      )
    }
    if (any(ev$.final > ev$.initial)) {
      plot <- plot + ggplot2::scale_fill_manual(values = type_colors, breaks = types, name = NULL)
    }
  }
  if (!is.na(w$anchor)) {
    plot <- plot + ggplot2::geom_vline(
      xintercept = sample_to_position(w$anchor, unit, srate), linetype = "dashed"
    )
  }
  ## the lines of the traces and the events share the color scale; traces
  ## beyond their panel are clipped, zooming out shows them
  plot +
    ggplot2::geom_line(ggplot2::aes(color = ifelse(.marked, "marked", .kind)), linewidth = .45) +
    ggplot2::scale_color_manual(
      values = c(component = "black", channel = "#1f5fa8", marked = "#c0392b", type_colors),
      breaks = types, name = NULL
    ) +
    ggplot2::facet_grid(.key ~ ., switch = "y") +
    ggplot2::coord_cartesian(ylim = c(-1, 1), expand = FALSE) +
    ggplot2::scale_x_continuous(if (unit == "samples") "Sample" else paste0("Time (", unit, ")")) +
    ggplot2::labs(y = NULL) +
    theme_eeguana() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      strip.text.y.left = ggplot2::element_text(angle = 0, size = 13),
      strip.placement = "outside",
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(0, "lines"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 13)
    )
}

## colors of the event types, strong enough to see behind the traces
## (Okabe and Ito's palette, without the black)
event_palette <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#D55E00", "#F0E442", "#999999")

topographies_plot <- function(p, labels, components, marked, electrodes, ncol = 4) {
  .ICA <- NULL
  topo <- p$topo[as.character(.ICA) %in% components]
  topo[, .ICA := factor(as.character(.ICA), levels = components)]
  labels <- labels[components]
  is_marked <- components %in% marked
  labels[is_marked] <- paste("\u2715", labels[is_marked])
  plot <- plot_topo(topo) +
    annotate_head() +
    ggplot2::geom_contour(color = "gray40", linewidth = .3) +
    ggplot2::facet_wrap(~.ICA, ncol = ncol, labeller = ggplot2::as_labeller(labels)) +
    ## heads stay round, whatever the size of the plot
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(size = 12))
  if (electrodes) plot <- plot + annotate_electrodes(color = "black", size = 3)
  if (any(is_marked)) {
    plot <- plot + ggplot2::geom_rect(
      data = data.frame(.ICA = factor(components[is_marked], levels = components)),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = NA, color = "#c0392b", linewidth = 1.5, inherit.aes = FALSE
    )
  }
  plot
}

#' Choices of event descriptions grouped by type, or of event types, with how
#' many events there are of each
#' @noRd
event_choices <- function(events, field = ".description") {
  .type <- .description <- NULL
  if (nrow(events) == 0) {
    return(character(0))
  }
  if (field == ".type") {
    counts <- events[, list(n = .N), by = .type][order(.type)]
    return(stats::setNames(counts$.type, paste0(counts$.type, " (", counts$n, ")")))
  }
  counts <- events[, list(n = .N), by = list(.type, .description)][order(.type, .description)]
  labels <- stats::setNames(short_labels(counts$.description), counts$.description)
  lapply(split(counts, by = ".type"), function(d) {
    stats::setNames(d$.description, paste0(labels[d$.description], " (", d$n, ")"))
  })
}

#' The blinks from eeg_artif_peak() when there are, or else the first
#' artifacts, or else the first events; for types, the artifacts or else the
#' first type
#' @noRd
default_events <- function(events, field = ".description") {
  if (nrow(events) == 0) {
    return(character(0))
  }
  if (field == ".type") {
    types <- unique(events$.type)
    return(if ("artifact" %in% types) "artifact" else types[1])
  }
  desc <- unique(events$.description)
  peaks <- grep("^peak", desc, value = TRUE)
  if (length(peaks) > 0) {
    return(peaks)
  }
  artifacts <- unique(events$.description[events$.type == "artifact"])
  if (length(artifacts) > 0) artifacts[1] else desc[1]
}

#' The artifact functions describe their events as "peak_threshold=100_...",
#' only the part before the settings is shown
#' @noRd
short_description <- function(x) sub("_.*$", "", x)

#' Short descriptions, except where two different descriptions would get the
#' same one, for example peaks found with two thresholds
#' @noRd
short_labels <- function(x) {
  short <- short_description(x)
  clash <- tapply(x, short, function(d) length(unique(d)) > 1)
  as.vector(ifelse(clash[short], x, short))
}

#' "VEOG" and "EOGV" become "V"; other names stay as they are
#' @noRd
eog_abbreviation <- function(x) {
  short <- gsub("eog", "", x, ignore.case = TRUE)
  ifelse(nchar(short) == 0, x, short)
}

#' The call to eeg_ica_keep() that removes the marked components
#' @noRd
ica_keep_code <- function(name, removed) {
  one_recording <- length(removed) == 1
  removed <- removed[lengths(removed) > 0]
  if (length(removed) == 0) {
    return("No components were marked for removal.")
  }
  sel <- vapply(removed, function(x) paste0("-c(", paste(x, collapse = ", "), ")"), character(1))
  args <- if (one_recording) {
    sel
  } else {
    paste0("`", names(removed), "` = ", sel, collapse = ", ")
  }
  paste0("To remove the marked components:\neeg_ica_keep(", name, ", ", args, ")")
}
