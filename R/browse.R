#' Browse the signal or the ICA components interactively
#'
#' `eeg_browse()` opens a Shiny app to look through the data. For an
#' `eeg_lst`, it shows the signal of the channels and, on request, the
#' topography of the window, and segments can be selected, for example the
#' ones to remove. For
#' an `eeg_ica_lst`, created with [eeg_ica()], it shows
#' the activations of the components, together with the EOG channels or any
#' other channels, and the topography of each component, labeled with the
#' proportion of the variance of the channels it explains (see
#' [eeg_ica_var_tbl()]) and its correlation with the EOG channels (see
#' [eeg_ica_cor_tbl()]), and components can be selected, for example the ones
#' to remove.
#'
#' The window can be placed around events and moved from one event to the
#' next, or it can be moved through the recording. It goes on from one segment
#' into the next, and each segment is shown in its own column. Through the
#' recording, the start of the window is set relative to the time zero of its
#' segment, or with a slider over the whole recording, with the segments one
#' after the other. With several
#' recordings, the buttons "Previous recording" and "Next recording", or the
#' field, change the recording shown, whose name is in the title. The events are chosen by
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
#' scale, and slow drifts do not flatten the traces. By default, all the
#' channels share one scale, and all the components another, so their sizes
#' can be compared; each trace can also be drawn at its own scale, which makes
#' the small ones visible. Large amplitudes go on into the rows of the traces
#' around them, unless the option to cut the traces at the edges of their rows
#' is on.
#'
#' The app needs the packages shiny and bslib.
#'
#' @section Selecting segments of an `eeg_lst`:
#' Clicking a segment on the signal selects it, and clicking it again
#' deselects it; M and the "Select segment" button do the same for the segment
#' of the event, or for the first segment shown. Selected segments are tinted
#' red. "Done" closes the app and returns the `.id` of the selected segments;
#' closing the window returns them as well. What to do with them is up to
#' [eeg_filter()]: keeping only them, or removing them. Only segmented data
#' can be selected: when each recording is a single segment, as before
#' [eeg_segment()], the app is only for browsing.
#'
#' The topography is in a sidebar on the right, closed at first, and it is
#' computed only while it is open. It shows the mean of each channel over the
#' part of the window in each segment, one head per segment; a box shows one
#' head for all the segments of the window, their mean, instead. It needs the
#' positions of the electrodes, see [channels_tbl()].
#'
#' @section Selecting ICA components:
#' Clicking a topography selects the component, and clicking it again
#' deselects it. "Done" closes the app and returns the selected components;
#' closing the window returns them as well. What to do with them is up to
#' [eeg_ica_keep()]: keeping only them, or removing them.
#'
#' The EOG channels are filtered before they are correlated with the
#' components, because slow drifts and offsets can hide how closely they
#' follow a component. In the intro vignette, the blink component correlates
#' 0.31 with the unfiltered VEOG and 0.91 with the filtered one. The filter
#' only affects the correlations: the channels are shown unfiltered.
#'
#' @param .data An `eeg_lst`, or an `eeg_ica_lst` created with [eeg_ica()].
#' @param ... For an `eeg_lst`, the channels shown when the app opens, all of
#'   them by default. Not used for an `eeg_ica_lst`.
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
#' @return For an `eeg_lst`, invisibly, the `.id` of the segments selected. A
#'   message lists them, and shows the calls to [eeg_filter()] that keep only
#'   them or remove them.
#'
#'   For an `eeg_ica_lst`, invisibly, a list with one element per recording,
#'   holding the names of the components selected. A message lists them, and
#'   shows the calls to [eeg_ica_keep()] that keep only them or remove them.
#' @examples
#' if (interactive()) {
#'   ## select the segments to remove
#'   segs <- eeg_segment(data_faces_10_trials, .description %in% c("s70", "s71"),
#'     .lim = c(-.2, .5)
#'   )
#'   to_remove <- eeg_browse(segs)
#'   clean <- eeg_filter(segs, !.id %in% to_remove)
#'
#'   ## select the components to remove
#'   ica <- eeg_ica(data_faces_10_trials, -EOGH, -EOGV, -M1, -M2,
#'     .method = fast_ICA
#'   )
#'   to_remove <- eeg_browse(ica)
#'   clean <- eeg_ica_keep(ica, -tidyselect::all_of(to_remove[[1]]))
#' }
#' @export
eeg_browse <- function(.data, ...) {
  UseMethod("eeg_browse")
}

#' @rdname eeg_browse
#' @export
eeg_browse.eeg_lst <- function(.data, ..., .viewer = NULL) {
  ## the value of .data is already evaluated by UseMethod(), its expression is not
  name <- rlang::as_label(substitute(.data))
  app <- browse_app(.data, .kind = "eeg", .channels = unname(sel_ch(.data, ...)))
  selected <- run_browse_app(app, .viewer)
  message(segment_selection_message(name, selected))
  invisible(selected)
}

#' @rdname eeg_browse
#' @export
eeg_browse.eeg_ica_lst <- function(.data, ..., .eog = NULL, .eog_freq = c(.1, 30),
                                   .n_components = 16, .viewer = NULL) {
  ## the value of .data is already evaluated by UseMethod(), its expression is not
  name <- rlang::as_label(substitute(.data))
  app <- browse_app(.data,
    .kind = "ica", .eog = .eog, .eog_freq = .eog_freq, .n_components = .n_components
  )
  selected <- run_browse_app(app, .viewer)
  message(ica_selection_message(name, selected))
  invisible(selected)
}

#' Runs the app until "Done" is clicked or the window is closed
#' @noRd
run_browse_app <- function(app, viewer) {
  rlang::check_installed(c("shiny", "bslib"), reason = "to use `eeg_browse()`.")
  if (is.null(viewer)) viewer <- shiny::browserViewer()
  shiny::runGadget(app, viewer = viewer, stopOnCancel = FALSE)
}

#' The Shiny app behind eeg_browse(), separate so that it can be tested. With
#' `.kind = "eeg"` it shows `.channels` and marks segments; with `.kind =
#' "ica"`, it shows the components and selects them
#' @noRd
browse_app <- function(data, .kind = c("eeg", "ica"), .channels = NULL, .eog = NULL,
                       .eog_freq = c(.1, 30), .n_components = 16) {
  .kind <- match.arg(.kind)
  is_ica <- .kind == "ica"
  if (is_ica && !inherits(data, "eeg_ica_lst")) {
    stop("`data` must be an eeg_ica_lst, created with `eeg_ica()`.", call. = FALSE)
  }
  if (!inherits(data, "eeg_lst")) {
    stop("`data` must be an eeg_lst.", call. = FALSE)
  }
  if (!is.null(.eog_freq) &&
    (length(.eog_freq) != 2 || !(is.numeric(.eog_freq) || all(is.na(.eog_freq))))) {
    stop("`.eog_freq` must be NULL or two cutoff frequencies, one of them can be NA.", call. = FALSE)
  }
  recs <- if (is_ica) names(data$.ica) else unique(data$.segments$.recording)
  if (is.null(.eog)) {
    .eog <- grep("(^eog)|(eog$)", channel_names(data), ignore.case = TRUE, value = TRUE)
  } else if (!all(.eog %in% channel_names(data))) {
    stop("Channels not found: ", toString(setdiff(.eog, channel_names(data))), call. = FALSE)
  }
  if (is.null(.channels)) .channels <- if (is_ica) .eog else channel_names(data)
  ## segments can be selected only when there is more than one per recording
  can_select_segments <- !is_ica && nrow(data$.segments) > length(recs)
  srate <- sampling_rate(data)
  units <- c("s" = "s", "ms" = "ms", "samples" = "samples")
  matches <- c(
    "is one of" = "exact", "starts with" = "starts", "ends with" = "ends",
    "contains" = "contains", "matches the regex" = "regex"
  )
  ## the inputs of a row share its width, unless they say otherwise
  row <- function(...) shiny::div(class = "d-flex gap-2 browse-row", ...)
  wide <- function(...) shiny::div(style = "flex: 3 1 0;", ...)
  ## the window starts as 2 s on each side of the events, or the first 4 s
  default_from <- -2 * srate
  default_to <- 2 * srate
  default_length <- 4 * srate

  window_panel <- bslib::accordion_panel(
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
      shiny::div(class = "small text-muted mb-2 browse-wrap", shiny::textOutput("n_events")),
      row(
        shiny::numericInput("from", tip_label("From", "Where the window starts, relative to the onset of the event."), -2),
        shiny::numericInput("to", tip_label("To", "Where the window ends, relative to the onset of the event."), 2),
        shiny::selectInput("unit", "Unit", units)
      ),
      row(
        wide(shiny::sliderInput("event_slider", "Event", min = 1, max = 1, value = 1, step = 1, ticks = FALSE)),
        shiny::numericInput("event_i", "Number", 1, min = 1, max = 1, step = 1)
      )
    ),
    shiny::conditionalPanel(
      "input.mode == 'continuous'",
      shiny::selectInput("segment", tip_label("Segment (.id)", "The segment where the window starts."), NULL),
      row(
        shiny::numericInput("start", tip_label(
          "Start",
          "Where the window starts, relative to the time zero of its segment: the event it was
          segmented around, or the beginning of an unsegmented recording."
        ), 0),
        shiny::numericInput("length", tip_label(
          "Length",
          "How long the window is. It can be longer than a segment, and then it goes on into the
          next ones."
        ), 4, min = 0),
        shiny::selectInput("unit_continuous", "Unit", units)
      ),
      shiny::sliderInput("position_slider", tip_label(
        "Position in the recording",
        "Where the window starts, counting the samples of all the segments of the recording one
        after the other, from the beginning of the first one."
      ), min = 0, max = 1, value = 0, ticks = FALSE)
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
        class = "browse-zoom",
        shiny::numericInput("zoom", NULL, 1, min = 1 / 64, max = 64, step = .1, width = "90px")
      )
    ),
    shiny::div(
      class = "small text-muted mt-2",
      paste0(
        "Keys: \u2190 \u2192 previous and next window; \u2191 \u2193 zoom the amplitudes in and out",
        if (can_select_segments) "; M selects or deselects the segment of the event, or the first one shown",
        "."
      )
    )
  )
  segments_panel <- if (can_select_segments) {
    bslib::accordion_panel(
      "Segments",
      shiny::actionButton("select_segment", "Select segment", class = "btn-sm mb-2"),
      shiny::selectizeInput("selected_segments", "Selected segments (.id)", NULL, multiple = TRUE),
      shiny::div(
        class = "small text-muted",
        "Click a segment on the signal to select or deselect it. M and the button do it for the
        segment of the event, or the first one shown."
      )
    )
  }
  components_panel <- if (is_ica) {
    bslib::accordion_panel(
      "Components",
      shiny::radioButtons("order", "Order by",
        c("Variance explained" = "var", "Correlation with EOG" = "cor"),
        inline = TRUE
      ),
      shiny::numericInput("n_components", "Show the first", .n_components, min = 1, step = 1),
      shiny::selectizeInput("components", "Components shown", NULL, multiple = TRUE),
      shiny::selectizeInput("selected", "Selected components", NULL, multiple = TRUE)
    )
  }
  eog_panel <- if (is_ica) {
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
    )
  }
  display_panel <- bslib::accordion_panel(
    "Display",
    shiny::selectizeInput("channels", "Channels shown", channel_names(data),
      selected = .channels, multiple = TRUE
    ),
    shiny::radioButtons("scale", "Amplitude scale",
      if (is_ica) {
        c(
          "Shared: one for the components, one for the channels" = "shared",
          "Separate: each trace fills its row" = "each"
        )
      } else {
        c("Shared by all the channels" = "shared", "Separate: each trace fills its row" = "each")
      }
    ),
    shiny::div(
      class = "small text-muted mb-3",
      if (is_ica) {
        "Shared: a larger component looks larger, so the components can be compared
        with each other, and so can the channels."
      } else {
        "Shared: a larger signal looks larger, so the channels can be compared."
      },
      "Separate: each trace is scaled to its own typical size, so even small ones are
      visible. The typical size of a trace is its typical SD, the median of its
      standard deviations in stretches of 1 s of the whole recording, so the scale
      is the same in every window."
    ),
    shiny::checkboxInput("cut", tip_label(
      "Cut the traces at the edges of their rows",
      "Large amplitudes go on into the rows around them. With this, what goes beyond the row of
      a trace is not drawn, so the traces do not overlap."
    ), FALSE),
    if (is_ica) shiny::checkboxInput("electrodes", "Electrode labels on the topographies", FALSE)
  )
  panels <- Filter(Negate(is.null), list(window_panel, segments_panel, components_panel, eog_panel, display_panel))

  traces_card <- bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between gap-2",
      if (is_ica) "Activations" else "Signal",
      shiny::span(class = "small text-muted", shiny::textOutput("scale_text", inline = TRUE))
    ),
    bslib::card_body(
      class = "browse-scroll",
      shiny::plotOutput("activations", height = "100%", click = if (can_select_segments) "trace_click")
    )
  )
  cards <- if (is_ica) {
    bslib::layout_columns(
      col_widths = c(7, 5),
      traces_card,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Topographies: click one to select it"),
        bslib::card_body(
          class = "browse-scroll",
          shiny::plotOutput("topographies", height = "100%", click = "topo_click")
        )
      )
    )
  } else {
    ## the topography is in a sidebar on the right, closed at first; while it
    ## is closed, it is not computed
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = tip_label(
          "Topography",
          "The mean of each channel over the part of the window in each segment. Only the
          channels with positions are used."
        ),
        position = "right", open = FALSE, width = 420,
        if (can_select_segments) {
          shiny::checkboxInput("topo_average", "One head for all the segments shown, their mean", FALSE)
        },
        shiny::checkboxInput("electrodes", "Electrode labels", FALSE),
        shiny::plotOutput("topographies", height = "auto")
      ),
      traces_card
    )
  }

  ui <- bslib::page_sidebar(
    title = shiny::div(
      class = "d-flex w-100 align-items-center gap-3",
      shiny::span(if (is_ica) "ICA components" else "EEG signal"),
      shiny::span(class = "fw-semibold", shiny::textOutput("recording_name", inline = TRUE)),
      shiny::span(class = "text-muted small text-truncate", shiny::textOutput("where", inline = TRUE)),
      shiny::actionButton("done", "Done", class = "btn-primary ms-auto")
    ),
    shiny::tags$script(shiny::HTML(browse_keys_js)),
    shiny::tags$style(shiny::HTML(browse_css)),
    sidebar = bslib::sidebar(
      width = 360,
      if (length(recs) > 1) {
        shiny::div(
          shiny::selectInput("recording", "Recording", recs),
          row(
            shiny::actionButton("prev_recording", "\u2190 Previous recording", class = "btn-sm"),
            shiny::actionButton("next_recording", "Next recording \u2192", class = "btn-sm")
          )
        )
      },
      do.call(bslib::accordion, c(list(multiple = TRUE), panels))
    ),
    cards
  )

  server <- function(input, output, session) {
    ## the summaries and topographies of each recording are computed once, the
    ## first time the recording is shown, and the correlations once for each
    ## filter of the EOG channels
    cache <- new.env(parent = emptyenv())
    ## the recording shown: chosen in the field, or with the buttons
    current_rec <- shiny::reactiveVal(recs[1])
    shiny::observeEvent(input$recording, {
      shiny::req(input$recording %in% recs)
      current_rec(input$recording)
    })
    step_recording <- function(step) {
      i <- match(current_rec(), recs)
      current_rec(recs[min(max(1, i + step), length(recs))])
    }
    shiny::observeEvent(input$prev_recording, step_recording(-1))
    shiny::observeEvent(input$next_recording, step_recording(1))
    shiny::observe({
      rec <- current_rec()
      if (!identical(shiny::isolate(input$recording), rec)) {
        shiny::updateSelectInput(session, "recording", selected = rec)
      }
      shiny::updateActionButton(session, "prev_recording", disabled = rec == recs[1])
      shiny::updateActionButton(session, "next_recording", disabled = rec == recs[length(recs)])
    })
    output$recording_name <- shiny::renderText(current_rec())
    prep <- shiny::reactive({
      rec <- current_rec()
      if (is.null(cache[[rec]])) {
        shiny::withProgress(
          message = paste0("Preparing ", rec, "..."),
          cache[[rec]] <- if (is_ica) browse_ica_prep(data, rec) else browse_eeg_prep(data, rec)
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
    components <- if (is_ica) {
      shiny::debounce(shiny::reactive(input$components), 1000)
    } else {
      shiny::reactive(character(0))
    }
    channels <- shiny::debounce(shiny::reactive(input$channels), 1000)
    ## the components marked in each recording, or the .id of the marked segments
    marks <- shiny::reactiveVal(stats::setNames(rep(list(character(0)), length(recs)), recs))
    selected_segments <- shiny::reactiveVal(integer(0))

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
    shiny::observeEvent(input$unit, priority = 3, set_unit(input$unit))
    shiny::observeEvent(input$unit_continuous, priority = 3, set_unit(input$unit_continuous))

    ## a new recording is set up before anything reads its window
    shiny::observeEvent(prep(), priority = 2, {
      p <- prep()
      shiny::updateSelectInput(session, "segment", choices = p$bounds$.id)
      shiny::updateRadioButtons(session, "mode",
        selected = if (nrow(p$events) > 0) "events" else "continuous"
      )
      if (is_ica) {
        shiny::updateSelectizeInput(session, "selected",
          choices = p$order_var,
          selected = marks()[[p$recording]]
        )
      }
      if (can_select_segments) {
        shiny::updateSelectizeInput(session, "selected_segments",
          choices = data$.segments$.id,
          selected = shiny::isolate(selected_segments())
        )
      }
      continuous(clamp_span(p$bounds, 0, default_length))
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
        shiny::req(is_ica, input$order)
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

    ## A field changed by the server comes back from the browser as a change of
    ## that field, sometimes after newer changes; taken as the user's, it would
    ## move the window back, and the fields would chase each other. The values
    ## sent are kept until they come back, and then ignored.
    sent <- new.env(parent = emptyenv())
    send <- function(id, value, update) {
      sent[[id]] <- c(sent[[id]], list(value))
      update(value)
    }
    came_back <- function(id, value) {
      same <- vapply(sent[[id]], function(v) {
        if (is.numeric(v)) isTRUE(abs(as.numeric(value) - v) <= 1e-8 * max(1, abs(v))) else identical(as.character(v), as.character(value))
      }, logical(1))
      if (!any(same)) {
        return(FALSE)
      }
      ## what was sent before it came back too, or never will
      sent[[id]] <- sent[[id]][-seq_len(max(which(same)))]
      TRUE
    }
    user_input <- function(id) {
      value <- input[[id]]
      !is.null(value) && !came_back(id, value)
    }

    shiny::observeEvent(input$from, {
      shiny::req(user_input("from"), !is.na(input$from))
      from(duration_to_samples(input$from, unit(), srate))
    })
    shiny::observeEvent(input$to, {
      shiny::req(user_input("to"), !is.na(input$to))
      to(duration_to_samples(input$to, unit(), srate))
    })

    ## The window through the recording is kept as the position of its first
    ## sample, counting the samples of all the segments of the recording one
    ## after the other, and its length, so it goes on into the next segments.
    ## The segment and start in the fields are those of its first sample.
    set_continuous <- function(start = NULL, length = NULL) {
      cur <- continuous()
      shiny::req(cur)
      continuous(clamp_span(prep()$bounds, start %||% cur$start, length %||% cur$length))
    }
    start_of <- function() {
      w <- continuous()
      shiny::req(w)
      from_position(prep()$bounds, w$start)
    }
    shiny::observeEvent(input$segment, {
      shiny::req(user_input("segment"), nzchar(input$segment))
      b <- prep()$bounds
      id <- as.integer(input$segment)
      shiny::req(id %in% b$.id)
      if (!identical(id, start_of()$id)) set_continuous(start = to_position(b, id, b$first[b$.id == id]))
    })
    shiny::observeEvent(input$start, {
      shiny::req(user_input("start"), !is.na(input$start))
      set_continuous(start = to_position(prep()$bounds, start_of()$id, position_to_sample(input$start, unit(), srate)))
    })
    ## the slider places the start anywhere in the recording
    shiny::observeEvent(input$position_slider, {
      shiny::req(user_input("position_slider"))
      set_continuous(start = round(input$position_slider * scaling(srate, unit())))
    })
    ## the length is set before the start, which is kept inside the recording
    ## for the length of the window
    shiny::observeEvent(input$length, priority = 1, {
      shiny::req(user_input("length"), !is.na(input$length), input$length > 0)
      set_continuous(length = max(1, duration_to_samples(input$length, unit(), srate)))
    })

    ## the fields follow the window and the unit
    show <- function(id, value, samples, position = FALSE) {
      shown <- shiny::isolate(input[[id]])
      read <- if (position) position_to_sample else duration_to_samples
      if (is.null(shown) || is.na(shown) || read(shown, unit(), srate) != samples) {
        send(id, value, function(v) shiny::updateNumericInput(session, id, value = v))
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
      st <- start_of()
      if (!identical(shiny::isolate(input$segment), as.character(st$id))) {
        send("segment", as.character(st$id), function(v) shiny::updateSelectInput(session, "segment", selected = v))
      }
      show("start", signif(sample_to_position(st$sample, u, srate), 6), st$sample, position = TRUE)
      show("length", signif(w$length / scaling(srate, u), 6), w$length)
    })
    ## The slider spans the whole recording, from the first sample to the last
    ## start that leaves room for the window. Its range is sent only when it
    ## changes, and its value only when it is not the one shown.
    slider_range <- NULL
    shiny::observe({
      w <- continuous()
      shiny::req(w)
      k <- scaling(srate, unit())
      last_start <- segment_positions(prep()$bounds)$total - w$length
      range <- c(last_start, k)
      shown <- shiny::isolate(input$position_slider)
      if (!identical(range, slider_range)) {
        slider_range <<- range
        send("position_slider", w$start / k, function(v) {
          shiny::updateSliderInput(session, "position_slider",
            min = 0, max = max(1, last_start) / k, value = v, step = 1 / k
          )
        })
      } else if (is.null(shown) || round(shown * k) != w$start) {
        send("position_slider", w$start / k, function(v) {
          shiny::updateSliderInput(session, "position_slider", value = v)
        })
      }
    })

    ## the window: the part of each segment it covers, the event it is around,
    ## and the segment that M and the button select (the one of the event, or
    ## the first one shown)
    window <- shiny::reactive({
      p <- prep()
      u <- unit()
      b <- p$bounds
      shiny::req(input$mode)
      if (input$mode == "events") {
        ev <- selected_events()
        i <- min(event_i(), nrow(ev))
        span <- event_span(b, ev[i], from = from(), to = to())
        list(
          span = span, pieces = window_pieces(b, span),
          anchor = list(id = ev$.id[i], sample = ev$.initial[i]), focus = ev$.id[i],
          at_start = i == 1, at_end = i == nrow(ev),
          label = sprintf(
            "Event %d of %d: %s \u00b7 %s at %s",
            i, nrow(ev), ev$.type[i], ev$.description[i], format_position(ev$.initial[i], u, srate)
          )
        )
      } else {
        span <- continuous()
        shiny::req(span)
        pieces <- window_pieces(b, span)
        n <- nrow(pieces)
        list(
          span = span, pieces = pieces, anchor = NULL, focus = pieces$.id[1],
          at_start = span$start <= 0,
          at_end = span$start + span$length >= segment_positions(b)$total,
          label = if (n == 1) {
            sprintf(
              "Segment %d, %s to %s", pieces$.id, format_position(pieces$first, u, srate),
              format_position(pieces$last, u, srate)
            )
          } else {
            sprintf(
              "Segments %d to %d, %s to %s", pieces$.id[1], pieces$.id[n],
              format_position(pieces$first[1], u, srate), format_position(pieces$last[n], u, srate)
            )
          }
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
        set_continuous(start = w$start + step * w$length)
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
        ArrowDown = step_zoom(-1),
        m = if (can_select_segments) toggle_segment()
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
      paste0(
        w$label,
        if (can_select_segments && w$focus %in% selected_segments()) paste(" \u00b7 segment", w$focus, "selected")
      )
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
        shiny::validate(shiny::need(
          n_traces() > 0,
          if (is_ica) "Choose components or channels to show." else "Choose channels to show."
        ))
        activations_plot(p, w,
          components = components(), channels = channels(),
          marked = marks()[[p$recording]], unit = unit(), srate = srate,
          scale = scale(), selected_segments = selected_segments(), cut = isTRUE(input$cut)
        )
      },
      ## the traces fill the card, and it scrolls when there are too many
      height = function() {
        max(session$clientData$output_activations_height %||% 400, 60 + 28 * n_traces())
      }
    )

    ## the heads keep their size, and the card scrolls when there are many
    n_heads <- shiny::reactive({
      if (is_ica) {
        length(components())
      } else if (isTRUE(input$topo_average)) {
        1
      } else {
        nrow(window()$pieces)
      }
    })
    topo_size <- shiny::reactive({
      n <- max(1, n_heads())
      ncol <- min(if (is_ica) 4 else 2, n)
      width <- session$clientData$output_topographies_width %||% 500
      panel <- min(floor(width / ncol), 300)
      list(ncol = ncol, width = ncol * panel, height = ceiling(n / ncol) * (panel + 40) + if (is_ica) 0 else 60)
    })
    if (!is_ica) {
      output$topographies <- shiny::renderPlot(
        {
          p <- prep()
          shiny::validate(shiny::need(
            nrow(p$coords) > 0,
            "The channels have no positions: add a layout to see the topography."
          ))
          window_topo_plot(p, window(),
            average = isTRUE(input$topo_average), selected = selected_segments(),
            electrodes = isTRUE(input$electrodes), ncol = topo_size()$ncol
          )
        },
        width = function() topo_size()$width,
        height = function() topo_size()$height
      )
    }
    if (is_ica) {
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
      shiny::observeEvent(input$selected, ignoreNULL = FALSE, ignoreInit = TRUE, {
        m <- marks()
        m[[prep()$recording]] <- as.character(input$selected)
        marks(m)
      })
      shiny::observeEvent(input$topo_click, {
        comp <- input$topo_click$panelvar1
        shiny::req(comp)
        m <- marks()
        rec <- prep()$recording
        m[[rec]] <- if (comp %in% m[[rec]]) setdiff(m[[rec]], comp) else c(m[[rec]], comp)
        marks(m)
        shiny::updateSelectizeInput(session, "selected", selected = m[[rec]])
      })
    }

    ## a click on a segment selects it, or deselects it; the M key and the
    ## button do it for the segment of the event, or the first one shown; the
    ## field lists the selected segments and can edit them
    toggle_segment <- function(id = window()$focus) {
      m <- selected_segments()
      selected_segments(if (id %in% m) setdiff(m, id) else sort(c(m, id)))
    }
    if (can_select_segments) {
      shiny::observeEvent(input$select_segment, toggle_segment())
      shiny::observeEvent(input$trace_click, toggle_segment(clicked_segment(input$trace_click, window())))
      shiny::observeEvent(input$selected_segments, ignoreNULL = FALSE, ignoreInit = TRUE, {
        selected_segments(sort(as.integer(input$selected_segments)))
      })
      shiny::observe({
        m <- selected_segments()
        if (!setequal(as.integer(shiny::isolate(input$selected_segments)), m)) {
          shiny::updateSelectizeInput(session, "selected_segments", selected = m)
        }
      })
      shiny::observe({
        id <- window()$focus
        shiny::updateActionButton(session, "select_segment",
          label = paste(if (id %in% selected_segments()) "Deselect segment" else "Select segment", id)
        )
      })
    }

    ## what the app returns: the selected components, or the selected segments
    result <- function() if (is_ica) marks() else selected_segments()
    shiny::observeEvent(input$done, shiny::stopApp(result()))
    session$onSessionEnded(function() shiny::stopApp(shiny::isolate(result())))
  }

  shiny::shinyApp(ui, server)
}

## Sends the arrow keys and M to the server, unless the focus is in a field,
## where they move the cursor, change the number, or are typed
browse_keys_js <- "
document.addEventListener('keydown', function(e) {
  var key = e.key === 'M' ? 'm' : e.key;
  if (['ArrowLeft', 'ArrowRight', 'ArrowUp', 'ArrowDown', 'm'].indexOf(key) < 0) return;
  if (e.ctrlKey || e.metaKey || e.altKey) return;
  if (e.target.closest('input, textarea, select, [contenteditable], .irs')) return;
  e.preventDefault();
  Shiny.setInputValue('arrow_key', {key: key}, {priority: 'event'});
});
"

#' A label with an information sign that shows `tip` when the pointer is on it
#' @noRd
tip_label <- function(label, tip) {
  shiny::span(label, bslib::tooltip(shiny::span(class = "browse-tip", "\u24d8"), tip))
}

browse_css <- "
.browse-row > * { flex: 1 1 0; min-width: 0; }
.browse-scroll { overflow-y: auto; }
.browse-wrap { overflow-wrap: anywhere; }
.browse-zoom .form-group { margin-bottom: 0; }
.browse-tip { color: var(--bs-secondary-color); cursor: help; margin-left: .25em; }
"

#' Everything eeg_browse() needs from the signal of one recording, which does
#' not depend on the window: `extra` are more traces, such as the activations
#' of the components, whose typical standard deviations are also needed
#' @noRd
browse_eeg_prep <- function(data, rec, extra = NULL) {
  one <- eeg_filter(data, .recording == !!rec)
  signal <- one$.signal
  chs <- channel_names(one)
  list(
    recording = rec,
    data = one,
    signal = signal,
    ids = signal$.id,
    samples = as.integer(signal$.sample),
    bounds = signal[, list(first = as.integer(min(.sample)), last = as.integer(max(.sample))), by = .id],
    ## a copy: as.data.table() returns the same table, and := would change
    ## the events of `data`
    events = data.table::copy(data.table::as.data.table(one$.events))[
      , `:=`(.initial = as.integer(.initial), .final = as.integer(.final))
    ][order(.id, .initial)],
    sd = typical_sd(
      cbind(extra, as.matrix(signal[, chs, with = FALSE])),
      ids = signal$.id, chunk = round(sampling_rate(one))
    ),
    ## the channels with positions, for the topographies
    coords = data.table::as.data.table(change_coord(channels_tbl(one), "polar"))[
      !is.na(.x) & !is.na(.y), list(.channel, .x, .y)
    ]
  )
}

#' Everything eeg_browse() needs from one recording of an ICA that depends
#' neither on the window nor on the filter of the EOG channels
#' @noRd
browse_ica_prep <- function(data, rec) {
  ica <- data$.ica[[rec]]
  ica_chs <- rownames(ica$unmixing_matrix)
  signal <- data$.signal[.id %in% data$.segments$.id[data$.segments$.recording == rec]]
  activations <- scale(as.matrix(signal[, ica_chs, with = FALSE]), scale = FALSE) %*%
    ica$unmixing_matrix
  p <- browse_eeg_prep(data, rec, extra = activations)
  var_tbl <- eeg_ica_var_tbl(p$data)
  c(p, list(
    ica = ica,
    ica_channels = ica_chs,
    var = var_tbl,
    order_var = var_tbl$.ICA,
    topo = data.table::as.data.table(components_topo_tbl(p$data))
  ))
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

## Windows can go on from one segment into the next: the segments of a
## recording are laid one after the other, and a window is a range of
## positions in that sequence, counted from 0

#' Where each segment starts in the sequence of segments of a recording, and
#' how many samples it has
#' @noRd
segment_positions <- function(bounds) {
  n <- bounds$last - bounds$first + 1L
  list(offset = cumsum(c(0L, utils::head(n, -1L))), n = n, total = sum(n))
}

#' The position of a sample of a segment, and the segment and sample of a
#' position
#' @noRd
to_position <- function(bounds, id, sample) {
  i <- match(id, bounds$.id)
  as.integer(segment_positions(bounds)$offset[i] + sample - bounds$first[i])
}
from_position <- function(bounds, pos) {
  sp <- segment_positions(bounds)
  i <- findInterval(pos, sp$offset)
  list(id = bounds$.id[i], sample = as.integer(bounds$first[i] + pos - sp$offset[i]))
}

#' A window of `length` samples from position `start`, moved back inside the
#' recording when it goes past an edge, and shortened only when the recording
#' is shorter
#' @noRd
clamp_span <- function(bounds, start, length) {
  total <- segment_positions(bounds)$total
  length <- min(max(1, round(length)), total)
  start <- min(max(round(start), 0), total - length)
  list(start = as.integer(start), length = as.integer(length))
}

#' The window around one event: `from` and `to` are in samples relative to its
#' onset, in either order
#' @noRd
event_span <- function(bounds, event, from, to) {
  anchor <- to_position(bounds, event$.id, event$.initial)
  clamp_span(bounds, anchor + min(from, to), abs(to - from) + 1)
}

#' The part of each segment that a window covers
#' @noRd
window_pieces <- function(bounds, span) {
  sp <- segment_positions(bounds)
  start <- span$start
  end <- span$start + span$length - 1L
  seg_end <- sp$offset + sp$n - 1L
  keep <- seg_end >= start & sp$offset <= end
  data.table::data.table(
    .id = bounds$.id[keep],
    first = as.integer(bounds$first[keep] + pmax(start, sp$offset[keep]) - sp$offset[keep]),
    last = as.integer(bounds$first[keep] + pmin(end, seg_end[keep]) - sp$offset[keep])
  )
}

#' The segment of a click on the signal, from the column it fell in, or else
#' the segment of the event, or the first one shown
#' @noRd
clicked_segment <- function(click, w) {
  m <- click$mapping
  for (nm in names(m)) {
    if (identical(m[[nm]], ".id") && !is.null(click[[nm]])) {
      return(as.integer(click[[nm]]))
    }
  }
  w$focus
}

#' Rows of the signal table and the activations of the components in a window,
#' which can span several segments
#' @noRd
window_tbl <- function(p, w, components, channels) {
  pieces <- w$pieces
  rows <- unlist(lapply(seq_len(nrow(pieces)), function(k) {
    which(p$ids == pieces$.id[k] & p$samples >= pieces$first[k] & p$samples <= pieces$last[k])
  }))
  seg <- p$ids[rows]
  ## each segment is centered on its own, as their offsets can differ
  center <- function(m) {
    for (id in unique(seg)) {
      r <- seg == id
      m[r, ] <- sweep(m[r, , drop = FALSE], 2, colMeans(m[r, , drop = FALSE], na.rm = TRUE))
    }
    m
  }
  S <- if (length(components) > 0) {
    X <- center(as.matrix(p$signal[rows, p$ica_channels, with = FALSE]))
    X %*% p$ica$unmixing_matrix[, components, drop = FALSE]
  } else {
    matrix(numeric(0), nrow = length(rows), ncol = 0)
  }
  Y <- center(as.matrix(p$signal[rows, channels, with = FALSE]))
  values <- cbind(S, Y)
  data.table::data.table(
    .id = rep(seg, ncol(values)),
    .sample = rep(p$samples[rows], ncol(values)),
    .key = factor(rep(colnames(values), each = length(rows)), levels = colnames(values)),
    .kind = rep(c("component", "channel"), c(ncol(S), ncol(Y)) * length(rows)),
    .value = c(values)
  )
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

activations_plot <- function(p, w, components, channels, marked, unit, srate, scale,
                             selected_segments = integer(0), cut = FALSE) {
  .x <- .y <- .value <- .key <- .kind <- .marked <- .center <- xmin <- xmax <- label <- x <- NULL
  tbl <- window_tbl(p, w, components, channels)
  keys <- levels(tbl$.key)
  ## All the traces are drawn in one panel, each around its own baseline, the
  ## first on top, which is much faster than one panel per trace. Each trace
  ## has a row from -1 to 1 around its baseline, and larger amplitudes go on
  ## into the rows around it, as in any plot; with `cut`, what goes beyond the
  ## row is not drawn (it is never flattened at the edge).
  centers <- stats::setNames(2 * (rev(seq_along(keys)) - 1), keys)
  tbl[, .x := sample_to_position(.sample, unit, srate)]
  tbl[, .center := centers[as.character(.key)]]
  tbl[, .y := .value / (scale$ref[as.character(.key)] * scale$half) + .center]
  tbl[, .marked := .key %in% marked]
  pieces <- w$pieces
  ## a window over several segments shows each one in its own column
  several <- nrow(pieces) > 1

  plot <- ggplot2::ggplot(tbl, ggplot2::aes(x = .x, y = .y, group = .key))
  ## the selected segments are tinted red
  selected <- intersect(pieces$.id, selected_segments)
  if (length(selected) > 0) {
    plot <- plot + ggplot2::geom_rect(
      data = data.frame(.id = selected), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = "#c0392b", alpha = .08, inherit.aes = FALSE
    )
  }
  plot <- plot + ggplot2::geom_hline(yintercept = centers, color = "gray88")
  ev <- data.table::rbindlist(lapply(seq_len(nrow(pieces)), function(k) {
    piece <- pieces[k]
    p$events[.id == piece$.id & .final >= piece$first & .initial <= piece$last][
      , `:=`(
        xmin = sample_to_position(pmax(.initial, piece$first), unit, srate),
        xmax = sample_to_position(pmin(.final, piece$last), unit, srate)
      )
    ]
  }))
  ## events on one channel are drawn on the band of that channel only, and
  ## dropped when the channel is not shown
  if (nrow(ev) > 0) ev <- ev[is.na(.channel) | .channel %in% keys]
  types <- sort(unique(ev$.type))
  type_colors <- stats::setNames(rep_len(event_palette, length(types)), types)
  if (nrow(ev) > 0) {
    ev[, `:=`(
      label = short_description(.description),
      ymin = ifelse(is.na(.channel), -Inf, centers[.channel] - 1),
      ymax = ifelse(is.na(.channel), Inf, centers[.channel] + 1)
    )]
    long <- ev[.final > .initial]
    if (nrow(long) > 0) {
      plot <- plot + ggplot2::geom_rect(
        data = long, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = .type),
        alpha = .3, inherit.aes = FALSE
      ) +
        ggplot2::scale_fill_manual(values = type_colors, breaks = types, name = NULL)
    }
    point <- ev[.final == .initial]
    if (nrow(point) > 0) {
      plot <- plot + ggplot2::geom_segment(
        data = point, ggplot2::aes(x = xmin, xend = xmin, y = ymin, yend = ymax, color = .type),
        linewidth = .8, inherit.aes = FALSE
      )
    }
    ## one label per event, at the top, while they are few enough to read
    if (nrow(ev) <= 30) {
      labels <- unique(ev[, list(.id, xmin, label)])
      plot <- plot + ggplot2::geom_text(
        data = labels, ggplot2::aes(x = xmin, y = Inf, label = label),
        hjust = -.05, vjust = 1.2, size = 4, color = "gray20", inherit.aes = FALSE
      )
    }
  }
  if (!is.null(w$anchor)) {
    plot <- plot + ggplot2::geom_vline(
      data = data.frame(.id = w$anchor$id, x = sample_to_position(w$anchor$sample, unit, srate)),
      ggplot2::aes(xintercept = x), linetype = "dashed"
    )
  }
  if (several) {
    segment_label <- function(id) paste0(".id ", id, ifelse(as.integer(id) %in% selected_segments, " \u2713", ""))
    plot <- plot + ggplot2::facet_grid(. ~ .id,
      scales = "free_x", space = "free_x",
      labeller = ggplot2::labeller(.id = segment_label)
    )
  }
  ## the lines of the traces and the events share the color scale
  traces <- if (cut) {
    ggplot2::geom_segment(
      data = cut_at_rows(tbl),
      ggplot2::aes(x = .x, y = .y, xend = .xend, yend = .yend, color = ifelse(.marked, "marked", .kind)),
      linewidth = .45
    )
  } else {
    ggplot2::geom_line(ggplot2::aes(color = ifelse(.marked, "marked", .kind)), linewidth = .45)
  }
  plot +
    traces +
    ggplot2::scale_color_manual(
      values = c(component = "black", channel = "#1f5fa8", marked = "#c0392b", type_colors),
      breaks = types, name = NULL
    ) +
    ggplot2::coord_cartesian(ylim = c(-1, max(centers) + 1), expand = FALSE) +
    ggplot2::scale_y_continuous(breaks = centers, labels = names(centers)) +
    ggplot2::scale_x_continuous(
      if (unit == "samples") "Sample" else paste0("Time (", unit, ")"),
      n.breaks = if (several) 3 else NULL
    ) +
    ggplot2::labs(y = NULL) +
    theme_eeguana() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 13, color = "black"),
      axis.ticks.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 11),
      panel.spacing.x = ggplot2::unit(.4, "lines"),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 13)
    )
}

#' The traces as the segments between consecutive samples, cut where they
#' leave the row of their trace, from -1 to 1 around its baseline, so that the
#' lines reach the edge of the row and stop there
#' @noRd
cut_at_rows <- function(tbl) {
  .x <- .y <- .xend <- .yend <- .key <- .id <- .sample <- .center <- NULL
  d <- data.table::copy(tbl)[order(.key, .id, .sample)]
  d[, `:=`(.xend = data.table::shift(.x, -1), .yend = data.table::shift(.y, -1)), by = list(.key, .id)]
  d <- d[!is.na(.xend) & !is.na(.y) & !is.na(.yend)]
  lo <- d$.center - 1
  hi <- d$.center + 1
  keep <- !((d$.y > hi & d$.yend > hi) | (d$.y < lo & d$.yend < lo))
  d <- d[keep]
  lo <- lo[keep]
  hi <- hi[keep]
  ## moves the end `a` along the segment to the edge it went past
  to_edge <- function(xa, ya, xb, yb) {
    for (edge in list(hi, lo)) {
      out <- if (identical(edge, hi)) ya > hi else ya < lo
      t <- (edge[out] - ya[out]) / (yb[out] - ya[out])
      xa[out] <- xa[out] + t * (xb[out] - xa[out])
      ya[out] <- edge[out]
    }
    list(xa, ya)
  }
  start <- to_edge(d$.x, d$.y, d$.xend, d$.yend)
  end <- to_edge(d$.xend, d$.yend, d$.x, d$.y)
  d[, `:=`(.x = start[[1]], .y = start[[2]], .xend = end[[1]], .yend = end[[2]])]
  d
}

#' The topography of the mean of each channel over the part of the window in
#' each segment, or over all of the window with `average`, as long tables
#' interpolated for plot_topo(); only the channels with positions count
#' @noRd
window_topo_tbl <- function(p, w, average = FALSE) {
  .x <- .y <- .channel <- .group <- NULL
  pieces <- w$pieces
  rows <- lapply(seq_len(nrow(pieces)), function(k) {
    which(p$ids == pieces$.id[k] & p$samples >= pieces$first[k] & p$samples <= pieces$last[k])
  })
  names(rows) <- pieces$.id
  if (average) rows <- list(all = unlist(rows))
  chs <- p$coords$.channel
  long <- data.table::rbindlist(lapply(names(rows), function(g) {
    means <- colMeans(as.matrix(p$signal[rows[[g]], chs, with = FALSE]), na.rm = TRUE)
    data.table::data.table(.group = g, .key = chs, .value = unname(means))
  }))
  long <- long[p$coords[, list(.key = .channel, .x, .y)], on = ".key"]
  long[, .group := factor(.group, levels = names(rows))]
  suppressWarnings(eeg_interpolate_tbl(tidytable::group_by(long, .group)))
}

window_topo_plot <- function(p, w, average, selected, electrodes, ncol = 2) {
  .group <- NULL
  topo <- data.table::as.data.table(window_topo_tbl(p, w, average))
  pieces <- w$pieces
  labels <- if (average) {
    c(all = if (nrow(pieces) > 1) {
      paste0("Mean of .id ", pieces$.id[1], " to ", pieces$.id[nrow(pieces)])
    } else {
      paste0(".id ", pieces$.id)
    })
  } else {
    stats::setNames(
      paste0(".id ", pieces$.id, ifelse(pieces$.id %in% selected, " \u2713", "")),
      pieces$.id
    )
  }
  ## all the heads share a scale centered on zero, so they can be compared
  lim <- max(abs(topo$.value), na.rm = TRUE)
  plot <- suppressMessages(
    plot_topo(topo) +
      annotate_head() +
      ggplot2::geom_contour(color = "gray40", linewidth = .3) +
      ggplot2::facet_wrap(~.group, ncol = ncol, labeller = ggplot2::as_labeller(labels)) +
      ggplot2::coord_fixed() +
      ggplot2::scale_fill_distiller(
        type = "div", palette = "RdBu", limits = c(-lim, lim), oob = scales::squish, name = NULL
      ) +
      ggplot2::theme(
        legend.position = "bottom", legend.key.width = ggplot2::unit(2, "lines"),
        strip.text = ggplot2::element_text(size = 12)
      )
  )
  if (electrodes) plot <- plot + annotate_electrodes(color = "black", size = 3)
  if (!average && any(pieces$.id %in% selected)) {
    plot <- plot + ggplot2::geom_rect(
      data = data.frame(.group = factor(intersect(pieces$.id, selected), levels = pieces$.id)),
      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
      fill = NA, color = "#c0392b", linewidth = 1.5, inherit.aes = FALSE
    )
  }
  plot
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
  labels[is_marked] <- paste("\u2713", labels[is_marked])
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

#' The segments selected, and the calls to eeg_filter() that keep only them
#' or remove them
#' @noRd
segment_selection_message <- function(name, selected) {
  if (length(selected) == 0) {
    return("No segments were selected.")
  }
  ids <- paste(selected, collapse = ", ")
  paste0(
    "Segments selected: ", ids, "\n",
    "To keep only these segments: eeg_filter(", name, ", .id %in% c(", ids, "))\n",
    "To remove them: eeg_filter(", name, ", !.id %in% c(", ids, "))"
  )
}

#' The components selected, and the calls to eeg_ica_keep() that keep only
#' them or remove them
#' @noRd
ica_selection_message <- function(name, selected) {
  one_recording <- length(selected) == 1
  selected <- selected[lengths(selected) > 0]
  if (length(selected) == 0) {
    return("No components were selected.")
  }
  comps <- vapply(selected, paste, character(1), collapse = ", ")
  listed <- if (one_recording) {
    paste0("Components selected: ", comps)
  } else {
    paste0("Components selected:\n", paste0("  ", names(selected), ": ", comps, collapse = "\n"))
  }
  call <- function(sign) {
    sel <- paste0(sign, "c(", comps, ")")
    args <- if (one_recording) sel else paste0("`", names(selected), "` = ", sel, collapse = ", ")
    paste0("eeg_ica_keep(", name, ", ", args, ")")
  }
  paste0(
    listed, "\n",
    "To keep only these components: ", call(""), "\n",
    "To remove them: ", call("-")
  )
}
