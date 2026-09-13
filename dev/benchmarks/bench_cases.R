## Benchmark cases shared by every version under test.
##
## Each case names why it is here. The ones that matter most are those whose
## code changed in the tidytable migration, and those that lean on data.table,
## since data.table is the next thing to be touched.
##
## `setup` runs untimed and puts objects in `env`; `expr` is what gets timed.

FIXTURES <- path.expand("~/.cache/R/eeguana/fixtures")
f_s04 <- file.path(FIXTURES, "s04.vhdr")        # 64 ch, 1,115,420 samples, 553 Mb
f_faces <- file.path(FIXTURES, "s1_faces.vhdr") # 34 ch,   424,488 samples, 113 Mb
f_edf <- file.path(FIXTURES, "truscan.edf")

## What a topographic plot expects: one mean amplitude per channel per
## condition over a time window, exactly as the plot_topo() examples do it.
## Grouping by .sample as well leaves .sample in the grouping while the
## long-format conversion produces .time, and the interpolation then fails.
topo_input <- function() {
  eeg_summarize(
    eeg_group_by(
      eeg_filter(eeguana::data_faces_ERPs,
                 dplyr::between(as_time(.sample, .unit = "milliseconds"), 100, 200)),
      condition
    ),
    across_ch(mean, na.rm = TRUE)
  )
}

bench_cases <- list(

  ## ---- reading: read.R and read_helpers.R were migrated, and read_vhdr()
  ## also gained the new check_dat_size() header check -------------------
  list(id = "read_vhdr_64ch", group = "Reading", iterations = 5,
       label = "read_vhdr(), 64 ch / 1.1M samples",
       why = "read.R migrated; check_dat_size() added; 553 Mb result",
       setup = function(env) NULL,
       expr = quote(read_vhdr(f_s04, .recording = "s04"))),

  list(id = "read_vhdr_34ch", group = "Reading", iterations = 7,
       label = "read_vhdr(), 34 ch / 424k samples",
       why = "same path, smaller object",
       setup = function(env) NULL,
       expr = quote(read_vhdr(f_faces, .recording = "faces"))),

  list(id = "read_edf", group = "Reading", iterations = 10,
       label = "read_edf()",
       why = "read.R migrated (dplyr::case_when, dplyr::tibble)",
       setup = function(env) NULL,
       expr = quote(read_edf(f_edf, .recording = "t"))),

  ## ---- core verbs on a large object -----------------------------------
  list(id = "eeg_select", group = "Verbs", iterations = 20,
       label = "eeg_select(), 8 of 34 channels",
       why = "dplyr_verbs.R; select on a large signal table",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_select(big, Fp1, Fp2, F3, F4, C3, C4, P3, P4))),

  list(id = "eeg_filter", group = "Verbs", iterations = 20,
       label = "eeg_filter() on .sample",
       why = "dplyr_verbs.R and dplyr_ext.R; semi_join on .segments was migrated",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_filter(big, .sample > 1000, .sample < 300000))),

  list(id = "eeg_mutate", group = "Verbs", iterations = 20,
       label = "eeg_mutate(), new channel",
       why = "dplyr_verbs.R; writes a column into a large signal table",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_mutate(big, extra = Fp1 - Fp2))),

  list(id = "eeg_summarize_ch", group = "Verbs", iterations = 10,
       label = "eeg_summarize(across_ch(mean))",
       why = "eeg_summarize() carries the setcolorder guard; across_ch over 34 channels",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_summarize(big, across_ch(mean, na.rm = TRUE)))),

  list(id = "eeg_group_summarize", group = "Verbs", iterations = 10,
       label = "eeg_group_by(.id) + summarize",
       why = "grouping path; eeg_group_vars() is now called directly",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_summarize(eeg_group_by(big, .sample), across_ch(mean, na.rm = TRUE)))),

  ## ---- segmentation and resampling, both data.table heavy -------------
  list(id = "eeg_segment", group = "Transform", iterations = 10,
       label = "eeg_segment()",
       why = "segmentation.R migrated; heavy data.table joins",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_segment(big, .description == "s70", .lim = c(-.2, .5)))),

  list(id = "eeg_downsample", group = "Transform", iterations = 10,
       label = "eeg_downsample(q = 4)",
       why = "signal_processing.R; data.table heavy, next to be migrated",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(eeg_downsample(big, .q = 4))),

  ## ---- conversions and summaries, all migrated ------------------------
  list(id = "as_data_table", group = "Output", iterations = 10,
       label = "as.data.table() on a large eeg_lst",
       why = "to_tbl.R migrated; long-format conversion of 424k x 34",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(data.table::as.data.table(big))),

  list(id = "summary_eeg_lst", group = "Output", iterations = 10,
       label = "summary()",
       why = "rewritten: group_by_at(vars(-...)) replaced with explicit .by",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(summary(big))),

  list(id = "channels_tbl", group = "Output", iterations = 30,
       label = "channels_tbl()",
       why = "tbl.R; the data.frame assignment method was rewritten in base R",
       setup = function(env) env$big <- read_vhdr(f_faces, .recording = "faces"),
       expr = quote(channels_tbl(big))),

  ## ---- events table verbs ---------------------------------------------
  list(id = "events_mutate", group = "Events", iterations = 30,
       label = "mutate() on an events table",
       why = "events_tbl.R migrated; as_events_tbl() now restores attributes",
       setup = function(env) env$ev <- read_vhdr(f_faces, .recording = "faces")$.events,
       expr = quote(dplyr::mutate(ev, .initial = .initial))),

  ## ---- plotting internals, the most heavily rewritten code ------------
  list(id = "interpolate_tbl", group = "Plotting", iterations = 10,
       label = "eeg_interpolate_tbl()",
       why = "rewritten: persistent grouping replaced with explicit .by",
       setup = function(env) env$erp <- topo_input(),
       expr = quote(eeg_interpolate_tbl(erp))),

  list(id = "plot_topo", group = "Plotting", iterations = 10, memory = FALSE,
       label = "plot_topo() object construction",
       why = "plot.R migrated; plot_topo.tbl_df widened to .data.frame",
       setup = function(env) env$erp <- topo_input(),
       expr = quote(plot_topo(erp)))
)
