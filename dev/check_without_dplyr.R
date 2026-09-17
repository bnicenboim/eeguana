## Does eeguana work on a machine where dplyr, tidyr, and tibble are not
## installed at all?
##
## Leaving them out of DESCRIPTION's Imports is not enough to know. Two things
## pull dplyr in anyway: purrr's map_dfr() and friends bind their rows with
## dplyr at run time, and ggplot2 loads dplyr during its own load whenever dplyr
## is installed. So on a normal development machine everything looks fine. The
## only honest check hides those packages completely, which is what this does.
##
##   Rscript dev/check_without_dplyr.R          # from the package root
##
## It installs the working tree into a temporary library, builds a second
## library that links every installed package except the hidden ones, and runs
## the checks in a fresh R that can see nothing else.

hidden <- c("dplyr", "tidyr", "tibble")
stopifnot(file.exists("DESCRIPTION"), read.dcf("DESCRIPTION")[, "Package"] == "eeguana")

tmp <- tempfile("without_dplyr_")
lib <- file.path(tmp, "lib")
empty <- file.path(tmp, "empty")
dir.create(lib, recursive = TRUE); dir.create(empty)

## base R's own library always stays on the path, so it must not hold them
in_base <- intersect(hidden, rownames(installed.packages(lib.loc = .Library)))
if (length(in_base)) stop("cannot hide ", paste(in_base, collapse = ", "), ": installed in .Library")

## link every other package; earlier library paths win, as they do in R
for (l in rev(setdiff(.libPaths(), .Library))) {
  for (p in setdiff(list.files(l), c(hidden, "eeguana"))) {
    to <- file.path(lib, p)
    if (file.exists(to)) unlink(to)
    file.symlink(file.path(l, p), to)
  }
}

message("installing the working tree ...")
status <- system2(file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-docs", "--no-byte-compile", "-l", shQuote(lib), "."),
  stdout = FALSE, stderr = FALSE)
if (status != 0) stop("R CMD INSTALL failed")

child <- file.path(tmp, "checks.R")
writeLines(con = child, r"---(
.libPaths(c(Sys.getenv("WITHOUT_DPLYR_LIB"), .libPaths()))
grDevices::pdf(NULL)
hidden <- c("dplyr", "tidyr", "tibble")
visible <- hidden[vapply(hidden, requireNamespace, logical(1), quietly = TRUE)]
if (length(visible)) stop("not hidden, so this would prove nothing: ", paste(visible, collapse = ", "))
cat("dplyr, tidyr, and tibble cannot be loaded here\n\n")

suppressMessages(library(eeguana)); options(eeguana.verbose = FALSE)
failed <- 0
check <- function(label, expr) {
  msg <- tryCatch({ force(expr); "ok" },
    error = function(e) { failed <<- failed + 1; paste("FAILED:", conditionMessage(e)) })
  cat(sprintf("  %-40s %s\n", label, msg))
}
td <- function(f) system.file("testdata", f, package = "eeguana")
seg <- eeg_segment(data_faces_10_trials, .description == "s70", .lim = c(-.1, .3))
win <- eeg_filter(data_faces_ERPs, as_time(.sample, .unit = "ms") >= 100 & as_time(.sample, .unit = "ms") <= 200)
topo <- eeg_summarize(eeg_group_by(win, condition), across_ch(mean, na.rm = TRUE))
layout_plot <- ggplot2::ggplot(eeg_select(data_faces_ERPs, Fz, Cz), ggplot2::aes(.time, .value)) +
  ggplot2::geom_line() + ggplot2::facet_wrap(~.key)

check("read_vhdr()", read_vhdr(td("bv_export_bv_txt_bin_multi.vhdr"), .recording = "bv"))
check("read_edf()", read_edf(td("bv_export_edf.edf"), .recording = "edf"))
check("read_set()", suppressWarnings(read_set(td("bv_export_bv_txt_bin_multi.set"), .recording = "set")))
check("eeg_filter()", eeg_filter(data_faces_10_trials, .sample > 100))
check("eeg_select()", eeg_select(data_faces_10_trials, Fz, Cz))
check("eeg_mutate()", eeg_mutate(data_faces_10_trials, extra = Fz - Cz))
check("eeg_group_by() + eeg_summarize()", eeg_summarize(eeg_group_by(data_faces_10_trials, .sample), across_ch(mean)))
check("eeg_segment()", seg)
check("eeg_downsample()", eeg_downsample(data_faces_10_trials, .q = 2))
check("eeg_filt_low_pass()", eeg_filt_low_pass(seg, .freq = 30))
check("summary()", summary(data_faces_10_trials))
check("as.data.table()", data.table::as.data.table(data_faces_10_trials))
check("channels_tbl() and channels_tbl<-", { x <- data_faces_10_trials; channels_tbl(x) <- channels_tbl(x); x })
check("drop_incomplete_segments()", drop_incomplete_segments(seg))
check("eeg_ica()", ica <- eeg_ica(eeg_select(seg, Fz, Cz, Pz, Oz)))
check("eeg_ica_var_tbl()", eeg_ica_var_tbl(ica))
check("eeg_ica_keep()", eeg_ica_keep(ica, ICA1, ICA2))
check("plot_components(), built", ggplot2::ggplot_build(plot_components(ica)))
check("eeg_interpolate_tbl()", eeg_interpolate_tbl(topo))
check("plot_topo(), built", ggplot2::ggplot_build(plot_topo(topo) + annotate_head() + annotate_electrodes()))
check("ggplot(eeg_lst), built", ggplot2::ggplot_build(layout_plot))
check("plot_in_layout()", plot_in_layout(layout_plot))

cat(sprintf("\n%s\n", if (failed) paste(failed, "check(s) failed") else "all checks passed"))
quit(status = as.integer(failed > 0))
)---")

status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(child)),
  env = c(paste0("R_LIBS_USER=", empty), paste0("R_LIBS_SITE=", empty),
          paste0("WITHOUT_DPLYR_LIB=", lib)))
unlink(tmp, recursive = TRUE)
if (status != 0) quit(status = 1)
