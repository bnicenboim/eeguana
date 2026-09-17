#!/usr/bin/env Rscript
## Re-measures the two cases whose ratio looked notable in the main sweep, at
## far more iterations. A sub-millisecond case needs it: at 30 iterations
## `mutate()` on an events table looked 1.27x slower, which was noise.
##
##   Rscript verify_outliers.R <label> <libpath> <out.csv>

args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(args[[2]], .libPaths()))
suppressMessages({library(eeguana); library(dplyr)})
options(eeguana.verbose = FALSE)

ev <- read_vhdr(path.expand("~/.cache/R/eeguana/fixtures/s1_faces.vhdr"),
                .recording = "f")$.events
erp <- eeg_summarize(
  eeg_group_by(eeg_filter(eeguana::data_faces_ERPs,
    between(as_time(.sample, .unit = "milliseconds"), 100, 200)), condition),
  across_ch(mean, na.rm = TRUE))

one <- function(id, expr, n) {
  b <- bench::mark(eval(expr), iterations = n, check = FALSE,
                   filter_gc = FALSE, memory = FALSE)
  data.frame(version = args[[1]], id = id, iterations = n,
             median_s = as.numeric(b$median),
             iqr_s = IQR(as.numeric(b$time[[1]])))
}

out <- rbind(
  one("events_mutate", quote(dplyr::mutate(ev, .initial = .initial)), 300),
  one("plot_topo", quote(plot_topo(erp)), 60)
)
print(out)
write.csv(out, args[[3]], row.names = FALSE)
