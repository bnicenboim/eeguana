#!/usr/bin/env Rscript
## Runs every case in bench_cases.R against one installed version of eeguana
## and writes a CSV. Called once per version, each with its own library, since
## two versions of a package cannot be loaded in one session.
##
##   Rscript run_bench.R <label> <libpath> <out.csv>

args <- commandArgs(trailingOnly = TRUE)
label <- args[[1]]; libpath <- args[[2]]; out <- args[[3]]
## optional 4th argument: comma-separated case ids, to re-run a subset
only <- if (length(args) >= 4) strsplit(args[[4]], ",")[[1]] else NULL

.libPaths(c(libpath, .libPaths()))
suppressMessages(library(eeguana))
stopifnot(identical(normalizePath(dirname(getNamespaceInfo("eeguana", "path"))),
                    normalizePath(libpath)))
options(eeguana.verbose = FALSE)

here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
source(file.path(here, "bench_cases.R"))

version <- as.character(utils::packageVersion("eeguana"))
cat(sprintf("\n=== %s (eeguana %s) ===\n", label, version))

if (!is.null(only)) bench_cases <- Filter(function(x) x$id %in% only, bench_cases)

rows <- list()
for (case in bench_cases) {
  env <- new.env(parent = globalenv())
  for (nm in c("f_s04", "f_faces", "f_edf")) assign(nm, get(nm), envir = env)

  res <- tryCatch({
    case$setup(env)
    b <- bench::mark(
      eval(case$expr, envir = env),
      iterations = case$iterations, check = FALSE, filter_gc = FALSE,
      ## some cases cannot be memory-profiled ("Memory profiling failed"),
      ## so they opt out and report timing only
      memory = !isFALSE(case$memory)
    )
    list(status = "ok",
         median = as.numeric(b$median), min = as.numeric(b$min),
         mem = as.numeric(b$mem_alloc), gc = sum(b$n_gc),
         times = paste(round(as.numeric(b$time[[1]]), 6), collapse = ";"))
  }, error = function(e) list(status = paste("error:", conditionMessage(e)),
                              median = NA, min = NA, mem = NA, gc = NA, times = NA))

  cat(sprintf("  %-22s %-8s %s\n", case$id, res$status,
              if (is.na(res$median)) "" else sprintf("%.4f s", res$median)))

  rows[[length(rows) + 1]] <- data.frame(
    version = label, eeguana_version = version, id = case$id, group = case$group,
    label = case$label, why = case$why, iterations = case$iterations,
    status = res$status, median_s = res$median, min_s = res$min,
    mem_alloc = res$mem, n_gc = res$gc, times = res$times,
    stringsAsFactors = FALSE
  )
  rm(env); gc(verbose = FALSE)
}

write.csv(do.call(rbind, rows), out, row.names = FALSE)
cat("written:", out, "\n")
