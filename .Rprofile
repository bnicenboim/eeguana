## Project startup for eeguana development.
##
## A project-level .Rprofile REPLACES the user-level ~/.Rprofile, so run that
## first to keep your personal settings.
if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}

## Load the development helpers (dev_test(), dev_install(), dev_fixtures(), ...).
## Interactive only, so R CMD check, CI, and Rscript runs are unaffected.
## Sourcing only defines functions; nothing is installed or downloaded here.
if (interactive() && file.exists("dev/dev.R")) {
  try(source("dev/dev.R"), silent = TRUE)
}
