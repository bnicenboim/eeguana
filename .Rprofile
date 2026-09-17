## Project startup for eeguana development.
##
## R reads only the FIRST profile it finds: this file, then R_PROFILE_USER,
## then ~/.Rprofile. So a project .Rprofile suppresses both of the others, and
## anything that relies on them has to be run here explicitly.
##
## The VS Code R extension injects its session watcher through R_PROFILE_USER;
## that is what provides completion of live objects, and the workspace and plot
## viewers. Without this it silently stops working.
local({
  user_profile <- Sys.getenv("R_PROFILE_USER", unset = "")
  if (!nzchar(user_profile) && file.exists("~/.Rprofile")) {
    user_profile <- path.expand("~/.Rprofile")
  }
  if (nzchar(user_profile) && file.exists(user_profile)) {
    source(user_profile)
  }
})

## Load the development helpers (dev_test(), dev_install(), dev_fixtures(), ...).
## Interactive only, so R CMD check, CI, and Rscript runs are unaffected.
## Sourcing only defines functions; nothing is installed or downloaded here.
if (interactive() && file.exists("dev/dev.R")) {
  try(source("dev/dev.R"), silent = TRUE)
}
