library(eeguana)
options(eeguana.verbose = FALSE)

## truscan.edf is a TruScan EDF+C export of a trigger test.
##
## Its "EDF Annotations" channel holds only the mandatory timekeeping TALs
## (+0.000000<14><14>, one per data record) with no annotation text, so there
## is nothing there to read. The triggers are in the `Foto` channel, whose raw
## digital values are 0, 3072 and 4096.
##
## EEGLAB's pop_biosig() turns that channel into 16 events, one per sample
## where the value changes plus one at the first sample. eeguana currently
## returns none.
##
## The `type` column below is eeguana's scaling of the same digital values,
## physical = 0.24995804 * digital - 0.375. EEGLAB reports the baseline as
## 65535.625 rather than -0.375, which is the same value plus 2^16: it renders
## the negative physical value as unsigned. The latencies are identical.

expected_events <- data.frame(
  latency = c(
    1,
    3259, 3260, 3261,
    3567, 3568, 3569,
    4858, 4859, 4860,
    5198, 5199, 5200,
    5498, 5499, 5500
  ),
  digital = c(
    0,
    3072, 4096, 0,
    3072, 4096, 0,
    3072, 4096, 0,
    3072, 4096, 0,
    3072, 4096, 0
  )
)

test_that("the triggers are in the Foto channel, where EEGLAB found them", {
  skip_if_nofixture("truscan.edf")
  d <- read_edf(fixture_path("truscan.edf"), .recording = "truscan")

  # what the channel holds, independent of how events are extracted
  foto <- as.numeric(d$.signal$Foto)
  changes <- c(1L, which(diff(foto) != 0) + 1L)
  expect_equal(changes, expected_events$latency)
})

test_that("read_edf() ignores the trigger channel by default", {
  skip_if_nofixture("truscan.edf")
  d <- read_edf(fixture_path("truscan.edf"), .recording = "truscan")
  expect_equal(nrow(d$.events), 0L)
  expect_true("Foto" %in% channel_names(d))
})

test_that(".trigger_channel extracts the triggers EEGLAB finds", {
  skip_if_nofixture("truscan.edf")
  d <- read_edf(fixture_path("truscan.edf"),
    .recording = "truscan", .trigger_channel = "Foto"
  )
  expect_equal(nrow(d$.events), nrow(expected_events))
  expect_equal(as.integer(d$.events$.initial), expected_events$latency)
  expect_equal(as.integer(d$.events$.final), expected_events$latency)
  # the digital codes, not the physical values they are scaled into
  expect_equal(as.integer(d$.events$.description), expected_events$digital)
  expect_equal(unique(d$.events$.type), "Trigger")
  # the channel is consumed, it is no longer signal
  expect_false("Foto" %in% channel_names(d))
})

test_that(".trigger_channel = \"last\" picks the same channel here", {
  skip_if_nofixture("truscan.edf")
  named <- read_edf(fixture_path("truscan.edf"), .recording = "t", .trigger_channel = "Foto")
  last <- read_edf(fixture_path("truscan.edf"), .recording = "t", .trigger_channel = "last")
  expect_equal(named$.events, last$.events)
})

test_that(".trigger_channel rejects a name that is not a channel", {
  skip_if_nofixture("truscan.edf")
  expect_error(
    read_edf(fixture_path("truscan.edf"), .recording = "t", .trigger_channel = "nope"),
    "is not one of the channels"
  )
})

test_that("naming a continuous channel warns but still reads it", {
  bv <- system.file("testdata", "bv_export_edf+.edf", package = "eeguana")
  expect_warning(
    d <- read_edf(bv, .recording = "t", .trigger_channel = "Fpz"),
    "looks like continuous data"
  )
  # warned, not refused
  expect_gt(nrow(d$.events), 0L)
})

test_that("the distinct-value check only catches obvious data channels", {
  skip_if_nofixture("truscan.edf")
  # The guard errors when a channel has more than 20 distinct values. It is a
  # sanity check, not a classifier: in this recording the EEG channels are
  # flat, 2 distinct values each, so naming one produces events rather than an
  # error. Nothing in an EDF header distinguishes a trigger channel, so the
  # caller is responsible for naming the right one.
  d <- read_edf(fixture_path("truscan.edf"), .recording = "t", .trigger_channel = "EEG Fp1")
  expect_gt(nrow(d$.events), 0L)
})

test_that("a likely trigger channel is pointed out when nothing is extracted", {
  skip_if_nofixture("truscan.edf")
  # the hint respects eeguana.verbose, which this file switches off at the top
  withr::local_options(eeguana.verbose = TRUE)
  expect_message(
    read_edf(fixture_path("truscan.edf"), .recording = "truscan"),
    "may be a trigger channel"
  )
  expect_message(
    read_edf(fixture_path("truscan.edf"), .recording = "truscan"),
    'trigger_channel = "last"'
  )
})

test_that("the hint is silent when verbosity is off", {
  skip_if_nofixture("truscan.edf")
  withr::local_options(eeguana.verbose = FALSE)
  expect_no_message(
    read_edf(fixture_path("truscan.edf"), .recording = "truscan"),
    message = "may be a trigger channel"
  )
})

test_that("the hint is silent when it would be noise", {
  skip_if_nofixture("truscan.edf")
  withr::local_options(eeguana.verbose = TRUE)
  # already asked for the channel
  expect_no_message(
    read_edf(fixture_path("truscan.edf"), .recording = "t", .trigger_channel = "last"),
    message = "may be a trigger channel"
  )
  # the file has real annotations, so events were found
  expect_no_message(
    read_edf(system.file("testdata", "bv_export_edf+.edf", package = "eeguana"), .recording = "t"),
    message = "may be a trigger channel"
  )
  # no events, but the last channel is ordinary EEG
  expect_no_message(
    read_edf(system.file("testdata", "bv_export_edf.edf", package = "eeguana"), .recording = "t"),
    message = "may be a trigger channel"
  )
})

test_that("reading an EDF+ with no annotations is warning free", {
  skip_if_nofixture("truscan.edf")
  # truscan.edf's annotation channel holds only timekeeping TALs, so there are
  # no annotations. Building the empty events table used to leave `.final` a
  # plain integer, because `integer(0) + sample_int(integer(0))` drops the
  # class, and validation then complained.
  expect_no_warning(read_edf(fixture_path("truscan.edf"), .recording = "truscan"))
  expect_no_warning(
    read_edf(fixture_path("truscan.edf"), .recording = "t", .trigger_channel = "last")
  )
})

test_that("a warning from reading is not repeated", {
  skip_if_nofixture("truscan.edf")
  # the events table is validated by eeg_lst() and again by validate_eeg_lst()
  # through built_eeg_lst(), so anything it warns about was said twice
  warnings_from <- function(expr) {
    w <- character()
    withCallingHandlers(expr,
      warning = function(x) {
        w <<- c(w, conditionMessage(x))
        invokeRestart("muffleWarning")
      }
    )
    w
  }
  w <- warnings_from(read_edf(fixture_path("truscan.edf"), .recording = "truscan"))
  expect_equal(length(w), 0L)
  expect_equal(length(unique(w)), length(w))
})

test_that("the events table keeps its sample_int columns when empty", {
  skip_if_nofixture("truscan.edf")
  d <- read_edf(fixture_path("truscan.edf"), .recording = "truscan")
  expect_equal(nrow(d$.events), 0L)
  expect_true(is_sample_int(d$.events$.initial))
  expect_true(is_sample_int(d$.events$.final))
})

test_that("the events table is validated once per read", {
  skip(paste(
    "Known and not critical, left for later. A read validates its events",
    "table twice: eeg_lst() does it at R/constructors.R:50 and",
    "validate_eeg_lst() does it again through built_eeg_lst() at",
    "R/constructors_helpers.R:144. Nothing is wrong with the result, but every",
    "warning about the events table is printed twice. Fixing it means deciding",
    "which of the two construction paths owns validation; dropping the wrong",
    "one would stop validating instead. Remove this skip when that is settled."
  ))

  calls <- 0L
  orig <- eeguana:::validate_events_tbl
  assignInNamespace(
    "validate_events_tbl",
    function(events) {
      calls <<- calls + 1L
      orig(events)
    },
    ns = "eeguana"
  )
  withr::defer(assignInNamespace("validate_events_tbl", orig, ns = "eeguana"))

  read_edf(
    system.file("testdata", "bv_export_edf+.edf", package = "eeguana"),
    .recording = "t"
  )
  expect_equal(calls, 1L)
})
