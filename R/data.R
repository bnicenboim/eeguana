#' ERPs elicited when one subject saw faces and non-faces objects.
#'
#' A dataset containing the ERPs elicited when one subject saw faces and non-faces objects. See also the vignette [Introduction - Manipulating a clean EEG file exported from BrainVision 2.0](https://bnicenboim.github.io/eeguana/articles/brainvision_files.html).
#'
#'
#' @format An eeg_lst  with 2 conditions (faces, non-faces)
#' @family EEG datasets
#' @usage data(data_faces_ERPs)
"data_faces_ERPs"

#' EEGs elicited when one subject saw faces and non-faces objects.
#'
#' A dataset containing 10 trials where  one subject either saw faces or a non-faces objects with preprocessing done in BrainVision 2.0. See also the vignette [Introduction - Manipulating a clean EEG file exported from BrainVision 2.0](https://bnicenboim.github.io/eeguana/articles/brainvision_files.html).
#'
#'
#' @format An eeg_lst with the following events:
#'   * `.type == "New Segment"` indicates the beginning of the recording.
#'   * `.type == "Bad Interval"` indicates an interval marked as an artifact by BrainVision 2.0.
#'   * `.type == "UserDefined", .description == "Blink` indicates an interval marked as a blink by BrainVision 2.0.
#'   * `.type == "Stimulus", .description == "s5` indicates the beginning of the experiment.
#'   * `.type == "Stimulus", .description == "s111` indicates
#'   * `.type == "Stimulus", .description == "s130` indicates the beginning of the trial.
#'   * `.type == "Stimulus", .description == "s122` indicates
#'   * `.type == "Stimulus", .description == "s121` indicates
#'   * `.type == "Stimulus", .description == "s70` indicates that a face was presented.
#'   * `.type == "Stimulus", .description == "s71` indicates that a non-face was presented.
#' @family EEG datasets
#' @usage data(data_faces_10_trials)
"data_faces_10_trials"


#' Layout for a 32 electrodes cap at the standard 10-20 system
#'
#' A dataset containing coordinates of a 32 electrodes mounted in an elastic cap at the standard 10-20 system (Jasper, 1958)
#'
#'
#' @format An table with channel names and locations
#' @family topographic plots and layouts
#' @usage data(layout_32_1020)
#' 
"layout_32_1020"
