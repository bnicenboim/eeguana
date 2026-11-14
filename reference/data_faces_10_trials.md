# EEGs elicited when one subject saw faces and non-faces objects.

A dataset containing 10 trials where one subject either saw faces or a
non-faces objects with preprocessing done in BrainVision 2.0. See also
the vignette [Introduction - Manipulating a clean EEG file exported from
BrainVision
2.0](https://bnicenboim.github.io/eeguana/articles/brainvision_files.html).

## Usage

``` r
data(data_faces_10_trials)
```

## Format

An eeg_lst with the following events:

- `.type == "New Segment"` indicates the beginning of the recording.

- `.type == "Bad Interval"` indicates an interval marked as an artifact
  by BrainVision 2.0.

- `.type == "UserDefined", .description == "Blink` indicates an interval
  marked as a blink by BrainVision 2.0.

- `.type == "Stimulus", .description == "s5` indicates the beginning of
  the experiment.

- `.type == "Stimulus", .description == "s111` indicates

- `.type == "Stimulus", .description == "s130` indicates the beginning
  of the trial.

- `.type == "Stimulus", .description == "s122` indicates

- `.type == "Stimulus", .description == "s121` indicates

- `.type == "Stimulus", .description == "s70` indicates that a face was
  presented.

- `.type == "Stimulus", .description == "s71` indicates that a non-face
  was presented.

## See also

Other EEG datasets:
[`data_faces_ERPs`](https://bruno.nicenboim.me/eeguana/reference/data_faces_ERPs.md)
