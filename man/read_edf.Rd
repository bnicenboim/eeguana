% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_edf}
\alias{read_edf}
\title{Read an edf/edf+/bdf file into R}
\usage{
read_edf(file, .recording = file)
}
\arguments{
\item{file}{A edf/bdf file}

\item{.recording}{Recording name (file name, by default). If set to NULL or NA, the patient name will be used.}
}
\value{
An \code{eeg_lst} object.
}
\description{
Creates an eeg_lst object from edf, edf+, and bdf file export formats.
}
\details{
When trigger information is stored in a "Status" or "Trigger" channel, the trigger
value is stored only when the value of the channel increases. This follows the
default behavior of \href{https://mne.tools/0.18/generated/mne.find_events.html?highlight=find_events#mne.find_events}{find_events in MNE 0.18}.
If you have a case where this assumption is incorrect, please open an issue in \url{https://github.com/bnicenboim/eeguana/issues}.
}
\examples{
\dontrun{
s1 <- read_edf("./faces.edf", .recording = 1)
}

}
\seealso{
Other reading and writing functions: 
\code{\link{read_fif}()},
\code{\link{read_ft}()},
\code{\link{read_set}()},
\code{\link{read_vhdr}()}
}
\concept{reading and writing functions}
