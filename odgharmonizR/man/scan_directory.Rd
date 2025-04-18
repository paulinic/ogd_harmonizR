% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonizeR-package.r
\name{scan_directory}
\alias{scan_directory}
\title{Scan a directory for supported data files}
\usage{
scan_directory(directory_path)
}
\arguments{
\item{directory_path}{Path to the directory containing data files}
}
\value{
A data frame with information about the discovered files
}
\description{
This function scans a directory and identifies supported data files for analysis
}
\examples{
\dontrun{
file_info <- scan_directory("path/to/data")
}
}
