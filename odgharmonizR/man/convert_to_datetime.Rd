% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonizeR-package.r
\name{convert_to_datetime}
\alias{convert_to_datetime}
\title{Convert a column to datetime format}
\usage{
convert_to_datetime(df, column)
}
\arguments{
\item{df}{A dataframe containing the column to convert}

\item{column}{Character string with name of the column to convert}
}
\value{
A list containing:
  \item{df}{The dataframe with the column converted (if successful)}
  \item{success}{Logical indicating if conversion was successful}
}
\description{
Attempts to convert a column in a dataframe to datetime format using multiple strategies.
First tries direct conversion with lubridate, then falls back to trying specific formats.
}
\examples{
\dontrun{
data <- data.frame(date_col = c("2022-01-01", "2022/02/01", "01.03.2022"))
result <- convert_to_datetime(data, "date_col")
}
}
