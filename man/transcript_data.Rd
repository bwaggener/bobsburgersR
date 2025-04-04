% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_documentation.R
\docType{data}
\name{transcript_data}
\alias{transcript_data}
\title{Transcript Data from Bob's Burgers}
\format{
A data frame with 181,031 rows and 6 variables:
\describe{
\item{season}{The season number in which the episode is part of the Bob's Burgers TV show.}
\item{episode}{The episode number within the specific season of Bob's Burgers.}
\item{title}{The title of the episode in which the dialogue line appears.}
\item{line}{The line number of the dialogue (the order in which it appears).}
\item{raw_text}{The original raw text of the dialogue, possibly including formatting or special characters.}
\item{dialogue}{Cleaned-up version of the \code{raw_text}, containing the actual dialogue spoken by the characters.}
}
}
\source{
Springfield! Springfield!
}
\usage{
transcript_data
}
\description{
A dataset containing the transcripts of Bob's Burgers episodes.
}
\keyword{datasets}
