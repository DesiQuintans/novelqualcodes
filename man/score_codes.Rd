% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exp_score_codes.R
\name{score_codes}
\alias{score_codes}
\title{Score novel and duplicates codes across interviews}
\usage{
score_codes(interviews)
}
\arguments{
\item{interviews}{(List) A list of dataframes, as generated by \code{\link[=import_coding_matrices]{import_coding_matrices()}}.}
}
\value{
A dataframe, with one row per interview and these columns:
\enumerate{
\item \code{itvw_seq}, the chronological order of interviews.
\item \code{n_codes}, the number of unique codes mentioned in this interview.
\item \code{n_duplicate}, how many of those codes are duplicates mentioned in previous interviews).
\item \code{n_novel}, how many of those codes are novel (mentioned for the first time in this interview).
\item \code{prop_duplicate}, the proportion of this interview's codes that are duplicates.
\item \code{prop_novel}, the proportion of this interview's codes that are novel.
\item \code{cumsum_novel}, the cumulative sum of novel codes over time (i.e. across interviews).
}
}
\description{
'Novel' and 'duplicate' codes are scored once per interview; the number of
times they are spoken in an interview does not matter.

The definition of whether a code is \emph{novel} or \emph{duplicated} is
entirely chronological:
\itemize{
\item A \strong{novel code} is a topic/idea/concept that, for example, is mentioned by
Interviewee 17, but was not mentioned by Interviewees 1 through 16.
\item A \strong{duplicate code} is one that has been talked about by other interviewees
previously.
}

The cumulative sum of novel codes is used to visualise a stopping
point for qualitative interviews.
}
\seealso{
\code{\link[=plot_novelty]{plot_novelty()}}, \code{\link[=plot_richness]{plot_richness()}}
}