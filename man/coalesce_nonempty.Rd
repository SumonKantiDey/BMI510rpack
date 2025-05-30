% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.r
\name{coalesce_nonempty}
\alias{coalesce_nonempty}
\title{Question 3: Coalesce First Non-Missing, Non-Empty String}
\usage{
coalesce_nonempty(x)
}
\arguments{
\item{x}{A character vector.}
}
\value{
A single character string: the first non-missing, non-empty element of \code{x}, or \code{NA} if none found.
}
\description{
Return the First Non-Missing, Non-Empty String
}
\details{
Returns the first element in a character vector that is neither \code{NA} nor an empty string (\code{""}).


This function is useful when you want to select the first meaningful value from a set of fallback options,
such as metadata fields or alternate labels.
}
\examples{
coalesce_nonempty(c(NA, "", "apple", "banana"))  # returns "apple"
coalesce_nonempty(c(NA, "", NA))                # returns NA

}
