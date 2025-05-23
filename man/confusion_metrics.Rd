% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.r
\name{confusion_metrics}
\alias{confusion_metrics}
\title{Question 10: Compute Confusion Matrix Metrics}
\usage{
confusion_metrics(truth, pred)
}
\arguments{
\item{truth}{A binary vector of ground truth labels (0/1 or FALSE/TRUE).}

\item{pred}{A binary vector of predicted labels (same length as \code{truth}).}
}
\value{
A list containing:
\describe{
  \item{accuracy}{Proportion of correct predictions.}
  \item{sensitivity}{True positive rate (recall).}
  \item{specificity}{True negative rate.}
}
}
\description{
Computes classification metrics including accuracy, sensitivity (recall), and specificity from binary truth and prediction vectors.
}
\details{
Assumes binary classification with labels 1 = positive, 0 = negative.
}
\examples{
truth <- c(1, 0, 1, 1, 0, 0, 1)
pred  <- c(1, 0, 1, 0, 0, 1, 1)
confusion_metrics(truth, pred)

}
