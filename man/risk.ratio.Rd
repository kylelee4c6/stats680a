% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/risk_ratio.R
\name{risk.ratio}
\alias{risk.ratio}
\title{Risk ratio}
\usage{
risk.ratio(data, confidence)
}
\arguments{
\item{data}{A 2x2 matrix.}

\item{confidence}{The confidence level in decimal form.}
}
\value{
results The output will be the point estimate followed by the
  the confidence interval of the risk ratio stored in a list
}
\description{
This function allows you to compute the risk ratio of a 2x2 matrix.
}
\examples{
x <- matrix(c(10,5,10,75),nrow = 2, ncol = 2)
relative.rsk(x,.95)

}
