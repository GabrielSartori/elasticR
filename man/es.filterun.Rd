% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ESfiltersimple.R
\name{es.filterun}
\alias{es.filterun}
\title{Filtering dataset by a specific variable}
\usage{
es.filterun(credential, variable, size, filter, answer)
}
\arguments{
\item{credential}{the url create by function es.credential}

\item{variable}{a vector of variable,}

\item{size}{limit rows to extract}

\item{filter}{variable filter. Only one variable}

\item{answer}{variable answer. Only one answer}
}
\value{
a data frame with specific
}
\description{
Filtering dataset by a specific variable
}
\examples{
 es.filterun(
  my_credential,
  c("var1", "var2", "var3"),
  100,
  "var1",
  "answer1"
 )

}
