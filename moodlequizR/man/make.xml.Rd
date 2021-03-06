% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.xml.R
\name{make.xml}
\alias{make.xml}
\title{make.xml}
\usage{
make.xml(fun, folder = getwd(), k = 1, delete.fun = TRUE, ...)
}
\arguments{
\item{fun}{name of function that makes a quiz}

\item{folder}{folder were fun.R is located}

\item{k}{how many quizzes?}

\item{delete.fun}{should file be deleted from workspace after we are done?}

\item{...}{further arguments passed to fun}
}
\value{
None
}
\description{
This function is a wrapper for genquiz. It reads file from folder and runs genquiz
}
\examples{
# not run
# make.xml(example1, 10, problem=1)
}
