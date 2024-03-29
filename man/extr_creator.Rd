% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eml.R
\name{extr_creator}
\alias{extr_creator}
\title{Extract creators to list format}
\usage{
extr_creator(x)
}
\arguments{
\item{x}{a row in the \code{creators_df} data.frame
(read from `data-raw/metadata/creators.csv``)}
}
\value{
an eml creator list element
}
\description{
Function to be applied to the \code{creators_df} to exctract tabular information
into creators format.
}
