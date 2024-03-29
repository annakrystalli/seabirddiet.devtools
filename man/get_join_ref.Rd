% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bibliography.R
\name{get_join_ref}
\alias{get_join_ref}
\title{Get join_ref table}
\usage{
get_join_ref(ref_tidy, ref)
}
\arguments{
\item{ref_tidy}{tidy reference tibble}

\item{ref}{ref_table exported by \code{get_ref_table()}}
}
\value{
a tibble containing the ref_id and valid reference fields for
each row in the seabird data. This table is prepared to be joined to the data
at the final processing stage.
}
\description{
Get join_ref table to be joined to seabird data at the final processing stage
}
\examples{
\dontrun{
get_join_ref(ref_tidy)
}
}
