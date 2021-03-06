% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impl-idf.R
\name{assign_idf_value_default}
\alias{assign_idf_value_default}
\title{Assign default field values}
\usage{
assign_idf_value_default(idd_env, idf_env, dt_value)
}
\arguments{
\item{idd_env}{An environment or list contains IDD tables including class,
field, and reference.}

\item{idf_env}{An environment or list contains IDF tables including object,
value, and reference.}

\item{dt_value}{A \code{\link[data.table:data.table]{data.table::data.table()}} that contains object value data.}
}
\value{
The updated version of \code{\link[data.table:data.table]{data.table::data.table()}}.
}
\description{
Assign default field values
}
\keyword{internal}
