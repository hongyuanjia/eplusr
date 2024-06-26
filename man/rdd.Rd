% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdd.R
\name{read_rdd}
\alias{read_rdd}
\alias{read_mdd}
\title{Read an EnergyPlus Report Data Dictionary File}
\usage{
read_rdd(path)

read_mdd(path)
}
\arguments{
\item{path}{For \code{read_rdd()}, a file path of EnergyPlus EnergyPlus Report Data
Dictionary file with an extension \code{.rdd}. For \code{read_mdd()}, a file path of
EnergyPlus EnergyPlus Meter Data Dictionary file with an extension \code{.mdd}}
}
\value{
For \code{read_rdd()}, an \code{RddFile} object. For \code{read_mdd()}, a \code{MddFile}
object.
}
\description{
\code{read_rdd()} takes a file path of EnergyPlus Report Data Dictionary (RDD)
file, parses it and returns a \code{RddFile} object. \code{read_mdd()} takes a file
path of EnergyPlus Meter Data Dictionary (MDD) file, parses it and returns a
\code{MddFile} object.
}
\details{
Basically, a \code{RddFile} and \code{MddFile} object is a
\link[data.table:data.table]{data.table} with 5 columns and 3 additional
attributes:

5 Columns:

*\code{index}: Integer. Index of each variable.
\itemize{
\item \code{reported_time_step}: Character. Reported time step for the variables.
Possible value: \code{Zone} and \code{HVAC}.
\item \code{report_type}: Character. Report types. Possible value: \code{Average}, \code{Sum}
and \code{Meter}. Note that \code{Meter} is only for MDD file. All variables will
have \code{report_type} being \code{Meter}.
\item \code{variable}: Character. Report variable names.
\item \code{units}: Character. Units of reported values. \code{NA} if report values do not
have units.
}

3 Attributes:
\itemize{
\item \code{eplus_version}: A \link[base:numeric_version]{numeric_version} object. The
version of EnergyPlus used during the simulation.
\item \code{eplus_build}: A single string. The build tag of EnergyPlus used during the
simulation.
\item \code{datetime}: A DateTime (POSIXct). The time when the simulation started.
}
}
\examples{
\dontrun{
# run simulation and get the err file
idf_name <- "1ZoneUncontrolled.idf"
epw_name <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
idf_path <- path_eplus_example("8.8", idf_name)
epw_path <- path_eplus_weather("8.8", epw_name)
job <- eplus_job(idf_path, epw_path)
job$run(dir = tempdir())

# read the err file
read_rdd(job$locate_output(".rdd"))
read_mdd(job$locate_output(".mdd"))
}
}
\author{
Hongyuan Jia
}
