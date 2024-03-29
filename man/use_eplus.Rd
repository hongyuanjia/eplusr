% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{use_eplus}
\alias{use_eplus}
\alias{eplus_config}
\alias{avail_eplus}
\alias{is_avail_eplus}
\alias{locate_eplus}
\title{Configure which version of EnergyPlus to use}
\usage{
use_eplus(eplus)

eplus_config(ver)

avail_eplus()

is_avail_eplus(ver)

locate_eplus()
}
\arguments{
\item{eplus}{An acceptable EnergyPlus version or an EnergyPlus installation
path.}

\item{ver}{An acceptable EnergyPlus version.}
}
\value{
\itemize{
\item For \code{use_eplus()} and \code{eplus_config()}, an (invisible for
\code{use_eplus()}) list of three contains EnergyPlus version, directory and
EnergyPlus executable.  version of EnergyPlus;
\item For \code{avail_eplus()}, a \link[base:numeric_version]{numeric_version} vector or \code{NULL} if no
available EnergyPlus is found;
\item For \code{is_avis_avail_eplus()}, a scalar logical vector.
}
}
\description{
Configure which version of EnergyPlus to use
}
\details{
\code{use_eplus()} adds an EnergyPlus version into the EnergyPlus version cache in
eplusr. That cache will be used to get corresponding \link{Idd} object when
parsing IDF files and call corresponding EnergyPlus to run models.

\code{eplus_config()} returns the a list of configure data of specified version of
EnergyPlus. If no data found, an empty list will be returned.

\code{avail_eplus()} returns all versions of available EnergyPlus.

\code{locate_eplus()} re-searches all EnergyPlus installations at the \strong{default}
locations and returns versions of EnergyPlus it finds. Please note that all
configure data of EnergyPlus installed at custom locations will be
\strong{removed}.

\code{is_avail_eplus()} checks if the specified version of EnergyPlus is
available or not.
}
\examples{
\dontrun{
# add specific version of EnergyPlus
use_eplus("8.9")
use_eplus("8.8.0")

# get configure data of specific EnergyPlus version if avaiable
eplus_config("8.6")
}

# get all versions of avaiable EnergyPlus
avail_eplus()

# check if specific version of EnergyPlus is available
is_avail_eplus("8.5")
is_avail_eplus("8.8")

}
\seealso{
\code{\link[=download_eplus]{download_eplus()}} and \code{\link[=install_eplus]{install_eplus()}} for downloading and
installing EnergyPlus
}
