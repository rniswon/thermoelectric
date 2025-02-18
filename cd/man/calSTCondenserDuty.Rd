% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calSTCondenserDuty.R
\name{calSTCondenserDuty}
\alias{calSTCondenserDuty}
\title{calSTCondenserDuty}
\usage{
calSTCondenserDuty(steam_heat, netgen, nominal_loss)
}
\arguments{
\item{steam_heat}{monthly steam heat by bogen, output of the assignBogen() function}

\item{netgen}{monthly net generation by bogen, output of the assignBogen() function}

\item{nominal_loss}{monthly nominal losses by bogen, output of the assignBogen()}
}
\value{
\code{condenser_duty} data.frame containing monthly condenser duty for stream plants
}
\description{
calculates steam bogen condenser duty \cr \cr
Executed By: analyzeSteamCD.R \cr
}
\examples{
calSTCondenserDuty(cd_cal_table_2[4:15], cd_cal_table_2[28:39], cd_cal_table_2[16:27])
}
