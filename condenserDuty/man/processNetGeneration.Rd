% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processNetGeneration.R
\name{processNetGeneration}
\alias{processNetGeneration}
\title{processNetGeneration}
\usage{
processNetGeneration(generation.data_df, plantList, page4)
}
\arguments{
\item{generation.data_df}{data.frame EIA-923 Monthly Generating Unit Net Generation Time
Series File, 2015 Final Revision, input file 2015_GenerationData.csv}

\item{plantList}{vector of all plant codes from 2015, input file 2015_Plants.csv}

\item{page4}{logical indicating whether page 4 generation.data is used as input, if FALSE page 1 gen_fuel_data is used}
}
\value{
\code{results} data.frame annual and montly process net generation by plant and generator
}
\description{
calculates annual and montly process net generation by plant and generator, replaces
negative net generation months with zero, and filters out generators with negative net
generation for the year.  For output file pos_netgen.csv \cr \cr
Executed By: non_nuke_condenser_duty.R \cr
}
