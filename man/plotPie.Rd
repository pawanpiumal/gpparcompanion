% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPie.R
\name{plotPie}
\alias{plotPie}
\title{Plot pie chart}
\usage{
plotPie(
  var,
  title = NA,
  varname = NA,
  levels = NA,
  seed = NA,
  textT = F,
  titleT = T,
  frequencyT = F,
  percentageT = F,
  legendT = T,
  outT = F,
  groupOutT = F,
  verbT = T
)
}
\arguments{
\item{var}{The variable that needs to be plotted. The variable needs to be
a qualitative variable.}

\item{title}{The title of the plot}

\item{varname}{The variable name}

\item{levels}{Vector of strings for the factor levels}

\item{seed}{The seed for the color of the plot}

\item{textT}{Print category name on plot}

\item{frequencyT}{Print frequency values on plot}

\item{percentageT}{Print percentage values on plot}

\item{legendT}{Print legend}

\item{outT}{Show legend outside}

\item{groupOutT}{Show the group name outside the chart}
}
\description{
This function plots the pie plot for
qualitative variables.
}
