% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neigbor2heatmap.R
\name{neighbor2hm}
\alias{neighbor2hm}
\title{neigbor2hm}
\usage{
neigbor2hm(data, states = c("E1", "E2", ...), title = "Main title", subtitle = "Subtitle", color = "cornflowerblue", scale_color = "scale", legend = F, label_size =2)
}
\arguments{
\item{data}{either a character vector with the file to be loaded or an object of class data.frame obtained by reading the input file with read.delim()}

\item{states}{a not-null haracter vector with the names of the states defined by crhomHMM. Initially should be set at paste("E", 1:n, sep=""), where n is the number of states defined in chromHMM.}

\item{title}{character vector of length 1 with the title of the heatmap.}

\item{subtitle}{character vector of lenght 1 with the subtitle of the heatmap.}

\item{xlab}{character vector of lenght 1 with the label to show in the x-axis.}

\item{color}{character vector of length 1 with the color to be used to color the heatmap. Default: "Cornflowerblue".}

\item{legend}{logical of length 1 indicating whether to show the legend or not. Default = F.}

\item{score_size}{numeric of length 1 with the size of the likelihood labels to draw in the heatmap. Default: 2.}

\item{show_scores}{logical of length 1 indicating whether to write in the plot the enrichment score of each of the positions. Default: T.}
}
\description{
This function allows to plot the heatmap of the overlap enrichments in several genomic regions obtained by chromHMM.

This function takes the x_n_overlaps.txt file as input, where x is the contition specified in the ChromHMM design matrix and n is the number of states used in the learnModel function.

Go to the chromHMM main page to get more information about chromHMM: http://compbio.mit.edu/ChromHMM/
}
\author{
amitjavilaventura
}
