# --------------------------------------------- #
# chromHMMviewR                                 #
# --------------------------------------------- #

# emission2hm -----------------------------------

#' @title neigbor2hm
#' @author amitjavilaventura
#'
#' @description This function allows to plot the heatmap of the overlap enrichments in several genomic regions obtained by chromHMM.
#' @description This function takes the x_n_overlaps.txt file as input, where x is the contition specified in the ChromHMM design matrix and n is the number of states used in the learnModel function.
#' @description Go to the chromHMM main page to get more information about chromHMM: http://compbio.mit.edu/ChromHMM/
#'
#' @usage enrich2hm(data, regions = c("fregion1", "region2", ...), states = c("E1", "E2", ...), title = "Main title", subtitle = "Subtitle", color = "cornflowerblue", scale_color = "scale", legend = F, label_size =2)
#'
#' @param data either a character vector with the file to be loaded or an object of class data.frame obtained by reading the input file with read.delim()
#' @param states a not-null haracter vector with the names of the states defined by crhomHMM. Initially should be set at paste("E", 1:n, sep=""), where n is the number of states defined in chromHMM.
#' @param title character vector of length 1 with the title of the heatmap.
#' @param subtitle character vector of lenght 1 with the subtitle of the heatmap.
#' @param xlab character vector of lenght 1 with the label to show in the x-axis.
#' @param color character vector of length 1 with the color to be used to color the heatmap. Default: "Cornflowerblue".
#' @param legend logical of length 1 indicating whether to show the legend or not. Default = F.
#' @param score_size numeric of length 1 with the size of the likelihood labels to draw in the heatmap. Default: 2.
#' @param show_scores logical of length 1 indicating whether to write in the plot the enrichment score of each of the positions. Default: T.
#'
#' @export
neighbor2hm <- function(data = NULL, states = NULL, title = "", subtitle = "", xlab = "Distance to TSS",
                        color = "Cornflowerblue", legend = F, score_size = 2, show_scores = T) {

  # PACKAGES
  require(tidyverse)
  require(magrittr)
  require(reshape2)
  require(ggpubr)
  require(purrr)

  # Require 'regions' and 'states' to be a character vector
  if(!is.character(states)){ return("'states' must be a not-null character vector") }

  # Read data either directly from a file or from a already read df. ChromHMM file (*_neighborhood.txt).
  if(is.character(data)){ df <- read.delim(data) }
  else if(is.data.frame(data)) { df <- data }
  else{ return("'data' should be one of character vector with a file path or data frame") }

  # Get position names
  positions <- colnames(df) %>%
    purrr::keep(~str_detect(string = .x, pattern = "State", negate = T)) %>%
    purrr::map_chr(~gsub(x = .x, pattern = "X\\.", replacement = "-")) %>%
    purrr::map(~gsub(x = .x, pattern = "X", replacement = ""))
  df <- df %>% set_colnames(c("State", positions)) %>% mutate(State = states)

  df.m <- df %>% melt() %>% group_by(variable) %>%
    mutate(total = sum(value), min = min(value), max = max(value)) %>% dplyr::ungroup()  %>%
    mutate(scale = (value-min)/max) %>%
    mutate(State = factor(State, levels = states))

  # Draw heatmap with ggplot2
  g <- ggplot(df.m, aes(variable, State, fill = value)) + geom_tile(show.legend = legend) +
    coord_equal() +
    scale_fill_gradient(low = "White", high = color) +
    ggtitle(title, subtitle) + ylab("State") + xlab(xlab) +
    theme_pubr() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
                         axis.title = element_text(face = "bold", size = 12),
                         axis.text = element_text(size = 10))


  if(show_scores) {
    g <- g +
      geom_text(mapping = aes(variable, State, label = round(value, 2)), size = score_size)
  }

  # Return plot
  return(g)
}
