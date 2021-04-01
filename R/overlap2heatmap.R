# --------------------------------------------- #
# chromHMMviewR - overlap2hm()                  #
# --------------------------------------------- #

#' @title overlap2hm
#' @author amitjavilaventura
#'
#' @description This function allows to plot the heatmap of the overlap enrichments in several genomic regions obtained by chromHMM.
#' @description This function takes the x_n_overlaps.txt file as input, where x is the contition specified in the ChromHMM design matrix and n is the number of states used in the learnModel function.
#' @description Go to the chromHMM main page to get more information about chromHMM: http://compbio.mit.edu/ChromHMM/
#'
#' @usage overlap2hm(data, regions = c("fregion1", "region2", ...), states = c("E1", "E2", ...), title = "Main title", subtitle = "Subtitle", color = "cornflowerblue", scale_color = "scale", legend = F, label_size =2)
#'
#' @param data either a character vector with the file to be loaded or an object of class data.frame obtained by reading the input file with read.delim()
#' @param regions a not-null character vector with the names of the regions where the chromatin states have been overlapped.
#' @param states a not-null haracter vector with the names of the states defined by crhomHMM. Initially should be set at paste("E", 1:n, sep=""), where n is the number of states defined in chromHMM.
#' @param title character vector of length 1 with the title of the heatmap.
#' @param subtitle character vector of lenght 1 with the subtitle of the heatmap.
#' @param color character vector of length 1 with the color to be used to color the heatmap. Default: "Cornflowerblue".
#' @param scale_color character vector of length 1 indicating which option to use to color the heatmap. One of 'scale', 'percent' or 'value'. Default: 'scale'.
#' @param legend logical of length 1 indicating wether to show the legend or not. Default = F.
#' @param score_size numeric of length 1 with the size of the likelihood labels to draw in the heatmap. Default: 2.
#' @param show_score logical of length 1 indicating whether to show or not the enrichment score to find a certain feature in a certain state. Default: T
#'
#' @export
overlap2hm <- function(data = NULL, regions = NULL, states = NULL,
                       title = "", subtitle = "", color = "Cornflowerblue", scale_color = "scale",
                       legend = F, score_size = 2, show_score = T){

  # PACKAGES
  require(dplyr)
  require(magrittr)
  require(reshape2)
  require(ggplot2)
  require(ggpubr)

  # Require 'regions' and 'states' to be a character vector
  if(!is.character(regions) | !is.character(states)){ return("Both 'regions' and 'states' must be a not-null character vector") }

  # If data is raw output from chromHMM, use either file or df
  if(is.character(data)){ df <- read.delim(data) }
  else if(is.data.frame(data)) { df <- data }
  else{ return("'data' should be one of character vector with a file path or data frame") }

  # Process data
  df <- df %>% set_colnames(c("State", regions)) %>% filter(State != "Base") %>% mutate(State = states) #%>% column_to_rownames("State")
  df.m <- df %>% melt() %>% group_by(variable) %>%
    mutate(total = sum(value), min = min(value), max = max(value)) %>% dplyr::ungroup()  %>%
    mutate(scale = (value-min)/max, percent = value/total*100) %>%
    mutate(State = factor(State, levels = rev(states)))


  # Draw heatmap with ggplot2
  if( scale_color == "scale" ) { g <- ggplot(df.m, aes(variable, State, fill = scale)) + geom_tile(show.legend = legend) }
  else if( scale_color == "percent") { g <- ggplot(df.m, aes(variable, State, fill = percent)) + geom_tile(show.legend = legend) }
  else if( scale_color == "value") { g <- ggplot(df.m, aes(variable, State, fill = value)) + geom_tile(show.legend = legend) }
  else { return("The option scale_color must be one of 'scale', 'percent' or 'value'. Most reccommended is 'scale'.")}

  g <- g + scale_fill_gradient(low = "White", high = color) +
    coord_equal() +
    ggtitle(title, subtitle) + ylab("State") + xlab("Genomic regions") +
    theme_pubr(border = T) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10),
          plot.title= element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5))


  if(show_score) {
    g <- g  +
      geom_text(mapping = aes(variable, State, label = round(value, 2)), size = score_size)
  }

  # Return plot
  return(g)

}
