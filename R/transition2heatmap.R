# --------------------------------------------- #
# chromHMMviewR - transition2hm()               #
# --------------------------------------------- #

#' @title transition2hm
#' @author amitjavilaventura
#'
#' @description This function allows to plot the heatmap of the transition states obtained by the chromHMM learnModel function.
#' @description This function takes the transitions_x.txt file as input, where x is the number of states used in the learnModel function.
#' @description Go to the chromHMM main page to get more information about chromHMM: http://compbio.mit.edu/ChromHMM/
#'
#'
#'
#' @usage transition2hm(data, states = c("S1", "S2", ...), title = "Main title", subtitle = "Subtitle", xlab = "To", ylab = "From", color = "Tomato", label_size = 2, show_lh = T)
#'
#' @param data either a character vector with the file to be loaded or an object of class data.frame obtained by reading the input file with read.delim()
#' @param states a not-null haracter vector with the names of the states defined by crhomHMM. Initially should be set to something as paste("S", 1:n, sep=""), where n is the number of states defined in chromHMM.
#' @param title character vector of length 1 with the title of the heatmap.
#' @param subtitle character vector of lenght 1 with the subtitle of the heatmap.
#' @param xlab character vector of length 1 with the label to be shown in the x-axis of the heatmap. Default: "To".
#' @param ylab character vector of length 1 with the label to be shown in the y-axis of the heatmap. Default: "From".
#' @param color character vector of length 1 with the color to be used to color the heatmap. Default: "Cornflowrblue".
#' @param show_lh logical of length 1 indicating whether to show the numbers used to plot the heatmap.
#' @param lh_size numeric of length 1 with the size of the labels. Default: 2.
#'
#' @export

transition2hm <- function(data = NULL, states = NULL,
                          title = "", subtitle = "",
                          ylab = "From", xlab = "To",
                          color = "Cornflowerblue",
                          lh_size = 2, show_lh= T){
  # PACKAGES
  require(dplyr)
  require(magrittr)
  require(reshape2)
  require(ggplot2)
  require(ggpubr)

  # Require 'regions' and 'states' to be a character vector
  if(!is.character(states)){ return("Both 'features' and 'states' must be a not-null character vector") }

  # If data is raw output from chromHMM, use either file or df
  if(is.character(data)){ df <- read.delim(data) }
  else if(is.data.frame(data)) { df <- data }
  else{ return("'data' should be one of character vector with a file path or data frame") }

  # Process data
  df <- df %>% set_colnames(c("State", states)) %>% mutate(State = states) #%>% column_to_rownames("State")
  df.m <- df %>% melt() %>% mutate(State = factor(State, levels = rev(states)))


  # Draw heatmap with ggplot2
  g <- ggplot(df.m, aes(variable, State, fill = value)) + geom_tile(color = "gray", show.legend = F) +
    scale_fill_gradient(low = "White", high = color) +
    coord_equal() +
    ggtitle(title, subtitle) + ylab(ylab) + xlab(xlab) +
    theme_pubr(border = T) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
          axis.title = element_text(face = "bold", size = 12),
          axis.text = element_text(size = 10),
          plot.title = element_text(hjust=.5), plot.subtitle = element_text(hjust=.5))

  if(show_lh) {
    g <- g  +
      geom_text(mapping = aes(variable, State, label = round(value, 2)), size = lh_size)
  }

  # Return plot
  return(g)

}
