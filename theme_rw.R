#######################################
## author: Rob Williams              ##
## contact: jayrobwilliams@gmail.com ##
## project: misc R functions         ##
## created: November 5, 2017         ##
## updated: November 5, 2017         ##
#######################################

## this function collects several ggplot2 theme elements that remove the default
## background grid

theme_rw <- function() {
  theme_bw() +
  theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), panel.border = element_blank())
  
}