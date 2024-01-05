## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(plotcli)  
library(ggplot2)  

mtcars$cf = as.character(mtcars$cyl)

mtcars_ggplot <- ggplot(mtcars, aes(x = mpg, y = wt, color = cf)) +  
  geom_point() +  
  labs(title = "Mtcars Dataset with Regression Line",  
       x = "Miles per Gallon",  
       y = "Weight")  
  
pp = ggplotcli(mtcars_ggplot, braille = FALSE)  
pp$print_plot()  

