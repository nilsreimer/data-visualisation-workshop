rm(list = ls())

# Instructions ------------------------------------------------------------
  # Start by running all code up to Exercise 1, before following the
  # instructions. Add your code after each instruction and use the
  # export_png() function to save your figure. Send the completed figures
  # and script to nils.reimer@psy.ox.ac.uk.


# Install packages --------------------------------------------------------
  # install.packages("tidyverse")


# Load packages -----------------------------------------------------------
  library(tidyverse)

  # Options
  theme_set(theme_grey(base_size = 14))
  
  
# Functions ---------------------------------------------------------------
  export_png <- function(name) {
    ggsave(
      file = paste0("practice/", name, ".png"),
      width = 19.05, height = 19.05, units = "cm",
      type = "cairo-png", dpi = 600
    )
  }
  

# Exercise 1 --------------------------------------------------------------
  
  # Import data 
  dl <- read_rds("materials/gwtp/dl_wk4.rds")
  
  # Inspect data
  print(dl, n = 10)
  count(dl, time, condition)
  
  #########################################################################
  # Compare mean attitudes before and after the manipulation in a bar     #
  # plot. (Hint: Use stat_summary() as in Example 3.) Add error bars to   #
  # reflect the uncertainty around the mean.                              #
  #########################################################################

  
  
  # Save figure
  export_png("exercise-1.png")
  

# Exercise 2 --------------------------------------------------------------
  
  #########################################################################
  # Compare mean attitudes before and after the manipulation across the   #
  # three experimental conditions. Use a bar plot (see above) with        #
  # aes(fill = condition) and position = "dodge" to differentiate between #
  # conditions. Alternatively, try mapping condition to the x-axis and    #
  # use fill = time to differentiate between time points.                  #
  #########################################################################
  
  
  
  # Save figure
  export_png("exercise-2.png")


# Exercise 3 --------------------------------------------------------------
  
  # Transform data
  dw <- dl %>% spread(time, attitudes) %>% mutate(diff = After - Before)
  
  # Inspect data
  print(dw, n = 10)
  count(dw, condition)
  
  #########################################################################
  # Experiment with different ways of visualising change over time across #
  # experimental conditions. (Hint: Visualise change scores (diff) with   #
  # geom_point and condition mapped to the x-axis. Alternatively, use     #
  # colours, shapes, or facets to differentiate conditions and/or use a   #
  # within-subjects scatterplot.) Save the plot you like the most.        #
  #########################################################################
  
  
  
  # Save figure
  export_png("exercise-3.png")
  
  #########################################################################
  # Write a few sentences about the advantages and disadvantages of the   #
  # data visualisation you have created.                                  #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #                                                                       #
  #########################################################################
  
  
# Feedback ----------------------------------------------------------------
  # At the end of the lecture, please fill in this brief teaching evaluation:
  # bit.ly/2WQZszf
