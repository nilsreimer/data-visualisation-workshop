rm(list = ls())

# INSTALL PACKAGES --------------------------------------------------------
  # install.packages(c("tidyverse", "viridis", "psych", "cowplot", "ggridges","ggrepel"))


# SET DEFAULT THEME -------------------------------------------------------
  library(tidyverse)
  theme_set(theme_grey(base_size = 14))
  fig_specs <- c(19.05, 19.05, 1200)

  
# EXAMPLE 1 ---------------------------------------------------------------
  
  # Load packages
  library(tidyverse)
  
  # Import data
  d1 <- read_rds("materials/d1.rds")
  
  # Plot 1
  ggplot(d1, aes(x = V1, y = V2, colour = V3)) +
    geom_point(aes(shape = V3), size = 2) +
    geom_smooth(aes(fill = V3), method = "lm") +
    scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
    scale_y_continuous(limits = c(0, 100)) +
    # scale_shape_discrete(solid = FALSE) +
    coord_fixed(ratio = 6/100) +
    facet_grid(V3 ~ V4)
  
  # Export for slides
  ggsave(file = "figures/f1.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  
# EXAMPLE 2 ---------------------------------------------------------------
  
  # Load packages
  library(tidyverse); library(modelr)
  
  # Import data
  d2 <- read_rds("materials/d2.rds")
  
  # Inspect data
  d2
  psych::describe(d2)
  View(d2)
  
  # Plot 2.1
  ggplot(d2, aes(x = time, y = attitudes)) +
    # stat_summary(geom = "bar", fun.y = "mean") +
    geom_violin(colour = NA, alpha = .5) + 
    geom_ref_line(h = 0) +
    geom_line(aes(group = id), alpha = .2) +
    geom_point(colour = "white", shape = 16) +
    geom_point(alpha = .2, shape = 16) +
    stat_summary(geom = "errorbar", 
                 fun.data = "mean_cl_boot",
                 width = 0.1) +
    stat_summary(geom = "point",
                 fun.y = "mean",
                 shape = 23, fill = "white", size = 4) +
    scale_y_continuous(limits = c(0, 100))
               
  
  # Export for slides
  ggsave(file = "figures/f2-1.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  
  # Inspect data
  d2 %>% select(-t1) %>% spread(time, attitudes)
  
  # Plot 2.2
  d2 %>% 
    select(-t1) %>% 
    spread(time, attitudes) %>%
    ggplot(., aes(x = Before, y = After)) +
    geom_point(aes(colour = After > Before),
               size = 2) +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    coord_fixed(ratio = 1)   
  
  # Export for slides
  ggsave(file = "figures/f2-2.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
    
  # I thank Matti Vuorre for the inspiration:
  # https://mvuorre.github.io/post/2017/within-subject-scatter/


# EXERCISE 1 --------------------------------------------------------------
  
  # Load packages
  library(tidyverse)
  
  # Import data
  d2 <- read_rds("materials/d2.rds")
  
  # Inspect data
  d2
  psych::describe(d2)
  View(d2)
  table(d2$time, d2$condition)
  
  # Find help
  ?ggplot
  ?stat_summary
  ?position_dodge
  
  ##############################################################################
  # If you have trouble seeing colour, add viridis::scale_colour_viridis() and #
  # viridis::scale_fill_viridis() to your plot; or use scale_colour_grey().    #
  ##############################################################################
  
  # Exercise 1.1  
  # Plot mean attitudes before and after the intervention as bars (see Plot 2.1). 
  # Use aes(fill) and position = "dodge" to differentiate between conditions. 
  # Try plotting condition on the x-axis, with fill = time.
  
  # Exercise 1.2  
  # Plot mean attitudes before and after the intervention as points with error-
  # bars (see Plot 2.1). Experiment with shape, colour, facets, and 
  # aes(group = condition) to visualise the effects of condition. Try connecting
  # condition-wise means with stat_summary(geom = "line", fun.y = "mean"). Try 
  # adding raw data / individual trajectories.
  
  # Exercise 1.3
  # Plot change over time as within-participant scatter plot (see Plot 2.2); try 
  # visualising the effects of condition on change over time.
  
  # Exercise 1.4
  # Figure out how these commands change the dataset (see below). Try using the 
  # transformed data to visualise the effects of condition on change over time.
  
  d2 %>%
    select(-t1) %>% 
    spread(time, attitudes) %>%
    mutate(diff = After - Before)
  
  # Exercise 1.5
  # Go back to Exercise 1.3, and visualise the effects of condition on change 
  # over time with geom_smooth(method = "lm"). Hint: Try aes(group = condition).
  
  
# EXAMPLE 3 ---------------------------------------------------------------
  
  # Load packages
  library(tidyverse); library(broom); library(modelr) 
  
  # Import data
  d2 <- read_rds("materials/d2.rds")
  d3 <- d2 %>% 
        select(-t1) %>% 
        spread(time, attitudes) %>%
        mutate(diff = After - Before)
  
  # Plot 2.3
  d2 %>% 
    select(-t1) %>% 
    spread(time, attitudes) %>%
  ggplot(., aes(x = Before, y = After, 
                  colour = condition)) +
    geom_point(aes(shape = condition), size = 2) +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_smooth(method = "lm") +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    facet_grid(. ~ condition) +
    coord_fixed(ratio = 1) +
    theme(legend.position = "none")
  
  # Export for slides
  ggsave(file = "figures/f2-3.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  
  # Import data
  d2 <- read_rds("materials/d2.rds")
  d3 <- d2 %>% 
        select(-t1) %>% 
        spread(time, attitudes) %>%
        mutate(diff = After - Before)
  
  # Estimate model
  fit  <- lm(diff ~ condition, data = d3) 
  pred <- augment_columns(fit, newdata = distinct(d3, condition)) 
  
  # Plot 3.1
  ggplot(d3, aes(x = condition, y = diff)) +
    geom_ref_line(h = 0) +
    geom_point(size = 2, colour = "grey70") +
    geom_errorbar(data = pred,
                  aes(y    = .fitted,
                      ymin = .fitted-1.96*.se.fit, 
                      ymax = .fitted+1.96*.se.fit),
                  width = 0.1) +
    geom_point(data = pred,
               aes(y = .fitted),
               shape = 23, fill = "white", size = 4)
  
  # Export for slides
  ggsave(file = "figures/f3-1.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  

# EXAMPLE 4 ---------------------------------------------------------------

  # Load packages
  library(tidyverse)
  
  # Import data
  d2 <- read_rds("materials/d2.rds")
  
  # Plot 4.1
  p4.1 <- d2 %>% 
    select(-t1) %>% 
    spread(time, attitudes) %>%
  ggplot(., aes(x = Before, y = After, colour = condition)) +
    geom_point(aes(shape = condition), size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_smooth(method = "lm") +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    facet_grid(. ~ condition) +
    coord_fixed(ratio = 1)
  
  # Install packages
  # devtools::install_github("cttobin/ggthemr") 
  # ggthemr::ggthemr("flat dark", type = "outer")
  
  # Prepare for publication
  p4.1 + 
    labs(
      x = "Before",
      y = "After",
      title = "Order effects in intergroup contact experiences",
      subtitle = "Consecutive positive contact experiences improved attitudes,\nmixed experiences did not.",
      caption = expression(italic("Reimer et al. (2018)")) 
    ) +
    theme_grey(base_size = 14) +
    theme(legend.position = "none")
  
  
  # Export for slides
  ggsave(file = "figures/f4.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  
  # Clear theme
  # ggthemr::ggthemr_reset()
  theme_set(theme_grey(base_size = 14))
  

# EXAMPLE 5 ---------------------------------------------------------------
  # Install packages
  # devtools::install_github("wilkelab/cowplot")
  # install.packages("colorspace", repos = "http://R-Forge.R-project.org")
  # devtools::install_github("clauswilke/colorblindr")
  
  # Load packages
  library(tidyverse); library(viridis); library(colorblindr)
  
  # Import data
  d5 <- read_rds("materials/d5.rds")
  
  # Plot 5.1
  p5.1 <- d5 %>%
    mutate(prop = total/n*1000) %>%
    filter(is.na(area) | area != "London, City of") %>%
  ggplot(., aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = prop)) +
    scale_fill_viridis(option = "A") +
    labs(fill = "Hate crimes reported\nper 1,000 people") +
    coord_map(xlim = c(-7, 2.5), ylim = c(49, 56.5)) +
    theme_void() +
    theme(legend.position = c(.85, .85))
  
  # Simulate color-vision deficiencies
  cvd_grid(p5.1)
  
  # Export for slides
  ggsave(p5.1, file = "figures/f5.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])


# EXAMPLE 6 ---------------------------------------------------------------
  
  # Load packages
  library(tidyverse)
  
  # Import data
  d6   <- read_rds("materials/d6.rds") %>% filter(V1 %in% 1:5, V2 %in% 1:5)
  d6_r <- read_rds("materials/d6_r.rds")
    
  
  # Plot 6.1
  ggplot(d6, aes(x1, x2)) +
    geom_point(alpha = .1) +
    geom_smooth(method = "lm") +
    facet_grid(V1 ~ V2)   
  
  # Export for slides
  ggsave(file = "figures/f6-1.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  
  # Plot 6.2
  ggplot(d6_r, aes(x = V1, y = V2)) +
    coord_fixed(xlim = c(0.5, 10.5), ylim = c(0.5, 10.5), expand = FALSE) +
    geom_text(data = filter(d6_r, V1 <=  V2), aes(label = round(r, 2), colour = r), size = 3) +
    geom_tile(data = filter(d6_r, V1 >=  V2), aes(fill = r), colour = NA) +
    geom_tile(data = filter(d6_r, V1 <=  V2), fill = NA, colour = "grey80") +
    geom_tile(data = filter(d6_r, V1 >   V2), fill = NA, colour = "white") +
    # geom_tile(aes(fill = r), colour = "white") +
    geom_hline(yintercept = c(0.5, 10.5), colour = "grey20") +
    geom_vline(xintercept = c(0.5, 10.5), colour = "grey20") +
    scale_x_continuous(breaks = 1:10, position = "top") +
    scale_y_reverse(breaks = 1:10) +
    scale_fill_distiller(type = "div", palette = "RdBu", direction = 1,
                         breaks = seq(-1, 1, 0.25), limits = c(-1, 1),
                         guide = guide_colourbar(
                           title.position = "left", 
                           title.hjust = 0.5,
                           title.theme = element_text(angle = 90),
                           barheight = unit(9, "cm"))) +
    scale_colour_distiller(type = "div", palette = "RdBu", direction = 1,
                           breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
    labs(
      x = NULL,
      y = "Items",
      colour = "Correlation",
      fill = "Correlation"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text = element_text(colour = "grey20"),
          panel.grid = element_blank())
  
  # Export for slides
  ggsave(file = "figures/f6-2.png", 
         width = 14, height = 10, units = "cm",
         type = "cairo-png", dpi = fig_specs[3])

  
# EXAMPLE 7 ---------------------------------------------------------------
  
  # Load packages
  library(tidyverse); library(ggridges); library(viridis)
  
  # Import data
  d7_est <- read_rds("materials/d7_est.rds")
  d7_sum <- read_rds("materials/d7_sum.rds")
  
  # Plot 7
  d7_est %>%
    left_join(d7_sum %>% select(Q1, VAR, p99), by = c("Q1", "VAR")) %>%
    mutate(p99 = ifelse(p99 == 5/5, 3/5, 1/5)) %>%
  ggplot(., aes(y = VAR, x = R_EST, fill = VAR, colour = VAR, alpha = p99)) +
    geom_vline(xintercept = 0.0, colour = "grey50", linetype = "dashed") +
    geom_density_ridges(aes(height = ..density..), 
                        scale = 0.9, 
                        stat = "density",
                        colour = NA) +
    geom_errorbarh(data = d7_sum, aes(xmin = l97, xmax = u97), height = 0, alpha = 1, colour = "grey90") +
    geom_errorbarh(data = d7_sum, aes(xmin = l97, xmax = u97), height = 0) +
    geom_point(data = d7_sum, shape = 16, alpha = 1, colour = "grey90") +
    geom_point(data = d7_sum, shape = 16) +
    scale_x_continuous(breaks = c(-0.3, 0.0, 0.3), minor_breaks = FALSE) +
    scale_colour_viridis(discrete = TRUE, begin = 1, end = 0) +
    scale_fill_viridis(discrete = TRUE, begin = 1, end = 0) +
    scale_alpha_continuous(range = c(1/5, 5/5)) +
    labs(x = expression(italic("r"))) +
    facet_grid(. ~ Q1) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_text(hjust = 0.0, vjust = 0.0, colour = "grey20"),
          axis.ticks.x = element_line(colour = "grey50", size = 0.5),
          panel.border = element_rect(colour = "grey50", fill = NA),
          strip.background = element_rect(colour = NA, fill = NA))
  
  # Export for slides
  ggsave(file = "figures/f7.png", 
         width = 16, height = 8, units = "cm",
         type = "cairo-png", dpi = fig_specs[3])
  
# EXAMPLE 8 ---------------------------------------------------------------
  
  # Load packages
  library(tidyverse); library(ggrepel); library(viridis)
  
  # Import data
  d8 <- read_rds("materials/d8.rds")
  
  # Plot 8
  p8 <- ggplot(d8, aes(x = GM, y = SCST)) +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey20") +
    geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey20") +
    annotate(x = c(-Inf, Inf, Inf, -Inf), y = c(Inf, -Inf, Inf, Inf), fill = "white", geom = "polygon") +
    geom_abline(intercept = 1, slope = -1, linetype = "solid", colour = "grey20") +
    geom_point(data = filter(d8, is.na(SELECTION)), aes(shape = NATION_RELIGION), size = 3, colour = "grey80") +
    geom_point(data = filter(d8, !is.na(SELECTION)), aes(colour = SELECTION, shape = NATION_RELIGION), size = 3) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    scale_colour_viridis(discrete = TRUE) +
    labs(
      x = "GM",
      y = "SC/ST",
      shape = "Nation, Religion",
      colour = "Selection"
    ) +
    coord_fixed() +
    theme(legend.justification = c(0.25, 1),
          legend.position = c(0.75, 1),
          legend.box.just = "right")
  
  # Annotate
  p8 + geom_text_repel(aes(label = LNAME, colour = SELECTION))

  # Export for slides
  ggsave(file = "figures/f8.png", 
         width = fig_specs[1], height = fig_specs[2], units = "cm", 
         type = "cairo-png", dpi = fig_specs[3])
  