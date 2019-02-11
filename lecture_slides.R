rm(list = ls())

# Install packages --------------------------------------------------------
  # install.packages(c("tidyverse", "cowplot"))
  # devtools::install_github("clauswilke/colorblindr")


# Load packages -----------------------------------------------------------
  library(tidyverse)

  # Options
  theme_set(theme_grey(base_size = 14))
  

# Functions ---------------------------------------------------------------
  export_png <- function(name) {
    ggsave(
      file = paste0("figures/", name, ".png"),
      width = 19.05, height = 19.05, units = "cm",
      type = "cairo-png", dpi = 600
    )
  } 


# Introduction ------------------------------------------------------------
  anscombe %>%
    mutate(id = 1:n()) %>%
    gather("key", "value", -id) %>%
    mutate(
      quartet = str_extract(key, "[0-9]"),
      variable = str_extract(key, "[a-z]")
    ) %>%
    select(-key) %>%
    spread(variable, value) %>% 
  ggplot(., aes(x, y)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    annotate(geom = "text", label = "r = .82", x = 0, y = 20, 
             hjust = 0, vjust = 1, size = 6, colour = "grey20") +
    facet_wrap("quartet", nrow = 2) +
    scale_x_continuous(breaks = seq(0, 20, 5), limits = c(0, 20)) +
    scale_y_continuous(breaks = seq(0, 20, 5), limits = c(0, 20)) +
    coord_fixed() +
    cowplot::theme_cowplot(font_size = 18) +
    cowplot::background_grid() +
    cowplot::panel_border(colour = "grey20") + 
    theme(strip.background = element_blank()) +
    labs(
      title = "Anscombe's Quartet", 
      subtitle = "Four reasons why we should plot our data."
    )

  # Export
  export_png("anscombe.png")


# Example 1: A First Plot -------------------------------------------------
  
  # Load packages
  library(tidyverse)
  
  # Import data
  d1 <- read_rds("materials/d1.rds")
  
  # Inspect data
  print(d1, n = 5)
  count(d1, V3, V4)
  summary(d1)
  
  # Figure 1
  ggplot(d1, aes(x = V1, y = V2)) +
    geom_point(
      aes(shape = V3, colour = V3),
      size = 2
    ) +
    scale_x_continuous(
      limits = c(1, 7),
      breaks = 1:7,
      minor_breaks = NULL
    ) + 
    scale_y_continuous(limits = c(0, 100)) +
    coord_fixed(ratio = 6/100) +
    facet_grid(. ~ V4)
  
  # Export
  export_png("f1")


# Example 2: Facets and Curves --------------------------------------------
  
  # Load packages
  library(tidyverse)
  
  # Import data
  d1 <- read_rds("materials/d1.rds")
  
  # Figure 2
  ggplot(d1, aes(x = V1, y = V2, colour = V3)) +
    geom_point(
      aes(shape = V3),
      size = 2
    ) +
    geom_smooth(
      aes(fill = V3, linetype = V3),
      method = "lm"
    ) + 
    scale_x_continuous(
      limits = c(1, 7),
      breaks = 1:7,
      minor_breaks = NULL
    ) + 
    scale_y_continuous(limits = c(0, 100)) +
    coord_fixed(ratio = 6/100) +
    facet_grid(. ~ V4)
  
  # Export
  export_png("f2")
  
  # Results
  lm(V2 ~ V1, data = d1, subset = (V3 == "Control" & V4 == "Group 1"))
  

# Example 3: Within-Subjects Plots ----------------------------------------
  
  # Import data 
  dl <- read_rds("materials/gwtp/dl_wk3.rds")
  
  # Inspect data
  print(dl, n = 5)
  
  # Figure 3
  ggplot(dl, aes(
      x = time, y = outcome, colour = group
    )) +
    geom_hline(
      yintercept = 0,
      colour = "white",
      size = 2
    ) +
    geom_line(aes(group = person)) +
    geom_point() +
    scale_y_continuous(limits = c(0, 100)) +
    coord_fixed(1/25)
  
  # Transform data
  dw <- dl %>% spread(time, outcome)
  print(dw, n = 5)

  # Figure 3
  ggplot(dw, aes(x = before, y = after)) +
    geom_abline(
      intercept = 0, 
      slope = 1, 
      linetype = "dashed"
    ) +
    geom_point() +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    coord_fixed(1)

  # Export
  export_png("f3")
  

# Example 4: Models -------------------------------------------------------
  
  # Import data 
  dl <- read_rds("materials/gwtp/dl_wk4.rds")
  
  # Inspect data
  print(dl, n = 5)
  count(dl, time, condition)
  
  # Transform data 
  dw <- dl %>% spread(time, attitudes)
  
  # Figure 4
  fig <- ggplot(dw, aes(
      x = Before,
      y = After,
      colour = condition
    )) + 
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) + 
    geom_point(aes(shape = condition)) + 
    scale_x_continuous(
      limits = c(0, 100), minor_breaks = NULL
    ) + 
    scale_y_continuous(
      limits = c(0, 100), minor_breaks = NULL
    ) + 
    facet_grid(. ~ condition) + 
    coord_fixed(1) + 
    theme(legend.position = "none")
      
  
  # Export
  export_png("f4")
  
  # Load package
  library(broom)
  
  # Estimate model
  fit <- lm(After ~ Before + condition, data = dw)
  summary(fit)
  tidy(fit)
  augment(fit)
  
  # Figure 4
  fig + 
    geom_line(
      data = augment(fit),
      aes(y = .fitted),
      size = 1
    ) + 
    geom_ribbon(
      data = augment(fit),
      aes(ymin = .fitted - 1.96*.se.fit, 
          ymax = .fitted + 1.96*.se.fit), 
      colour = NA,
      alpha = 0.25
    )
  
    # Export
  export_png("f4")
  
  # Figure 4
  ggplot(augment(fit), aes(Before, After)) + 
    geom_point(aes(colour = .cooksd)) +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    facet_grid(. ~ condition) + 
    coord_fixed(1)
  
  # Export
  export_png("f4")

  # Figure 4
  ggplot(tidy(fit), aes(
      x = term, 
      y = estimate, 
      ymin = estimate - 1.96*std.error, 
      ymax = estimate + 1.96*std.error)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange() +
    coord_flip()
  
  # Export
  export_png("f4")
  
# Example 5: Themes and Labels --------------------------------------------

  # Figure 4
  fig <- fig + 
    geom_line(
      data = augment(fit), 
      aes(y = .fitted), 
      size = 1
    ) + 
    geom_ribbon(
      data = augment(fit),
      aes(ymin = .fitted - 1.96*.se.fit, 
          ymax = .fitted + 1.96*.se.fit),
      size = 1,
      colour = NA, 
      alpha = 0.25
    )
  
  # Figure 5
  fig + 
    # labs(
    #   title = "Order effects in intergroup contact experiences",
    #   subtitle = "Consecutive positive contact experiences improved attitudes,\nmixed experiences did not.",
    #   caption = expression(italic("Reimer et al., 2018"))
    # ) +
    cowplot::theme_cowplot(font_size = 14) +
    theme(
      legend.position = "none",
      strip.background = element_blank()
    )
  
  # Export
  export_png("f5")

  ggsave(
    "figures/f5.png", 
    width = 19.05, height = 19.05, units = "cm", 
    dpi = 600, 
    type = "cairo-png"
  )
  

# Example 6: Colours ------------------------------------------------------
  
  # Import data
  dm <- read_rds("materials/d5.rds")
  
  # Figure 6
  ggplot(dm, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill = "grey50") +
    geom_polygon(aes(fill = region)) +
    scale_fill_viridis_d() +
    coord_map(xlim = c(-7, 2.5), ylim = c(49, 56.5)) +
    theme_void() +
    theme(legend.position = "none")
                 
  # Simulate color-vision deficiencies
  colorblindr::cvd_grid()

  # Export
  export_png("f6")

  # Figure 6
  ggplot(dm, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill = prop)) +
    scale_fill_viridis_c(option = "A") +
    labs(
      fill = "Hate crimes reported\nper 1,000 people"
    ) +
    coord_map(xlim = c(-7, 2.5), ylim = c(49, 56.5)) +
    theme_void() +
    theme(legend.position = c(.85, .85))
      
  # Export
  export_png("f6")
  
