## ---------------------------
##
## Script name: 
##
## Author: Dr. Joan Dudney
##
## Date Created: 2026-02-01
##
## Copyright (c) Joan Dudney, 2026
## Email: dudney@ucsb.edu
##
## ---------------------------

library(tidyverse)
library(lme4)
library(patchwork)
library(marginaleffects)
library(ggeffects)


# Read the two CSV files
rwi_filtered <- read_csv("rwi_filtered.csv")
rwi_sept <- read_csv("RWI_sept4.csv")

rwi_data <- rwi_filtered |>
  left_join(
    rwi_sept |> select(plot_id_needle, tree_num, year, tmax, ppt),
    by = c("plot" = "plot_id_needle", "tree_num", "year")
  )


# Fit the GLMER model
model <- lmer(
  rwi ~ tmax * ppt + (1 | plot),
  data = rwi_data
)


############################################################
# Creating figures
############################################################
# Get the precipitation range
ppt_range <- range(rwi_data$ppt, na.rm = TRUE)
ppt_range

# Create predictions 
pred_ppt_fixed <- ggpredict(model, terms = c("ppt", "tmax"))

# plot
plotppt <- plot(pred_ppt_fixed)
margplot <- plot(plotppt)+ 
  scale_color_manual(values = c("#2e2930ff", "#773a86ff", "#751ac5f2")) + 
  scale_fill_manual(values = c("#2e2930ff", "#773a86ff", "#751ac5f2"))+
  labs(title = "",
    x = "Precipitation (mm)",
    y = "RWI"
  ) 

margplot


############################################################
# Figures
############################################################
# Calculate mean temperatures for three time periods
temp_1900_1980 <- rwi_data |>
  filter(year >= 1900 & year < 1980) |>
  summarise(mean_tmax = mean(tmax, na.rm = TRUE)) |>
  pull(mean_tmax)

temp_1980_2000 <- rwi_data |>
  filter(year >= 1980 & year < 2000) |>
  summarise(mean_tmax = mean(tmax, na.rm = TRUE)) |>
  pull(mean_tmax)

temp_2000_2020 <- rwi_data |>
  filter(year >= 2000 & year <= 2020) |>
  summarise(mean_tmax = mean(tmax, na.rm = TRUE)) |>
  pull(mean_tmax)


# Calculate slopes (marginal effects) of precipitation at different temperature values
# Use the three period mean temperatures
slopes_ppt <- slopes(
  model,
  variables = "ppt",
  newdata = datagrid(
    tmax = c(temp_1900_1980, temp_1980_2000, temp_2000_2020),
    ppt = mean(rwi_data$ppt, na.rm = TRUE)
  )
)

# Tibble for plotting
slopes_df <- slopes_ppt |>
  as_tibble() |>
  select(tmax, estimate, std.error, conf.low, conf.high) |>
  mutate(
    time_period = c("1900-1980", "1980-2000", "2000-2020"),
    time_period = factor(time_period, levels = c("1900-1980", "1980-2000", "2000-2020"))
  )

slopes_df

# Create figure with marginal effects from slopes() function
fig_slopes_marginal <- ggplot(slopes_df, aes(x = time_period, y = estimate, color = time_period)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  size = 1, linewidth = 1) +
  scale_color_manual(values = c("#440154FF", "#21908CFF", "#FDE725FF")) +
  labs(
    x = "Time Period",
    y = "Marginal Effect of Precipitation"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10)
  )

fig_slopes_marginal


# Create tempfig with period-specific mean temperature lines
tempfig <- rwi_data |> 
  na.omit() |> 
  group_by(year) |> 
  summarize(temp_mean = mean(tmax, na.omit = T)) |> 
  ggplot(aes(x = year, y = temp_mean)) +
  geom_point(size = 1, color = "black", alpha=.5) +
  geom_smooth(color = "black", alpha=.5) +
  geom_line(color = "black", alpha=.5) +
  # Add colored background rectangles for each time period
  annotate("rect", xmin = 1900, xmax = 1980, ymin = -Inf, ymax = Inf, 
           fill = "#440154FF", alpha = 0.1) +
  annotate("rect", xmin = 1980, xmax = 2000, ymin = -Inf, ymax = Inf, 
           fill = "#21908CFF", alpha = 0.1) +
  annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, 
           fill = "#FDE725FF", alpha = 0.1) +
  # Add horizontal dashed lines for mean temperature ONLY within each period
  annotate("segment", x = 1900, xend = 1980, 
           y = temp_1900_1980, yend = temp_1900_1980,
           color = "#440154FF", linewidth = 1) +
  annotate("segment", x = 1980, xend = 2000, 
           y = temp_1980_2000, yend = temp_1980_2000,
           color = "#21908CFF", linewidth = 1) +
  annotate("segment", x = 2000, xend = 2020, 
           y = temp_2000_2020, yend = temp_2000_2020,
          color = "#FDE725FF", linewidth = 1) +
  labs(
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 10)
  )

tempfig

margplot + (tempfig/fig_slopes_marginal)

tempfig/ (margplot + fig_slopes_marginal)

