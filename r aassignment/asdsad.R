install.packages("tidyverse")
library("tidyverse")
install.packages("plotly")
library("plotly")
install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")
indicator1 <- read.csv("indicator1.csv")
metadata <- read.csv("metadata.csv")

data_join <- full_join(metadata, indicator1)
data_join <- full_join(metadata, indicator1, by = join_by(country, year))
data_join <- full_join(metadata, indicator1, by = c("country" , "year" ))
full_join(metadata, indicator1, by = join_by(country, year))
full_join(metadata, indicator1, by = c("country" , "year" ))


#map
map_world <- map_data("world")

map_data_join <- full_join(indicator1, map_world, by = c("country" = "region"))                       
ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  labs(title = "World Map Visualization for All Years")                       
data_join_2015 <- data_join %>%
  filter(year == 2015)
map_data_join_2015 <- full_join(data_join_2015, map_world, by = c("country" = "region"))
ggplot(map_data_join_2015) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  labs(title = "Proportion of population with no handwashing facility at home")

#barchart

data_filtered <- data_join %>%
  filter(year >= 2018 & year <= 2022) %>%
  group_by(country) %>%
  summarize(avg_obs = mean(obs_value, na.rm = TRUE)) %>%
  filter(avg_obs > 50)
# Create a ggplot bar chart
ggplot(data_filtered, aes(x = reorder(country, avg_obs), y = avg_obs)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Country", y = "Average Obs Value", title = "Average Obs Value > 50 (2018-2022)")

# Convert to plotly for an interactive plot
bar_chart <- ggplot(data_filtered, aes(x = reorder(country, avg_obs), y = avg_obs, fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#FF9999", "#66B2FF", "#99FF99", "#FFCC99", "#CCCCFF")) +  # Custom colors
  theme_minimal() +
  labs(x = "Country", y = "Average Obs Value", title = "Average Obs Value > 50 (2018-2022)") +
  theme(legend.position = "none")

# To display the plot in an R environment that supports HTML widgets (like RStudio)
plotly_bar_chart
bar_chart <- bar_chart +
  theme(plot.background = element_rect(fill = "lightblue"),  # Change plot background color
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        text = element_text(size = 12, color = "darkblue"))

# Assuming data_filtered is already created and contains the required information

# Create a ggplot bar chart with specified colors using scale_fill_manual
bar_chart <- ggplot(data_filtered, aes(x = reorder(country, avg_obs), y = avg_obs, fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  # Use the viridis color scale for discrete data
  theme_minimal() +
  labs(x = "Country", y = "Average Obs Value", title = "Average Obs Value > 50 (2018-2022)") +
  theme(legend.position = "none")  # Hide the legend if not needed

# If you want specific colors, you can use scale_fill_manual as follows:
# bar_chart <- bar_chart + scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", ...))

# Convert to plotly for an interactive plot
plotly_bar_chart <- ggplotly(bar_chart)

# To display the plot
plotly_bar_chart



#scatter plot
scatter_plot <- ggplot(data_join, aes(x = year(), y = obs_value, group= country)) +
  geom_point(aes(color = country)) +  # Add points to the scatter plot, color-coded by country
  geom_smooth(method = "lm", se = FALSE, aes(group = country)) +  # Add a linear regression line per country
  labs(title = "Indicator over Years",
       x = "year",
       y = "obs_value") +
  theme_minimal()
filter(year >= 2018 & year <= 2022) %>%
  group_by(country) %>%
  scatter_plot
plotly_scatter_plot <- ggplotly(scatter_plot)

# Display the interactive plot in an environment that supports HTML widgets
plotly_scatter_plot
plotly_demo <- ggplotly(year(), tooltip = "text")
coord_flip()


install.packages("ggplot2")
library(ggplot2)

#timeseries

filtered_data <- data %>%
  filter(country == "Country Name", indicator == "Indicator Name") %>%
  select(year, obs_value) %>%
  arrange(year)

filtered_data <- data %>%
  dplyr::filter(country == "Country Name", indicator == "Indicator Name") %>%
  dplyr::select(year, obs_value) %>%
  dplyr::arrange(year)

ggplot(filtered_data, aes(x = year, y = obs_value)) +
  geom_line() +  # Line plot
  geom_point() +  # Points on the line
  labs(title = "Time Series of Indicator Values",
       x = "Year",
       y = "Observed Value (%)",
       subtitle = "Indicator: Indicator Name") +
  theme_minimal()


