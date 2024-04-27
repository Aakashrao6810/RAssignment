install.packages("tidyverse")
library("tidyverse")
install.packages("plotly")
library("plotly")
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("maps")
library(maps)
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
plotly_bar_chart <- bar_chart +
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


# bar_chart <- bar_chart + scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", ...))

# Convert to plotly for an interactive plot
plotly_bar_chart <- ggplotly(bar_chart)

# To display the plot
plotly_bar_chart



#scatter plot
<- ggplot(data_join, aes(x = year, y = obs_value, group= country)) +
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
# Assuming data_join is already loaded and available
# Filter and prepare data
prepared_data <- data_join %>%
  dplyr::filter(year >= 2018 & year <= 2022) %>%
  dplyr::group_by(country)

# Now create the scatter plot using the prepared data
scatter_plot <- ggplot(prepared_data, aes(x = year, y = obs_value, color = country)) +
  geom_point() +  # Add points to the scatter plot, color-coded by country
  geom_smooth(method = "lm", se = FALSE, aes(group = country)) +  # Add a linear regression line per country
  labs(title = "Indicator over Years",
       x = "Year",
       y = "Observed Value") +
  theme_minimal()
# Load plotly if not already loaded
library(plotly)

# Convert the ggplot object to a plotly object
plotly_scatter_plot <- ggplotly(scatter_plot)

# Optionally, display the plotly plot
print(plotly_scatter_plot)


#timeseries

# Create a time series plot
time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value)) +
  geom_line() +  # Add a line to connect points
  geom_point() +  # Add points to each data entry
  labs(title = "Time Series of Indicator",
       x = "Year",
       y = "Observed Value") +
  theme_minimal()

# Print the plot
print(time_series_plot)

ggplot(data_filtered, aes(x = year, y = obs_value, group = country, color = country)) +
  geom_line() +  # Line plot to connect points year-wise for each country
  geom_point() +  # Points to show actual data points
  labs(title = "Time Series of Indicator Values from 2010 to 2020",
       x = "Year",
       y = "Indicator Value") +
  theme_minimal() +
  facet_wrap(~country, scales = "free_y")  # Optional: Create a separate plot for each country

filtered_data <- data %>%
  filter(year > 2018 & year < 2020)  # Adjust as needed to include exactly the years you want
library(ggplot2)

filtered_data <- data %>%
  filter(year >= 2018 & year < 2020, !is.na(obs_value)) %>%
  select(country, year, obs_value)
time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value, group = country, color = country)) +
  geom_line() +  # Add lines for each country
  geom_point() +  # Add points to highlight actual data points
  labs(title = "Observed Values by Country (2018-2019)",
       subtitle = "Data from indicator1.csv",
       x = "Year",
       y = "Observed Value",
       color = "Country") +  # Label for the legend
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position

# Print the plot
print(time_series_plot)
ggsave("time_series_plot.png", plot = time_series_plot, width = 10, height = 6)

# Creating the time series plot
ggplot(filtered_data, aes(x = year, y = obs_value, color = country)) +  # Color distinguishes countries
  geom_line() +  # Line plot showing trends over time
  labs(title = "Time Series of Observed Values", x = "Year", y = "Observed Value") +
  theme_minimal()
ggsave("timeseries_plot.png", plot = last_plot(), width = 10, height = 8)
