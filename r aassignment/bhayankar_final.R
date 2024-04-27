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

ggplot(data_filtered, aes(x = reorder(country, avg_obs), y = avg_obs)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Country", y = "Average Obs Value", title = "Average Obs Value > 50 (2018-2022)")


bar_chart <- ggplot(data_filtered, aes(x = reorder(country, avg_obs), y = avg_obs, fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#FF9999", "#66B2FF", "#99FF99", "#FFCC99", "#CCCCFF")) +  # Custom colors
  theme_minimal() +
  labs(x = "Country", y = "Average Obs Value", title = "Average obs value of a country > 50 (2018-2022)") +
  theme(legend.position = "none")


plotly_bar_chart <- bar_chart +
  theme(plot.background = element_rect(fill = "lightblue"),  # Change plot background color
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        text = element_text(size = 12, color = "darkblue"))



# Create a ggplot bar chart with specified colors using scale_fill_manual
bar_chart <- ggplot(data_filtered, aes(x = reorder(country, avg_obs), y = avg_obs, fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d() +  
  theme_minimal() +
  labs(x = "Country", y = "Average Obs Value", title = "Average Obs Value > 50 (2018-2022)") +
  theme(legend.position = "none")  
plotly_bar_chart <- ggplotly(bar_chart)
plotly_bar_chart



#scatter plot

 ggplot(data_join, aes(x = year, y = obs_value, group= country)) +
  geom_point(aes(color = country)) +  
  geom_smooth(method = "lm", se = FALSE, aes(group = country)) +  
  labs(title = "Indicator over Years",
       x = "year",
       y = "obs_value") +
  theme_minimal()
filter(year >= 2018 & year <= 2022) %>%
  group_by(country) %>%
  scatter_plot
plotly_scatter_plot <- ggplotly(scatter_plot)

plotly_scatter_plot
plotly_demo <- ggplotly(year(), tooltip = "text")
coord_flip()

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

library(plotly)


plotly_scatter_plot <- ggplotly(scatter_plot)

print(plotly_scatter_plot)

  

prepared_data <- data_join %>%
  dplyr::filter(year >= 2018 & year <= 2022) %>%  
  dplyr::group_by(country)                       

scatter_plot <- ggplot(prepared_data, aes(x = year, y = obs_value, color = country)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, aes(group = country)) +  
  labs(title = "Indicator Over Years",
       x = "Year",
       y = "Observed Value") +
  theme_minimal()

plotly_scatter_plot <- ggplotly(scatter_plot)

print(plotly_scatter_plot)

time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value)) +
  geom_line() +  
  geom_point() +  
  labs(title = "Time Series of Indicator",
       x = "Year",
       y = "Observed Value") +
  theme_minimal()
  #adffffffffffffffffffffff
  prepared_data <- data_join %>%
    dplyr::filter(year >= 2018 & year <= 2022) %>%
    dplyr::group_by(country)
  
  # Create the scatter plot with linear regression lines for each country
  scatter_plot <- ggplot(prepared_data, aes(x = year, y = obs_value, color = country)) +
    geom_point() +  # Add points to the scatter plot, color-coded by country
    geom_smooth(method = "lm", se = FALSE, aes(group = country)) +  # Add a linear regression line per country
    labs(title = "Indicator Over Years",
         x = "Year",
         y = "Observed Value") +
    theme_minimal()
  
  # Convert the ggplot object to a plotly object for interactive visualization
  plotly_scatter_plot <- ggplotly(scatter_plot)
  
  # Display the interactive plot
  print(plotly_scatter_plot)
  
  
# Print the plot

  filtered_data <- data %>%
    filter(year >= 2018 & year < 2020, !is.na(obs_value)) %>%
    select(country, year, obs_value)
  
  # Create a ggplot
  time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value, color = country, group = country)) +
    geom_line() +  # Line plot for trends over time
    geom_point() +  # Points to highlight actual data points
    labs(title = "Observed Values by Country (2018-2019)",
         subtitle = "Data from indicator1.csv",
         x = "Year",
         y = "Observed Value",
         color = "Country") +  # Label for the legend
    theme_minimal() +
    theme(legend.position = "bottom")  # Adjust legend position
  
  # Convert ggplot to an interactive Plotly plot
  interactive_plot <- ggplotly(time_series_plot)
  
  # Save the interactive plot as HTML file
  htmlwidgets::saveWidget(interactive_plot, "time_series_plot.html")
  
  # Print the interactive plot to view in RStudio Viewer or compatible environment
  print(interactive_plot)
  
  
  
  
  print(class(data))
  
  # Use explicit dplyr::filter to avoid any namespace conflict
  filtered_data <- data %>%
    dplyr::filter(year >= 2018 & year < 2020, !is.na(obs_value)) %>%
    select(country, year, obs_value)
  
  # Create a ggplot
  time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value, color = country, group = country)) +
    geom_line() +  # Line plot for trends over time
    geom_point() +  # Points to highlight actual data points
    labs(title = "Observed Values by Country (2018-2019)",
         subtitle = "Data from indicator1.csv",
         x = "Year", y = "Observed Value", color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Convert to interactive plot using Plotly
  interactive_plot <- ggplotly(time_series_plot)
  
  # Print the interactive plot
  print(interactive_plot)
  
  
  
  #timeseries
  ggplotly(time_series_plot)
  filtered_data <- data %>%
    filter(year >= 2015 & year <= 2020, !is.na(obs_value)) %>%
    arrange(country, year)
  time_series_plot <- data_join %>%
    ggplot()+
    aes(year, obs_value, group = country, colour= country) +
    geom_line()
  
    # Optional: arrange data by country and year for better visibility
  
  # Generate the time series plot using ggplot
  time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value, color = country, group = country)) +
    geom_line() +  # Add line for each country
    geom_point() +  # Optionally add points to the lines
    labs(title = "Observed Values by Country (2015-2020)",
         subtitle = "Data extracted from indicator1.csv",
         x = "Year",
         y = "Observed Value",
         color = "Country") +  # Labels and titles
    theme_minimal() +
    theme(legend.position = "bottom")  # Adjust the position of the legend
  
  # Convert the ggplot object to an interactive plot using plotly
  interactive_plot <- ggplotly(time_series_plot)
  
  # Save the interactive plot as HTML for sharing or viewing in browsers
  htmlwidgets::saveWidget(interactive_plot, "time_series_plot.html")
  
  # Print the interactive plot to view in RStudio Viewer or compatible environment
  print(interactive_plot)
  
  
  
  
  ********************************
    
    
    filtered_data2 <- dplyr::filter(data, year >= 2017 & year <= 2021, !is.na(obs_value))
  filtered_data2 <- dplyr::arrange(filtered_data2, country, year)
  
  time_series_plot <- ggplot(filtered_data2, aes(x = year, y = obs_value, color = country, group = country)) +
    geom_line() +  # Add line for each country
    geom_point() +  # Optionally add points to the lines
    labs(title = "Observed Values by Country (2017-2021)",
         subtitle = "Data extracted from indicator1.csv",
         x = "Year",
         y = "Observed Value",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom")
  interactive_plot <- ggplotly(time_series_plot)
  print(interactive_plot)
  
  
  
  filtered_data <- data_join %>%
    filter(
      country %in% c( "India", "Bangladesh", "Pakistan", "Indonesia", "Philippines")) %>%
    group_by(country)
  time_series_plot <- ggplot(filtered_data, aes(x = year, y = obs_value, color = country, group = country)) +
    geom_line() +  # Add line for each country
    geom_point() +  # Optionally add points to the lines
    labs(title = "Observed Values by Country (2017-2021)",
         subtitle = "Data extracted from indicator1.csv",
         x = "Year",
         y = "Observed Value",
         color = "Country") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Convert the plot to an interactive Plotly plot
  interactive_plot <- ggplotly(time_series_plot, tooltip = c("x", "y", "color"))
  
  # Print the interactive plot
  print(interactive_plot)
  
  
  
  
  #scatter plot
  
  filtered_data <- data_join %>%
    filter(
      country %in% c( "India", "Bangladesh", "Pakistan", "Germany", "Philippines")) %>%
    group_by(country)
  # Create scatter plot with regression line
  scatter_plot <- ggplot(filtered_data, aes(x = GNI..current.US.., y = Military.expenditure....of.GDP.)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "lm", se = FALSE) +  # Add regression line without confidence interval
    labs(x = "Military Eexpenditure as % of GDP", y = "GPD Per Capita", title = "Scatter Plot with Regression Line") +  # Labels
    theme_minimal()  # Optional: Customize plot theme
  
  # Convert to plotly for an interactive plot
  plotly_scatter_plot <- ggplotly(scatter_plot, tooltip = "text")
  
  # If you're in an R environment that supports it, you can directly display the plot
  plotly_scatter_plot
  
  head(filtered_data)
  

  
  
  filtered_data <- data_join %>%
    filter(country %in% c("India", "Pakistan", "Germany")) %>%
    group_by(country)
  
  # Define a color palette for the countries
  country_colors <- c("India" = "#E41A1C", "Pakistan" = "#4DAF4A",
                      "Germany" = "#984EA3")
  
  # Create the scatter plot with custom colors
  scatter_plot <- ggplot(filtered_data, aes(x = GNI..current.US.., y = Military.expenditure....of.GDP., color = country)) +
    geom_point() +  # Scatter plot
    geom_smooth(method = "lm", se = FALSE) +  # Add regression line without confidence interval
    scale_color_manual(values = country_colors) +  # Apply custom colors
    labs(x = "Military Expenditure as % of GDP", y = "GNI (Current US$)",
         title = "Scatter Plot with Regression Line",
         subtitle = "Analyzing the relationship between military spending and GNI per capita") +
    theme_minimal()  # Optional: Customize plot theme
  
  # Convert to an interactive plotly plot
  plotly_scatter_plot <- ggplotly(scatter_plot, tooltip = "text")
  
  # Display the plot
  plotly_scatter_plot
  

  library(tidyverse)
  
  # Assuming threshold is defined here. Adjust this as needed.
  threshold <- 5
  
  # Read the CSV file
  data <- read_csv("indicator1.csv")
  
  # Filter the data for entries with observation values above 50 and countries with enough data points
  filtered_data <- data %>%
    filter(obs_value > 50) %>%
    group_by(country) %>%
    filter(n() >= threshold) %>%
    ungroup() %>%
    select(country, year, obs_value)
  
  # Plot the time series chart
  plot <- ggplot(filtered_data, aes(x = year, y = obs_value, color = country)) +
    geom_line() +
    geom_point(size = 1.5, alpha = 0.8) +
    labs(
      title = "Time Series of Observation Values by Country",
      x = "Year",
      y = "Observation Value",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    ) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  
  print(plot)
  
  # Save the plot to a file
  ggsave("observation_values_time_series.png", plot, width = 12, height = 6)

  
  
  
  
  
  threshold <- 5
  
  # Read the CSV file
  data <- read.csv("indicator1.csv")
  
  # Filter the data for entries with observation values above 50 and countries with enough data points
  filtered_data <- data %>%
    filter(obs_value > 50) %>%
    group_by(country) %>%
    filter(n() >= threshold) %>%
    ungroup() %>%
    select(country, year, obs_value)
  
  # Create the plot using ggplot
  ggplot_object <- ggplot(filtered_data, aes(x = year, y = obs_value, color = country)) +
    geom_line() +
    geom_point(size = 1.5, alpha = 0.8) +
    labs(
      title = "Time Series of Observation Values by Country",
      x = "Year",
      y = "Observation Value",
      color = "Country"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    ) +
    guides(colour = guide_legend(override.aes = list(size=4)))
  
  # Convert ggplot to an interactive Plotly graph
  plotly_object <- ggplotly(ggplot_object)
  
  # Display the interactive plot
  plotly_object
  

  
  metadata <- read.csv("metadata.csv")
  
  
  # Line time series plot
  # Filter data for India and France
  data_selected <- subset(metadata, country %in% c("India", "China"))
  
  # Plot time series data for India and France
  ggplot(data_selected, aes(x = year, y = Population..total, color = country, group = country)) +
    geom_line() +
    labs(x = "Date", y = "Population", title = "Time Series Data for India and China") +
    theme_minimal()
