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

data_join <- read.csv("indicator1.csv")  

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
  
 
  
  
  
  
 
  



