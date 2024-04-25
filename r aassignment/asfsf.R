install.packages("tidyverse")
library("tidyverse")
install.packages("plotly")
library("plotly")
cleaned_meta <- read_csv("cleaned_meta.csv")
data_left <- read_csv("data_left.csv")

data_join <- full_join(cleaned_meta, data_left)
data_join <- full_join(cleaned_meta, data_left, by = join_by(country, year))
data_join <- full_join(cleaned_meta, data_left, by = c("country" , "year" ))
full_join(cleaned_meta, data_left, by = join_by(country, year))
full_join(cleaned_meta, data_left, by = c("country" , "year" ))


map_world<- map_data("world")

#map1
map_data_join <- full_join(data_left, map_world, by = c("country" = "region"))

ggplot(map_data_join) +
  aes(x = long, y = lat,group = group, fill = obs_value) +
  geom_polygon()
#map2
data_join_2020 <- data_join %>%
  filter(year == 2015)  
map_data_join_2020 <- full_join(data_join, map_world, by = c("country" = "region"))
ggplot(map_data_join_2020) +
  aes(x = long, y = lat,group = group, fill = obs_value) +
  geom_polygon()

