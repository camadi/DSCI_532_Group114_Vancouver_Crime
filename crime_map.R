library(tidyverse)
library(cowplot)
library(plotly)
library(sf)
library(geojsonio)
# Reading Vancouver City Population from 2001 to 2018
pop_yr <- read_csv("Data/Population_trend.csv")
pop_yr <- pop_yr %>% select(YEAR, Population)

# Reading each neighborhood's proportion of population to overall city's population
pop_prop <- read_csv("Data/population_proportion.csv")

# Reading Crime Data
mydata <- read_csv("Data/crimedata_csv_all_years.csv")

# Removing columns not needed & cleaning data
mydata <- mydata %>% 
  filter(!(X==0 | Y==0 | is.na(NEIGHBOURHOOD))) %>%
  select(-DAY, -MINUTE, -HUNDRED_BLOCK)

# Excluding Year 2019 because the data is till Oct only whereas other
# years have full data
mydata <- mydata %>% 
  filter(YEAR!=2019)

# Relacing Stanley Park with West End because its a subset
# Relacing Musqueam with Marpole because its a subset
# Renaming others
mydata <- mydata %>%
  mutate(NEIGHBOURHOOD = case_when(
    NEIGHBOURHOOD=='Stanley Park' ~ 'West End',
    NEIGHBOURHOOD=='Musqueam' ~ 'Marpole',
    NEIGHBOURHOOD=='Arbutus Ridge' ~ 'Arbutus-Ridge',
    NEIGHBOURHOOD=='Central Business District' ~ 'Downtown',
    TRUE ~ NEIGHBOURHOOD
  ), TYPE = case_when(
    TYPE=='Vehicle Collision or Pedestrian Struck (with Fatality)' ~ 'Vehicle Collision or Pedestrian Struck',
    TYPE=='Vehicle Collision or Pedestrian Struck (with Injury)' ~ 'Vehicle Collision or Pedestrian Struck',
    TRUE ~ TYPE 
  ))

pop_prop <- pop_prop %>%
  mutate(NEIGHBOURHOOD = case_when(
    NEIGHBOURHOOD=='Stanley Park' ~ 'West End',
    NEIGHBOURHOOD=='Musqueam' ~ 'Marpole',
    NEIGHBOURHOOD=='Arbutus Ridge' ~ 'Arbutus-Ridge',
    NEIGHBOURHOOD=='Central Business District' ~ 'Downtown',
    TRUE ~ NEIGHBOURHOOD
  ))

## Elliotts GEO DATA VAN CRIME MAP WORK :)
# comment from derek on slack
# If anyone is working with the geojsonio package for making choropeth maps in R, it was a confusing process to get all the dependant system packages before I could  install the R package. Below is a summary of the installs that I did to get the geojsonio package successfully installed:
# sudo apt install libprotobuf-dev
# sudo apt install libv8-dev
# sudo apt install libjq-dev
# sudo apt install protobuf-compiler
#install.packages("geojsonio")

vancouver_hood_map <- sf::st_read('data/local-area-boundary.geojson')

create_hood_plot <- function(crime_data) {
  crime_data_per_hood <- mydata %>%
    group_by(TYPE, YEAR, NEIGHBOURHOOD) %>%
    summarize(COUNT = n())
  hood_map <- vancouver_hood_map %>% mutate(NEIGHBOURHOOD = as.character(name))
  hood_join <- full_join(hood_map, crime_data_per_hood, by = c('NEIGHBOURHOOD', 'NEIGHBOURHOOD'))
  max_crime <- max(hood_join$COUNT)
  min_crime <- min(hood_join$COUNT)
  ggplotly(hood_join %>%
             ggplot(mapping = aes(fill = .data[['COUNT']])) +
             geom_sf(color = 'white', size = 0.2) +
             scale_fill_viridis_c(option = 'plasma',
                                  name = "Crime Levels",
                                  limits = c(min_crime, max_crime)) +
             theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank()
             ))
}

create_hood_plot(crime_data=mydata)

















