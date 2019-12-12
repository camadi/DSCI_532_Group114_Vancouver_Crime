
# Reading Vancouver City Population from 2001 to 2018
pop_yr <- read_csv("data/Population_trend.csv")
pop_yr <- pop_yr %>% select(YEAR, Population)

# Reading each neighborhood's proportion of population to overall city's population
pop_prop <- read_csv("data/population_proportion.csv")

# Reading Crime Data
mydata <- read_csv("data/crimedata_csv_all_years.csv")

# Removing columns not needed & cleaning data
mydata <- mydata %>% 
    filter(!(X==0 | Y==0 | is.na(NEIGHBOURHOOD))) %>%
    select(-DAY, -MINUTE, -HUNDRED_BLOCK)

# Excluding Year 2019 because the data is till Oct only whereas other
# years have full data
mydata <- mydata %>% 
    filter(YEAR!=2019)
    
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