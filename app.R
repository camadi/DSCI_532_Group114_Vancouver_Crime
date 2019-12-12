library(tidyverse)
library(cowplot)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(gapminder)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Selection components


#setwd("C:/MyDisk/MDS/DSCI_532/grp_prjct/New/DSCI_532_Group114_SKEC")



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

#We can get the years from the dataset to make ticks on the slider

yearMarks <- map(unique(mydata$YEAR), as.character)
names(yearMarks) <- unique(mydata$YEAR)
yearSlider <- dccRangeSlider(
  id = "year",
  marks = yearMarks,
  min = 2003,
  max = 2018,
  step = 5,
  value = list(2010, 2018)
  
)

neigh_Dropdown <- dccDropdown(
  id = "neighbourhood",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    levels(as.factor(mydata$NEIGHBOURHOOD)), function(x){
      list(label=x, value=x)
    }),
  value = levels(as.factor(mydata$NEIGHBOURHOOD)), #Selects all by default
  multi = TRUE
)


type_Dropdown <- dccDropdown(
  id = "type",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    levels(as.factor(mydata$TYPE)), function(x){
      list(label=x, value=x)
    }),
  value = levels(as.factor(mydata$TYPE)), #Selects all by default
  multi = TRUE
)

type_lst <- unique(mydata$TYPE)
ngbrhd_lst <- unique(mydata$NEIGHBOURHOOD)
yr_lst <- unique(mydata$YEAR)
all_neig <- unique(mydata$NEIGHBOURHOOD)
all_year <- unique(mydata$YEAR)
all_types <- unique(mydata$TYPE)

#Creating chart1
make_charts1 <- function(yr_lst = all_year, ngbrhd_lst = all_neig, type_lst = all_types){
    df <- mydata %>% 
        filter(TYPE %in% type_lst & NEIGHBOURHOOD %in% ngbrhd_lst & YEAR %in% yr_lst)

    MOY <- df %>% 
        group_by(MONTH) %>%
        summarise(N = n()) %>%
        arrange(MONTH)
    MOY <- MOY %>% 
        mutate(MONTH_NAME = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

    TOD <- df %>% 
        group_by(HOUR) %>%
        summarise(N = n()) %>%
        arrange(HOUR)

    type_crimes <- df %>% 
        group_by(TYPE) %>%
        summarise(N = n()) %>%
        arrange(-N)
    type_crimes <- type_crimes %>% mutate(contri = N/sum(type_crimes$N))

    crime_rate <- df %>% 
        group_by(YEAR) %>%
        summarise(N = n()) %>%
        arrange(YEAR)



    # Adding population data to plot crime rate
    required_prop <-pop_prop %>% filter(NEIGHBOURHOOD %in% ngbrhd_lst) %>% pull(proportion) %>% sum()
    pop_yr <- pop_yr %>% filter(YEAR %in% yr_lst) %>% mutate(Population = Population*required_prop)

    crime_rate <- inner_join(crime_rate, pop_yr)
    crime_rate <- crime_rate %>% mutate(rate = (N/Population)*1000)

    #chart = {}
    chart1 <- ggplot(MOY, aes(x=reorder(MONTH_NAME, MONTH), y=N)) + 
        geom_bar(stat = "identity") + 
        labs(x='MONTH', y= 'Occurrence Count') + 
        scale_y_continuous(labels = scales::comma) + 
        ggtitle("Crime Occurrence by Month") + 
        theme_minimal_grid() + 
        theme(
            text = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(angle = 30, hjust = 0.5))
    ggplotly(chart1)

}
make_charts2 <- function(yr_lst = all_year, ngbrhd_lst = all_neig, type_lst = all_types){
    df <- mydata %>% 
        filter(TYPE %in% type_lst & NEIGHBOURHOOD %in% ngbrhd_lst & YEAR %in% yr_lst)

    MOY <- df %>% 
        group_by(MONTH) %>%
        summarise(N = n()) %>%
        arrange(MONTH)
    MOY <- MOY %>% 
        mutate(MONTH_NAME = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

    TOD <- df %>% 
        group_by(HOUR) %>%
        summarise(N = n()) %>%
        arrange(HOUR)

    type_crimes <- df %>% 
        group_by(TYPE) %>%
        summarise(N = n()) %>%
        arrange(-N)
    type_crimes <- type_crimes %>% mutate(contri = N/sum(type_crimes$N))

    crime_rate <- df %>% 
        group_by(YEAR) %>%
        summarise(N = n()) %>%
        arrange(YEAR)



    # Adding population data to plot crime rate
    required_prop <-pop_prop %>% filter(NEIGHBOURHOOD %in% ngbrhd_lst) %>% pull(proportion) %>% sum()
    pop_yr <- pop_yr %>% filter(YEAR %in% yr_lst) %>% mutate(Population = Population*required_prop)

    crime_rate <- inner_join(crime_rate, pop_yr)
    crime_rate <- crime_rate %>% mutate(rate = (N/Population)*1000)

    chart2 <- ggplot(TOD, aes(x=HOUR, y=N)) + 
        geom_bar(stat = "identity") + 
        labs(x='HOUR', y= 'Occurrence Count') + 
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = seq(0, 23, 2)) +
        ggtitle("Crime Occurrence by Time of Day") + 
        theme_minimal_grid() + 
        theme(
            text = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(angle = 30, hjust = 0.5))
    ggplotly(chart2)
}
make_charts3 <- function(yr_lst = all_year, ngbrhd_lst = all_neig, type_lst = all_types){
    df <- mydata %>% 
        filter(TYPE %in% type_lst & NEIGHBOURHOOD %in% ngbrhd_lst & YEAR %in% yr_lst)

    MOY <- df %>% 
        group_by(MONTH) %>%
        summarise(N = n()) %>%
        arrange(MONTH)
    MOY <- MOY %>% 
        mutate(MONTH_NAME = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

    TOD <- df %>% 
        group_by(HOUR) %>%
        summarise(N = n()) %>%
        arrange(HOUR)

    type_crimes <- df %>% 
        group_by(TYPE) %>%
        summarise(N = n()) %>%
        arrange(-N)
    type_crimes <- type_crimes %>% mutate(contri = N/sum(type_crimes$N))

    crime_rate <- df %>% 
        group_by(YEAR) %>%
        summarise(N = n()) %>%
        arrange(YEAR)



    # Adding population data to plot crime rate
    required_prop <-pop_prop %>% filter(NEIGHBOURHOOD %in% ngbrhd_lst) %>% pull(proportion) %>% sum()
    pop_yr <- pop_yr %>% filter(YEAR %in% yr_lst) %>% mutate(Population = Population*required_prop)

    crime_rate <- inner_join(crime_rate, pop_yr)
    crime_rate <- crime_rate %>% mutate(rate = (N/Population)*1000)

    chart3 <- ggplot(crime_rate, aes(x=YEAR, y=rate)) + 
        geom_point() + 
        geom_line() + 
        labs(x='YEAR', y= 'Crime Occurrences per 1000 People') + 
        scale_x_continuous(breaks = seq(min(crime_rate$YEAR), max(crime_rate$YEAR), 1)) +
        ggtitle("Crime Rate") + 
        theme_minimal_grid() + 
        theme(
            text = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15))
    ggplotly(chart3)
}
 make_charts4 <- function(yr_lst = all_year, ngbrhd_lst = all_neig, type_lst = all_types){
    df <- mydata %>% 
        filter(TYPE %in% type_lst & NEIGHBOURHOOD %in% ngbrhd_lst & YEAR %in% yr_lst)

    MOY <- df %>% 
        group_by(MONTH) %>%
        summarise(N = n()) %>%
        arrange(MONTH)
    MOY <- MOY %>% 
        mutate(MONTH_NAME = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'))

    TOD <- df %>% 
        group_by(HOUR) %>%
        summarise(N = n()) %>%
        arrange(HOUR)

    type_crimes <- df %>% 
        group_by(TYPE) %>%
        summarise(N = n()) %>%
        arrange(-N)
    type_crimes <- type_crimes %>% mutate(contri = N/sum(type_crimes$N))

    crime_rate <- df %>% 
        group_by(YEAR) %>%
        summarise(N = n()) %>%
        arrange(YEAR)



    # Adding population data to plot crime rate
    required_prop <-pop_prop %>% filter(NEIGHBOURHOOD %in% ngbrhd_lst) %>% pull(proportion) %>% sum()
    pop_yr <- pop_yr %>% filter(YEAR %in% yr_lst) %>% mutate(Population = Population*required_prop)

    crime_rate <- inner_join(crime_rate, pop_yr)
    crime_rate <- crime_rate %>% mutate(rate = (N/Population)*1000)

    chart4 <- ggplot(type_crimes, aes(x=reorder(TYPE, -contri), y=contri)) + 
        geom_bar(stat = "identity") + 
        labs(x='', y= 'Contribution') + 
        scale_y_continuous(labels = scales::percent) +
        ggtitle("Constituents of Selected Crimes") + 
        theme_minimal_grid() + 
        theme(
            text = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(chart4)
}
    #return(plot_grid(chart1, chart2, chart3, chart4))
    #return chart
#}

# Now we define the graph as a dash component using generated figure
graph1 <- dccGraph(
  id = 'graph1',
  figure=make_charts1(), # gets initial data using argument defaults
 style = list(
        width ='49%',
        display = 'inline-block',
        padding = '0 20'))

graph2 <- dccGraph(
  id = 'graph2',
  figure=make_charts2(), # gets initial data using argument defaults
  style = list(
        width ='49%',
        display = 'inline-block',
        padding = '0 20')
)

graph3 <- dccGraph(
  id = 'graph3',
  figure=make_charts3(), # gets initial data using argument defaults
style = list(
        width ='49%',
        display = 'inline-block',
        padding = '0 20')
)

graph4 <- dccGraph(
  id = 'graph4',
  figure=make_charts4(), # gets initial data using argument defaults
  style = list(
        width ='49%',
        display = 'inline-block',
        padding = '0 20'))
app$layout(
  htmlDiv(
    list(
      htmlH1('Vancouver Crime Tracker'),
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
      htmlH3("Using Data from VPD we have created several plots for residents and visitors of Vancouver to view crime data. 
      There are three selectors, one for neighbourhood, crime type, and year. All three selectors impact each of the plots. 
      The crime rate plot is normalized for population change in Vancouver, other than that the data is non-normalized."),
      #selection components
      #htmlLabel('Select a year range:'),
      yearSlider,
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
      #htmlLabel('Select continents:'),
      neigh_Dropdown,
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
      type_Dropdown,
      htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
      graph1, 
      graph2,
      graph3,
      graph4,
      htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
      #htmlLabel('Try sorting by table columns!'),
      #table,
      htmlIframe(height=20, width=10, style=list(borderWidth = 0))#space
      #dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
    )
  )
)




# type_lst <- unique(mydata$TYPE)
# ngbrhd_lst <- unique(mydata$NEIGHBOURHOOD)
# yr_lst <- unique(mydata$YEAR)

# make_charts(type_lst[1:3], ngbrhd_lst, yr_lst)

# try running each chart in ggplotly

# Adding callbacks for interactivity
# We need separate callbacks to update graph and table
# BUT can use multiple inputs for each!
app$callback(
  #update figure of gap-graph
  output=list(id = 'graph1', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'year', property='value'),
              input(id = 'neighbourhood', property='value'),
              input(id = 'type', property='value')),
  #this translates your list of params into function arguments
  function(year_value, neighbourhood_value, type_value) {
    make_charts1(year_value, neighbourhood_value, type_value)
  })

app$callback(
  #update figure of gap-graph
  output=list(id = 'graph2', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'year', property='value'),
              input(id = 'neighbourhood', property='value'),
              input(id = 'type', property='value')),
  #this translates your list of params into function arguments
  function(year_value, neighbourhood_value, type_value) {
    make_charts2(year_value, neighbourhood_value, type_value)
  })

  app$callback(
  #update figure of graph3
  output=list(id = 'graph3', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'year', property='value'),
              input(id = 'neighbourhood', property='value'),
              input(id = 'type', property='value')),
  #this translates your list of params into function arguments
  function(year_value, neighbourhood_value, type_value) {
    make_charts3(year_value, neighbourhood_value, type_value)
  })

  app$callback(
  #update figure of graph4
  output=list(id = 'graph4', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'year', property='value'),
              input(id = 'neighbourhood', property='value'),
              input(id = 'type', property='value')),
  #this translates your list of params into function arguments
  function(year_value, neighbourhood_value, type_value) {
    make_charts4(year_value, neighbourhood_value, type_value)
  })

app$run_server()

