Proposal- Vancouver Crime Tracker
================
Chimaobi Amadi, Elliott Ribner, Shivam Verma, Xugang Zhong(Kirk)

Updated 2019-12-07

## Motivation and Purpose

-----

Living in a neighborhood with high rates of crime could potentially be a traumatizing experience, this is especially true for school children exposed to a neighborhood full of crime and violence. For school children, such environments can result in difficulty learning, difficulty to focus, and additional stress and depression. Violent crimes are usually geographically concentrated in some particular neighborhoods, and often called `hot spots` in some localized places. If these `hot spots` are well known, the public can make well-informed proactive decisions about living in such areas. Additionally, problem-oriented policing efforts in and around the hot spots can be improved.

We, therefore, propose to solve this problem by creating an interactive dashboard that is capable of providing crime statistics and awareness. This will help the public make better decisions, and assist the Vancouver Police Department to raise awareness on the crime rates in Vancouver neighborhoods.  Also, it provides the ability to compare neighborhoods based on overall historic rates for crimes which include: `Break and Enter`, `Homicide`, `Mischief`, `Offence Against a Person`, `Theft from Vehicle`, `Theft of Bicycle`, `Theft of Vehicle`, and `Vehicle Collision or Pedestrian Struck`.



## Description of the Data

-----

The dataset is obtained from the [Vancouver Police Department (VPD)
website](https://geodash.vpd.ca/opendata/). It is based on the VPD
Records Management System and contains information about the type of
crime, nearest block, nearest neighborhood and date & time of the crime.
The data contains information from the 1st of January 2003 to the 31st
of October 2019. VPD aims to provide this data for the public interest,
to enhance awareness & maintain transparency about crimes in Vancouver.
Please note that the dataset does not include all the crimes because of
privacy & investigative reasons. Location information in the data was
offset so that no association with a specific person or property can be
made.

### Variable Description

| S No. | Variable       | Description                                  | Insight                                                                                                                                                                                                                                                                                                   |
| ----- | -------------- | -------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | TYPE           | Type of crime reported                       | The most common type of crime is `Theft from Vehicle`, `Mischief` and `Break and Enter Residential/Other`                                                                                                                                                                                                 |
| 2     | YEAR           | Year of crime reported                       | Number of crime occurrences reduced from 2003 to 2011 after which it started increasing. In 2018 the number of crimes was the same as in 2007                                                                                                                                                             |
| 3     | MONTH          | Month of crime reported                      | There is no significant difference in the number of crimes by month                                                                                                                                                                                                                                       |
| 4     | DAY            | Day of the month when the crime was reported | There is no significant difference in the number of crimes by day of month                                                                                                                                                                                                                                |
| 5     | HOUR           | Hour of crime reported                       | Most of the crimes happen between 5 PM and midnight. Past midnight there are fewer occurrences                                                                                                                                                                                                            |
| 6     | NEIGHBOURHOOD  | Neighbourhood of crime reported              | The data is for 24 neighborhoods in Vancouver. 10.42% of the crimes had no neighborhood associated with it. Even though the neighbourhood is missing for some records, the crime did happen. Therefore, neither imputing the data nor excluding those records as it describes other relevant information. |
| 7     | X              | X-coordinate of the location                 | Longitude                                                                                                                                                                                                                                                                                                 |
| 8     | Y              | Y-coordinate of the location                 | Latitude                                                                                                                                                                                                                                                                                                  |
| 9     | HUNDRED\_BLOCK | Generalized location of Crime Activity       | Not using                                                                                                                                                                                                                                                                                                 |
| 10    | MINUTE         | Hour of crime reported                       | Not using                                                                                                                                                                                                                                                                                                 |

## Research Questions and Usage Scenarios

-----

Peter and his family plan to relocate to Canada. As a result of the
favorable weather condition in Vancouver, they decide to settle in
Vancouver. Their first goal is to find accommodation: renting and
eventually owning a house in a safe neighborhood, as well as safety for
their kids, especially at school. The family does not know anyone in
Vancouver and relies on a reliable source of information regarding the
crime situation and trend in each of the neighborhoods in Vancouver.

There are many people like Peter and his family, and to help solve their
problem, we plan on creating an interactive app that has the capability
of displaying crime information at a glance. The secondary motivation
for this app is to answer general crime trends in Vancouver and create
awareness about crime rates per neighborhood and need for drastic
intervention.

Using the dataset provided by the Vancouver Police Department (VPD), we
will build an interactive dashboard with the historic crime situation in
the city from 2003 to 2019. There are 3 filters provided in the
dashboard which include the neighborhood, type of crime and the period
in years. The filter option streamlines the visualization and produces
charts that answer the questions.

The filtered data is molded to show the crime occurrences by
Month-of-Year (*Chart 1*) & Time-of-Day (*Chart 2*) to help him
understand the behavior of crimes around the city. In addition to this,
he can look at the trend of crime rate by year (*Chart 3*) and get to
know the composition of crimes (*Chart 4*) as
well.


## Sketch of the Application

-----

### Design 1
![Sketch](https://github.com/UBC-MDS/DSCI_532_Group114_SKEC/blob/master/Img/sketch.png?raw=true "Crime Information by Vancouver Neighbourhood")

### Design 2
![Sketch1](https://github.com/UBC-MDS/DSCI_532_Group114_SKEC/blob/master/Img/sketch1.png?raw=true)

![Sketch2](https://github.com/UBC-MDS/DSCI_532_Group114_SKEC/blob/master/Img/sketch2.png?raw=true)

### App Desciription

-----

- We will have interactive visualizations of the VPD dataset described above.
- The sketch above gives a glimpse of how the data will be visualized.
- We will have three selectors (described below) that will allow us to filter the data that is represented in the five visualizations above.
	- The neighborhood dropdown will allow users to select a single neighborhood or multiple neighborhoods, crimes only in that neighborhood(s) will show up.
	- Through the crime dropdown, users can filter the data set and the data visualizations by crime type. The default seletion will include all tyeps of crimes.
	- The slider for selecting years is given because old data may be less relevant, so we can let the user decide the time period that is most relevant to them.
- A bar-chart of crime by month will show the monthly crime patterns.
- A bar-chart of crime by the hour will show the patterns of crime throughout the day.
- A map of Vancouver will be used to show crime instances by location. Different colors will be applied to different regions of the map to denote high and low crime areas.
- A line chart will show the pattern of crime over the years, normalized for population change.
- Lastly, a pie chart will be used to show what percentage of the total crimes any particular type of crime represents.
