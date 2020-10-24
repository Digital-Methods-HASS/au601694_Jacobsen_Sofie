#socure for used code: http://swcarpentry.github.io/r-novice-gapminder/10-functions/index.html
library(pacman)
pacman::p_load(tidyverse, gapminder)
gap_data <- gapminder
#gpd = gpp per capita * population

##################

#Define a defensive function that calculates the Gross Domestic Product of a nation

calculate_gdp <- function(data) {
  if (!is.numeric(data$gdpPercap)) {
    stop("gdpPercap must be a numeric vector.")
  }
  if (!is.numeric(data$pop)) {
    stop("Population must be a numeric vector.")
  }
  gdp <- data$gdpPercap*data$pop
  return(gdp)
}

#calculate the GDP of Denmark in the following years: 1967, 1977, 1987, 1997, 2007, and 2017.

unique(gap_data$year) #2017 does not exist

filt_gdp <- gap_data %>% 
  filter(year == 1967 | year == 1977| year == 1987| year == 1997| year == 2007) %>% 
  filter(country == "Denmark")

calculate_gdp(filt_gdp)
#1967 = 77116977700, 1977 = 103920280028, 1987 = 128771236166, 1997 = 157476118456, 2007 = 192906627081

##################
#make a function and tests whether each country starts with a ‘B’.
#if yes, print out whether the life expectancy is smaller than 50, between 50 and 70, or greater than 70.

gap_data$country <- as.character(gap_data$country)
for (j in gap_data$country){
  if (startsWith(j, 'B')) {
    print( "Country starts with B")
  } else {
    print( "Country doesn't start with B")
  }
}

#ifelse(grepl("B*", j), print( "Country starts with B"), print( "Country doesn't start with B"))
for (i in gap_data$lifeExp) {
   if (i < 50) {
    print("Life expectancy is below 50")
  } else if (between(i, 50,70)) {
    print("Life expectancy is between 50 and 70")
  } else {
    print("Life expectancy is greater than 70")
    }
}

