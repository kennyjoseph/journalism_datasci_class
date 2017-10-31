
library(ggplot2)
library(ggthemes)

theme_set(theme_fivethirtyeight(20))

df <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
region_data <- data.frame(state=state.abb,region=state.region,state_name=state.name)
df <- merge(df, region_data,by="state")
p <- ggplot(df, aes(race)) + geom_bar() + facet_wrap(~region, scales="free_y")


# https://www.kff.org/other/state-indicator/distribution-by-raceethnicity/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
race_data <- read.csv("~/git/lazerlab/journalist/class/raw_data.csv",skip = 2)

head(race_data)

clean_up_column <- function(column_data){
  column_data <- as.numeric(as.character(column_data))
  column_data[is.na(column_data)] <- 0
  return(column_data)
}

race_data[,c(2:ncol(race_data))] <- lapply(race_data[,c(2:ncol(race_data))], clean_up_column)
race_data