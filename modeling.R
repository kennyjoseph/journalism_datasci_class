library(ggplot2)
library(reshape2)
library(ggthemes)
library(dplyr)
library(tidyr)


theme_set(theme_economist(20))

# Read in the Washington Post police shooting Data
df <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")

# Take a look at it!
head(df)

# Our question of interest is at the region level, so we need to get that data somehow
# Conveniently, R has this data for us
region_data <- data.frame(state=state.abb,region=state.region,state_name=state.name)
head(region_data)

# Now, we can merge this data into our shootings data
df <- merge(df, region_data,by="state")

# lets take a look!
ggplot(df, aes(race)) + geom_bar() + facet_wrap(~region, scales="free_y")


# Read in the raw data from the states

# https://www.kff.org/other/state-indicator/distribution-by-raceethnicity/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
race_data <- read.csv("raw_data.csv",skip = 2)

# check it out ... it looks weird! We have to clean it a bit
head(race_data)

# This is a function that will clean the numeric columns for us
clean_up_column <- function(column_data){
  column_data <- as.numeric(as.character(column_data))
  column_data[is.na(column_data)] <- 0
  return(column_data)
}

# We're going to apply this function to the columns we are interested in
race_data[,c(2:ncol(race_data))] <- lapply(race_data[,c(2:ncol(race_data))], clean_up_column)

# Note, we have two Native American columns, so we're going to have to sum those
race_data$Native <- race_data$American.Indian.Alaska.Native + race_data$Native.Hawaiian.Other.Pacific.Islander

# Clean up the column names
colnames(race_data) <- c("Location","White","Black","Hispanic","Asian","Native_1","Native_2","Other","Total","Native")

# Take a subset of some of the columns
race_data <- race_data[,c("Location","White","Black","Hispanic","Asian","Other","Native")]

# Match the column names with our police shooting data
colnames(race_data) <- c("Location", "W","B","H","A","O","N")

# Check out the data - its still weird!
print(race_data)

# Clean it up
race_data <- race_data[1:52,]

# Finally looks good. BUT, we have to still determine the regional data!
head(race_data)
race_data <- merge(race_data,region_data,by.x="Location",by.y="state_name")

# Okay, we can finally aggregate by region. We want averages of averages
# This code uses dplyr and magrittr to do this in a clean way.  
# You should definitely check out these packages (part of the "tidyverse")
# As tools for learning R
aggregated_race_data <- race_data %>% 
                          group_by(region) %>% 
                          summarise_if(is.numeric,mean)
# Cool, we summarised! Now were going to "melt" the data so that we can
# Merge it with the shooting data
aggregated_race_data <- melt(data.frame(aggregated_race_data),id.vars="region")

# lets do a similar aggregation and merging with the shooting data
aggregated_shootings <- df %>% 
                          group_by(region,race) %>% 
                          summarise(n = n()) %>% 
                          spread(race,n) 
names(aggregated_shootings)[names(aggregated_shootings) == ""] <- "U"
# Create percentages instead of counts
aggregated_shootings[,2:ncol(aggregated_shootings)] <- aggregated_shootings[,2:ncol(aggregated_shootings)]/rowSums(aggregated_shootings[,2:ncol(aggregated_shootings)])
# Do the same "melting"
aggregated_shootings <- melt(data.frame(aggregated_shootings), id.vars="region")

# FINALLY, we can merge the datasets together and plot the results!
final_dataframe <- merge(aggregated_race_data,aggregated_shootings, by=c("region","variable"))
p <- ggplot(final_dataframe, aes(value.x+.001,value.y+.001,color=variable,shape=region)) 
p <- p + geom_point(size=6) + scale_x_log10() + scale_y_log10() 
p <- p + geom_abline(slope=1,color='grey') 
p <- p + xlab("Percent of Individuals in Region of Given Race") 
p <- p + ylab("Percent of Individuals\nShot by Police In Region")
p <- p + theme_minimal(20)
p