library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)

theme_set(theme_economist(20))

df <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv")
region_data <- data.frame(state=state.abb,region=state.region,state_name=state.name)
df <- merge(df, region_data,by="state")

ggplot(df, aes(race)) + geom_bar() + facet_wrap(~region, scales="free_y")


# https://www.kff.org/other/state-indicator/distribution-by-raceethnicity/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
race_data <- read.csv("~/git/lazerlab/journalist/class/raw_data.csv",skip = 2)

head(race_data)

clean_up_column <- function(column_data){
  column_data <- as.numeric(as.character(column_data))
  column_data[is.na(column_data)] <- 0
  return(column_data)
}

race_data[,c(2:ncol(race_data))] <- lapply(race_data[,c(2:ncol(race_data))], clean_up_column)
race_data$Native <- race_data$American.Indian.Alaska.Native + race_data$Native.Hawaiian.Other.Pacific.Islander
colnames(race_data) <- c("Location","White","Black","Hispanic","Asian","Native_1","Native_2","Other","Total","Native")
race_data <- race_data[,c("Location","White","Black","Hispanic","Asian","Other","Native")]
colnames(race_data) <- c("Location", "W","B","H","A","O","N")

print(race_data)

race_data <- race_data[1:52,]

head(race_data)
race_data <- merge(race_data,region_data,by.x="Location",by.y="state_name")

aggregated_race_data <- race_data %>% group_by(region) %>% summarise_if(is.numeric,mean)
aggregated_race_data <- melt(aggregated_race_data,id.vars="region")

aggregated_shootings <- df %>% group_by(region,race) %>% summarise(n = n()) %>% spread(race,n)
names(aggregated_shootings)[names(aggregated_shootings) == ""] <- "U"
aggregated_shootings[,2:ncol(aggregated_shootings)] <- aggregated_shootings[,2:ncol(aggregated_shootings)]/rowSums(aggregated_shootings[,2:ncol(aggregated_shootings)])
aggregated_shootings <- melt(aggregated_shootings, id.vars="region")


final_dataframe <- merge(aggregated_race_data,aggregated_shootings, by=c("region","variable"))
p <- ggplot(final_dataframe, aes(value.x+.001,value.y+.001,color=variable,shape=region)) 
p <- p + geom_point(size=6) + scale_x_log10() + scale_y_log10() 
p <- p + geom_abline(slope=1,color='grey') 
p <- p + xlab("Percent of Individuals in Region of Given Race") 
p <- p + ylab("Percent of Individuals\nShot by Police In Region")
p <- p + theme_minimal(20)
p