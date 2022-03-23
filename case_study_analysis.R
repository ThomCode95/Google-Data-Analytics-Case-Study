
library(ggplot2)     #plotting
library(dplyr)       #dataframe manipulation
library(hms)         #manipulating hours, minutes, and seconds
library(geosphere)   #calculate trip distances
library(readr)       #file reading
library(lubridate)   #format dates

#import data from csv file
divvy_trip_data <- read_csv("data/merged_data/divvy-trip-data.csv", 
                            col_types = cols(date_start = col_date(format = "%d/%m/%Y"), 
                                             time_start = col_time(format = "%H:%M"), 
                                             date_end = col_date(format = "%d/%m/%Y"), 
                                             time_end = col_time(format = "%H:%M")))
View(divvy_trip_data)
#view a small preview of the data along with types
str(divvy_trip_data)


#plot time starts and time ends of trips
ggplot(data = divvy_trip_data) +
  geom_point(mapping = aes(x = time_start, y = time_end, colour = member_type)) + ggtitle("Time start VS Time end")
#observation: the casual riders tend to finish their trips later than the member riders

#Question: How many of the trips are occurring per membership type?
ggplot(data = divvy_trip_data) +
  geom_bar(mapping = aes(x = member_type, fill= member_type)) + ggtitle("Casual riders VS Member riders")
#Conclusion: We have more member riders than casual riders


#Lets plot values based on month and day of the week
#First we create variables of the week and month of each observation:
divvy_trip_data$Month <- as.Date(cut(divvy_trip_data$date_start, breaks = "month"))
divvy_trip_data$Weekday <- as.Date(cut(divvy_trip_data$date_start,breaks = "day",start.on.monday = TRUE)) # changes weekly break point to Sunday

divvy_trip_data <- mutate(divvy_trip_data, Month = months(Month))
divvy_trip_data <- mutate(divvy_trip_data, Weekday = wday(Weekday, label = TRUE))
#################
# graph by month:

#Checking that months are distincs
#distinct(divvy_trip_data,Month)

#Factor is needed on Month column to order and display every label of the month
divvy_trip_data$Month <- factor(divvy_trip_data$Month, levels = unique(divvy_trip_data$Month))

#Question: In which months do member types ride more often?
#Plotting values based on month and filled by member_type
ggplot(data = divvy_trip_data) +
  geom_bar(mapping = aes(x = Month, fill = member_type), position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Trips by Month") +
  xlab("Month")

#Conclusion: we can see that member types ride more often from december to april, while casual riders ride more often from may to september


#We do the same for days of the week
#Factor is needed on Month column to order and display every label of the month
divvy_trip_data$Weekday <- factor(divvy_trip_data$Weekday, levels = unique(divvy_trip_data$Weekday))

#Question: In which days of the week do members ride more often?
ggplot(data = divvy_trip_data) +
  geom_bar(mapping = aes(x = Weekday, fill = member_type), position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Trips by day of Week") +
  xlab("Day of Week")
#Conclusion: Barchart clearly shows that casual riders prefer riding on weekends while members are constant regardless of day of the week.


################


#calculate time difference between rides
time_diff <- with(divvy_trip_data, difftime(time_end, time_start, units = "mins") )

#add column to dataframe
divvy_trip_data<- mutate(divvy_trip_data,trip_time = time_diff)

##next question: Do member riders ride longer trips?
ggplot(data = divvy_trip_data) +
  geom_bar(mapping = aes(x = trip_time, fill= member_type)) +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Trip time of users") +
  xlab("Trip time")
  #geom_text(aes(label = mean(trip_time)),size = 3.5)
#conclusion: No, it is casual riders that ride the longest


#show types of rideable bikes
distinct(divvy_trip_data, rideable_type)


#how to know know if the number of trips for each rideable  type is different 
#depending on the type of the bike
ggplot(data =  divvy_trip_data) +
  geom_bar(mapping = aes(x = member_type, fill= rideable_type)) + ggtitle("Member type by type of bike")
#observation : members prefer electric and classic bikes more than casual riders. Correction, is not that they preferre it more
#it's just that member riders use each time fo bike more, followed by electric bike and classic bike

#Distribution of members by type of bike 
ggplot(data = divvy_trip_data) +
  geom_bar(mapping = aes(x = member_type, fill = member_type)) +
  facet_wrap(~rideable_type) +
  ggtitle("Distribution of members by type of bike") +
  theme(axis.text.x = element_text(angle = 45)) 
#Observation: It is observed that classic bikes are way more preferred by member riders.



#calculate trip distances
divvy_trip_data <- mutate(divvy_trip_data,
                            trip_distance_km = geosphere::distGeo(cbind(start_lng, start_lat), cbind(end_lng, end_lat)) / 1000)
#rounding off trip distances to two decimal places
divvy_trip_data$Round_off <- round(divvy_trip_data$trip_distance_km ,digit=2)




#means
#we make a subset to calculate mean trip time by member_type
df <- subset(divvy_trip_data, select = c(trip_time, member_type))


#####################3
#We calculate the mean trip time for member riders
avg_trip_time_member = df %>% 
  filter(member_type == "member") %>%
  summarise(Mean = mean(trip_time))

###move element from a row into a value 
mean_trip_time_member = avg_trip_time_member[1]
rm(avg_trip_time_member)

#Do the same for casual members
avg_trip_time_casual = df %>% 
  filter(member_type == "casual") %>%
  summarise(Mean = mean(trip_time))

###move element from a row into a value 
mean_trip_time_casual = avg_trip_time_casual[1]
rm(avg_trip_time_casual)

print(paste("Mean time for casual riders: ", mean_trip_time_casual))
print(paste("Mean time for member riders: ", mean_trip_time_member))


