library(dplyr)
library(ggplot2) 
install.packages("lubricate")
library(lubricate)
library(stringr)
library(tidyr)

## CTA ride analysis

setwd("/Users/cgundy/Documents/Projects/CTA")

cta_stops <- read.csv("CTA stops.csv")
cta_rides <- read.csv("CTA Rides.csv")


############################ CTA Stops data cleaning ############################

# count number of stops
length(unique(cta_stops$STATION_DESCRIPTIVE_NAME))
length(unique(cta_stops$MAP_ID))
length(unique(cta_stops$Location))

# look at differences
View(cta_stops %>%
  distinct(STATION_DESCRIPTIVE_NAME, MAP_ID, Location) %>%
  arrange(MAP_ID))

# take first row to create unique data set
cta_stops_dist <- cta_stops %>%
  select(STATION_DESCRIPTIVE_NAME, MAP_ID, Location) %>%
  group_by(MAP_ID) %>%
  arrange(STATION_DESCRIPTIVE_NAME) %>%
  slice(1) %>%
  ungroup
  
length(cta_stops_dist$Location)
  

############################# CTA rides data cleaning ##############################

# convert date and parse out year
cta_rides$date <- as.Date(cta_rides$date,'%m/%d/%y')
cta_rides$year <- format(as.Date(cta_rides$date, format="%d/%m/%Y"),"%Y")

# count rides per year
data_prep <- cta_rides %>%
  group_by(year,station_id) %>%
  summarize(rides = sum(rides))

# join to get station coordinates
data_prep <- left_join(data_prep,cta_stops_dist,by = c("station_id" = "MAP_ID"))

# extract coordinates from Location column
rides_by_year <- separate(data = data_prep, col = Location, into = c("lat", "long"), sep = ", ")
rides_by_year$lat <- as.numeric(str_sub(rides_by_year$lat, 2))
rides_by_year$long <- as.numeric(str_sub(rides_by_year$long,1,-2))

# remove NA rows
rides_by_year <- na.omit(rides_by_year)


################################## analysis start ###############################

# find ride number for the year of 2001
old_vals <- rides_by_year %>%
  filter(year == 2001) %>%
  select(station_id, rides) %>%
  rename(old_ride = rides) 

old_vals <- unique(old_vals[,-1])

# find difference between current year and 2001
rides_dif <- left_join(rides_by_year, old_vals)

rides_dif <- rides_dif %>%
  filter(year < 2018) %>%
  mutate(abs_dif = rides - old_ride) %>%
  mutate(pct_dif = abs_dif/old_ride * 100)

# find stations with biggest change in rides
View(rides_dif %>%
       filter(year == 2017) %>%
       arrange(desc(abs_dif)))

View(rides_dif %>%
       filter(year == 2017) %>%
       arrange(desc(pct_dif)))



# just look at logan square
ggplot(filter(rides_dif,station_id %in% c(41020)), 
       aes(x = year, y = rides, group = STATION_DESCRIPTIVE_NAME))+
  geom_point()+
  geom_line()

ggplot(filter(rides_dif,station_id %in% c(41020, 40590, 40570, 40320)), 
       aes(x = year, y = rides, color = STATION_DESCRIPTIVE_NAME, group = STATION_DESCRIPTIVE_NAME))+
  geom_point()+
  geom_line()
       
# armitage, belmont, addision
ggplot(filter(rides_dif,station_id %in% c(40660, 41420, 41320, 40650)), 
       aes(x = year, y = rides, color = STATION_DESCRIPTIVE_NAME, group = STATION_DESCRIPTIVE_NAME))+
  geom_point()+
  geom_line()



ggplot(filter(rides_dif,station_id %in% c(41020, 41670, 40210, 40830)), 
       aes(x = year, y = rides, color = STATION_DESCRIPTIVE_NAME, group = STATION_DESCRIPTIVE_NAME))+
  geom_point()+
  geom_line()





ggmap(mapgilbert) +
  geom_point(data = filter(rides_dif,year == 2017), aes(x = long, y = lat, fill = "red", alpha = 0.8, size = abs_dif), shape = 21,
             height = 400, width = 600) +
  scale_size_continuous(limits=c(1000,2500000)) +
  guides(fill=FALSE, alpha=FALSE) +
  ggtitle(2017) +
  theme(plot.title = element_text(size = 40, face = "bold"))



library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-20, 59), ylim = c(35, 71), asp = 1)

points(rides_2010$long, rides_2010$lat, col = "red", cex = .6)



# loading the required packages
install.packages("ggmap", type = "source")

library(ggplot2)
library(ggmap)

# getting the map
mapgilbert <- get_map(location = c(lon = mean(rides_2010$long), lat = mean(rides_2010$lat)), zoom = 12,
                      maptype = "roadmap", scale = 2)

# plotting the map with some points on it
year <- unique(loop_test$year)

for (i in year){
  
  ggmap(mapgilbert) +
    geom_point(data = filter(loop_test,year == 2002), aes(x = long, y = lat, fill = "red", alpha = 0.8, size = rides), shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE) +
    ggtitle(2002)
  
}


ggmap(mapgilbert) +
  geom_point(data = rides_2010, aes(x = long, y = lat, fill = "red", alpha = 0.8, size = rides), shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)




logan_square <- cta_rides %>%
  filter(stationname == 'Logan Square') %>%
  ggplot(date,rides)

plot(logan_square$month, logan_square$rides, logan_square)

logan_square_month = logan_square %>%
  group_by(month) %>%
  summarize(ride_sum = sum(rides))

plot(as.factor(logan_square_month$month), logan_square_month$ride_sum, logan_square_month)

plot(logan_square_month)

is.na(logan_square_month)

logan_square$month<-format(logan_square$date,'%Y%m') 

logan_square$month<-as.Date(logan_square$date) 



unique(cta_rides$stationname) %>%
  ggplot(date,rides)
  
  
armitage = cta_rides %>%
  filter(stationname == 'Armitage') 

armitage$month<-format(armitage$date,'%Y%m') 

armitage_month <- armitage %>%
  group_by(month) %>%
  summarize(ride_sum = sum(rides))

plot(as.factor(armitage_month$month), armitage_month$ride_sum, armitage_month)
