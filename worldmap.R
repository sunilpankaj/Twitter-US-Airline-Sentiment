#Location of tweets visualisation on maps
library(ggplot2)
library(maps)
library(gridExtra)
#read data file 
data = read.csv('Tweets.csv')
print (dim(data))
print (colnames(data))

location = data$tweet_coord #cordinates of tweets location
location = location[complete.cases(location)] # remove NAs
location = as.data.frame(location) #making dataframe
location$count =  1 # add a count column filled with 1's
location$location = as.character(location$location)
#remove duplicate locations and count the times they appeared, write the count in the count column
location = aggregate(count~location, data = location, FUN = sum)
#print(location)
location = location[-5,] # removes row containing coords [0,0] which are probably wrong
#print(location)
coords = strsplit(location$location, ',')  #splitting lat and log,  [lat, log]

# separate lat and long from location
lat = NULL
long = NULL
for (i in 1:length(coords)) {
    lat = c(lat, substring(coords[[i]][1], 2)) # removes first character which is [
    long = c(long, coords[[i]][2]) 
}

location$lat = lat #lat dataframe
location$long = long #long dataframe

# remove ]
location$long = substr(location$long, 1, nchar(location$long)-1)
location$lat = as.numeric(location$lat)
location$long = as.numeric(location$long)
#removing first and 2 rows from location datasets as it contains na's and 0,0
location = location[-c(1, 2), ]
head(location) #frist top head rows(6) lines 
dim(location) #printing dimensions

require(maps)
#word map using ggplot 
world_map <- map_data("world")
g1 = ggplot()
g1 = g1 + geom_polygon(data=world_map, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
ggtitle("Location of tweets across the World")
g1 = g1 + geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + scale_size(name="Total Tweets")
g1 = g1 + ylim(-50, 80)
g1

#state map using ggplot
states <- map_data("state")
g2 = ggplot()
g2 =g2 + geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
ggtitle("Location of tweets across the States")
g2 = g2 + geom_point(data=location, aes(x=long, y=lat, size = count), color="coral1") + scale_size(name="Total Tweets")
g2 = g2 + xlim(-125, -65) + ylim(25, 50)
g2
#showiing both plot in one plot i mean subplot(2,1)
grid.arrange(g1, g2, ncol=1, nrow = 2)
