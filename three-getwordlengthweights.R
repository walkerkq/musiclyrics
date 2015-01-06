########## 
# Step Three in songlyrics-composite: calculate weights based on average song length
# Requires one-getsongs.R for data.frame allthesongs
# Requires two-getlyrics.R for .csv "wikisongs.csv"
##########

library("ggplot2")

for ( i in 1:length(allsongs$lyrics) ) { 

  y<-strsplit(allsongs[i, ]$lyrics, " ") # split each lyric entry into words
  
  allsongs$wordcount[i] <- length(y[[1]]) # count the words
  
}

countbyyear <- aggregate(wordcount~year,data=m, FUN=sum) # add up all word counts per year

countbyyear$persong <- countbyyear$wordcount/100 # calculate the average word count per song (each year has 100 songs)

countbyyear$weights <- mean(countbyyear$persong)/countbyyear$persong # divide the average length by each year's avg length to weight appropriately

# make a nice plot of this data
ggplot(countbyyear, aes(year, persong)) + geom_line(aes(color="persong"),size=1.25) + labs(title="Avg. Words Per Song\nin Billboard Year-End Hot 100 Singles\n1964-2014", x="",y="") + coord_cartesian(ylim=c(0,500)) + theme(legend.position="none", plot.title = element_text(size=20, face="bold", vjust=2), axis.ticks.y = element_blank(), panel.background = element_rect(fill = 'white'))

### end result: plot of song length data and data.frame containing average number of words per song per year, along with weights based on the overall average.