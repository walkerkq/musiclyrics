########## 
# Step Four in songlyrics-composite: explain song lengths by determining percentage of songs w/ 2+ artists over time
# Requires one-getsongs.R for data.frame allthesongs
##########

library("ggplot2")

library("XML")

library("stringr")

allthesongs <- data.frame() # create an empty data frame

# loop through wikipedia entries to collect top 100 songs from 1964-2014

for (i in 1964:2014) { 
     
     URL <- paste("http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_",i,sep="")
     results <- htmlTreeParse(URL, useInternal=TRUE) # parse html
     x <- xpathSApply(results, "//table[@class='wikitable sortable']//tr",xmlValue) 
     y <- str_split_fixed(x,"\n",3) # split table into three chunks: rank, song and artist
     w <- as.data.frame(y, stringsAsFactors=FALSE) # turn into a data frame
     w <- w[2:101, ] # remove the header
     colnames(w) <- c("rank","songnames","artists") # add column names
     w$songnames <- gsub('\\"|\\\n|\\,|\\!|\\?|\\+|\\(|\\)|\\#','',w$songnames) # remove etc. characters from the song names
     w$songnames <- gsub("\\.|/|\\'"," ",w$songnames)
     w$artists <- gsub('\\\n|\\,|\\?|\\+|\\(|\\)|\\#','',w$artists) # remove etc. characters from artist names
     w$artists <- gsub("\\.|/|\\'"," ",w$artists) # replace periods, slashes and apostrophes with spaces 
     #w$artists <- str_split_fixed(w$artists, " [Ff]eaturing| [Dd]uet", 2) # split artists if containing featured or duet (works better for songlyrics.com)
     w$year <- rep(i,100) # add the year in a new column
     allthesongs <- rbind(allthesongs,w) # row bind this year's data to the df
     
}
feat <- allthesongs[grep("[Ff]eaturing|[Dd]uet|[Ff]eat| and",allthesongs$artists),]
featbyyear <- aggregate(artists~year,data=feat, FUN=length)
ggplot(featbyyear, aes(year, artists)) + geom_line(aes(color="artists"),size=1.25) + labs(title="Songs Featuring 2+ Artists\nin Billboard Year-End Hot 100 Singles\n1964-2014", x="",y="") + coord_cartesian(ylim=c(0,50)) + theme(legend.position="none", plot.title = element_text(size=20, face="bold", vjust=2), axis.ticks.y = element_blank(), panel.background = element_rect(fill = 'white'))

### end result: plot displaying proportion of songs by two or more artists, to help explain song weights