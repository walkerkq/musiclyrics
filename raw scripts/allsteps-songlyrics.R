# library("ggplot2")
# library("XML")
# library("stringr")

##########
# 1. pull list of song names from wikipedia top 100 hot singles pages
##########

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
     
     w$artists <- str_split_fixed(w$artists, " [Ff]eaturing| [Dd]uet", 2) # split artists if containing featured or duet (works better for songlyrics.com)
     
     w$year <- rep(i,100) # add the year in a new column
     
     allthesongs <- rbind(allthesongs,w) # row bind this year's data to the df
          
}

########## 
# 2. grab lyrics for each song from songlyrics.com
##########

for (w in 1:length(allthesongs$songnames)) {
     
     URL2 <- paste("http://songlyrics.com",allthesongs$artist[[w]],allthesongs$songnames[[w]],sep="/") # create a new URL for each song
     
     URL2 <- paste(URL2,"-lyrics",sep="") # add -lyrics a the end of the URL
     
     URL2 <- gsub(" ","-",URL2) # change all spaces to hyphens
     
     results <- 12 # set results object to arbitrary number
     
     # catch errors and warnings when trying to parse the HTML (if the lyrics web page does not exist, e.g.)
     
     tryCatch({
          # try to parse the HTML
          results <- htmlTreeParse(URL2, useInternal=TRUE, isURL=TRUE)
     },
     # error message
     error=function(cond) {
          message(paste(URL2, "does not exist"))
          return("NA")
     },
     # warning message
     warning=function(cond) {
          message(paste(URL2, "caused a warning"))
          return("NA")
     },
     finally={ })
     
     # if the try worked, go get the lyrics. If not, write NA.
     
     if (is.numeric(results)) { 
          
          allthesongs$lyrics[[w]] <- "NA" # add to df
          
     } else { 
          
          lyrics <- xpathSApply(results, "//div[@id='songLyricsDiv-outer']", xmlValue)
          
          lyrics <- gsub("\n|\t|","",lyrics) # clean up the text
          
          lyrics <- gsub("[^[:alnum:][:space:]']","",lyrics) # clean up the text
          
          lyrics <- tolower(lyrics)
          
          allthesongs$lyrics[[w]] <- lyrics[[1]] # add to df
     }
}
write.csv(allthesongs, file="wikisongs.csv")

########## 
# 3. calculate weights based on average song length
##########

for ( i in 1:length(allsongs$lyrics) ) { 

  y<-strsplit(allsongs[i, ]$lyrics, " ") # split each lyric entry into words
  
  allsongs$wordcount[i] <- length(y[[1]]) # count the words
  
}

countbyyear <- aggregate(wordcount~year,data=m, FUN=sum) # add up all word counts per year

countbyyear$persong <- countbyyear$wordcount/100 # calculate the average word count per song (each year has 100 songs)

countbyyear$weights <- mean(countbyyear$persong)/countbyyear$persong # divide the average length by each year's avg length to weight appropriately

# make a nice plot of this data
ggplot(countbyyear, aes(year, persong)) + geom_line(aes(color="persong"),size=1.25) + labs(title="Avg. Words Per Song\nin Billboard Year-End Hot 100 Singles\n1964-2014", x="",y="") + coord_cartesian(ylim=c(0,500)) + theme(legend.position="none", plot.title = element_text(size=20, face="bold", vjust=2), axis.ticks.y = element_blank(), panel.background = element_rect(fill = 'white'))



########## 
# 4. explain song lengths by determining percentage of songs w/ 2+ artists over time
##########

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



########## 
# 5. find strings in the lyrics and create ggplots
##########

allsongs <- read.csv("wikisongs.csv", stringsAsFactors=FALSE) ## written in step 2

grouping <- data.frame() # create an empty frame

words <- c("love", "hate") # input words to check

for (w in words) { # loop through those words

     thesesongs <- allsongs[grep(w,allsongs$lyrics), ] # subset out songs with lyrics containing the word
     
     count <- aggregate(thesesongs$rank~thesesongs$year, FUN=length) # count how many songs there are
     
     ### since the number of words per song has increased dramatically since 1964, weight each year's count appropriately
     
          
     for (i in 1:length(count[,1])) { # account for missing years with a loop
     
          # loop over each row in count
          
          for ( y in 1:51 ) { 
          
               # compare to each row in countbyyear until we find the right year
               
               if (count[i,1]==countbyyear[y,1]) { #countbyyear created in step 3
               
                    # multiply by the appropriate weight
                    
                    count$weight[i] <- countbyyear$weights[y]
                    
                    count$weighted[i] <- countbyyear$weights[y]*count[i,2]
                    
                    count$check[i] <- count$weighted[i]/count$weight[i]
               } 
          }
     }
     
     count$word <- rep(w, length(count$check))
     
     grouping <- rbind(grouping, count)
}

colnames(grouping) <- c("year", "unweighted", "weight", "weighted", "check", "word")

# make the plot

ggplot(grouping, aes(year, weighted)) + geom_line(aes(color=word),size=1.25) + ylim(0,100) + 
  scale_color_brewer(palette="Paired") + 
  labs(title=paste("Billboard Year-End Hot 100 Singles 1964-2014\nlyrics containing certain words"), x="", y="") + 
  theme(plot.title = element_text(size=20, face="bold", vjust=2), axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), panel.background = element_rect(fill = 'white')) 

