library(RCurl)
library(XML)
library(stringr)

################### SCRAPE SONG NAMES ###################
#### source: wikipedia.org

allthesongs <- data.frame() 
for (i in 1965:2015) { 
     # create the URL for each year
     URL <- paste("http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_",i,sep="")
     # parse the HTML
     results <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
     billboard_text <- xpathSApply(results, "//table[@class='wikitable sortable']//tr",xmlValue)
     split_billboard_text <- str_split_fixed(billboard_text,"\n",3) 
     billboard <- as.data.frame(cbind(split_billboard_text[2:101, ], rep(i,100)), stringsAsFactors=FALSE)
     # row bind this year's data to all the data
     allthesongs <- rbind(allthesongs, billboard) 
     
}
colnames(allthesongs) <- c("Rank", "Song", "Artist", "Year")
allthesongs$Song <- gsub('\\"', "", allthesongs$Song)
allthesongs$Song <- tolower(gsub("[^[:alnum:] ]", "", allthesongs$Song))
allthesongs$Song <- gsub("\\'", "", iconv(allthesongs$Song, to='ASCII//TRANSLIT')) # fix special accent chars

allthesongs$Artist <- tolower(gsub("[^[:alnum:] ]", "", allthesongs$Artist))
allthesongs$Artist <- gsub("'e", "e", iconv(allthesongs$Artist, to='ASCII//TRANSLIT')) # fix special accent chars
allthesongs$Artist<- gsub("'o", "o", allthesongs$Artist)

# new variables
allthesongs$Lyrics <- ""
allthesongs$Source <- ""

################### SCRAPE THE LYRICS ###################
### source: multiple. metorlyics.com, songlyrics.com
for (s in 1:length(allthesongs$Song))  {
     
     lyrics <- "Not set yet."
     results <- 12 # arbitrary number
     
     # clean up the artist field to fit in the URL
     artist <- strsplit(allthesongs$Artist[s], " featuring | feat | feat. | with | duet | and ")
     artist <- unlist(artist)[[1]]
     artist2 <- gsub("the ", "", artist)
     
     # make URLs
     metroURL <- paste("http://metrolyrics.com/",allthesongs$Song[s],"-lyrics-",artist2,".html",sep="")
     metronotheURL <- paste("http://metrolyrics.com/",allthesongs$Song[s],"-lyrics-",artist,".html",sep="")
     songURL <- paste("http://songlyrics.com/",artist2,"/",allthesongs$Song[s],"-lyrics",sep="")
     songnotheURL <- paste("http://songlyrics.com/",artist,"/",allthesongs$Song[s],"-lyrics",sep="")
     
     URLs <- c(metroURL, metronotheURL, songURL, songnotheURL)
     
     lyriclocs <- c("//div[@id='lyrics-body-text']", 
                    "//div[@id='lyrics-body-text']", 
                    "//p[@id='songLyricsDiv']", 
                    "//p[@id='songLyricsDiv']")
     
     for (b in 1:length(URLs)) {
          allthesongs$Lyrics[s] <- "Not set yet."
          
          results <- 12 # arbitrary number
          
          URL <- tolower(gsub(" ", "-", URLs[b]))
          
          tryCatch({ 
               results <- htmlTreeParse(URL, useInternal=TRUE, isURL=TRUE)
               lyrics <- xpathSApply(results, lyriclocs[b], xmlValue) },
               error = function(x) { 
                    message(paste(s, "failed")) },
               finally={ 
                    if (!is.numeric(results)) { 
                         if (length(lyrics)!=0) { 
                              allthesongs$Lyrics[s] <- lyrics[[1]]
                              message(paste(s, "success"))
                              allthesongs$Source[s] <- b
                              break
                         }
                    } 
               }) # end tryCatch
     } # end URL for
} # end for

allthesongs$Lyrics <- gsub("\\\n|\\\t"," ",allthesongs$Lyrics)
allthesongs$Lyrics <- tolower(gsub("[^[:alnum:] ]", "", allthesongs$Lyrics))
missing <- round(length(allthesongs[allthesongs$Lyrics=="not set yet", 1])/length(allthesongs[,1]), 4)*100
## 6.41% of lyrics are missing
allthesongs$Lyrics <- gsub("not set yet", "NA", allthesongs$Lyrics)

# write.csv(allthesongs, "songs_and_lyrics.csv", row.names=FALSE)
# allthesongs <- read.csv("songs_and_lyrics.csv", stringsAsFactors=FALSE)

