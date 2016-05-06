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
### source: multiple. 1=metorlyics.com, 3=songlyrics.com, 5=lyricsmode.com
for (s in 1:length(allthesongs$Song))  {
     
     lyrics <- "Not set yet."
     results <- 12 # arbitrary number
     
     # clean up the artist field to fit in the URL
     artist <- strsplit(allthesongs$Artist[s], " featuring | feat | feat. | with | duet | and ")
     artist <- unlist(artist)[[1]]
     artist2 <- gsub("the ", "", artist)
     firstletter <- substring(artist2, 1, 1)
     
     # make URLs
     metroURL <- paste("http://metrolyrics.com/",allthesongs$Song[s],"-lyrics-",artist2,".html",sep="")
     songURL <- paste("http://songlyrics.com/",artist2,"/",allthesongs$Song[s],"-lyrics",sep="")
     modeURL <- paste("http://www.lyricsmode.com/lyrics/", firstletter, "/", artist2, "/", allthesongs$Song[s], ".html", sep="")
     
     
     URLs <- c(metroURL, songURL, modeURL)
     
     lyriclocs <- c("//div[@id='lyrics-body-text']", 
                    "//p[@id='songLyricsDiv']", 
                    "//p[@id='lyrics_text']")
     
     for (b in 1:length(URLs)) {
          allthesongs$Lyrics[s] <- "Not set yet."
          
          results <- 12 # arbitrary number
          
          if(b!=3) URL <- tolower(gsub(" ", "-", URLs[b]))
          if(b==3) URL <- URLs[b]
          
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
## 3.67% of lyrics are missing
allthesongs$Lyrics <- gsub("not set yet", "NA", allthesongs$Lyrics)
allthesongs$Lyrics <- gsub("we are not in a position to display these lyrics due to licensing restrictions sorry for the inconvenience", "NA", allthesongs$Lyrics)

# setwd("/Users/kaylinwalker/R/kw_musiclyrics")
# write.csv(allthesongs, "billboard_lyrics_1964-2015.csv", row.names=FALSE)
# allthesongs <- read.csv("billboard_lyrics_1964-2015.csv", stringsAsFactors=FALSE)



