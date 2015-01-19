########## 
# Step Two in songlyrics-composite: grab lyrics for each song from songlyrics.com
# Requires one-getsongs.R for data.frame allthesongs
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

### end result: a .csv containing song names, artist names, rank, year and lyrics for 51 years' worth of top 100 songs. Roughly 2% will need to be added manually (do not exist in songlyrics.com database, e.g.)