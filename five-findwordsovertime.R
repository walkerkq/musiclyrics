########## 
# Step Five in songlyrics-composite:  find strings in the lyrics and create ggplots
# Requires one-getsongs.R for data.frame allthesongs
# Requires two-getlyrics.R for .csv "wikisongs.csv"
# Requires three-getwordlegnthweights.R for data.frame countbyyear
##########

library("ggplot2")

allsongs <- read.csv("wikisongs.csv", stringsAsFactors=FALSE) ## written in step 2

grouping <- data.frame() # create an empty frame

words <- c("love", "hate") # input words to check #### MODIFY THIS LIST TO CHANGE RESULTS

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

### end result: plot of instances of chosen word(s) over time.