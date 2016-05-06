# depends on 01_songscrape.R for billboard_lyrics_1964-2015.csv
# in: billboard_lyrics_1964-2015.csv
# out: plots average total/unique words scatterplot, total/unique words bloxplots
# out: 2+ artists line plot, number of songs per artist bar chart, top 20 artists bar chart

library(ggplot2)
library(RColorBrewer)
setwd("/Users/kwalker/git_projects/kw_musiclyrics")
allthesongs <- read.csv("billboard_lyrics_1964-2015.csv", stringsAsFactors=FALSE)

############ Avg song length 

allthesongs$wc <- sapply(allthesongs$Lyrics, function(x) length(strsplit(x, " ")[[1]]))
allthesongs$uwc <- sapply(allthesongs$Lyrics, function(x) length(unique(strsplit(x, " ")[[1]])))
allthesongs <- allthesongs[allthesongs$wc > 5, ]

slen <- aggregate(wc ~ Year, allthesongs, mean)
slen$Type <- "Total"
sulen <- aggregate(uwc ~ Year, allthesongs, mean)
sulen$Type <- "Unique"
colnames(sulen) <- c("Year", "wc", "Type")
slplot <- rbind(slen, sulen)

mycolors <- c("#2DA58A", "#1B687E", "#8C2498", "#293781", "#C5844D", "#266E35")
ggplot(slplot, aes(Year, wc)) + geom_point(aes(color=Type), size=1.5) + 
    labs(title="Average Total and Unique Words per Song\nBillboard Year End Hot 100 1965-2015") + 
    ylab("") + xlab("") +
    theme_classic() + theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0))) +
    scale_color_manual(values=mycolors) 

ggplot(allthesongs, aes(Year, wc)) + geom_boxplot(aes(group=Year), fill="#2DA58A", color="#293781") + 
    labs(title="Total Words per Song\nBillboard Year End Hot 100 1965-2015") + 
    ylab("") + xlab("") +
    theme_classic() + theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0)))

ggplot(allthesongs, aes(Year, uwc)) + geom_boxplot(aes(group=Year), fill="#293781", color="#666666") + 
    labs(title="Unique Words per Song\nBillboard Year End Hot 100 1965-2015") + 
    ylab("") + xlab("") +
    theme_classic() + theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0)))

wcfit <- lm(wc ~ Year, allthesongs)
uwcfit <- lm(uwc ~ Year, allthesongs)

allthesongs$ratio <- round(allthesongs$uwc/allthesongs$wc, 4)*100
allthesongs <- allthesongs[order(allthesongs$ratio), ]
ggplot(allthesongs, aes(Year, ratio)) + geom_point(color="#2DA58A", size=1.5) + stat_smooth(method="lm", se=FALSE, color="black") + 
    labs(title="Percent Unique Words per Song\nBillboard Year End Hot 100 1965-2015") + 
    ylab("") + xlab("") +
    theme_classic() + theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0))) 

fitratio <- lm(ratio ~ Year, allthesongs)



############ Mult. artists ############ 
multiples <- allthesongs[grepl("feat|duet| with ", allthesongs$Artist), ]
mult <- data.frame(table(multiples$Year))
colnames(mult) <- c("Year", "Freq")
mult$Year <- as.numeric(as.character(mult$Year))

ggplot(mult, aes(Year, Freq)) + geom_line(size=1.5, color="#2DA58A") + 
    labs(title="Songs Featuring 2+ Artists\nBillboard Year End Hot 100 1965-2015") + 
    ylab("") + xlab("") +
    theme_classic() + theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0))) 

############ Top artists ############ 
artists <- data.frame(table(allthesongs$Artist))
artists$Var1 <- as.character(artists$Var1)
artists$Artist <- sapply(artists$Var1, function(x) strsplit(x, " featuring")[[1]][1])
artists <- aggregate(Freq ~ Artist, artists, sum)
artists <- artists[order(-artists$Freq), ]
artists20 <- artists[1:20, ]
artists20<- artists20[order(artists20$Freq), ]
artists20$Artist <- factor(artists20$Artist, levels=artists20$Artist)

ggplot(artists20, aes(Artist, Freq)) + geom_bar(stat="identity") + theme_classic() + 
    labs(title="Number of Songs, Top 20\nBillboard Year End Hot 100 1965-2015") + 
    ylab("") + xlab("") + coord_flip() 

ggplot(artists, aes(Freq)) + geom_bar()+ theme_classic() + ylab("") + xlab("") +
    labs(title="Number of Songs per Artist\nBillboard Year End Hot 100 1965-2015") 


############ Longest/shortest songs ############ 
allthesongs <- allthesongs[order(allthesongs$wc), ]
shortest <- allthesongs[c(1,2,4:6),]
allthesongs <- allthesongs[order(-allthesongs$wc), ]
longest <- allthesongs[1:6,]

allthesongs <- allthesongs[order(allthesongs$uwc), ]
shortestu <- allthesongs[1:5,]
allthesongs <- allthesongs[order(-allthesongs$uwc), ]
longestu <- allthesongs[1:6,]







