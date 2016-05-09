# depends on 01_songscrape.R for billboard_lyrics_1964-2015.csv
# in: billboard_lyrics_1964-2015.csv
# out: artists_billboard.png, feat_billboard.png, words_billboard.png

library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
setwd("/Users/kwalker/git_projects/kw_musiclyrics")
allthesongs <- read.csv("billboard_lyrics_1964-2015.csv", stringsAsFactors=FALSE)

mycolors <- c("#2DA58A", "#1B687E", "#8C2498", "#293781", "#C5844D", "#266E35")
allthesongs$Word.Count <- sapply(allthesongs$Lyrics, function(x) length(strsplit(x, " ")[[1]]))
allthesongs$Unique.Word.Count <- sapply(allthesongs$Lyrics, function(x) length(unique(strsplit(x, " ")[[1]])))
allthesongs$Inverse.Density <- round(allthesongs$Word.Count/allthesongs$Unique.Word.Count, 4)
allthesongs$Density <- round(allthesongs$Unique.Word.Count/allthesongs$Word.Count, 4)*100
allthesongs <- allthesongs[allthesongs$Word.Count > 5, ]


############ WORD COUNTS
a <- ggplot(allthesongs, aes(Year, Word.Count)) + geom_point(color="#2DA58A", alpha=.4, size=4) + 
    labs(title="Words per Song (Total)") + 
    annotate("text", x=1990, y=-10, label="Billboard Year End Hot 100 1965-2015") +
    stat_smooth(color="black", se=FALSE, method="lm") +
    ylab("Count") + xlab("") + scale_color_manual(values = mycolors) +
    theme_classic() + theme(plot.title = element_text(size=18), 
                            axis.title.y=element_text(margin=margin(0,10,0,0)), legend.position="none")

b <- ggplot(allthesongs, aes(Year, Unique.Word.Count)) + geom_point(color="#1B687E", alpha=.4, size=4) + 
    labs(title="Words per Song (Unique)") + 
    annotate("text", x=1990, y=-10, label="Billboard Year End Hot 100 1965-2015") +
    stat_smooth(color="black", se=FALSE, method="lm") +
    ylab("Count") + xlab("") + scale_color_manual(values = mycolors) +
    theme_classic() + theme(plot.title = element_text(size=18), 
                            axis.title.y=element_text(margin=margin(0,10,0,0)), legend.position="none")

grid.arrange(a,b, ncol=2) #words_billboard.png

fitwc <- lm(log(Word.Count) ~ Year, allthesongs)
fituwc  <- lm(log(Unique.Word.Count) ~ Year, allthesongs)


############ 2+ ARTISTS ############ 
multiples <- allthesongs[grepl("feat|duet| with ", allthesongs$Artist), ]
mult <- data.frame(table(multiples$Year))
colnames(mult) <- c("Year", "Freq")
mult$Year <- as.numeric(as.character(mult$Year))

ggplot(mult, aes(Year, Freq)) + geom_bar(stat="identity", fill="#2DA58A") + 
    labs(title="Songs Featuring 2+ Artists") + ylim(c(-2,40)) +
    annotate("text", x=1990, y=-2, label="Billboard Year End Hot 100 1965-2015") +
    ylab("") + xlab("")  +
    theme_classic() + theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0))) 
# feat_billboard.png


############ TOP ARTISTS ############ 
artists <- data.frame(table(allthesongs$Artist))
artists$Var1 <- as.character(artists$Var1)
artists$Artist <- sapply(artists$Var1, function(x) strsplit(x, " featuring")[[1]][1])
artists <- aggregate(Freq ~ Artist, artists, sum)
artists <- artists[order(-artists$Freq), ]
artists20 <- artists[1:20, ]
artists20<- artists20[order(artists20$Freq), ]
artists20$Artist <- factor(artists20$Artist, levels=artists20$Artist)

c <- ggplot(artists20, aes(Artist, Freq)) + geom_bar(stat="identity", fill="#1B687E") + theme_classic() + 
    labs(title="Number of Songs, Top 20 Artists") + geom_text(aes(label=Freq), hjust=-0.25) +
    annotate("text", y=30, x=4, label="Billboard Year End\nHot 100 1965-2015") +
    ylab("") + xlab("") + coord_flip() 

d <- ggplot(artists, aes(Freq)) + geom_bar(fill="#1B687E") + theme_classic() + ylab("") + xlab("") +
    labs(title="Number of Songs per Artist") +
    annotate("text", y=150, x=30, label="Billboard Year End\nHot 100 1965-2015") 

grid.arrange(d,c, ncol=2, widths=c(0.45, 0.55)) #artists_billboard.png


