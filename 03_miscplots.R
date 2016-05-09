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
mycolors2 <- c("#053340", "#20687d", "#5a9fb3", 
               "#0b6851", "#35a58a", "#71dcc2",
               "#0e3f18", "#296d37", "#6ab078",            
               "#101b4e", "#2a3971", "#7b8ad1",
               "#053340", "#20687d",
               "#5a1164", "#8a2b96", "#cc78d6",
               "#8e5223", "#c48452", "#e9b286")
allthesongs$Word.Count <- sapply(allthesongs$Lyrics, function(x) length(strsplit(x, " ")[[1]]))
allthesongs$Unique.Word.Count <- sapply(allthesongs$Lyrics, function(x) length(unique(strsplit(x, " ")[[1]])))
allthesongs$Inverse.Density <- round(allthesongs$Word.Count/allthesongs$Unique.Word.Count, 4)
allthesongs$Density <- round(allthesongs$Unique.Word.Count/allthesongs$Word.Count, 4)*100
allthesongs <- allthesongs[allthesongs$Word.Count > 5, ]

nol <- allthesongs[,c(1:4,6:10)]

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

######## CAREERS
keeps <- artists20$Artist
keeps2 <- artists[artists$Freq>5, 1]
careers <- NULL
for(artist in keeps2){
    sub <- nol[nol$Artist==artist, ]
    sub <- sub[order(-sub$Year), ]
    start <- sub$Year[length(sub[,1])]
    end <- sub$Year[1]
    span <- end - start
    row <- data.frame(artist, start, end, span)
    careers <- rbind(careers, row)
}

spans <- merge(careers, artists, by.x="artist", by.y="Artist")
spans$Rate <- round(spans$Freq/spans$span,2)
ggplot(spans, aes(span, Rate)) + geom_point(color="#2DA58A", size=4, alpha=0.75) + theme_bw() + 
    ylab("Avg. Songs per Year") + xlab("Career Span") + labs(title="Song Frequency by Career Span") +
    annotate("text", hjust=0, y=4.25, x=23, label="Billboard Year End Hot 100 1965-2015\nartists with > 5 songs\navg. songs per year = \ncharted hits / career span (yrs)") +
    theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0))) 

songcareer <- lm(log(Rate) ~ span, spans) # .59 Rsquared, -.06 is estimate, p <0.000

careers20 <- careers[careers$artist %in% keeps, ]
ggplot(careers20, aes(color=artist)) + geom_segment(aes(x=start, xend=end, y=artist, yend=artist), color="#20687d", size=6) +
    geom_text(aes(x=start+.25, hjust=0, vjust=0.36, y=artist, label=paste(artist, " (", span, " yrs)", sep="")), color="#FFFFFF") +
    theme_bw() + ylab("") + xlab("") + annotate("text", y=2, x=1967, label="Billboard Year End\nHot 100 1965-2015") +
    labs(title="Career Spans of Top 20 Most-Charted Artists") + scale_y_discrete(breaks=NULL) +
    theme(plot.title = element_text(size=18), axis.title.y=element_text(margin=margin(0,10,0,0)), 
          legend.position="none", panel.grid.major = element_blank()) 

