# depends on 01_songscrape.R for billboard_lyrics_1964-2015.csv
# in: billboard_lyrics_1964-2015.csv
# out: tdm_bydecade.csv, LL_bydecade.csv, LL_bydecade_ranked.csv
# out: wordcloud plot, most characteristic lyrics plot

library(tm)
library(RWeka)
library(stringr)
library(ggplot2)
library(RColorBrewer)

setwd("/Users/kwalker/git_projects/kw_musiclyrics")
allthesongs <- read.csv("billboard_lyrics_1964-2015.csv", stringsAsFactors=FALSE)

# get characteristic words by decade
allthesongs$Decade <- paste(substring(allthesongs$Year, 1, 3), 0, sep="")
songsByDecade <- aggregate(Lyrics ~ Decade, allthesongs, paste, sep=" ")

# create corpus
myReader <- readTabular(mapping=list(content="Lyrics", id="Decade"))
corpus <- Corpus(DataframeSource(songsByDecade), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))

# create term document matrix
options(mc.cores=1)
allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))
all.tdm <- removeSparseTerms(tdm, 0.5) # 15545 / 1475681
dim(all.tdm)
all.df <- data.frame(inspect(all.tdm))
all.df$keyword <- row.names(all.df)
all.df <- all.df[,c(7,1:6)]
# write.csv(all.df, "tdm_bydecade.csv", row.names=FALSE)
all.df <- read.csv("tdm_bydecade.csv", stringsAsFactors=FALSE)

############ TOP WORDS WORDCLOUD ############
top <- data.frame(keyword=all.df$keyword, count=rowSums(all.df[,2:7]))
top <- top[order(-top$count), ]
library(wordcloud)
cloud <- wordcloud(top$keyword, top$count, scale=c(3,0.5), min.freq=500, max.words=200, random.order=FALSE)

############ LOG LIKELIHOOD ############
log.likelihood <- function(column.no, df) { 
    LL.df <- NULL
    year <- gsub("X", "", names(df)[column.no])
    total.group <- sum(df[,column.no])
    total.other <- sum(colSums(df[, -c(1, column.no)]))
    total.all <- sum(total.group + total.other)
    df <- df[df[,column.no]> 5, ]
    for(word in seq_along(df[,1])){
        row <- df[word, ]
        total.word <- sum(row[,-1])
        o1 <- row[1, column.no]; if(o1==0) o1 <- 0.01
        o2 <- sum(row[1, -c(1, column.no)]); if(o2==0) o2 <- 0.01
        e1 <- total.group * (total.word/total.all)
        e2 <- total.other * (total.word/total.all)
        LL <- 2 * (o1 * log(o1/e1) + o2 * log(o2/e2))
        if(o1 < e1) LL <- LL * -1
        r <- data.frame(year, keyword=row$keyword[1], LL, total.word, o1, o2, e1, e2)
        LL.df <- rbind(LL.df, r)
    }
    LL.df <- LL.df[order(LL.df$LL), ]
    return(LL.df)
}

LL.final <- NULL
for(j in 2:7){
    set <- log.likelihood(j, all.df)
    LL.final <- rbind(LL.final, set)
}
# write.csv(LL.final, "LL_bydecade.csv", row.names=FALSE)
LL <- read.csv("LL_bydecade.csv", stringsAsFactors=FALSE)

# get top 25 for each decade
LL.rank <- NULL
for(year in unique(LL$year)){
    sub <- LL[LL$year==year, ]
    sub <- sub[abs(sub$LL) > 10.83, ]
    sub$Repeats <- FALSE
    for(word in seq_along(sub$keyword)){
        total <- length(strsplit(sub$keyword[word], " ")[[1]])
        unique <- length(unique(strsplit(sub$keyword[word], " ")[[1]]))
        if(total > unique) sub$Repeats[word] <- TRUE
    }
    sub <- sub[sub$Repeats==FALSE, ]
    sub <- sub[order(-sub$LL), ]
    sub <- sub[1:25, ]
    sub$Rank <- 1:25
    LL.rank <- rbind(LL.rank, sub)
}
# write.csv(LL.rank, "LL_bydecade_ranked.csv", row.names=FALSE)

# log likelihood
LL.rank$year <- factor(LL.rank$year, levels=c("1960", "1970", "1980", "1990", "2000", "2010"))
mycolors <- c("#2DA58A", "#1B687E", "#8C2498", "#293781", "#C5844D", "#266E35")
ggplot(LL.rank, aes(year, (-1*Rank))) + geom_point(color="white") + 
    geom_text(aes(label=LL.rank$keyword, color=LL.rank$year),  fontface='bold', size=5) + 
    theme(legend.position="none", plot.title = element_text(size=18), 
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          axis.text.x=element_text(size=16)) + 
    labs(title="Most Characteristic Lyrics by Decade \n Billboard Year-End Top 100, 1965-2015") + 
    xlab("") + ylab("Ranking") +
    scale_y_continuous(limits=c(-25,-1), breaks=c(-20, -10, -.5), labels=c("#20", "#10", "#1")) +
    scale_color_manual(values = mycolors) + theme_classic() 


