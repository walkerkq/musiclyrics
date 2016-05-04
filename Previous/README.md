kw_musiclyrics
==============

##Goal
Show word/topic trends in popular music from 1964 to 2014.

##Method
This R code uses the XML package to scrape Billboard Year-End Hot 100 Singles data (1964-2014) from Wikipedia (e.g. http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_1964)
and then song lyrics for each from www.songlyrics.com. The text is then cleaned for analysis. 
Roughly 2% of song lyrics were not available from the songlyrics.com database and were thus manually entered for analysis completion and integrity.
Exploratory analysis showed a bias for any given word to show up more often in later years than earlier years. 
Analysis of average word counts per song per year showed a positive correlation between year and average words per song. 
Because of this, the final analyses were weighted based on deviation from the mean. 
Finally, grep was used to locate particular strings within lyrics, count instances per year and then plot using the ggplot2 package.
