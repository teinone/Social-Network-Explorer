#NECESSARY
library("data.table")
library("readr")
library("dplyr")
library("stringr")
library("ggplot2")
#Extra:
library("igraph")

setwd("C:/Users/minxi/Documents/RFiles/Subreddit network")

# NOT USED RIGHT NOW: Creates a data set without the properties fields
#DATA USED: =================================================================================
#The full dataset for body hyperlinks
reddit_body_raw <- read_delim("soc-redditHyperlinks-title.tsv", 
     "\t", escape_double = FALSE, col_types = cols( 
               TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
     			 trim_ws = TRUE)
View(reddit_body_raw)

#Store this into a data.frame
df.reddit_body_raw <- as.data.frame(reddit_body_raw)

#The full dataset for title hyperlinks
reddit_title_raw <- read_delim("soc-redditHyperlinks-body.tsv", 
     "\t", escape_double = FALSE, col_types = cols( 
		       TIMESTAMP = col_datetime(format = "%Y-%m-%d %H:%M:%S")), 
     			 trim_ws = TRUE)
View(reddit_title_raw)

#Store this into a data.frame
df.reddit_title_raw <- as.data.frame(reddit_title_raw)

#============================================================================================

#COMBINING THE DATA:

#Add a column with the source dataset name
df.reddit_body <- mutate(df.reddit_body_raw, bodytitle = "body")
View(df.reddit_body)
df.reddit_title <- mutate(df.reddit_title_raw, bodytitle = "title")

#Combine the two tables
df.reddit_both <- bind_rows(df.reddit_body, df.reddit_title, id = NULL)
View(df.reddit_both)

#Creating a new column with the string: source|target
df.reddit_both$sourcetarget <- str_c(df.reddit_both$SOURCE_SUBREDDIT, "|", df.reddit_both$TARGET_SUBREDDIT)

#Only take one row for each distinct source|target pair
df.reddit_both_simple <- distinct(df.reddit_both, df.reddit_both$sourcetarget, keep_all = FALSE )
View(df.reddit_both_simple)

#============================================================================================

# Subsetting data
dt.reddit.both <- as.data.table(df.reddit_both)
dt.reddit.both.subset <- dt.reddit.both[grep("2014",TIMESTAMP)]
dt.reddit.both.subset

save(dt.reddit.both, file="reddit_complete.RData")
save(dt.reddit.both.subset, file = "reddit_subset.RData")
