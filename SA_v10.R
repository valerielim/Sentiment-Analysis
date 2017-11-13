# Date: 13 Nov 2017
# Title: Grab SA v10
# Reference: https://github.com/abromberg/sentiment_analysis/blob/master/sentiment_analysis.R
# Reference 2: http://rpubs.com/chengjun/sentiment
# Comparison of dictionaries: https://www.kaggle.com/apasupat/sentiment-analysis-3-different-methods/code
# Graphs for bubble plot: http://t-redactyl.io/blog/2016/02/creating-plots-in-r-using-ggplot2-part-6-weighted-scatterplots.html
# Graphs for sentiment: https://medium.com/@actsusanli/text-mining-is-fun-with-r-35e537b12002

# ---------------------------------------------------------------------------- #
# [Load, format dataset]

library(dplyr)
library(lubridate)
library(tidytext)
library(stringr)
library(ggplot2)
library(grid)
library(ggthemes)
library(plyr)

# Load data
setwd("C:/Users/valeriehy.lim/Documents/Grab")
uber_2017 <- read.csv("uber_comments_clean.csv", header=T, na.strings=c("", " ","NA"))
grab_2017 <- read.csv("Grab_comments_clean.csv", header=T, na.strings=c("", " ","NA"))
uber_2016 <- read.csv("uber_comments_2016_raw.csv", header=T, na.strings=c("", " ","NA"))
grab_2016 <- read.csv("grab_comments_2016_raw.csv", header=T, na.strings=c("", " ","NA"))
options(stringsAsFactors = FALSE)

# Drop unwanted columns
uber_2016 <- uber_2016[,1:5]
grab_2016 <- grab_2016[,1:5]
grab_2016 <- mutate(grab_2016, sentiment = NA)
uber_2016 <- mutate(uber_2016, sentiment = NA)

# Add company labels
grab_2017 <- mutate(grab_2017, Company="grab"); grab_2016 <- mutate(grab_2016, Company="grab")
uber_2017 <- mutate(uber_2017, Company="uber"); uber_2016 <- mutate(uber_2016, Company="uber")

names(uber_2017) <- c("creation_time", "post_username", "post_ID", 
                 "post_comment", "message_ID", "sentiment", "Company")
names(grab_2017) <- c("creation_time", "post_username", "post_ID", 
                 "post_comment", "message_ID", "sentiment", "Company")
names(grab_2016) <- c("creation_time", "post_username", "post_ID", 
                      "post_comment", "message_ID", "sentiment", "Company")
names(uber_2016) <- c("creation_time", "post_username", "post_ID", 
                      "post_comment", "message_ID", "sentiment", "Company")

# Bind, format all columns
data <- rbind(uber_2017, uber_2016, grab_2017, grab_2016)
data <- transform(data, creation_time = as.character(creation_time), 
                  post_username = as.character(post_username),
                  post_ID = as.character(post_ID), 
                  post_comment = as.character(post_comment),
                  message_ID = as.character(message_ID),
                  sentiment = as.factor(sentiment),
                  Company = as.factor(Company)); str(data)
data <- data[(!is.na(data$creation_time)),]
     
# ---------------------------------------------------------------------------- #
# [Clean up time format, build feature for number of words per comment]

## convert date format, and shift timezone to SG
format_FBtime <- function(df) {
    library(lubridate)
    df$creation_time <- as.POSIXct(df$creation_time, 
                                  format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
    df$DatetimeSG <- with_tz(df$creation_time, "Singapore")
    df <- subset(df, select= -c(creation_time))
    return(df)
}
data <- format_FBtime(data)

# Keep only data from March to Aug
data <- data[(data$DatetimeSG >= as.Date("010317", "%d%m%y") &
                  data$DatetimeSG < as.Date("010917", "%d%m%y") | # 2017
                  data$DatetimeSG >= as.Date("010316", "%d%m%y") &
                  data$DatetimeSG < as.Date("010916", "%d%m%y")), ] # 2016

# Extract hour, day of week
data$Hour <- hour(data$DatetimeSG)
data$Dayofweek <- as.POSIXlt(data$DatetimeSG)$wday

# Extract number of words in each comment
data$wordcount <- str_count(data$post_comment, '\\s+')+1

# Remove non-ASCII characters in comments, usernames
data$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", 
                          "", data$post_comment)

# ---------------------------------------------------------------------------- #
# [Explore data, plot number of posts by week, weekday, time]

# Analyse only for 2017
data_2017 <- data[data$DatetimeSG > as.Date("010117", "%d%m%y"),]

# Plot number of posts by WEEK
data_week <- data_2017 %>% 
    dplyr::group_by(week = strftime(DatetimeSG, format = "%V"), Company) %>% 
    dplyr::summarise(num_comments = n()) 

plot_week <- ggplot(data_week, aes(x=week, y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + labs(colour="Company", y=NULL) + 
    scale_x_discrete(name = "Date by weeks", breaks=rep(09,32,4)) + 
    ggtitle("Total Comments per Week", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15), legend.position = "none",
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9),
          plot.subtitle = element_text(face="italic",size=6),) + facet_grid(. ~ Company); plot_week

# Plot number of comments by MONTH
data_month <- data_2017 %>% 
    dplyr::group_by(Month = month(DatetimeSG), Company) %>% 
    dplyr::summarise(num_comments = n()) 

Labels <- data.frame(num = c(3:9), Date = c("Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep"))
data_month <- left_join(data_month, Labels, by=c("Month" = "num"))    

plot_month <- ggplot(data_month, aes(x=reorder(Date, Month), y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) +
    labs(colour="Company", x="Date by Month", y=NULL) +
    scale_x_discrete(name = "Month", labels=abbreviate) + facet_grid(. ~ Company) +
    ggtitle("Total Comments per Month", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), legend.position = "none",
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9))

# Plot number of posts by WEEKDAY
data_weekday <- data_2017 %>% 
    dplyr::group_by(Dayofweek, Company) %>% 
    dplyr::summarise(num_comments = n()) 

Labels <- data.frame(num = c(0:6), Date = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
data_weekday <- left_join(data_weekday, Labels, by=c("Dayofweek" = "num"))

plot_weekday <- ggplot(data_weekday, aes(x=reorder(Date, Dayofweek), y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + 
    facet_grid(. ~ Company) + labs(colour="Company", y=NULL) + 
    scale_x_discrete(name = "Day of Week", labels=abbreviate) +
    ggtitle("Total Comments per Day of Week", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15), axis.title = element_text(size=9),
          plot.subtitle = element_text(face="italic",size=6),
          legend.position = "none", strip.text=element_text(face="bold", size=9))

# Plot number of posts by HOUR
data_hour <- data_2017 %>% 
    dplyr::group_by(Hour, Company) %>% 
    dplyr::summarise(num_comments = n()) 

plot_hour <- ggplot(data_hour, aes(x=Hour, y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + facet_grid(. ~ Company) + 
    labs(colour="Company", x= "Hour of Day, Midnight to Midnight", y=NULL) + 
    # scale_x_discrete("Hour", labels=c(0,6,12,18)) +
    ggtitle("Total Comments per Hour", subtitle = "(based on public data from Facebook, 24:00 clock)") +
    theme(plot.title = element_text(face="bold", size=15), axis.title = element_text(size=9),
          plot.subtitle = element_text(face="italic",size=6), legend.box = "horizontal",
          legend.box.just="left", legend.key.size = unit(0.5, "cm"), 
          legend.text = element_text(face="bold",size = 6),
          legend.position = "none", strip.text=element_text(face="bold", size=9))

# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Ref: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

multiplot(plot_week, plot_month, plot_weekday, plot_hour, cols=2)

# ---------------------------------------------------------------------------- #
# [Load and customise word dictionary]

# SetWD to documents folder; stored files MUST be in [SENTIMENT ANALYSIS] folder
setwd("~/sentiment_analysis")

# Load word files 
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

# Check if word exists & return score: (reuse this)
afinn_list$score[afinn_list$word=="YOURWORDHERE"]

# Modify scores (for words already in AFINN)
afinn_list$score[afinn_list$word %in% c("best", "appreciation")] <- 4
afinn_list$score[afinn_list$word %in% c("charges", "charged", "pay", "like", 
                                        "joke", "improvement")] <- 0
afinn_list$score[afinn_list$word %in% c("irresponsible", "whatever")] <- -1
afinn_list$score[afinn_list$word %in% c("cheat", "cheated", "frustrated", 
                                        "scam", "pathetic", "hopeless",
                                        "useless", "dishonest", "tricked", 
                                        "waste", "gimmick", "liar", "lied")] <- -4

# Add scores (for words not in AFINN)
pos4 <- data.frame(word = c("bagus", "yay", ":)", "kindly", "^^", "yay", "swee", 
                            "awesome", "polite", "thnks", "thnk", "thx", "thankyou",
                            "tq", "ty", "professional", "pls"), score = 4)
pos2 <- data.frame(word = c("jiayou", "assist", "amin", "amen", "supper", "dating",
                            "arigato", "well", "bro"), score = 2)
pos1 <- data.frame(word = c("hi", "dear", "hello"), score = 1)
neg1 <- data.frame(word = c("silly", "dafaq", "dafuq", "cringe", "picky"), score = -1)
neg2 <- data.frame(word = c("jialat", "waited", "waiting", "rubbish", "lousy", "siao", 
                            "??", "-_-", "-.-", "slap", "slapped", "sicko",
                            "lying", "lies", "wtf", "wts"), score = -2)
neg4 <- data.frame(word = c("freaking", "knn", "ccb", "fk", "fking", "moronic"), score = -4)

# Save changes to main list
afinn_list <- rbind(afinn_list, pos4, pos2, pos1, neg1, neg2, neg4)

# Customise stopwords for removal
library(tm)
stop_words <- as.data.frame(stopwords("en"))
more_stopwords <- as.data.frame(c("uber", "grab", "will", "can", "get", 'u')) # add more if you want
names(stop_words)[1] <- "words"; names(more_stopwords)[1] <- "words"
stop_words <- rbind(stop_words, more_stopwords)
detach("package:tm", unload=TRUE)

# ---------------------------------------------------------------------------- #
# [Calculate AFINN scores for each comment]

# Split comments to single words per cell
data_AFINN <- subset(data, select = c(post_comment, message_ID))
data_indv <- strsplit(data_AFINN$post_comment, split = " ") 

# Relink single words to parent comment, "message_ID"
data_words <- data.frame(message_ID = rep(data_AFINN$message_ID, sapply(data_indv, length)), 
                         words = unlist(data_indv))

# Prep for stopwords removal: Remove punctuation, convert to lowercase
data_words$words <- tolower(data_words$words)
data_words <- data_words[(!data_words$words==""),]
data_words <- transform(data_words, words = (sub("^([[:alpha:]]*).*", "\\1", 
                                                 data_words$words)))

# Remove stopwords
data_words <- data_words[!(data_words$words %in% stop_words$words),] 

# ---------------------------------------------------------------------------- #
# [Match words with AFINN dict to calc sentiment by words]

# Match each word to their score
data_words <- left_join(data_words, afinn_list, by= c("words" = "word")) 

# Calculate mean of each comments' word scores
data_afinn <- data_words %>%
    dplyr::group_by(message_ID) %>%
    dplyr::summarise(mean = mean(score, na.rm=TRUE))

# Convert NAN to 0
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
data_afinn[is.nan(data_afinn)] <- 0 

# Attach AFINN scores to main df
data <- left_join(data, data_afinn, by= c("message_ID" = "message_ID")) 

# ---------------------------------------------------------------------------- #
# [Compare companies]

# Split datasets
uber_2017 <- data[(data$Company=="uber" & data$DatetimeSG >= as.Date("010117", "%d%m%y")),]
grab_2017 <- data[(data$Company=="grab" & data$DatetimeSG >= as.Date("010117", "%d%m%y")),]
uber_2016 <- data[(data$Company=="uber" & data$DatetimeSG < as.Date("010117", "%d%m%y")),]
grab_2016 <- data[(data$Company=="grab" & data$DatetimeSG < as.Date("010117", "%d%m%y")),]

# Compare:
t.test(uber_2017$mean, grab_2017$mean) # sig
t.test(uber_2016$mean, grab_2016$mean) # sig

# Note: Do not reset is.na(data$mean) to zero. Resetting their mean score to 
# zero means we are labelling them as 'neutral', when they might have been 
# image or emoji comments that carry other sentiments. Leaving them as NA 
# means we exclude them from calculations.

# ---------------------------------------------------------------------------- #
# [plot 5 = boxplot of changes in sentiment by month]

# Analyse only for 2017
data_2017 <- data[data$DatetimeSG > as.Date("010117", "%d%m%y"),]
data_2016 <- data[data$DatetimeSG < as.Date("010117", "%d%m%y"),]

# Segment out data for boxplot
afinn_boxplot <- data_2017 %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select = c(Month_num, mean, Company))
afinn_boxplot <- afinn_boxplot[!is.na(afinn_boxplot$mean),]
afinn_boxplot$mean <- round(afinn_boxplot$mean, digits=2)

# Label month names
Labels <- data.frame(num = c(3:9), Date = c("Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep"))
afinn_boxplot <- left_join(afinn_boxplot, Labels, by=c("Month_num" = "num")) 

# Boxplot per month by AFINN score
ggplot(afinn_boxplot, aes(x=reorder(factor(Date), Month_num), y=mean)) + xlab(NULL) +
    geom_boxplot(aes(fill=Company)) + 
    ggtitle("Distribution of Sentiment by Word (2017)",
    subtitle = "(Sentiment distribution using AFINN dictionary, based on public data from Facebook, Mar'17 to Aug'17 (n=6354))") +
    scale_y_continuous(name = "AFINN score (neg=rude, pos=polite)", breaks = seq(-4, 4, 1.0), limits=c(-4, 4)) + 
    scale_fill_brewer(palette = "Accent") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.title = element_text(face="italic",size=6), legend.position = "bottom", 
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), axis.title = element_text(size=9)) 

# 2016 data
afinn_boxplot <- data_2016 %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select = c(Month_num, mean, Company))
afinn_boxplot <- afinn_boxplot[!is.na(afinn_boxplot$mean),]
afinn_boxplot$mean <- round(afinn_boxplot$mean, digits=2)
afinn_boxplot <- left_join(afinn_boxplot, Labels, by=c("Month_num" = "num")) 

# Boxplot per month by AFINN score
ggplot(afinn_boxplot, aes(x=reorder(factor(Date), Month_num), y=mean)) + xlab(NULL) +
    geom_boxplot(aes(fill=Company)) + 
    ggtitle("Distribution of Sentiment by Word (2016)",
    subtitle = "(Sentiment analysis through AFINN dictionary, based on public data from Facebook, Mar'16 to Aug'16 (n=2457))") +
    scale_y_continuous(name = "AFINN score (neg=rude, pos=polite)", breaks = seq(-4, 4, 1.0), limits=c(-4, 4)) + 
    scale_fill_brewer(palette = "Accent") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.title = element_text(face="italic",size=6), legend.position = "bottom", 
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), axis.title = element_text(size=9)) 

# ---------------------------------------------------------------------------- #
# SentimentR package

library(sentimentr)

# Single sentences
data_sentence <- get_sentences(data$post_comment)
data_sr <-sentiment(data_sentence)

# Score by cell 
data_sr <- data_sr %>%
    dplyr::group_by(element_id) %>%
    dplyr::summarise(mean(sentiment))

# Bind back to main frame
data <- cbind(data, SentimentR = data_sr$`mean(sentiment)`)

# Exclude scores for empty lines
exclude <- data[is.na(data$mean), message_ID] 
exclude$SentimentR <- NA
data$SentimentR <- ifelse(is.na(data$mean) == TRUE, data$sentimentR == NA, data$SentimentR) 

# Split datasets
uber_2017 <- data[(data$Company=="uber" & data$DatetimeSG >= as.Date("010117", "%d%m%y")),]
grab_2017 <- data[(data$Company=="grab" & data$DatetimeSG >= as.Date("010117", "%d%m%y")),]
uber_2016 <- data[(data$Company=="uber" & data$DatetimeSG < as.Date("010117", "%d%m%y")),]
grab_2016 <- data[(data$Company=="grab" & data$DatetimeSG < as.Date("010117", "%d%m%y")),]

# T-test
t.test(uber_2016$SentimentR, grab_2016$SentimentR) # not sf 
t.test(uber_2017$SentimentR, grab_2017$SentimentR) # sf
