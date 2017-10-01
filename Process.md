# Title

### Background

Some cool stuff here

### Process

Grab and Uber are competing pretty hard for the local market right now. After 
Travis' dramatic exit, I wondered if the company's negative press image would 
affect the way day-to-day consumers feel towards the brand. 

This post outlines the process I used to build a simple `naive bayes` sentiment
classifier, along with results of my project. 

```
# Load libraries
library(dplyr)
library(lubridate)
library(tidytext)
library(stringr)

library(ggplot2)
library(grid)
library(ggthemes)
```

[Here](link) is a function to tidy up and convert the date-time format 
from Facebook's API, from `YYYYMMDDTHH:MM:SS` to `YYYY-MM-DD HH:MM:SS`. 
It's a little long-winded so I won't put it here.

```
format_FBtime <- function(df){ 
    
    # What this function needs:
    # 1. Datetime column in CHAR format titled "creation_time"
    
    # What this function returns: 
    # 1. "DatetimeSF" in DATE format (UTC -8:00) as YYYY-MM-DD HH:MM:SS 
    # 2. "DatetimeSG" in DATE format (SGT +8:00) as YYYY-MM-DD HH:MM:SS 
    # 3. Drops unformatted column "creation_time"
    
    df <- mutate(df, Date = paste(year(df$creation_time), 
                                  month(df$creation_time), 
                                  day(df$creation_time), 
                                  sep="-"))
    df <- mutate(df, Time=substr(as.character(df$creation_time),12, 19))
    df <- mutate(df, DatetimeSF = paste(df$Date, df$Time))
    df$DatetimeSF <- as.POSIXct(strptime(df$DatetimeSF, 
                                         "%Y-%m-%d %H:%M:%S", 
                                         tz="Pacific/Easter"))
    df <- mutate(df, DatetimeSG = with_tz(df$DatetimeSF, "Singapore"))
    
    # Use this to manually update any other timezone inaccuracies

    # Remove any unwanted columns here
    df <- subset(df, select = -c(Date, Time, creation_time))
    return(df)
}

# Apply format_FBtime function
grab <- format_FBtime(grab); str(grab)
uber <- format_FBtime(uber); str(uber)
```
For this study, I'll only keep data from `March 2017` to `August 2017` inclusive,
over a 6-month period from after Travis left in late Feb.

```
grab <- grab[grab$DatetimeSG >= as.Date("010317", "%d%m%y") & grab$DatetimeSG < as.Date("010917", "%d%m%y"), ] # n=3182
uber <- uber[uber$DatetimeSG >= as.Date("010317", "%d%m%y") & uber$DatetimeSG < as.Date("010917", "%d%m%y"), ] # n=3159
```
I'll throw the data into a simple plot to check the number of posts per day for 
both companies. 
```
# Format: Group by number of posts per company
grab1 <- grab %>% 
    dplyr::group_by(DateSG) %>% 
    dplyr::summarise(n = n()) 
uber1 <- uber %>% 
    dplyr::group_by(DateSG) %>% 
    dplyr::summarise(n = n()) 
colnames(grab1)[2] = "Grab"; colnames(uber1)[2] = "Uber"
timeseries <- left_join(grab1, uber1, by=c("DateSG"="DateSG"))
timeseries[is.na(timeseries)] <- 0
timeseries <- transform(timeseries, 
              Date = as.POSIXct(strptime(timeseries$DateSG, "%Y-%m-%d", tz="Singapore")),
              Grab = as.numeric(Grab), Uber = as.numeric(Uber))

# [ Plot 1 = num posts per day as timeseries line graph]
ggplot(timeseries, aes(as.Date(DateSG))) + 
    geom_line(aes(y = Uber, color="Uber"), size= 0.75, colour="black") + 
    geom_line(aes(y = Grab, color="Grab"), size= 0.75, colour="forest green") + 
    labs(colour="Company", x=NULL, y="Number of Comments") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    ggtitle("Total Comments per Day, Grab & Uber", 
            subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal",
          legend.box.just="left",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6),
          legend.position = "bottom",
          axis.title = element_text(size=9))

# [plot 2 = num posts per day per company as back-to-back histogram]
# Plot number of posts over time, by day
colnames(grab1)[2] = "Num_comments"; colnames(uber1)[2] = "Num_comments"
uber1 <- mutate(uber1, Company='Uber')
grab1 <- mutate(grab1, Company='Grab', Num_comments=Num_comments*-1)
timeseries2 <- rbind(uber1, grab1) %>%
    transform(DateSG = as.Date(DateSG)) 

# Uber (right side)
plot_uber <- ggplot(timeseries2, aes(x=DateSG)) + 
    geom_bar(data = subset(timeseries2, Company == 'Uber'), aes(y=Num_comments, fill = Num_comments), 
    colour="black", stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b")

# Grab (left)
print(plot_uber + geom_bar(data = subset(timeseries2, Company == 'Grab'), 
             aes(y= Num_comments, fill = Num_comments), colour="dark green", stat = 'identity') +
    scale_y_continuous(breaks= seq(-75, 75, 25), limits = c(-75, 75), position = "right", 
    name = "Grab  - - -  Uber") + xlab("") + 
    scale_fill_gradient2(low ="forest green", mid = "white", high = "black", midpoint = 0, space = "rgb") +
        theme(legend.position = "none") + coord_flip())
```
Now I'll get to engineering some features to make sentiment detection easier. 

Here's a basic one for counting the number of words per comment.
```
uber$wordcount <- str_count(uber$post_comment, '\\s+')+1
grab$wordcount <- str_count(grab$post_comment, '\\s+')+1

# Remove non-ASCII characters in comments, usernames
uber$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", uber$post_comment)
uber$post_username <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", uber$post_username)
grab$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", grab$post_comment)
grab$post_username <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", grab$post_username)
```
Next, I was inspired by [this post](link) by Andy Bromberg who used the [AFINN dictionary](link) 
to make his own classifier. 

The AFINN dictionary is ... (explanation)

Initially, I replicated Bromberg's method, but those features did not significantly increase the accuracy
of prediction with this dataset. This was likely due to high statistical collinearity of variables, so I left them 
out of the rest of the discussion. 
(If you're curious, you can still find them in the [full R code](link) hosted on my github.)

Following, I modified the AFINN dictionary slightly to include more local colour, and made a 
single feature to calculate the mean AFINN score of each comment, spanning several sentences. 

```
### Modify AFINN

# SetWD to home dir; stored files MUST be in [SENTIMENT_ANALYSIS] folder in there
setwd("~/sentiment_analysis")

# Load word files 
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

# Check if word exists & return score: (reuse this)
afinn_list$score[afinn_list$word=="YOURWORDHERE"]

# Modify scores (for words already in AFINN)
afinn_list$score[afinn_list$word %in% c("best", "nice", "appreciation")] <- 4
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

# Merge changes with main AFINN list
afinn_list <- rbind(afinn_list, pos4, pos2, pos1, neg1, neg2, neg4)
```
*Comment: I don't relaly have an objective basis for why some swear words get a higher rating
than others, but I classified them by how angry my mother would be when I say them.*

Count sentiment per comment based on net word score:

```
library(plyr)

# Split comments to single words per cell
grab_indv <- strsplit(grab$post_comment, split = " ") 
uber_indv <- strsplit(uber$post_comment, split = " ") 
uber_words <- data.frame(message_ID = rep(uber_words$message_ID, sapply(uber_indv, length)), words = unlist(uber_indv)) # n=94,856
grab_words <- data.frame(message_ID = rep(grab_words$message_ID, sapply(grab_indv, length)), words = unlist(grab_indv)) # n=80,334
grab_words$words <- tolower(grab_words$words); uber_words$words <- tolower(uber_words$words)

# Customise stopwords
library(tm)
stop_words <- as.data.frame(stopwords("en"))
more_stopwords <- as.data.frame(c("uber", "grab", "will", "can", "get", 'u')) # add more if you want
names(stop_words)[1] <- "words"; names(more_stopwords)[1] <- "words"
stop_words <- rbind(stop_words, more_stopwords)
detach("package:tm", unload=TRUE)

# Remove stopwords, punctuation, empty rows, NA, stopwords again
uber_words <- uber_words[!(uber_words$words %in% stop_words$words),]
grab_words <- grab_words[!(grab_words$words %in% stop_words$words),]
grab_words <- transform(grab_words, words = (sub("^([[:alpha:]]*).*", "\\1", grab_words$words)))
uber_words <- transform(uber_words, words = (sub("^([[:alpha:]]*).*", "\\1", uber_words$words)))
grab_words <- grab_words[(!grab_words$words==""),]
uber_words <- uber_words[(!uber_words$words==""),]
grab_words <- grab_words[!(grab_words$words %in% stop_words$words),] 
uber_words <- uber_words[!(uber_words$words %in% stop_words$words),] 
grab_words <- grab_words[!is.na(grab_words$words),]
uber_words <- uber_words[!is.na(uber_words$words),]
detach("package:plyr", unload=TRUE)

# Match words with AFINN score to calc sentiment by words score
grab_words <- left_join(grab_words, afinn_list, by= c("words" = "word"))
uber_words <- left_join(uber_words, afinn_list, by= c("words" = "word")) 

# Calculate mean of each comments' scores
uber_afinn <- uber_words %>%
    dplyr::group_by(message_ID) %>%
    dplyr::summarise(mean = mean(score, na.rm=TRUE))
grab_afinn <- grab_words %>%
    dplyr::group_by(message_ID) %>%
    dplyr::summarise(mean = mean(score, na.rm=TRUE))

# Convert NAN to 0
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
uber_afinn[is.nan(uber_afinn)] <- 0; grab_afinn[is.nan(grab_afinn)] <- 0

# Independent samples two-tailed t-test
t.test(uber_afinn$mean, grab_afinn$mean)
```
Pausing here, I can see some green flags in this: the t-test returned a p-value of `p<0.0000`,
suggesting that the difference in sentiments between companies is not insignificant. 

``# With original dictionary and score=0 only: p-value = 1.8e-14``

```
# Independent samples two-tailed t-test
t.test(uber$SentimentR, grab$SentimentR)

# Join data back to frame
grab <- dplyr::left_join(grab, grab_afinn, by = "message_ID") 
uber <- dplyr::left_join(uber, uber_afinn, by = "message_ID")
grab[is.na(grab)] <- 0
uber[is.na(uber)] <- 0

# ---------------------------------------------------------------------------- #
# [plot 5 = boxplot for AFINN distribution]

# Plot bar charts
grab_boxplot <- grab %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, mean)) %>%
    mutate(Company = "Grab")
uber_boxplot <- uber %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, mean)) %>%
    mutate(Company = "Uber")

# Format decimal places, remove NAs
boxplots <- rbind(grab_boxplot, uber_boxplot)
boxplots$mean <- round(boxplots$mean, digits=2)
boxplots[is.na(boxplots)] <- 0

# Label month names
# Labels <- data.frame(num = c(3:9), Date = c("Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep"))
boxplots <- left_join(boxplots, Labels, by=c("Month_num" = "num")) 

# Boxplot per month by AFINN score
ggplot(data = boxplots, aes(x=reorder(factor(Date), Month_num), y=mean)) + 
    geom_boxplot(aes(fill=Company)) + 
    ggtitle("Distribution of Sentiment by Word", 
            subtitle = "(Sentiment analysis through AFINN dictionary, based on public data from Facebook)") +
        xlab(NULL) +
    scale_y_continuous(name = "AFINN score (neg=rude, pos=polite)",
                       breaks = seq(-5, 5, 1.0),
                       limits=c(-5, 5)) + 
    scale_fill_brewer(palette = "Accent") +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6),
          legend.title = element_text(face="italic",size=6),
          legend.position = "bottom", 
          legend.box = "horizontal",
          legend.box.just="left",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6),
          axis.title = element_text(size=9)) 

# ---------------------------------------------------------------------------- #
# Make smaller categories of terms

# Not useful: 
# vNegTerms <- c(afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4])
# negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 |  afinn_list$score==-1])
# posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1])
# vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4])

# Function to calculate number of words in each category within a sentence
library(plyr)
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
    final_scores <- matrix('', 0, 5)
    scores <- plyr::laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
        initial_sentence <- sentence
        
        #remove unnecessary characters and split up by word 
        sentence <- gsub('[[:punct:]]', '', sentence)
        sentence <- gsub('[[:cntrl:]]', '', sentence)
        sentence <- gsub('\\d+', '', sentence)
        sentence <- tolower(sentence)
        wordList <- str_split(sentence, '\\s+')
        words <- unlist(wordList)
        
        #build vector with matches between sentence and each category
        vPosMatches <- match(words, vPosTerms)
        posMatches <- match(words, posTerms)
        vNegMatches <- match(words, vNegTerms)
        negMatches <- match(words, negTerms)
        
        #sum up number of words in each category
        vPosMatches <- sum(!is.na(vPosMatches))
        posMatches <- sum(!is.na(posMatches))
        vNegMatches <- sum(!is.na(vNegMatches))
        negMatches <- sum(!is.na(negMatches))
        score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
        
        #add row to scores table
        newrow <- c(initial_sentence, score)
        final_scores <- rbind(final_scores, newrow)
        
        return(final_scores)
    }, vNegTerms, negTerms, posTerms, vPosTerms)
    
    return(scores)
}

# ---------------------------------------------------------------------------- #
# [Apply score counting function]

# Convert and apply function
grab_vec <- as.vector(grab$post_comment)
grab_scores <- as.data.frame(sentimentScore(grab_vec, vNegTerms, negTerms, posTerms, vPosTerms))

uber_vec <- as.vector(uber$post_comment)
uber_scores <- as.data.frame(sentimentScore(uber_vec, vNegTerms, negTerms, posTerms, vPosTerms))
detach("package:plyr", unload=TRUE)

# Rename
colnames(grab_scores) <- c('post_comment', 'vNegTerms', 'negTerms', 'posTerms', 'vPosTerms')
colnames(uber_scores) <- c('post_comment', 'vNegTerms', 'negTerms', 'posTerms', 'vPosTerms')

# Transform to suitable data type
grab_scores <- transform(grab_scores, 
                         vNegTerms = as.numeric(vNegTerms),
                         negTerms = as.numeric(negTerms),
                         posTerms = as.numeric(posTerms),
                         vPosTerms = as.numeric(vPosTerms)); str(grab_scores)
uber_scores <- transform(uber_scores, 
                         vNegTerms = as.numeric(vNegTerms),
                         negTerms = as.numeric(negTerms),
                         posTerms = as.numeric(posTerms),
                         vPosTerms = as.numeric(vPosTerms)); str(uber_scores)

# Recombine
grab <- cbind(grab, grab_scores$vNegTerms, grab_scores$negTerms, 
              grab_scores$posTerms, grab_scores$vPosTerms)
uber <- cbind(uber, uber_scores$vNegTerms, uber_scores$negTerms, 
              uber_scores$posTerms, uber_scores$vPosTerms)

# Format again
colnames(grab)[13:16] <- c("vNegTerms", "negTerms", "posTerms", "vPosTerms") 
colnames(uber)[13:16] <- c("vNegTerms", "negTerms", "posTerms", "vPosTerms") 

# Save data
write.csv(uber, file="uber_features.csv")
write.csv(grab, file="grab_features.csv")

# ---------------------------------------------------------------------------- #
# NB model

library(e1071)

# Pos Neg feature -- found to be not effective
# uber$net_score <- uber$vNegTerms*-5 + uber$negTerms*-2 + uber$posTerms*2 + uber$vPosTerms*5
# grab$net_score <- grab$vNegTerms*-5 + grab$negTerms*-2 + grab$posTerms*2 + grab$vPosTerms*5

# Extract posts with labels
uber_lab <- uber[!is.na(uber$sentiment),] #n=1,391
grab_lab <- grab[!is.na(grab$sentiment),] #n=1,237

# Build naive bayes model -- note that it tests and trains on labelled data
classifier_g <- naiveBayes(grab_lab[,c(11:12, 18)], grab_lab[,5])
classifier_u <- naiveBayes(uber_lab[,c(11:12, 17)], uber_lab[,5])

# Predict & print results
predicted_g <- predict(classifier_g, grab_lab)
predicted_u <- predict(classifier_u, uber_lab)

results_g <- table(predicted_g, grab_lab[,5], dnn=list('predicted','actual'))
results_u <- table(predicted_u, uber_lab[,5], dnn=list('predicted','actual'))

# Binomial test for confidence intervals
binom.test(results_g[1,1] + results_g[2,2]+ results_g[3,3], nrow(grab_lab), p=0.5)
binom.test(results_u[1,1] + results_u[2,2]+ results_u[3,3], nrow(uber_lab), p=0.5)

##################################
#           results - Grab [wordcount, mean AFINN, sentimentR score] 
# actual
# predicted  negative neutral positive
# negative       57      17        8
# neutral       126     659      137
# positive       24      40      169
# 95 percent confidence interval: 0.6894006 0.7404522
# sample estimates:
#     probability of success 
# 0.7154406 
##################################
#           results - Uber [wordcount, mean AFINN, sentimentR score] 
# actual
# predicted  negative neutral positive
# negative      192      53       14
# neutral       168     666      101
# positive       16      75      106
# 95 percent confidence interval: 0.6680392 0.7171959
# sample estimates:
#     probability of success 
# 0.6930266 
##################################

# ---------------------------------------------------------------------------- #
# SentimentR package

library(sentimentr)
mytext <- c(
    'do you like it?  But I hate really bad dogs',
    'I am the best friend.',
    'Do you really like it?  I\'m not a fan'
)
mytext <- get_sentences(mytext)
sentiment(mytext)

# Single sentences
grabtext <- get_sentences(grab$post_comment)
grab_sr <- sentiment(grabtext)
ubertext <- get_sentences(uber$post_comment)
uber_sr <- sentiment(ubertext)

# Score by cell 
grab_sr <- grab_sr %>%
    dplyr::group_by(element_id) %>%
    dplyr::summarise(mean(sentiment))
colnames(grab_sr) <- c("post", "sentimentr")
uber_sr <- uber_sr %>%
    dplyr::group_by(element_id) %>%
    dplyr::summarise(mean(sentiment))
colnames(uber_sr) <- c("post", "sentimentr")

# Bind back to main frame
grab <- cbind(grab, grab_sr$sentimentr)
uber <- cbind(uber, uber_sr$sentimentr)

# count score
# neutral <- nrow(grab[grab$`out$sentimentr`==0,]) #1123 (35%)
# positive <- nrow(grab[grab$`out$sentimentr`>0,]) #1394 (44%)
# negative <- nrow(grab[grab$`out$sentimentr`<0,]) #665 (21%)
colnames(grab)[18] <- "SentimentR"
colnames(uber)[18] <- "SentimentR"

# Test if score is correlated to AFINN dictionary
x <- grab[,12] # Mean score for AFINN
y <- grab[,18] # SentimentR score
cor(x, y, use="complete.obs", method="pearson") # r = 0.411

a <- uber[,12] # Mean score for AFINN
b <- uber[,18] # SentimentR score
cor(a, b, use="complete.obs", method="pearson") # r = 0.410

# ---------------------------------------------------------------------------- #
# [ plot 6 - boxplot for SentimentR by sentence]

# Plot bar charts for SentimentR
grab_boxplot2 <- grab %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, SentimentR)) %>%
    mutate(Company = "Grab")
uber_boxplot2 <- uber %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, SentimentR)) %>%
    mutate(Company = "Uber")

# Format decimal places, remove NAs
boxplots2 <- rbind(grab_boxplot2, uber_boxplot2)
boxplots2$SentimentR <- round(boxplots2$SentimentR, digits=2)
boxplots2[is.na(boxplots2)] <- 0

# Label month names
# Labels <- data.frame(num = c(3:9), Date = c("Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep"))
boxplots2 <- left_join(boxplots2, Labels, by=c("Month_num" = "num")) 

# Boxplot per month by score
ggplot(data = boxplots2, aes(x=reorder(factor(Date), Month_num), y=SentimentR)) + 
    geom_boxplot(aes(fill=Company)) + 
    ggtitle("Distribution of Sentiment by Sentence", 
            subtitle = "(Sentiment analysis through SentimentR pkg, based on public data from Facebook)") +
    xlab(NULL) +
    scale_y_continuous(name = "SR score (neg=rude, pos=polite)",
                       breaks = seq(-1, 1, 0.2),
                       limits=c(-1, 1)) + 
    scale_fill_brewer(palette = "Pastel2") +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6),
          legend.title = element_text(face="italic",size=6),
          legend.position = "bottom", 
          legend.box = "horizontal",
          legend.box.just="left",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6),
          axis.title = element_text(size=9)) 

# ---------------------------------------------------------------------------- #
# Apply labels to unlabelled data

# Load unlabelled data 
unlab_u <- dplyr::filter(uber, is.na(sentiment)) # n=1768
unlab_g <- dplyr::filter(grab, is.na(sentiment)) # n=1945

# Predict sentiments using nb model 
predicted_unlab_g <- predict(classifier_g, unlab_g)
predicted_unlab_u <- predict(classifier_u, unlab_u)

# Merge prediction sentiments into one table
unlab_g <- cbind(unlab_g, as.data.frame(predicted_unlab_g))
unlab_u <- cbind(unlab_u, as.data.frame(predicted_unlab_u))

# Check distribution
t <- unlab_u %>%
    dplyr::count(predicted_unlab_u)
t <- unlab_g %>%
    dplyr::count(predicted_unlab_g)

# A tibble: 3 x 2
# predicted_unlab_UBER     n
# <fctr> <int>
# 1          negative   281
# 2           neutral  1162
# 3          positive   325

# A tibble: 3 x 2
# predicted_unlab_GRAB     n
# <fctr> <int>
# 1          negative    60
# 2           neutral  1728
# 3          positive   157

# Merge prediction sentiments back with main dataset
unlab_g <- subset(unlab_g, select = -c(sentiment))
colnames(unlab_g)[18] <- "sentiment"
unlab_g <- mutate(unlab_g, Type = "machine")

unlab_u <- subset(unlab_u, select = -c(sentiment))
colnames(unlab_u)[18] <- "sentiment"
unlab_u <- mutate(unlab_u, Type = "machine")

grab <- mutate(grab, Type="manual"); uber <- mutate(uber, Type="manual")

# Merge
grab_final <- rbind(grab_lab, unlab_g)
uber_final <- rbind(uber_lab, unlab_u)

# Save data
write.csv(uber_final, file="uber_final.csv")
write.csv(grab_final, file="grab_final.csv")

# ---------------------------------------------------------------------------- #
# [plot 8 = plot of most frequent words per company x sentiment] 

# Load from save
uber <- read.csv(file.choose(), header=T, na.strings=c("", " ","NA")); View(uber)
grab <- read.csv(file.choose(), header=T, na.strings=c("", " ","NA")); View(grab)
options(stringsAsFactors = FALSE)

# Combine datasets
uber$Company <- "Uber"
grab$Company <- "Grab"
combined <- rbind(uber, grab)

# Group by date
plot1 <- combined %>%
    group_by(DateSG, Company) %>%
    summarise(SentimentR = mean(SentimentR),
              AFINN = mean(mean),
              num_posts = n())

# Basic plot
plot1 <- transform(plot1, DateSG = as.Date(DateSG),
                   Company = as.factor(Company))

# Find standard dev of columns for range of plot
### for sentence
apply(as.matrix(grab$SentimentR), 2, mean) # 0.113
apply(as.matrix(uber$SentimentR), 2, mean) # 0.0889
apply(as.matrix(grab$SentimentR), 2, sd) # 0.358
apply(as.matrix(uber$SentimentR), 2, sd) # 0.373

### for word
apply(as.matrix(uber$mean), 2, mean) # 0.259
apply(as.matrix(grab$mean), 2, mean) # 0.481
apply(as.matrix(uber$mean), 2, sd) # 1.22
apply(as.matrix(grab$mean), 2, sd) # 1.18

# Subset plots within 1SD
plot2 <- plot1[plot1$SentimentR<0.6&plot1$SentimentR>-0.2,]

# Melt and reshape to double facet plot
SR <- subset(plot1, select=-c(AFINN))
SR$package <- "By sentence"
AFINN <- subset(plot1, select=-c(SentimentR))
AFINN$package <- "By word"
colnames(SR)[3] <- "Score";colnames(AFINN)[3] <- "Score"
plot3 <- rbind(SR, AFINN)
plot4 <- plot3[plot3$Score<=1&plot3$Score>=-0.5,]

# plot together
ggplot(plot4, aes(x = DateSG, y = Score, size = num_posts, 
                  fill = Score)) +
    geom_point(shape=21) + 
    labs(x=NULL, y="Tone (Negative to Positive)", 
         size = "Total Comments per Day", 
         fill = "Sentiment") + 
    ggtitle("Sentiment toward Grab & Uber in Singapore", 
            subtitle = "(based on public data from Facebook)")+
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6),
          # text = element_text(family = "Tahoma"),
          legend.title = element_text(face="italic",size=6),
          legend.position = "bottom", 
          legend.box = "horizontal",
          legend.box.just="left",
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6),
          strip.text=element_text(face="bold", size=9), # facet 
          axis.title = element_text(size=9)) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(breaks=NULL, #limits = c(-0.5, 1), # breaks = seq(-0.2, 0.6, 0.2)
                    position = "left") +
    scale_size(range = c(1, 15)) + 
    scale_fill_continuous(low = "firebrick1", high = "green") + 
    facet_grid(Company ~ package) 
# ---------------------------------------------------------------------------- #
# [plot 9 = Weighted scatterplot of sentiment X company X time]

# Merge both datasets for graph
combined1 <- transform(combined, post_comment = as.character(post_comment),
                       message_ID = as.character(message_ID), 
                       sentiment = as.character(sentiment),
                       DatetimeSG = as.Date(DatetimeSG))

# Split comments to single words per cell, count frequency of words
c_indv <- strsplit(combined1$post_comment, split = " ") 
c_words <- data.frame(message_ID = rep(combined1$message_ID, sapply(c_indv, length)), 
                         words = unlist(c_indv)) # n=94,856
c_words1 <- left_join(c_words, combined1, by="message_ID") %>%
    select(message_ID, words, sentiment, DatetimeSG, mean, SentimentR, 
           Type, Company)

# Keep only pos, neg posts
c_words2 <- c_words1[c_words1$sentiment!="neutral",]
c_words2 <- c_words2[!is.na(Company)&&!is.na(sentiment),]

# Remove garbage stopwords, punctuation, empty rows, stopwords again.
c_words2 <- c_words2[!(c_words2$words %in% stop_words$words),] 
c_words2 <- transform(c_words2, words = (sub("^([[:alpha:]]*).*", "\\1", c_words2$words)))
c_words2 <- c_words2[(!c_words2$words==""),] 
c_words2 <- c_words2[!(c_words2$words %in% stop_words$words),]
c_words2$words <- toupper(c_words2$words)

# Final group by words
c_words3 <- c_words2 %>%
    dplyr::group_by(words, sentiment, Company) %>%
    dplyr::summarise(num_words =n()) %>%
    dplyr::arrange(desc(num_words))

# Select top words for every combination
library(sqldf)
c_ubpos <- sqldf('SELECT words, num_words, sentiment, Company
                 FROM c_words3
                 WHERE company = "Uber" AND sentiment = "positive"
                 AND words NOT IN ("THE", "UBER", "CODE", "ALWAYS", "EVEN", "HTTPS", "I")
                 ORDER BY num_words DESC 
                 LIMIT 15')
c_ubneg <- sqldf('SELECT words, num_words, sentiment, Company
                 FROM c_words3
                 WHERE company = "Uber" AND sentiment = "negative"
                 AND words NOT IN ("THE", "UBER", "CODE", "ONE", "EVEN", "AND", "LIKE", "I")
                 ORDER BY num_words DESC 
                 LIMIT 15')
c_grpos <- sqldf('SELECT words, num_words, sentiment, Company
                 FROM c_words3
                 WHERE company = "Grab" AND sentiment = "positive"
                 AND words NOT IN ("THE", "GRAB", "CODE", "ONE", "EVEN", "IT", "US", "I")
                 ORDER BY num_words DESC 
                 LIMIT 15')
c_grneg <- sqldf('SELECT words, num_words, sentiment, Company
                 FROM c_words3
                 WHERE company = "Grab" AND sentiment = "negative"
                 AND words NOT IN ("THE", "GRAB", "CODE", "ONE", "EVEN", "THIS", "I", "GOT", "NOW", "N", "LIKE",  "TAKE", "ALSO")
                 ORDER BY num_words DESC 
                 LIMIT 15')
detach("package:sqldf", unload=TRUE)

# Make plots for each: pos/neg/uber/grab
grneg <- ggplot(c_grneg, aes(x=reorder(words, num_words), y=num_words, fill=words)) + 
    geom_bar(stat="identity") +
    xlab(NULL) + ylab("Complaints, Grab") +
    scale_y_continuous(breaks=seq(0,400,50), limits=c(0,360)) +
    theme(legend.position = "none",
          axis.title = element_text(size=15, face="bold")) + coord_flip()+
    scale_fill_hue(l=40) 

grpos <- ggplot(c_grpos, aes(x=reorder(words, num_words), y=num_words, fill=words)) + 
    geom_bar(stat="identity") +
    xlab(NULL) + ylab("Positive comments, Grab") +
    scale_y_continuous(breaks=seq(0,400,50), limits=c(0,360)) +
    theme(legend.position = "none",
          axis.title = element_text(size=15, face="bold")) + coord_flip() +
    scale_fill_hue(c=75, l=75)

ubpos <- ggplot(c_ubpos, aes(x=reorder(words, num_words), y=num_words, fill=words)) + 
    geom_bar(stat="identity") +
    xlab(NULL) + ylab("Positive comments, Uber") +
    scale_y_continuous(breaks=seq(0,400,50), limits=c(0,360)) +
    theme(legend.position = "none",
          axis.title = element_text(size=15, face="bold")) + coord_flip() +
    scale_fill_hue(c=75, l=75)

ubneg <- ggplot(c_ubneg, aes(x=reorder(words, num_words), y=num_words, fill=words)) + 
    geom_bar(stat="identity") +
    xlab(NULL) + ylab("Complaints, Uber") +
    scale_y_continuous(breaks=seq(0,400,50), limits=c(0,360)) +
    theme(legend.position = "none",
          axis.title = element_text(size=15, face="bold")) + coord_flip() +
    scale_fill_hue(l=40)

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
multiplot(grpos, ubpos, grneg, ubneg, cols=2)

# Select only top words, non-NA words
# c_words4 <- c_words3 %>%
#     group_by(sentiment, Company) %>%
#     top_n(n = 5, wt = num_words)
# c_words4 <- c_words4[!is.na(c_words4$sentiment),]

# Old plot
# grab_w <- grab_words %>% 
#     dplyr::group_by(words) %>% 
#     dplyr::summarise(n = n()) %>%
#     arrange(desc(n))

# Plot top words per brand
# uber_tw <- uber_w[1:9,]
# grab_tw <- grab_w[1:9,]
# ggplot(uber_tw, aes(x=reorder(words, n), y=n, fill=factor(n))) + geom_bar(stat="identity") +
#     ylab("Popularity of words, Uber") + xlab("Words") + 
#     theme(legend.position = "none") + coord_flip() + 
#     scale_fill_brewer(palette="Greys")
# ggplot(grab_tw, aes(x=reorder(words, n), y=n, fill=factor(n))) + geom_bar(stat="identity") +
#     ylab("Popularity of words, Grab") + xlab("Words") + 
#     theme(legend.position = "none") + coord_flip() + 
#     scale_fill_brewer(palette="Greens")


# ---------------------------------------------------------------------------- #
# Depreciated: Only able to produce two-level outputs

library(RTextTools)

# Shuffle order of rows to distribute positive, negative posts thru dataset
labelled_data <- labelled_data[sample(nrow(labelled_data)),]

# New dataframe of columns to be used
labelled_data1 <- subset(labelled_data, select = c(1:5, 8, 10))
View(labelled_data1)

# Build DTM for other models
matrix_uber <- create_matrix(labelled_data1[,1:7], language="english",
                             removeStopwords = FALSE, removeNumbers = TRUE,
                             stemWords = FALSE)
mat_uber <- as.matrix(matrix_uber)

# Build container to specify response input var, test set & training set
container_uber <- create_container(matrix_uber, 
                                   as.numeric(as.factor(labelled_data[,9])),
                                   trainSize=1:1000, testSize=1001:1519, # 75%
                                   virgin=FALSE)


# Train model with multiple ML Algos
models_all <- train_models(container_uber, 
                           algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

# Classify training set
results_all <- classify_models(container_uber, models_all)

# accuracy table
table(as.numeric(labelled_data[1001:1519, 9]), results_all[,"FORESTS_LABEL"])
table(as.numeric(as.factor(labelled_data[1001:1519, 9])), results_all[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(labelled_data[1:666, 9])), results_all[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(labelled_data[1:666, 9])), results_all[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(labelled_data[1:666, 9])), results_all[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(labelled_data[1:666, 9])), results_all[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(labelled_data[1:666, 9])), results_all[,"SVM_LABEL"])

# To summarize the results (especially the validity) in a formal way:
analytics <- create_analytics(container_uber, results_all)
summary(analytics)
head(analytics@document_summary)
analytics@ensemble_summary

# Cross validate with out of sample estimates of k-folds
N=5
set.seed(2014)
cross_validate(container_uber,N,"MAXENT")
cross_validate(container_uber,N,"TREE")
cross_validate(container_uber,N,"SVM")
cross_validate(container_uber,N,"RF")

# ---------------------------------------------------------------------------- #
# Depreciated: Maxent does not work for multilevel modelling; measures presence-only data

# LOAD LIBRARY
library(maxent)

# READ THE DATA, PREPARE THE CORPUS, and CREATE THE MATRIX
data <- read.csv(system.file("data/NYTimes.csv.gz",package="maxent"))
corpus <- Corpus(VectorSource(data[1:150,]))
matrix <- DocumentTermMatrix(corpus)

# TRAIN/PREDICT USING SPARSEM REPRESENTATION
sparse <- as.compressed.matrix(matrix)
model <- maxent(sparse[1:100,],data$Topic.Code[1:100,])
results <- predict(model,sparse[101:150,])

# PRINT RESULTS
results <- cbind(as.data.frame(results), True = data[101:150,5])
correct_preds <- nrow(results[results$labels==results$True,])
num_preds <- nrow(results)
accuracy <- paste0("Accuracy of Max_Ent machine with n=", num_preds,
                   " data points: ", (correct_preds/num_preds)*100, "%");accuracy

