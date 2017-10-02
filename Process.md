# A Tale of Two Taxi Giants

### Background

Grab and Uber are competing pretty darn hard for the local market right now. After Susan Fowler's post and 
Travis' controversies starting Feb 2017, I noticed some friends around me grew polarised towards one or the other. 
I wondered if the company's negative press image would affect the way other customers felt towards 
the brand as well. 

This post outlines the process I used to mine public data using R and the Facebook Graph API for Uber and Grab. 
I then built a `naive bayes` sentiment classifier to label the data and visualised the results below. All in all,
I'm looking to see if there is a difference in the way people talk about/to Grab and Uber. 

Quick outline of workflow: 

* Set up Facebook API 
* Clean data
* Build features for machine learning
* Set up algorithms to label the rest
* Visualise results

### Mining Facebook Data with R 

I chose Facebook as it has the platform with highest social activity for the brands, compared to Twitter or Instagram.
Setting up the API took less than 5 minutes. I made an app, used the `RFacebook` package found 
[here](https://cran.r-project.org/web/packages/Rfacebook/Rfacebook.pdf) and copied my authentication key from
the developer's settings page.

```
library(RFacebook)

# Get your own key from facebook
app_id <- "ABCDEFGHIJK...Z"
app_secret <- "ABCDEFGHIJK...Z"

# Authenticate
fbOAuth(app_id, app_secret, extended_permissions = FALSE,
legacy_permissions = FALSE, scope = NULL)
```
From here, I decided to mine all public comments for the last 6 months, from 01 March 2017 to 31 August 2017. I kept only the *parent* comment in each thread as child comments tend to be the respective customer support staff replying to users' needs. This returns `3182` comments for Grab and `3195` for Uber. 

```
# Call Graph API for posts
# Note: I'm using API v.2.10 but you can leave it as NULL if errors occur
uber <- callAPI(url, token, api = 2.10)
grab <- callAPI(url, token, api = 2.10)

# Data retrieved as JSON, convert to dataframe for working
uber <- as.data.frame(uber)
grab <- as.data.frame(grab)

# A tibble:
[preview data]
```

### Cleaning the data

First off, I cleaned up the date column. Data was stored in USA time (UTC -8:00) so I need to convert it to Singapore Time (SGT +8:00). I wrote a function `format_FBtime` to do this so I could reuse it on subsequent analyses. This simple function requires the date column to be in string format, and titled `creation_time` (which it is, by default). It returns 2 columns to work with: (1) Original date time (USA) (2) Converted date time (SG).

```
library(dplyr)
library(lubridate)

format_FBtime <- function(df){ 
    
    # What this function needs:
    # 1. Datetime column in CHAR format titled "creation_time"
    
    # What this function returns: 
    # 1. "DatetimeSF" in DATE format (UTC -8:00) as YYYY-MM-DD HH:MM:SS 
    # 2. "DatetimeSG" in DATE format (SGT +8:00) as YYYY-MM-DD HH:MM:SS 
    # 3. Drops unformatted column "creation_time"
    
    df <- mutate(df, Date = paste(year(df$creation_time), month(df$creation_time), day(df$creation_time), sep="-"))
    df <- mutate(df, Time=substr(as.character(df$creation_time),12, 19))
    df <- mutate(df, DatetimeSF = paste(df$Date, df$Time))
    df$DatetimeSF <- as.POSIXct(strptime(df$DatetimeSF, "%Y-%m-%d %H:%M:%S", tz="Pacific/Easter"))
    df <- mutate(df, DatetimeSG = with_tz(df$DatetimeSF, "Singapore"))
    
    # Remove any unwanted columns here:
    df <- subset(df, select = -c(Date, Time, creation_time))
    return(df)
}

grab <- format_FBtime(grab); str(grab)
uber <- format_FBtime(uber); str(uber)
```
Next, I'll do a simple visualisation of the data to check the number of posts per day for both companies, and see if there are any trends in comments by `month`, `day of week` or `hour` the comment was posted. 
```
# Plot 1: Group by number of posts per company per day
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
              
### EDIT THIS TO CHECK FOR NUM POSTS PER WEEKDAY, HOUR.
```
Plot this out: 
```
library(ggplot2)
library(ggthemes)
library(grid)

# [ Plot 1 = num posts per day as timeseries line graph]
ggplot(timeseries, aes(as.Date(DateSG))) + 
    geom_line(aes(y = Uber, color="Uber"), size= 0.75, colour="black") + 
    geom_line(aes(y = Grab, color="Grab"), size= 0.75, colour="forest green") + 
    labs(colour="Company", x=NULL, y="Number of Comments") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    ggtitle("Total Comments per Day, Grab & Uber", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"), 
          legend.text = element_text(face="bold",size = 6), legend.position = "bottom",
          axis.title = element_text(size=9))
```
GRAPH HERE
```
# [plot 2 = num posts per day per company as back-to-back histogram]
colnames(grab1)[2] = "Num_comments"; colnames(uber1)[2] = "Num_comments"
uber1 <- mutate(uber1, Company='Uber'); grab1 <- mutate(grab1, Company='Grab', Num_comments=Num_comments*-1)
timeseries2 <- rbind(uber1, grab1) %>%
    transform(DateSG = as.Date(DateSG)) 

# Uber (right side)
plot_uber <- ggplot(timeseries2, aes(x=DateSG)) + 
    geom_bar(data = subset(timeseries2, Company == 'Uber'), aes(y=Num_comments, fill = Num_comments), 
    colour="black", stat = "identity") + scale_x_date(date_breaks = "1 month", date_labels = "%b")

# Grab (left side)
print(plot_uber + geom_bar(data = subset(timeseries2, Company == 'Grab'), 
             aes(y= Num_comments, fill = Num_comments), colour="dark green", stat = 'identity') +
    scale_y_continuous(breaks= seq(-75, 75, 25), limits = c(-75, 75), position = "right", name = "Grab  - - -  Uber") + 
    xlab("") + scale_fill_gradient2(low ="forest green", mid = "white", high = "black", midpoint = 0, space = "rgb") +
        theme(legend.position = "none") + coord_flip())
```
GRAPH HERE

Doesn't seem to be any major differences in number of comments per month, although I note that there's a slight increase in comments in ______ month due to ______ hosting X promos. That aside, there isn't any particular surge in number of posts by weekday or hour either. 

Finally, I make a simple column to count the number of words per comment. Just in case, y'know, angry comments tend to be longer or something. 
```
# Remove non-ASCII characters in comments, usernames
uber$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", uber$post_comment)
uber$post_username <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", uber$post_username)
grab$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", grab$post_comment)
grab$post_username <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", grab$post_username)

# Count words
uber$wordcount <- str_count(uber$post_comment, '\\s+')+1
grab$wordcount <- str_count(grab$post_comment, '\\s+')+1

```
I can't think of any (publicly available) features that might be correlated to the sentiment of comments for now, so I'll move on to sentiment analysis.

### Sentiment Analysis

From the dataset, I manually tagged about 20% of the comments (1,200) with labels as `positive`, `neutral`, `negative` based on their tone. Although most comments were short (<2 sentences), this took about 4 hours. I noticed that most comments were complaints or queries, so I tried to ignore their content (mostly neutral or negative) and focused on their *tone* (which could still be positive - ie. a confused but still polite customer). 

Also, to get a baseline, I ran the pure comments data straight through the `naive bayes` machine from the `e1071` package without any support feature vectors. I'm aware that training and testing on the same dataset tends to lead to overfitting, but in this situation I just wanted to get a baseline estimate of how well the classifier would perform. 

```
library(e1071)
# Extract only labelled posts
uber_lab <- uber[!is.na(uber$sentiment),] #n=1,391
grab_lab <- grab[!is.na(grab$sentiment),] #n=1,237

# Build naive bayes model -- note that it tests and trains on same set of data
classifier_g <- naiveBayes(grab_lab[,c(11:12, 18)], grab_lab[,5])
classifier_u <- naiveBayes(uber_lab[,c(11:12, 17)], uber_lab[,5])

# Predict & print results in table
predicted_g <- predict(classifier_g, grab_lab)
predicted_u <- predict(classifier_u, uber_lab)
results_g <- table(predicted_g, grab_lab[,5], dnn=list('predicted','actual'))
results_u <- table(predicted_u, uber_lab[,5], dnn=list('predicted','actual'))

# Binomial test for confidence intervals
binom.test(results_g[1,1] + results_g[2,2]+ results_g[3,3], nrow(grab_lab), p=0.5)
binom.test(results_u[1,1] + results_u[2,2]+ results_u[3,3], nrow(uber_lab), p=0.5)
```
The accuracy was really low - `41%`. 
```
RESULTS OF JUST NB WITHOUT FEATURES:
```
Moving forward, I started building things to help the machine better detect sentiment. I came across [this post](here) by XXXXXXX detailing three sentiment dictionaries commonly used in classification: The AFINN, NRC and BING lists. I chose to use the AFINN dictionary as it dealt directly with sentiment rather than emotion, which is the focus of this project. 

> The AFINN dictionary... (explanation here)

Initially, I replicated Bromberg's method, but those features did not significantly increase the accuracy of prediction so I'll leave them out of my discussion. You can find the adapted workings in my [full R code](link) hosted on my github. The results looked like this:

```
RESULTS WITH AFINN BROMBERG
```
Following, I modified the AFINN dictionary to include more local colour, and made a single feature to calculate the mean AFINN score of each comment rather than take an aggregated sum of scores. 

```
### Load AFINN dictionary

# SetWD to home dir; stored files MUST be in [SENTIMENT_ANALYSIS] folder in there
setwd("~/sentiment_analysis")
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

# Check if word exists & return score: (reuse this)
afinn_list$score[afinn_list$word=="YOURWORDHERE"]

# Modify scores (for words already in AFINN)
afinn_list$score[afinn_list$word %in% c("best", "nice", "appreciation")] <- 4
afinn_list$score[afinn_list$word %in% c("charges", "charged", "pay", "like", "joke", "improvement")] <- 0
afinn_list$score[afinn_list$word %in% c("irresponsible", "whatever")] <- -1
afinn_list$score[afinn_list$word %in% c("cheat", "cheated", "frustrated", "scam", "pathetic", "hopeless",
                                        "useless", "dishonest", "tricked", "waste", "gimmick", "liar", "lied")] <- -4

# Add scores (for words not in AFINN)
pos4 <- data.frame(word = c("bagus", "yay", ":)", "kindly", "^^", "yay", "swee", "awesome", "polite", "thnks", "thnk", "thx", "thankyou",
                            "tq", "ty", "professional", "pls"), score = 4)
pos2 <- data.frame(word = c("jiayou", "assist", "amin", "amen", "supper", "dating", "arigato", "bro"), score = 2)
pos1 <- data.frame(word = c("hi", "dear", "hello"), score = 1)
neg1 <- data.frame(word = c("silly", "dafaq", "dafuq", "cringe", "picky"), score = -1)
neg2 <- data.frame(word = c("jialat", "waited", "waiting", "rubbish", "lousy", "siao", "??", "-_-", "-.-", "slap", "slapped", "sicko",
                            "lying", "lies", "wtf", "wts"), score = -2)
neg4 <- data.frame(word = c("freaking", "knn", "ccb", "fk", "fking", "moronic"), score = -4)

# Merge changes with main AFINN list
afinn_list <- rbind(afinn_list, pos4, pos2, pos1, neg1, neg2, neg4)
```
*Note: I don't relaly have an objective basis for why some swear words get a higher rating
than others, but I classified them by how angry my mother would be when I say them.*

Count sentiment per comment based on net word score:

```
library(plyr)

# Split comments to single words per cell
grab_indv <- strsplit(grab$post_comment, split = " ") ; uber_indv <- strsplit(uber$post_comment, split = " ") 
uber_words <- data.frame(message_ID = rep(uber_words$message_ID, sapply(uber_indv, length)), words = unlist(uber_indv)) 
grab_words <- data.frame(message_ID = rep(grab_words$message_ID, sapply(grab_indv, length)), words = unlist(grab_indv)) 
grab_words$words <- tolower(grab_words$words); uber_words$words <- tolower(uber_words$words)

# Customise stopwords
library(tm)
stop_words <- as.data.frame(stopwords("en")); more_stopwords <- as.data.frame(c("uber", "grab")) 
names(stop_words)[1] <- "words"; names(more_stopwords)[1] <- "words"
stop_words <- rbind(stop_words, more_stopwords)
detach("package:tm", unload=TRUE)

# Remove stopwords, punctuation, empty rows, NA, stopwords again
uber_words <- uber_words[!(uber_words$words %in% stop_words$words),]
uber_words <- transform(uber_words, words = (sub("^([[:alpha:]]*).*", "\\1", uber_words$words)))
uber_words <- uber_words[(!uber_words$words==""),]; uber_words <- uber_words[!is.na(uber_words$words),]
uber_words <- uber_words[!(uber_words$words %in% stop_words$words),] 

grab_words <- grab_words[!(grab_words$words %in% stop_words$words),]
grab_words <- transform(grab_words, words = (sub("^([[:alpha:]]*).*", "\\1", grab_words$words)))
grab_words <- grab_words[(!grab_words$words==""),]; grab_words <- grab_words[!is.na(grab_words$words),]
grab_words <- grab_words[!(grab_words$words %in% stop_words$words),] 
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
```
From here, I tested to see if there's any significance in scores:
```
# Independent samples two-tailed t-test
t.test(uber_afinn$mean, grab_afinn$mean)

# RESULTS
```
Turns out there's a significant difference in tone of comments towards both companies, with 
Grab's customers being a fraction friendlier. 

However, it may be that this dictionary merely picks up valenced lexicon without care for negators or amplifiers within a sentence. This means it wouldn't detect sarcasm or awkward double negatives very well. 

To check, I'll run another package on it - the `sentimentR` package, that ... (explanation). 

```
library(SentimentR)

# Separate out single sentences 
grabtext <- get_sentences(grab$post_comment)
ubertext <- get_sentences(uber$post_comment)

# Pass them one sentence at a time to function
grab_sr <- sentiment(grabtext)
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
grab <- cbind(grab, grab_sr$sentimentr); uber <- cbind(uber, uber_sr$sentimentr)
colnames(grab)[18] <- "SentimentR"
colnames(uber)[18] <- "SentimentR"

```
Results look like this:

```
# Independent samples two-tailed t-test
t.test(uber$SentimentR, grab$SentimentR)

# Test if score is correlated to AFINN dictionary
x <- grab[,12] # Mean score for AFINN
y <- grab[,18] # SentimentR score
cor(x, y, use="complete.obs", method="pearson") # r = 0.411

a <- uber[,12] # Mean score for AFINN
b <- uber[,18] # SentimentR score
cor(a, b, use="complete.obs", method="pearson") # r = 0.410

```
Sweet. There's a difference here too. It's interesting to note that they're not too strongly correlated with each other, in spite of measuring the same broad concept. 

I'll plot the differences in scores of both companies with a graph:

```
# [plot 5 = boxplot for AFINN distribution]
grab_boxplot <- grab %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, AFINN)) %>%
    mutate(Company = "Grab")
uber_boxplot <- uber %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, AFINN)) %>%
    mutate(Company = "Uber")

# Format decimal places, set NA to zero to maintain power
boxplots <- rbind(grab_boxplot, uber_boxplot)
boxplots$mean <- round(boxplots$mean, digits=2)
boxplots[is.na(boxplots)] <- 0

# Create nice labels for month names
# Labels <- data.frame(num = c(3:9), Date = c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
boxplots <- left_join(boxplots, Labels, by=c("Month_num" = "num")) 

# Boxplot per month by AFINN score
ggplot(data = boxplots, aes(x=reorder(factor(Date), Month_num), y=mean)) + 
    geom_boxplot(aes(fill=Company)) + xlab(NULL) + 
    scale_y_continuous(name = "AFINN score (neg=rude, pos=polite)", breaks = seq(-5, 5, 1.0), limits=c(-5, 5)) + 
    ggtitle("Distribution of Sentiment by Word", subtitle = "(Sentiment analysis through AFINN dictionary, 
    based on public data from Facebook)") +
    scale_fill_brewer(palette = "Accent") +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6),
          legend.title = element_text(face="italic",size=6), legend.position = "bottom", legend.box = "horizontal",
          legend.box.just="left", legend.key.size = unit(0.5, "cm"), legend.text = element_text(face="bold",size = 6),
          axis.title = element_text(size=9)) 
          
### Repeat the exact same steps for making SENTIMENTR plot
```
GRAPH HERE AFINN
GRAPH HERE SENTIMENTR

This is getting pretty exciting. Moving forward, I'll keep these features for use in the machine. I use almost the exact same syntax as before so I won't reprint it here. The results look like this:  
```
FINAL NB SCORES
```
I'll apply this same algorithm to the remaining unlabelled data. A note - this classifier isn't perfect (~70%). However, one good thing remains: it skews classification the same way for both datasets. Meaning, if it tends to classify Uber's posts as negative, it would classify Grab's posts as negative by the same degree too.
```
# Load remaining unlabelled data 
unlab_u <- dplyr::filter(uber, is.na(sentiment)); unlab_g <- dplyr::filter(grab, is.na(sentiment)) 

# Predict sentiments using nb model 
predicted_unlab_g <- predict(classifier_g, unlab_g); predicted_unlab_u <- predict(classifier_u, unlab_u)

# Attach prediction labels
unlab_g <- cbind(unlab_g, as.data.frame(predicted_unlab_g))
unlab_u <- cbind(unlab_u, as.data.frame(predicted_unlab_u))

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
```
Let's check the distribution of predictions. 
```
t <- unlab_u %>%
    dplyr::count(predicted_unlab_u)

# A tibble: 3 x 2
# predicted_unlab_UBER     n
# <fctr> <int>
# 1          negative   281
# 2           neutral  1162
# 3          positive   325

t <- unlab_g %>%
    dplyr::count(predicted_unlab_g)

# A tibble: 3 x 2
# predicted_unlab_GRAB     n
# <fctr> <int>
# 1          negative    60
# 2           neutral  1728
# 3          positive   157
```
I guess they both skew towards `neutral` when in doubt. 

With these results, I'll make two final graphs of words. 
```
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
