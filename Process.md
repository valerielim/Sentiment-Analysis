# A Tale of Two Taxi Giants

### Background

Grab and Uber are competing pretty hard for the local market right now. After 
Travis' controversies started in Feb 2017, I noticed some friends grew polarised towards one company or the other. 
I wondered if the company's negative press image would affect the way other customers felt towards the brand as well. 

This post outlines the process I used to mine public data using R and the Facebook Graph API for Uber and Grab. 
I then built a `naive bayes` sentiment classifier to label the data and visualised the results below. All in all,
I'm looking to see if there is a difference in the way people talk and feel toward Grab and Uber. 

**Quick outline of workflow:**

* Collect data from Facebook API 
* Clean data
* Build features for machine learning
* Label some data for training & testing
* Evaluate & modify features if necessary
* Deploy(?) final model on unlabelled data
* Visualise results

### Mining Data from Facebook API with R 

I chose to mine data from Facebook as it is the platform with the highest social activity for both brands, compared to Twitter or Instagram.

Setting up the API authentication took less than 5 minutes. From my Facebook account, I made a simple developer's app, and copied my authentication key from the app's [settings](https://developers.facebook.com/tools/accesstoken/). I then set up the API authentication in R below. Detailed Facebook API set-up instructions can be found [in this tutorial](http://thinktostart.com/analyzing-facebook-with-r/) or on the [Rfacebook documentation](https://cran.r-project.org/web/packages/Rfacebook/Rfacebook.pdf).

```R 
library(RFacebook)

# Get your own key from facebook
app_id <- "XXX"
app_secret <- "XXX"

# Authenticate
fb_oauth <- fbOAuth(app_id, app_secret, extended_permissions = FALSE,
legacy_permissions = FALSE, scope = NULL)
save(fb_oauth, file="fb_oauth"); load("fb_oauth")
```
`RFacebook` allows mining of post data from singular pages using `getPage`, or through a custom URL using `callAPI`. Facebook also has a [neat interface](https://developers.facebook.com/tools/explorer/145634995501895/) to pull more specific data like comments, likes, (public) users info into a JSON file. I tried both and find the interface smoother, but it's up to personal preference. 

I decided to mine all public comments for the last 6 months, from 01 March 2017 to 31 August 2017. I only kept the *parent* comment in each thread as child comments tend to be the respective customer support staff replying to users' questions, which isn't what I'm looking for. 

```R
# Example for using Rfacebook: Not run
# fb_grab_page_posts <- getPage(page="grab", token = fb_oauth, n = 5000, feed = TRUE,
#                    since='2017/02/27', until='2017/09/01', # Extract more to buffer for timezone conversion
#         reactions = FALSE, verbose = TRUE, api = "v2.10")
        
# Call Graph API for all comments. Note: Max limit n = 100 comments at a time. 
fb_grab_all_comments <- callAPI("https://graph.facebook.com/v2.10/grab?fields=posts.limit(100){comments}", 
        token = fb_oauth, api = "v2.10")
fb_uber_all_comments <- callAPI("https://graph.facebook.com/v2.10/grab?fields=posts.limit(100){comments}", 
        token = fb_oauth, api = "v2.10")
options(stringsAsFactors = FALSE)

# Combine dataframes
names(fb_grab_all_comments) <- c("DatetimeGMT", "post_username", "post_ID", "post_comment", "message_ID")
names(fb_uber_all_comments) <- c("DatetimeGMT", "post_username", "post_ID", "post_comment", "message_ID")
uber <- mutate(fb_uber_all_comments, Company="uber"); 
grab <- mutate(fb_grab_all_comments, Company="grab")
data <- rbind(uber, grab)

# Format for consistency
data <- transform(data, DatetimeGMT = as.character(DatetimeGMT), 
                  post_username = as.character(post_username),
                  post_ID = as.character(post_ID), 
                  post_comment = as.character(post_comment),
                  message_ID = as.character(message_ID)); str(data)
```

This returns just under `6500` comments for both Grab and Uber.

### Cleaning the data

First off, I cleaned up the date column. Data was stored in GMT time (UTC -8:00) so I converted it to Singapore Time (SGT +8:00). I wrote a function `format_FBtime` to do this so I could reuse it on subsequent analyses. This simple function requires the date column to be in string format. It returns the converted `DatetimeSG` in Date format.

```R
library(lubridate)
library(dplyr)

format_FBtime <- function(df) {
    df$DatetimeGMT <- as.POSIXct(df$DatetimeGMT, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
    df$DatetimeSG <- with_tz(df$DatetimeGMT, "Singapore")
    df <- subset(df, select= -c(DatetimeGMT))
    return(df)
}

# Apply
data <- format_FBtime(data); str(data)

# Select only data from March to Aug 2017 after changing timezones
data <- data[data$DatetimeSG >= as.Date("010317", "%d%m%y") &
                 data$DatetimeSG < as.Date("010917", "%d%m%y"), ] 

```
Next, I'll do a visualisation of the data to see if the number of posts per day for both companies, and see if there are any trends in comments by `month`, `week`, `day of week` or `hour` the comment was posted. 
```R
# Comments by Week
data1 <- data %>% 
    dplyr::group_by(week = week(DatetimeSG), Company) %>% 
    dplyr::summarise(num_comments = n()) 

# Comments by DAY
data1 <- data %>% 
    dplyr::group_by(week = week(DatetimeSG), Company) %>% 
    dplyr::summarise(num_comments = n()) 

plot1 <- ggplot(data1, aes(x=week, y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + facet_grid(. ~ Company) + 
    labs(colour="Company", x="Date by weeks", y=NULL) + 
    ggtitle("Total Comments per Week", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), legend.position = "none",
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9))

# Comments by MONTH
data2 <- data %>% 
    dplyr::group_by(Month = month(DatetimeSG), Company) %>% 
    dplyr::summarise(num_comments = n()) 

Labels <- data.frame(num = c(3:9), Date = c("Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep"))
data2 <- left_join(data2, Labels, by=c("Month" = "num"))    

plot2 <- ggplot(data2, aes(x=reorder(Date, Month), y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + facet_grid(. ~ Company) + 
    labs(colour="Company", x="Date by Month", y=NULL) + scale_x_discrete(name = "Month", labels=abbreviate) +
    ggtitle("Total Comments per Month", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), legend.position = "none",
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9))

# Comments by weekday
data3 <- data %>% 
    dplyr::group_by(Dayofweek, Company) %>% 
    dplyr::summarise(num_comments = n()) 
data3 <- left_join(data3, Labels, by=c("Dayofweek" = "num"))

plot3 <- ggplot(data3, aes(x=reorder(Date, Dayofweek), y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + labs(colour="Company", y=NULL) + 
    scale_x_discrete(name = "Day of Week", labels=abbreviate) + facet_grid(. ~ Company) +
    ggtitle("Total Comments per Day of Week", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15),  plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), legend.position = "none",
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9))

# Comments by hour of day
data4 <- data %>% 
    dplyr::group_by(Hour, Company) %>% 
    dplyr::summarise(num_comments = n()) 

plot4 <- ggplot(data4, aes(x=Hour, y=num_comments)) +
    geom_bar(stat = "identity", aes(fill = num_comments)) + facet_grid(. ~ Company) + 
    labs(colour="Company", x= "Hour of Day, Midnight to Midnight", y=NULL) + 
    ggtitle("Total Comments per Hour", subtitle = "(based on public data from Facebook, 24:00 clock)") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6),
          legend.box = "horizontal", legend.box.just="left", legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(face="bold",size = 6), legend.position = "none",
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9))
          
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    # Ref: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
    plots <- c(list(...), plotlist)
    numPlots = length(plots)
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    if (numPlots==1) {
        print(plots[[1]])
    } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

multiplot(plot1, plot2, plot3, plot4, cols=2)
```
![Imgur](https://i.imgur.com/ckykcVm.png)

Doesn't seem to be any major differences in number of comments per month, although I note that there's a slight increase in comments in *May* due to *Uber* hosting more giveaways on their page. There's an interesting curve in the popularity of `Days of Week` where Uber gets more comments on weekends while Grab gets them on weekdays. 

Next, I added a simple column to count the number of words per comment. 
```R
# Remove non-ASCII characters in comments, usernames
data$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", data$post_comment)
data$post_username <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", data$post_username)

# Count words
data$wordcount <- str_count(data$post_comment, '\\s+')+1
```
I can't access data on users' age, country or even gender, so there isn't much else to form. I couldn't think of any (publicly available) features that might be correlated to the sentiment of comments for now, so I'll move on to sentiment analysis. 

### Sentiment Analysis

From the dataset, I manually tagged about ~30% of the comments (2,600) with labels as `positive`, `neutral`, `negative`. Although most comments were short (<2 sentences), this took about 4 hours. I noticed that most comments were complaints or queries, so I tried to ignore their content (mostly neutral or negative) and focused on their *tone* (which could still be positive - ie. an unhappy but still polite customer). 

To get a baseline of how well the classifier would perform without support features, I ran the pure comments data straight through the `naive bayes` machine from the `e1071` package.

```R
library(e1071)
data_lab <- data[!is.na(data$sentiment),] #n=2,636
classifier_l <- naiveBayes(data_lab[,3], data_lab[,5])
predicted_l <- predict(classifier_l, data_lab)
results_l <- table(predicted_l, data_lab[,5], dnn=list('predicted','actual'))
binom.test(results_l[1,1] + results_l[2,2]+ results_l[3,3], nrow(data_lab), p=0.5)
```
The classifier couldn't work (as expected):
```R
          actual
predicted  negative neutral positive
  negative        0       0        0
  neutral       584    1516      536
  positive        0       0        0
```
I ran it again with the predictors including variables like `hour`, `day of week` and `wordcount`. This increased the accuracy by 5%:
```R
classifier_l <- naiveBayes(data_lab[,c(3,7,8,10)], data_lab[,5])
predicted_l <- predict(classifier_l, data_lab)
results_l <- table(predicted_l, data_lab[,5], dnn=list('predicted','actual'))
binom.test(results_l[1,1] + results_l[2,2]+ results_l[3,3], nrow(data_lab), p=0.5)

            actual
predicted  negative neutral positive
  negative      117      48       63
  neutral       451    1453      450
  positive       16      15       23
probability of success: 0.6043247 
```
Going further, I decided to make more features to improve the accuracy of the machine. I came across [this Tidytext post](https://rstudio-pubs-static.s3.amazonaws.com/236096_2ef4566f995e48c1964013310bf197f1.html) detailing three sentiment dictionaries commonly used in classification: The AFINN, NRC and BING. In summary:

> [FINN](https://github.com/fnielsen/afinn)
is a list of words available in English, Swedish and Danish that are rated with integers corresponding to how `positive` TO `negative`-ly valenced they are. The AFINN-111.txt version contains 2477 common words and phrases, ranging from `Very Negative` (rated -5, -4), `Negative` (-3, -2, -1), `Positive` (rated 1, 2, 3) or `Very Positive` (4, 5). This list includes common swear words and internet lexicon like "lol". 

> [BING](https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon) is a dictionary by Prof Bing Liu where words are strictly sorted as either `positive` or `negative`. It has 4782 negative keywords and 2006 positive keywords. 

> The [NRC](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) is a large list by Saif Mohammad with over 14,000 words corresponding to eight basic emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive). 

I chose to use the AFINN dictionary as it offers a greatest range of sensitivity to sentiment, which I felt was more important than detecting emotion (which would likely be only anger / joy). 
```R
# Load word files 
setwd("~/sentiment_analysis")
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)
```
I then modified the dictionary to include some local flavour and shorthand. I also removed some words like `charged` and `pay` which were rated as negative, but likely meant as neutral in the context of discussing taxi payments. 
```R
# Remove topic-specific words
afinn_list$score[afinn_list$word %in% c("charges", "charged", "pay", "like", "joke", "improvement")] <- 0

# Check if word exists & return score: (reuse this)
afinn_list$score[afinn_list$word=="YOURWORDHERE"]

# Modify scores for words in AFINN
afinn_list$score[afinn_list$word %in% c("best", "nice", "appreciation")] <- 4
afinn_list$score[afinn_list$word %in% c("irresponsible", "whatever")] <- -1
afinn_list$score[afinn_list$word %in% c("cheat", "cheated", "frustrated", "scam", "pathetic", "hopeless", "lousy"
                                        "useless", "dishonest", "tricked", "waste", "gimmick", "liar", "lied")] <- -4
                                        
# Add scores (for words not in AFINN)
pos4 <- data.frame(word = c("bagus", "yay", ":)", "(:", "kindly", "^^", "yay", "swee", "awesome", "polite", 
                            "professional", "thnks", "thnk", "thx", "thankyou","tq", "ty", "tyvm", "please", "pls"), score = 4)
pos2 <- data.frame(word = c("jiayou", "assist", "amin", "amen", "arigato", "well", "bro"), score = 2)
pos1 <- data.frame(word = c("hi", "dear", "hello"), score = 1)
neg1 <- data.frame(word = c("silly", "dafaq", "dafuq", "cringe", "picky"), score = -1)
neg2 <- data.frame(word = c("jialat", "waited", "waiting", "rubbish", "lousy", "siao", "??", "-_-", "-.-", 
                            "lying", "lies", "wtf", "wts", "sicko", "slap", "slapped"), score = -2)
neg4 <- data.frame(word = c("freaking", "knn", "ccb", "fk", "fking", "moronic"), score = -4)

# Merge changes with main AFINN list
afinn_list <- rbind(afinn_list, pos4, pos2, pos1, neg1, neg2, neg4)
```
*Note: I don't really have an objective basis for why some swear words get a higher rating
than others, but I classified them by how angry my mother would be when I say them.*

With this dictionary, I could calculate the average sentiment for each comment *based on their net
Count sentiment per comment based on net word score:

```R
## Split comments to single words per cell
data_indv <- strsplit(data$post_comment, split = " ")
    
# Relink single words to parent comment, "message_ID"
data_words <- data.frame(message_ID = rep(data$message_ID, sapply(data_indv, length)), 
                         words = unlist(data_indv)) # n=174,405
# To lower
data_words$words <- tolower(data_words$words)

# Customise stopwords
library(tm)
stop_words <- as.data.frame(stopwords("en"))
more_stopwords <- as.data.frame(c("uber", "grab", "will", "can", "get", 'u')) # add more if you want
names(stop_words)[1] <- "words"; names(more_stopwords)[1] <- "words"
stop_words <- rbind(stop_words, more_stopwords)
detach("package:tm", unload=TRUE)

# Remove stopwords
data_words <- data_words[!(data_words$words %in% stop_words$words),]

# Remove punctuation, empty rows
data_words <- transform(data_words, words = (sub("^([[:alpha:]]*).*", "\\1", data_words$words)))
data_words <- data_words[(!data_words$words==""),] # n= 39826
data_words <- data_words[!(data_words$words %in% stop_words$words),]
data_words <- data_words[!is.na(data_words$words),]
detach("package:plyr", unload=TRUE)

# Calculate mean of each comments' word scores via the AFINN list
data_afinn <- data_words %>%
    dplyr::group_by(message_ID) %>%
    dplyr::summarise(mean = mean(score, na.rm=TRUE)) # 2899

# Convert NAN to 0
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan)); data_afinn[is.nan(data_afinn)] <- 0
```
From here, I tested to see if there's any significance in scores:
```R
# Independent samples two-tailed t-test (lol freshman year stats making a comeback)
uber <- data[data$Company=="uber",]
grab <- data[data$Company=="grab",]
t.test(uber$AFINN, grab$AFINN)

### Results
95 percent confidence interval: -0.2786706 -0.1390095
mean of x   mean of y 
0.4417077   0.6505478 

t = -5.8628, df = 5836.9, p-value = 4.8e-09
```
Turns out there's a significant difference in tone of comments towards both companies, with 
Grab's customers being a fraction more positive. I'll visualise this:

```R
grab_boxplot <- grab %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, AFINN, Company))
uber_boxplot <- uber %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, AFINN, Company)) 

# Format decimal places, remove NAs, add labels for month names
boxplots <- rbind(grab_boxplot, uber_boxplot)
boxplots$AFINN <- round(boxplots$AFINN, digits=2) # hate trailing zeros
boxplots[is.na(boxplots)] <- 0
boxplots <- left_join(boxplots, Labels, by=c("Month_num" = "num"))

# Create boxplot per month by AFINN score to visualise difference in t-test means
ggplot(data = boxplots, aes(x=reorder(factor(Date), Month_num), y=AFINN)) + geom_boxplot(aes(fill=Company)) + 
    scale_y_continuous(name = "AFINN score (neg=rude, pos=polite)", breaks = seq(-5, 5, 1.0), limits=c(-5, 5)) + 
    scale_fill_brewer(palette = "Accent") + ggtitle("Distribution of Sentiment by Word", 
    subtitle = "(Sentiment analysis through AFINN dictionary, based on public data from Facebook)") + xlab(NULL) +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6), legend.title = element_text(face="italic",size=6),
          legend.position = "bottom", legend.box = "horizontal", legend.box.just="left",
          legend.key.size = unit(0.5, "cm"), legend.text = element_text(face="bold",size = 6),
          axis.title = element_text(size=9)) 
```
![Imgur](https://i.imgur.com/Wlqgqvd.jpg)

However, I want to push the theory even further. This method of calculation assigns scores to individual words without regard for features of natural language, like negators or amplifiers, that change the meaning of a sentence. This method also fails to accurately detect intent in cases of double negatives or sarcasm.  

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
