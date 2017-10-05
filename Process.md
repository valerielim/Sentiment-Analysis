# A Tale of Two Taxi Giants

### Background

Grab and Uber are competing pretty hard for the local market right now. After 
Travis' controversies started in Feb 2017, I noticed some friends grew polarised towards one company or the other. 
I wondered if the company's negative press image would affect the way other customers felt towards the brand as well. 

This post outlines the process I used to mine public data using R and the Facebook Graph API for Uber and Grab. 
I then built a `naive bayes` sentiment classifier to label the data and visualised the results below. All in all,
I'm looking to see if there is a difference in the way people talk and feel toward Grab and Uber. 

**Quick outline of workflow:**

* Mining Data from Facebook API with R
* Cleaning the data
* Building features for machine learning
* Machine learning for sentiment classification
* Visualise results

# Mining Data from Facebook API with R 

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
# Pull official posts through API
fb_grab_posts <- getPage(page="grab", token = fb_oauth, n = 3100, feed = TRUE,
                         since='2017/03/01', until='2017/08/31',
                         reactions = FALSE, verbose = TRUE, api = "v2.10")
fb_uber_posts <- getPage(page="UberSingapore", token = fb_oauth, n = 3100, feed = TRUE,
                         since='2017/03/01', until='2017/08/31',
                         reactions = FALSE, verbose = TRUE, api = "v2.10")

# Call Graph API for posts' comments. Note: Max limit n = 100 comments at a time. 
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

A quick plot of the number of likes, comments and shares each company has earned: 

```R
# Melt variables, group by month
fb_posts <- rbind(fb_grab_posts, fb_uber_posts)
colnames(fb_posts)[4] <- "DatetimeGMT"; fb_posts <- format_FBtime(fb_posts)
fb_plot_type <- fb_posts[fb_posts$from_name=="Uber"|fb_posts$from_name=="Grab",c(2:4,8:11)]

# Melt - by metric 
fb_plot_type1 <- fb_plot_type %>%
    mutate(month_num = month(DatetimeSG)) %>%
    left_join(y=Labels, by=c("month_num"="num")) %>%
    melt(id = c("from_name", "message", "month_num", "Date", "DatetimeSG")) %>%
    dplyr::group_by(variable, month_num, Date, type, from_name) %>%
    dplyr::summarise(total = sum(value))

ggplot(data = fb_plot_type1) + 
    geom_point(aes(x = reorder(Date, month_num), y = total, colour=variable)) + 
    geom_line(aes(x = reorder(Date, month_num), y = total, colour = variable, group = variable), size=1, stat="identity") +
    scale_x_discrete(name = NULL, labels=abbreviate) + scale_y_continuous(name = "Number of likes/comments") +
    ggtitle("Distribution of Facebook Engagement by Likes, Shares, Comments", subtitle = "(based on public data from Facebook)") +
    theme(plot.title = element_text(face="bold", size=15), plot.subtitle = element_text(face="italic",size=6), 
          legend.title = element_text(face="italic",size=6), legend.position = "right", legend.box = "vertical",
          legend.key.size = unit(0.5, "cm"), legend.text = element_text(face="bold",size = 6), legend.box.just="left", 
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9)) + 
     facet_grid(. ~ from_name, scales="free")
```
![Imgur](https://i.imgur.com/hz0XVCA.jpg)

# Cleaning the data

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

Grab appears to have wildly engaging giveaways with high user participation (top left, total comments per *week* shows Grab has 4 bright spikes in activity), while Uber maintains a more consistent performance week to week. This trend reverses itself when data is aggregated by *month* (bottom left) -- suggesting that Uber had overall strong Facebook engagement performance in May while Grab's engagement has spaced itself out more consistently over the 6 months.

On micro-trends, Uber users' commenting habits fit expectations in that they were most active on Saturday and Sunday, but Grab's users post comments the most on Tuesday and Wednesday. (I have no explanation why this is so.) Uber's users make the most comments around 6pm while Grab's users make them at both 11am and 6pm. Again. I have no idea what people would complain about at 11am in the morning when there's no peak hour but OK. 

Back to the data, I'll added a simple line to count the number of words per comment to finish prepping for sentiment analysis. 
```R
# Remove non-ASCII characters in comments, usernames
data$post_comment <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", data$post_comment)
data$post_username <- gsub("[^0-9A-Za-z\\\',!?:;.-_@#$%^&*()\" ]", "", data$post_username)

# Count words per comment
data$wordcount <- str_count(data$post_comment, '\\s+')+1
```
I couldn't think of any (publicly available) features that might be correlated to the sentiment of comments for now, so I'll move on. Unfortunately, I can't access personal data on users' age, country or even gender, so we're quite limited on what we can do with regards to demographics. 

# Building features for machine learning

From the dataset, I manually tagged about ~30% of the comments (2,600) with labels as `positive`, `neutral`, `negative`. Although most comments were short (<2 sentences), this took about 6 hours. I noticed that most comments were complaints or queries, so I tried to ignore their content (mostly neutral or negative) and focused on their *tone* (which could still be positive - ie. an unhappy but still polite customer). 

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
I ran it again with the predictors including variables like `hour`, `day of week` and `wordcount`. This increased the accuracy by `3%`:
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
```
That wasn't very helpful, so I decided to make more features to improve the accuracy of the machine. 

I came across [this Tidytext post](https://rstudio-pubs-static.s3.amazonaws.com/236096_2ef4566f995e48c1964013310bf197f1.html) detailing three commonly used dictionaries in sentiment classification: The AFINN, NRC and BING. In summary:

> [FINN](https://github.com/fnielsen/afinn)
is a list of words available in English, Swedish and Danish that are rated with integers corresponding to how `positive` or `negative`  they are. The AFINN-111 version contains 2477 common words and phrases, ranging from `Very Negative` (rated -5, -4), `Negative` (-3, -2, -1), `Positive` (rated 1, 2, 3) or `Very Positive` (4, 5). I appreciate that this list also includes common swear words and internet lexicon like "lol". 

> [BING](https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon) is a dictionary by Prof Bing Liu where words are strictly sorted as either `positive` or `negative`. It has 4782 negative keywords and 2006 positive keywords. 

> The [NRC](http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm) is a large list by Saif Mohammad with over 14,000 words corresponding to eight basic emotions (`anger`, `fear`, `anticipation`, `trust`, `surprise`, `sadness`, `joy`, and `disgust`) and two sentiments (`negative` and `positive`). 

I chose to use the AFINN dictionary as it offers a greatest range of sensitivity to sentiment, which I felt was more important than detecting emotion (which would be predominantly anger due to it being complaints). 
```R
# Load word files 
setwd("~/sentiment_analysis")
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)
```
I then modified the dictionary to include some local flavour and shorthand. I also removed scores for words like `charged` and `pay` which were rated as negative, but likely meant as neutral in the context of discussing taxi payments. 
```R
# Remove topic-specific words
afinn_list$score[afinn_list$word %in% c("charges", "charged", "pay", "like", "joke", "improvement")] <- 0

# Check if word exists & return score: (reuse this)
afinn_list$score[afinn_list$word=="YOURWORDHERE"]

# Modify scores for words in AFINN
afinn_list$score[afinn_list$word %in% c("best", "nice", "appreciation")] <- 4
afinn_list$score[afinn_list$word %in% c("irresponsible", "whatever")] <- -1
afinn_list$score[afinn_list$word %in% c("cheat", "cheated", "frustrated", "scam", "pathetic", "hopeless", "lousy", "useless", "dishonest", "tricked", "waste", "gimmick", "liar", "lied")] <- -4
                                        
# Add scores (for words not in AFINN)
pos4 <- data.frame(word = c("bagus", "yay", ":)", "(:", "kindly", "^^", "yay", "swee", "awesome", "polite", "professional", "thnks", "thnk", "thx", "thankyou","tq", "ty", "tyvm", "please", "pls"), score = 4)
pos2 <- data.frame(word = c("jiayou", "assist", "amin", "amen", "arigato", "well", "bro"), score = 2)
pos1 <- data.frame(word = c("hi", "dear", "hello"), score = 1)
neg1 <- data.frame(word = c("silly", "dafaq", "dafuq", "cringe", "picky"), score = -1)
neg2 <- data.frame(word = c("jialat", "waited", "waiting", "rubbish", "lousy", "siao", "??", "-_-", "-.-", "lying", "lies", "wtf", "wts", "sicko", "slap", "slapped"), score = -2)
neg4 <- data.frame(word = c("freaking", "knn", "ccb", "fk", "fking", "moronic"), score = -4)

# Merge changes with main AFINN list
afinn_list <- rbind(afinn_list, pos4, pos2, pos1, neg1, neg2, neg4)
```
*Note: I don't really have an objective basis for why some swear words get a higher rating
than others, but I classified them by how angry my mother would be when I say them.*

With this dictionary, I calculated the net sentiment for each comment.

```R
## Split comments to single words per cell
data_indv <- strsplit(data$post_comment, split = " ")
data_words <- data.frame(message_ID = rep(data$message_ID, sapply(data_indv, length)), words = unlist(data_indv))
data_words$words <- tolower(data_words$words)

# Customise stopwords list
library(tm)
stop_words <- as.data.frame(stopwords("en"))
more_stopwords <- as.data.frame(c("uber", "grab")) # add more if you want
names(stop_words)[1] <- "words"; names(more_stopwords)[1] <- "words"
stop_words <- rbind(stop_words, more_stopwords)
detach("package:tm", unload=TRUE)

# Remove stopwords
data_words <- data_words[!(data_words$words %in% stop_words$words),]

# Remove punctuation, empty rows, NA
data_words <- transform(data_words, words = (sub("^([[:alpha:]]*).*", "\\1", data_words$words)))
data_words <- data_words[(!data_words$words==""),] # n= 39826
data_words <- data_words[!(data_words$words %in% stop_words$words),]
data_words <- data_words[!is.na(data_words$words),]
detach("package:plyr", unload=TRUE)

# Calculate mean of each comments' word scores via the AFINN list
data_afinn <- data_words %>%
    dplyr::group_by(message_ID) %>%
    dplyr::summarise(mean = mean(score, na.rm=TRUE)) 
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan)); data_afinn[is.nan(data_afinn)] <- 0
```
From there, I tested to see if there's any significant difference in customers' sentiment scores between the two companies:
```R
# Independent samples two-tailed t-test 
uber <- data[data$Company=="uber",]
grab <- data[data$Company=="grab",]
t.test(uber$AFINN, grab$AFINN)

### Results
95 percent confidence interval: -0.2786706 -0.1390095
mean of x   mean of y 
0.4417077   0.6505478 

t = -5.8628, df = 5836.9, p-value = 4.8e-09
```
Turns out there's a significant difference `p<0.05` in tone of comments towards both companies, with 
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
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15), axis.title = element_text(size=9),
          plot.subtitle = element_text(face="italic",size=6), legend.title = element_text(face="italic",size=6),
          legend.position = "bottom", legend.box = "horizontal", legend.box.just="left",
          legend.key.size = unit(0.5, "cm"), legend.text = element_text(face="bold",size = 6)) 
```
![Imgur](https://i.imgur.com/Wlqgqvd.jpg)

As we can see, there's also a visible difference in customer sentiment towards Grab and Uber at word level, with Grab scoring slightly higher than Uber on the positive words scale. Could it be that Grab's customers are nicer? *Or simply that they've been giving more promos? :-)*

Anyway, let's test if this trend continues under a more sensitive linguistics analysis. So far, the method of assigning scores to dictionaries presents a rudimentary way to estimate the tone of a comment. In a funny way, it's definitely handy for checking out which company's users swear more. But this method misses out on many other aspects of natural language -- double negatives, amplifiers, adversative conjunctions and negators.

Tyler Rinkle wrote a beautiful primer that explains what these linguistic features mean, and why they matter. In his [introduction](https://github.com/trinker/sentimentr#why-sentiment), he writes: 

> "A negator flips the sign of a polarized word (e.g., "I do **not** like it."). An amplifier (intensifier) increases the impact of a polarized word (e.g., "I **really** like it.").  A de-amplifier (downtoner) reduces the impact of a polarized word (e.g., "I **hardly** like it."). An adversative conjunction overrules the previous clause containing a polarized word (e.g., "I like it **but** it's not worth it.")."

I've not been paying attention to grammatical features in the language and perhaps that's why Grab appears to be doing better. To address those, I used his `sentimentr` package. 

> `SentimentR` is a dictionary lookup approach that tries to incorporate weighting for valence shifters. Matthew Jocker created the `syuzhet` package that utilizes dictionary lookups for the Bing, NRC, and Afinn methods as well as a custom dictionary. ... Jocker's dictionary methods are fast but are more prone to error in the case of valence shifters. ... In my own work I need better accuracy than a simple dictionary lookup; something that considers valence shifters yet optimizes speed which the Stanford's parser does not. This leads to a trade off of speed vs. accuracy. Simply, sentimentr attempts to balance accuracy and speed.

His package was delightfully simple to use:
```R
library(sentimentr)

data_sentence <- get_sentences(data$post_comment)
data_sr <-sentiment(data_sentence)

# Score by cell; calculate avg per comment
data_sr <- data_sr %>%
    dplyr::group_by(element_id) %>%
    dplyr::summarise(mean(sentiment))

# Bind back to main frame
data <- cbind(data, SentimentR = data_sr$sentimentr)
```
When visualised, the results looks like this:

```R
uber <- data[data$Company=="uber",]
grab <- data[data$Company=="grab",]

# Shape data for boxplots
grab_boxplot2 <- grab %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, SentimentR)) %>%
    mutate(Company = "Grab")
uber_boxplot2 <- uber %>%
    mutate(Month_num = month(DatetimeSG)) %>%
    subset(select=c(Month_num, SentimentR)) %>%
    mutate(Company = "Uber")

# Format decimal places, remove NAs, add labels
boxplots2 <- rbind(grab_boxplot2, uber_boxplot2)
boxplots2$SentimentR <- round(boxplots2$SentimentR, digits=2)
boxplots2[is.na(boxplots2)] <- 0
boxplots2 <- left_join(boxplots2, Labels, by=c("Month_num" = "num")) 

# Boxplot per month by score
ggplot(data = boxplots2, aes(x=reorder(factor(Date), Month_num), y=SentimentR)) + 
    geom_boxplot(aes(fill=Company)) + scale_fill_brewer(palette = "Pastel2") + xlab(NULL) +
    ggtitle("Distribution of Sentiment by Sentence", subtitle = "(Sentiment analysis through SentimentR pkg, based on public data from Facebook)") +
    scale_y_continuous(name = "SR score (neg=rude, pos=polite)", breaks = seq(-1, 1, 0.2), limits=c(-1, 1)) +
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15), axis.title = element_text(size=9),
          plot.subtitle = element_text(face="italic",size=6), legend.title = element_text(face="italic",size=6),
          legend.position = "bottom", legend.box = "horizontal", legend.box.just="left",
          legend.key.size = unit(0.5, "cm"), legend.text = element_text(face="bold",size = 6)) 

```
![Imgur](https://i.imgur.com/VNWzYnY.jpg)

Sweet. 

Trend looks similar to the `AFINN` boxplots - Uber leading in April and May, while Grab leads in June, July and August. 

In fact, we can test the correlation of these distributions:
```R
# Test if SR score is correlated to AFINN dictionary
cor.test(grab$AFINN, grab$SentimentR, use="complete.obs", method="pearson")
# t = 23.727, df = 2889, p-value < 2.2e-16
# Pearson's: 0.4038415 

cor.test(uber$AFINN, uber$SentimentR, use="complete.obs", method="pearson")
# t = 23.558, df = 2952, p-value < 2.2e-16
# Pearson's: 0.3978006 
```
Scores in both companies' datasets are moderately correlated `40%` with certainty `p<0.05`. 

This is getting pretty exciting. 

Moving forward, I'll definitely keep these features for use in the machine. 

# Machine learning for sentiment classification

... 

Some explanation about naive bayes 

```R
library(e1071)

# Applied
lab_data <- data[!is.na(data$sentiment),] 
lab_data[is.na(lab_data)] <- 0
lab_uber <- lab_data[lab_data$Company=="uber",]
lab_grab <- lab_data[lab_data$Company=="grab",]

# Uber
classifier_u <- naiveBayes(lab_uber[,c(3,7,8,10:12)], lab_uber[,5])
predicted_u <- predict(classifier_u, lab_uber)
results_u <- table(predicted_u, lab_uber[,5], dnn=list('predicted','actual'))
binom.test(results_u[1,1] + results_u[2,2]+ results_u[3,3], nrow(lab_uber), p=0.5)

# Grab
classifier_g <- naiveBayes(lab_grab[,c(3,7,8,10:12)], lab_grab[,5])
predicted_g <- predict(classifier_u, lab_grab)
results_g <- table(predicted_g, lab_grab[,5], dnn=list('predicted','actual'))
binom.test(results_g[1,1] + results_g[2,2]+ results_g[3,3], nrow(lab_grab), p=0.5)
```
Results:
```R 
           Actual (GRAB)
predicted  negative neutral positive
negative   57      17      8 
neutral    126     659     137  
positive   24      40      169
# 95 percent confidence interval: 0.6894006 0.7404522
# Final probability of success: 0.7154406 

           Actual (UBER)
predicted  negative neutral positive
negative   192      53      14
neutral    168     666     101  
positive   16      75      106
# 95 percent confidence interval: 0.6680392 0.7171959
# Final probability of success: 0.6930266 
```
I'll apply this same algorithm to classify the remaining 4000+ unlabelled comments.  

A note - While this classifier isn't exceptionally accurate (at `70%`), it skews sentiment detection *in the same way* for both datasets. Meaning, if it tends to classify Uber's posts as negative, it would classify Grab's posts as negative by the same degree too.

```
# Select remaining unlabelled data 
unlab_u <- dplyr::filter(uber, is.na(sentiment))
unlab_g <- dplyr::filter(grab, is.na(sentiment)) 

# Predict
predicted_unlab_g <- predict(classifier_g, unlab_g); predicted_unlab_u <- predict(classifier_u, unlab_u)
unlab_g <- cbind(unlab_g, as.data.frame(predicted_unlab_g)); unlab_u <- cbind(unlab_u, as.data.frame(predicted_unlab_u))

# Merge prediction sentiments back with main dataset
unlab_g <- subset(unlab_g, select = -c(sentiment)); unlab_u <- subset(unlab_u, select = -c(sentiment))
colnames(unlab_u)[18] <- "sentiment"; colnames(unlab_g)[18] <- "sentiment"
grab_final <- rbind(grab_lab, unlab_g); uber_final <- rbind(uber_lab, unlab_u)
```
Let's check the distribution of predictions. 
```
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

t <- unlab_g %>%
    dplyr::count(predicted_unlab_g)

# A tibble: 3 x 2
# predicted_unlab_GRAB     n
# <fctr> <int>
# 1          negative    60
# 2           neutral  1728
# 3          positive   157
```
I guess they both skew towards `neutral` when in doubt. With these results, I'll make two final graphs of words. Taking negative comments as complaints, and positive comments as compliments, we can visualise the distribution of sentiments. Here's a final weighted scatterplot: 
```R
uber$Company <- "Uber"
grab$Company <- "Grab"
combined <- rbind(uber, grab)

# Group by date
plot1 <- combined %>%
    group_by(Date(DatetimeSG), Company) %>%
    summarise(SentimentR = mean(SentimentR),
              AFINN = mean(mean),
              num_posts = n())

plot1 <- transform(plot1, DateSG = as.Date(DateSG),
                   Company = as.factor(Company))

# Subset plots within 1SD of mean*: (Calculations for each variable's SD on another page, they were bloody long)
plot2 <- plot1[plot1$SentimentR<0.6&plot1$SentimentR>-0.2,]

# Melt and reshape to double facet plot
SR <- subset(plot1, select=-c(AFINN))
SR$package <- "By sentence"
AFINN <- subset(plot1, select=-c(SentimentR))
AFINN$package <- "By word"
colnames(SR)[3] <- "Score";colnames(AFINN)[3] <- "Score"
plot3 <- rbind(SR, AFINN)
plot4 <- plot3[plot3$Score<=1&plot3$Score>=-0.5,]


ggplot(plot4, aes(x = DateSG, y = Score, size = num_posts, fill = Score)) + geom_point(shape=21) + 
    labs(x=NULL, y="Tone (Negative to Positive)", size = "Total Comments per Day", fill = "Sentiment") + 
    ggtitle("Sentiment toward Grab & Uber in Singapore", subtitle = "(based on public data from Facebook)")+
    theme(plot.title = element_text(family="Tahoma", face="bold", size=15),
          plot.subtitle = element_text(face="italic",size=6),
          legend.title = element_text(face="italic",size=6), legend.position = "bottom", legend.box = "horizontal", 
          legend.box.just="left", legend.key.size = unit(0.5, "cm"), legend.text = element_text(face="bold",size = 6),
          strip.text=element_text(face="bold", size=9), axis.title = element_text(size=9)) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + scale_size(range = c(1, 15)) + 
    scale_y_continuous(breaks=NULL, #limits = c(-0.5, 1), position = "left") +
    scale_fill_continuous(low = "firebrick1", high = "green") + facet_grid(Company ~ package)
```
![Imgur](https://i.imgur.com/5bBG8oP.jpg)

The size of the bubbles correspond to the number of posts that day, while the colour marks their sentiment. 

Final visualisation: Top words per category. Bear with me, this is gonna be long. 
````
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
                         words = unlist(c_indv))
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
