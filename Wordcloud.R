install.packages("twitteR")
install.packages("ROAuth")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("tm")
install.packages("SnowballC")
install.packages("topicmodels")
install.packages("syuzhet")
install.packages("ROAuth")
install.packages("NLP")
install.packages("corpus")

library("corpus")
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("ROAuth")
library("ggplot2")
#Connect your twitter account to R, in order to extract the required tweets.
consumer_key <- '3BnaL1T3NeW88k4nYxxBdtonB'
consumer_secret <- 'MZXguk6Y6XZLgpX7unNtllGUJA432RbCGJBTPo6PCRGwBQZhxL'
access_token <- '2955462462-MPvfvKP4Y1HSPZOKpRcp0HI0SwQhkAQTK6TZdIM'
access_secret <- 'kt0oJUR3gTidYgIDF1DO7UxLlptgcuxQdULCLvEcAOvF7'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extracting tweets using a particular hashtag:
tweets_pk <- searchTwitter(" #HBDPowerStar", n=500,lang = "en")

#Convert this extracted data to a dataframe which makes it more readable and easier to work with.
PSPK_tweets <- twListToDF(tweets_pk)

View(PSPK_tweets)

#Here is how the data would look like:
#It is very clear that in the text section of the data, which is what we need to process there are a lot of special characters and unnecessary data which we would not require. Hence it becomes extremely important to pre-process this data and then we can continue with out analysis.

#Below is a code to pre-process the data and remove tabs, blank spaces, links etc. This section can be modified according to one's requirements.
PSPK_tweets<- PSPK_tweets$text

#convert all text to lower case

PSPK_text<- tolower(PSPK_tweets)

# Replace blank space ("rt")
PSPK_text <- gsub("rt", "", PSPK_text)

# Replace @UserName
PSPK_text <- gsub("@\\w+", "", PSPK_text)

# Remove punctuation
PSPK_text <- gsub("[[:punct:]]", "", PSPK_text)

# Remove links
PSPK_text <- gsub("http\\w+", "", PSPK_text)

# Remove tabs
PSPK_text <- gsub("[ |\t]{2,}", "", PSPK_text)

# Remove blank spaces at the beginning
PSPK_text <- gsub("^ ", "", PSPK_text)

# Remove blank spaces at the end
PSPK_text <- gsub(" $", "", PSPK_text)

#A little more pre-processing - Removal of stop words!

#What are Stop Words?
#When working with text mining applications, we often hear of the term "stop words" or "stop word list" or even "stop list". Stop words are basically a set of commonly used words in any language, not just English. The reason why stop words are critical to many applications is that, if we remove the words that are very commonly used in a given language, we can focus on the important words instead.

#Stop words are generally thought to be a "single set of words". It really can mean different things to different applications. For example, in some applications removing all stop words right from determiners (e.g. the, a, an) to prepositions (e.g. above, across, before) to some adjectives (e.g. good, nice) can be an appropriate stop word list. To some applications however, this can be detrimental. For instance, in sentiment analysis removing adjective terms such as 'good' and 'nice' as well as negations such as 'not' can throw algorithms off their tracks. In such cases, one can choose to use a minimal stop list consisting of just determiners or determiners with prepositions or just coordinating conjunctions depending on the needs of the application.

#clean up by removing stop words
PSPK_tweets.text.corpus <- Corpus(VectorSource(PSPK_text))

stopwords_swe <- c("är", "från", "än")
#Just a short example above, the real one is very large
PSPK_tweets_text <- removeWords(PSPK_text,stopwords_swe)

#Create corpus:
PSPK_tweets.text.corpus <- Corpus(VectorSource(PSPK_text))

#See notes in the longer text about the corpus vector
PSPK_tweets.text.corpus <- tm_map(PSPK_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#We are done pre-processing our data, and are ready to do some analysis.
#Data visualizations (like charts, graphs, infographics, and more) give analysts a valuable way to communicate important information at a glance. If you want a stunning visualization format to highlight important textual data points, using a word cloud can make dull data sizzle and immediately convey crucial information.

#What are Word Clouds?
#Word clouds (also known as text clouds or tag clouds) work in a simple way: the more a specific word appears in a source of textual data (such as a speech, blog post, or database), the bigger and bolder it appears in the word cloud.
#So let's generate some word clouds and find out some of the frequent and important terms being used in the tweets we have extracted.

library("wordcloud")

#generate wordcloud
windows()
wordcloud(PSPK_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#Introduction to Sentiment Analysis
#getting emotions using in-built function
mysentiment_PSPK<-get_nrc_sentiment((PSPK_text))

#calculationg total score for each sentiment
Sentimentscores_PSPK<-data.frame(colSums(mysentiment_PSPK[,]))

names(Sentimentscores_PSPK)<-"Score"
Sentimentscores_PSPK<-cbind("sentiment"=rownames(Sentimentscores_PSPK),Sentimentscores_PSPK)
rownames(Sentimentscores_PSPK)<-NULL

#plotting the sentiments with scores
ggplot(data=Sentimentscores_PSPK,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on PSPK")

