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

library("ggplot2")
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
        
#Connect your twitter account to R, in order to extract the required tweets.
consumer_key <- '3BnaL1T3NeW88k4nYxxBdtonB'
consumer_secret <- 'MZXguk6Y6XZLgpX7unNtllGUJA432RbCGJBTPo6PCRGwBQZhxL'
access_token <- '2955462462-MPvfvKP4Y1HSPZOKpRcp0HI0SwQhkAQTK6TZdIM'
access_secret <- 'kt0oJUR3gTidYgIDF1DO7UxLlptgcuxQdULCLvEcAOvF7'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extracting tweets using a particular hashtag:
tweets_pk <- searchTwitter("@pawanKalyan", n=800,lang = "en")

#Convert this extracted data to a dataframe which makes it more readable and easier to work with.
PSPK_tweets <- twListToDF(tweets_pk)
setwd("D:/DS/Assignments/Assignments/Text Mining/Twitter")
write.table(PSPK_tweets,"PSPK.txt",row.names = F)
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
PSPK_tweets.text.corpus <- c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

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
wordcloud(PSPK_tweets.text.corpus,min.freq = 4,colors=rainbow(50),random.color = TRUE,random.order=T,rot.per=0.50, max.words = 500)
#wordcloud(PSPK_tweets.text.corpus,min.freq = 4,colors=brewer.pal(8,"Accent),random.color = TRUE,random.order=F,rot.per=.50, max.words = 800)


#Introduction to Sentiment Analysis
#getting emotions using in-built function
mysentiment_PSPK<-get_nrc_sentiment((PSPK_text))

#calculationg total score for each sentiment
Sentimentscores_PSPK<-data.frame(colSums(mysentiment_PSPK[,]))

names(Sentimentscores_PSPK)<-"Score"
Sentimentscores_PSPK<-cbind("sentiment"=rownames(Sentimentscores_PSPK),Sentimentscores_PSPK)
rownames(Sentimentscores_PSPK)<-NULL

#plotting the sentiments with scores
windows()
ggplot(data=Sentimentscores_PSPK,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
        theme(legend.position="none")+
        xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on PSPK")
legend("topright",colnames(PSPK_tweets.text.corpus),col=1:3,cex=0.8,fill=1:3)

PSPK_tweets <- readLines("D:/DS/Assignments/Assignments/Text Mining/Twitter/PSPK.txt")

length(PSPK_tweets)

mydata.corpus <- Corpus(VectorSource(PSPK_tweets))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

my_stopwords <- c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)

## build a term-document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3

dim(mydata.dtm3)

# dtm <- as.DocumentTermMatrix(mydata.dtm3)
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(mydata.dtm3)

rowTotals <- apply(dtm, 1, sum)
?apply

dtm.new   <- dtm[rowTotals > 0, ]
dim(dtm.new)

lda <- LDA(dtm.new, 10) # find 10 topics
?LDA

term <- terms(lda, 20) # first 5 terms of every topic
term

tops <- terms(lda)
?terms
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)
?unlist

cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
?hclust
par(family = "HiraKakuProN-W3")
windows()
plot(cls)
?par
######

####################### Emotion mining ##############################

install.packages("syuzhet")
library("syuzhet")

my_example_text <- readLines("D:/DS/Assignments/Assignments/Text Mining/Twitter/PSPK.txt")

s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)

sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)

afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)

nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)

sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)

# plot
windows()
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive

# more depth
poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method="bing")
windows()
plot(
        poa_sent, 
        type="h", 
        main="Example Plot Trajectory", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence"
)
abline(h = 0, col = "red")
# percentage based figures
percent_vals <- get_percentage_values(poa_sent)
windows()
plot(
        percent_vals, 
        type="l", 
        main="Throw the ring in the volcano Using Percentage-Based Means", 
        xlab = "Narrative Time", 
        ylab= "Emotional Valence", 
        col="red"
)

ft_values <- get_transformed_values(
        poa_sent, 
        low_pass_size = 3, 
        x_reverse_len = 100,
        scale_vals = TRUE,
        scale_range = FALSE
)
windows()
plot(
        ft_values, 
        type ="h", 
        main ="LOTR using Transformed Values", 
        xlab = "Narrative Time", 
        ylab = "Emotional Valence", 
        col = "red"
)

# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
nrc_score_sent <- get_nrc_sentiment(negative)
nrc_score_word <- get_nrc_sentiment('grim')
# subset

sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

############
############

####################### Structured data extraction (NER) ##################
install.packages("rJava")
library(rJava)
install.packages("NLP")
library(NLP)
install.packages("openNLP")
library(openNLP)
install.packages("RWeka")
library(RWeka)
install.packages("magrittr")
library(magrittr)
install.packages("qdap")
library("qdap")
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library(openNLPmodels.en)

bio <- readLines("D:/DS/Assignments/Assignments/Text Mining/Twitter/PSPK.txt")
bio <- paste(bio, collapse = " ")
bio <- as.String(bio)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_ann, word_ann))

class(bio_annotations)
head(bio_annotations)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

entities <- function(doc, kind) {
        s <- doc$content
        a <- annotations(doc)[[1]]
        if(hasArg(kind)) {
                k <- sapply(a$features, `[[`, "kind")
                s[a[k == kind]]
        } else {
                s[a[a$type == "entity"]]
        }
}

entities(bio_doc, kind = "person")

entities(bio_doc, kind = "location")

entities(bio_doc, kind = "organization")
