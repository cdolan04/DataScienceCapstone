# Load libraries to be used
library(tm)
library(stringi)
library(data.table)
library(wordcloud)
library(quanteda)
library(magrittr)
library(dplyr)

# Download and load data
if(!file.exists("Coursera-SwiftKey.zip")){
        fileURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(fileURL, destfile = "Dataset.zip")
        unlink(fileURL)
        unzip("Dataset.zip")
}else{
        print("Already Have Data")
}

# Create Profanity word list for later filtering
profanity <- read.table("http://www.bannedwordlist.com/lists/swearWords.txt")
profanity <- as.character(profanity[-c(37:40),])

# Load the three data files
twitter <- readLines(file("en_US.twitter.txt"),skipNul = TRUE, encoding="UTF-8") 
#blogs <- readLines(file("en_US.blogs.txt"), skipNul = TRUE, encoding="UTF-8") 
news <- readLines(file("en_US.news.txt"), skipNul = TRUE, encoding="UTF-8")

# Sample 10% of data for each file
stwitter <- twitter[as.logical(rbinom(length(twitter),1, prob=0.15))]
#sblogs <- blogs[as.logical(rbinom(length(blogs),1, prob=0.01))]
snews <- news[as.logical(rbinom(length(news),1, prob=0.15))]

## We want to remove strange characters/words
stwitter <- unlist(strsplit(stwitter, split=", "))
twitterremove <- grep("stwitter", iconv(stwitter, "latin1", "ASCII", sub="stwitter"))
stwitter <- stwitter[-twitterremove]
stwitter<- paste(stwitter, collapse = ", ")

# sblogs <- unlist(strsplit(sblogs, split=", "))
# blogsremove <- grep("sblogs", iconv(sblogs, "latin1", "ASCII", sub="sblogs"))
# sblogs <- sblogs[-blogsremove]
# sblogs<- paste(sblogs, collapse = ", ")

snews <- unlist(strsplit(snews, split=", "))
newsremove <- grep("snews", iconv(snews, "latin1", "ASCII", sub="snews"))
snews <- snews[-newsremove]
snews<- paste(snews, collapse = ", ")

# Put all 3 sampled files in to one
allsamples <- c(stwitter, snews)


# Create Corpus of 3 sampled files, allows easy processing with quanteda library
samplecorpus <- corpus(allsamples)

# Create Document Feature Matrix while changing to all lowercase, removing stop words & punctuation,
# and stemming
all.1grams <- dfm(samplecorpus, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE,
                  removeTwitter = TRUE, stem = FALSE)
all.1grams <- tf(x=all.1grams, scheme = "prop")

all.2grams <- dfm(samplecorpus, ngrams = 2, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE,
                  removeTwitter = TRUE, stem = FALSE)
all.2grams <- tf(x=all.2grams, scheme = "prop")

all.3grams <- dfm(samplecorpus, ngrams = 3, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE,
                removeTwitter = TRUE, stem = FALSE)
all.3grams <- tf(x=all.3grams, scheme = "prop")

all.4grams <- dfm(samplecorpus, ngrams = 4, toLower = TRUE, removeNumbers = TRUE, removePunct = TRUE,
                removeTwitter = TRUE, stem = FALSE)
all.4grams <- tf(x=all.4grams, scheme = "prop")



#############################################################################################################

### Create dataframe of frequencies
onegram <- t(rbind(names(all.1grams), colSums(all.1grams)))
onegram <- as.data.frame(onegram)
onegram[,1] <- as.numeric(onegram[,1])
onegram <- cbind(rownames(onegram), onegram)
onegram <- onegram[order(onegram[,2], decreasing = TRUE),]

twogram <- t(rbind(names(all.2grams), colSums(all.2grams)))
twogram <- as.data.frame(twogram)
twogram[,1] <- as.numeric(twogram[,1])
twogram <- cbind(rownames(twogram), twogram)
twogram <- twogram[order(twogram[,2], decreasing = TRUE),]

threegram <- t(rbind(names(all.3grams), colSums(all.3grams)))
threegram <- as.data.frame(threegram)
threegram[,1] <- as.numeric(threegram[,1])
threegram <- cbind(rownames(threegram), threegram)
threegram <- threegram[order(threegram[,2], decreasing = TRUE),]

fourgram <- t(rbind(names(all.4grams), colSums(all.4grams)))
fourgram <- as.data.frame(fourgram)
fourgram[,1] <- as.numeric(fourgram[,1])
fourgram <- cbind(rownames(fourgram), fourgram)
fourgram <- fourgram[order(fourgram[,2], decreasing = TRUE),]


rownames(onegram) <- NULL
rownames(twogram) <- NULL
rownames(threegram) <- NULL
rownames(fourgram) <- NULL

names(onegram)<- c("gram","freq")
names(twogram)<- c("gram","freq")
names(threegram)<- c("gram","freq")
names(fourgram)<- c("gram","freq")

# save(onegram, file="onegramTF.RData")
# save(twogram, file="twogramTF.RData")
# save(threegram, file="threegramTF.RData")
# save(fourgram, file="fourgramTF.RData")



## removing every gram tied for last

onegram <- onegram[-(which.min(onegram[,2]):nrow(onegram)),]
twogram <- twogram[-(which.min(twogram[,2]):nrow(twogram)),]
threegram <- threegram[-(which.min(threegram[,2]):nrow(threegram)),]
fourgram <- fourgram[-(which.min(fourgram[,2]):nrow(fourgram)),]

onegram$gram <- as.character(onegram$gram)
twogram$gram <- as.character(twogram$gram)
threegram$gram <- as.character(threegram$gram)
fourgram$gram <- as.character(fourgram$gram)












write.table(fourgram, file = "fourgramtftr4.txt")
write.table(threegram, file = "threegramtftr4.txt")
write.table(twogram, file = "twogramtftr4.txt")
write.table(onegram, file = "onegramtftr4.txt")

fourgram <- data.frame(fread("fourgramtftr4.txt", select = c(2,3)))
threegram <- data.frame(fread("threegramtftr4.txt",select = c(2,3)))
twogram<-data.frame(fread("twogramtftr4.txt",select = c(2,3)))
onegram<-data.frame(fread("onegramtftr4.txt",select = c(2,3)))


names(onegram)<- c("gram","freq")
names(twogram)<- c("gram","freq")
names(threegram)<- c("gram","freq")
names(fourgram)<- c("gram","freq")