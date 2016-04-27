# Pulls n-1 gram and looks for frequency in n-1gram table to calculate proper frequencies

# two gram
splitwordstwo <- data.frame(apply(twogram, 1, function(x) unlist(strsplit(x, split="_"))[1]), 
                            stringsAsFactors=FALSE)

names(splitwordstwo) <- c("gram")
splitwordstwo$gram <- as.character(splitwordstwo$gram)

twomingramfreq<-left_join(splitwordstwo, onegram)

twogram$minfreq <- twomingramfreq[,2]
twogram$newfreq <- twogram$freq/twogram$minfreq
twogram <- twogram[order(twogram[,4], decreasing = TRUE),]
twogram <- twogram[,c(-2,-3)]

onegram$gram <- as.character(onegram$gram)
twogram$gram <- as.character(twogram$gram)

# three gram
splitwordsthree <- data.frame(apply(threegram, 1, function(x) paste(unlist(strsplit(x, split="_"))[1],
                                                                          unlist(strsplit(x, split="_"))[2], sep = "_"))
                              ,stringsAsFactors=FALSE)

names(splitwordsthree) <- c("gram")
splitwordsthree$gram <- as.character(splitwordsthree$gram)

threemingramfreq<-left_join(splitwordsthree, twogram)

threegram$minfreq <- threemingramfreq[,2]
threegram$newfreq <- threegram$freq/threegram$minfreq
threegram <- threegram[order(threegram[,4], decreasing = TRUE),]
threegram <- threegram[,c(-2,-3)]

threegram$gram <- as.character(threegram$gram)



# Four gram

splitwordsfour <- data.frame(apply(fourgram, 1, function(x) paste(unlist(strsplit(x, split="_"))[1],
                                                                  unlist(strsplit(x, split="_"))[2],
                                                                  unlist(strsplit(x, split="_"))[3],sep="_")),
                             stringsAsFactors=FALSE)

names(splitwordsfour) <- c("gram")
splitwordsfour$gram <- as.character(splitwordsfour$gram)

fourmingramfreq<-left_join(splitwordsfour, threegram)

fourgram$minfreq <- fourmingramfreq[,2]
fourgram$newfreq <- fourgram$freq/fourgram$minfreq
fourgram <- fourgram[order(fourgram[,4], decreasing = TRUE),]
fourgram <- fourgram[,c(-2,-3)]

fourgram$gram <- as.character(fourgram$gram)

write.table(fourgram, file = "fourgramcf4.txt")
write.table(threegram, file = "threegramcf4.txt")
write.table(twogram, file = "twogramcf4.txt")
write.table(onegram, file = "onegramcf4.txt")

fourgram <- data.frame(fread("fourgramcf.txt", select = c(2,3)))
threegram <- data.frame(fread("threegramcf.txt",select = c(2,3)))
twogram<-data.frame(fread("twogramcf.txt",select = c(2,3)))
onegram<-data.frame(fread("onegramcf.txt",select = c(2,3)))