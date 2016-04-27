interpo <- function(guess) {
l1 <- .8
l2 <- .1
l3 <- .05
l4 <-.05
        
quads <- data.table(grep(guess, fourgram[,1], value = TRUE))
names(quads) <- c("gram")
quads$gram <- as.character(quads$gram)

if (nrow(quads) > 0) {

tris <- data.table(apply(quads, 1, function(x) paste(unlist(strsplit(x, split="_"))[2],
                                                unlist(strsplit(x, split="_"))[3],
                                                unlist(strsplit(x, split="_"))[4],sep="_")))
names(tris) <- c("gram")
tris$gram <- as.character(tris$gram)


bis <- data.table(apply(tris, 1, function(x) paste(unlist(strsplit(x, split="_"))[2],
                                                        unlist(strsplit(x, split="_"))[3], sep = "_")))
names(bis) <- c("gram")
bis$gram <- as.character(bis$gram)


unis <- data.table(apply(bis, 1, function(x) paste(unlist(strsplit(x, split="_"))[2])))
names(unis) <- c("gram")
unis$gram <- as.character(unis$gram)


preds<-data.frame(cbind(unis$gram,l1*merge(quads, fourgram, by = "gram")$freq+l2*merge(tris,threegram, by = "gram")$freq
             +l3*merge(bis, twogram, by = "gram")$freq+l4*merge(unis,onegram, by = "gram")$freq))

names(preds)<- c("gram", "freq")
preds <<- preds[order(preds$freq, decreasing = TRUE),]
preds$gram <<- as.character(preds$gram)
#preds[1,1]

}

else {
        guess <- unlist(strsplit(guess, split = "_"))
        if (sum(guess == "NA") > 0) {
                guess <- paste(guess[1],guess[2],sep = "_")
                guess <- paste(guess, "_", sep = "")
        }
        else {
                guess <- paste(guess[2],guess[3], sep = "_")
                guess <- paste("^", guess, "_", sep = "")
        }
        
        tris <- data.table(grep(guess, threegram[,1], value = TRUE))
        names(tris) <- c("gram")
        tris$gram <- as.character(tris$gram)
        
        if (nrow(tris) > 0) { 
        
        bis <- data.table(apply(tris, 1, function(x) paste(unlist(strsplit(x, split="_"))[2],
                                                           unlist(strsplit(x, split="_"))[3], sep = "_")))
        names(bis) <- c("gram")
        bis$gram <- as.character(bis$gram)
        
        
        unis <- data.table(apply(bis, 1, function(x) paste(unlist(strsplit(x, split="_"))[2])))
        names(unis) <- c("gram")
        unis$gram <- as.character(unis$gram)
        
        
        preds<-data.frame(cbind(unis$gram,(l1+l2)*merge(tris,threegram, by = "gram")$freq
                                +l3*merge(bis, twogram, by = "gram")$freq+l4*merge(unis,onegram, by = "gram")$freq))
        
        names(preds)<- c("gram", "freq")
        preds <<- preds[order(preds$freq, decreasing = TRUE),]
        preds$gram <<- as.character(preds$gram)
        #preds[1,1]
        }
        
        else { 
                guess <- unlist(strsplit(guess, split = "_"))
                if (sum(guess == "NA") > 0) {
                        guess <- paste(guess[1],sep = "_")
                        guess <- paste(guess, "_", sep = "")
                }
                else {
                        guess <- paste(guess[2],sep = "_")
                        guess <- paste("^", guess, "_", sep = "")
                }
                
                bis <- data.table(grep(guess, twogram[,1], value = TRUE))
                names(bis) <- c("gram")
                bis$gram <- as.character(bis$gram)
                
                if (nrow(bis) > 0) {
                
                unis <- data.table(apply(bis, 1, function(x) paste(unlist(strsplit(x, split="_"))[2])))
                names(unis) <- c("gram")
                unis$gram <- as.character(unis$gram)
                
                
                preds<-data.frame(cbind(unis$gram,(l1+l2+l3)*merge(bis, twogram, by = "gram")$freq+l4*merge(unis,onegram, by = "gram")$freq))
                
                names(preds)<- c("gram", "freq")
                preds$gram <- as.character(preds$gram)
                preds <<- preds[order(preds$freq, decreasing = TRUE),]
                #preds[1,1]
                }
                
                else if (nrow(bis) == 0) { 
                        preds <<- "the"
                        }
}





}
as.character(preds[1,1])
}