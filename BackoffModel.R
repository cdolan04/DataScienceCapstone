library(doParallel)
        ### Studpid Backoff model

      
predfinal <- function(input) {
        
        ### Give n-gram used for predicition
        predgram <- grep(guess, fourgram[,1], value = TRUE)[1]
        
        ## Gives prediction
        pred <<- unlist(strsplit(predgram, split = "_"))[length(unlist(strsplit(predgram, split =
                                                                                        "_")))]
        # Checks for prediction and if there isnt one it backs off to the next n gram
        if (is.na(pred)) {
                guess <- unlist(strsplit(guess, split = "_"))
                if (sum(guess == "NA") > 0) {
                        guess <- paste(guess[1],guess[2],sep = "_")
                        guess <- paste(guess, "_", sep = "")
                }
                else {
                        guess <- paste(guess[2],guess[3], sep = "_")
                        guess <- paste("^", guess, "_", sep = "")
                }
                
                
                ### Give n-gram used for predicition
                predgram <- grep(guess, threegram[,1], value = TRUE)[1]
                
                ## Gives prediction
                pred <<- unlist(strsplit(predgram, split = "_"))[length(unlist(strsplit(predgram, split =
                                                                                                "_")))]
        }
        # Checks for prediction and if there isnt one it backs off to the next n gram
        if (is.na(pred)) {
                guess <- unlist(strsplit(guess, split = "_"))
                if (sum(guess == "NA") > 0) {
                        guess <- paste(guess[1], sep = "_")
                        guess <- paste(guess,"_", sep = "")
                }
                else {
                        guess <- paste(guess[2], sep = "_")
                        guess <- paste("^", guess, "_", sep = "")
                }
                
                
                
                ### Give n-gram used for predicition
                predgram <- grep(guess, twogram[,1], value = TRUE)[1]
                
                ## Gives prediction
                pred <<- unlist(strsplit(predgram, split = "_"))[length(unlist(strsplit(predgram, split =
                                                                                                "_")))]
        
        } 
        # uses most common words as alternative since there is no other info available
        if (is.na(pred))   {
                pred <<- onegram[sample(nrow(onegram), 1), ][1,1]
        }
        pred
}
        
        

