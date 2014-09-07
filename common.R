# THIS FILE HOLDS ALL THE COMMON LEVEL1 FUNCTIONS THAT WILL BE USED FOR TEXT ANALYTICS
# THE FUNCTIONS HAVE BEEN ARRANGED IN GROUPS

# TDM, TTM, WORD COUNT ----


# This function takes a text vector as input
# performs 4 cleaning functions on it
# returns the corpus
# Author: Tejas Mahajan
stdTextCleaning <- function(text){
    text <- tolower(text)
    text <- removeWords(text,  stopwords("english"))
    text <- removePunctuation(text)
    text <- stripWhitespace(text)
}

# This function takes a corpus as input
# performs 4 cleaning functions on it
# returns the corpus
# Author: Tejas Mahajan
stdCorpusCleaning <- function(corpus){
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removeWords, stopwords("english")) 
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    return(corpus)
}


# This function takes a text source as input and creates a tdm from it
# Text source should be a one dimensional text array, such as a column of a data frame
# In the TDM, wordGroupSize will represent the size of the terms.
# For example, wordGroupSize = 1 will create a TDM with single words,
# 2 will create terms composed of word pairs and so on
# returns the tdm
# Author: Tejas Mahajan
createTDM <- function(textSource, wordGroupSize = 1){
    
    library(RWeka)
    library(tm)
    # form the first tdm
    maxRange <- 5000
    if (length(textSource) <= 5000) maxRange <- length(textSource)
    
    searchTerms <- Corpus(VectorSource(textSource[1:maxRange]), list(reader = readPlain))
    searchTerms <- stdCorpusCleaning(searchTerms)
    
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = wordGroupSize, max = wordGroupSize))
    tdmB <- TermDocumentMatrix(searchTerms, control = 
                                   list(tokenize = BigramTokenizer, wordLengths=c(2, Inf)))
    
    # if the length of text is more than 
    if (length(textSource) > 5000){
        maxCount = ceiling(length(textSource) / 5000)
        for (i in 1:(maxCount - 1)){
            
            minRange <- i*5000 + 1
            maxRange <- (i+1)*5000
            if (length(textSource) < (i+1)*5000) maxRange <- length(textSource)
            
            searchTerms <- Corpus(VectorSource(textSource[minRange:maxRange]), list(reader = readPlain))
            searchTerms <- stdCorpusCleaning(searchTerms)
            tdm <- TermDocumentMatrix(searchTerms, control = list(tokenize = BigramTokenizer))
            tdmB <- c(tdmB, tdm)
        }
    }
    return(tdmB)
}
# The following function was an attempt at improving the performance of createTDM function
# by replacing the for loop with lapply
# However, no noticeable improvement was seen
# The function is being saved as reference for future work
# Author: Jayant Phate
# my_createTDM<-function(textSource,wordGroupSize = 1,chunkSize=5000){
#     
#     library(RWeka)
#     library(tm)
#     BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = wordGroupSize, max = wordGroupSize))
#     
#     loop_range<-1:ceiling(length(textSource)/chunkSize)
#     loop_range<-c(0,chunkSize,(loop_range[-1]*chunkSize))
#     loop_range[length(loop_range)]<-length(textSource)  
#     
#     sample_tdm<- lapply(2:length(loop_range),function(x)
#     {
#         searchTerms <- Corpus(VectorSource(textSource[(loop_range[x-1]+1):loop_range[x]]), list(reader = readPlain)) 
#         searchTerms <- stdCorpusCleaning(searchTerms)
#         tdm <- TermDocumentMatrix(searchTerms, control = list(tokenize = BigramTokenizer))
#     })
#     var<-do.call('c',sample_tdm)
# }

# The following function was an attempt at improving the performance of createTDM function
# by replacing the for loop with tapply
# However, no noticeable improvement was seen
# The function is being saved as reference for future work
# Author: Tejas Mahajan
# createTDM2 <- function(textSource, wordGroupSize = 1){
#     
#     library(RWeka)
#     library(tm)
#     
#     #textSource <- as.data.frame(textSource)
#     # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = wordGroupSize, max = wordGroupSize))
#     
#     tdm.list <- tapply(textSource, 
#                        #ceiling(as.numeric(rownames(as.data.frame(textSource))) / 500),
#                        ceiling(seq(1, length(textSource)) / 5000),
#                        function(x) {
#             searchTerms <- Corpus(VectorSource(x), list(reader = readPlain)) 
#             searchTerms <- stdCorpusCleaning(searchTerms)
#             TermDocumentMatrix(searchTerms) 
#                         })
#     tdmB <- do.call('c', tdm.list)
# }


# The difference between this function and the createTDM function is that the text corpus
# will be stemmed before creating the TDM. The TDM will be composed of stems instead
# of actual words.
# Author: Tejas Mahajan
createStemmedTDM <- function(textSource, wordGroupSize = 1){
    
    library(RWeka)
    library(tm)
    # form the first tdm
    maxRange <- 5000
    if (length(textSource) <= 5000) maxRange <- length(textSource)
    
    searchTerms <- Corpus(VectorSource(textSource[1:maxRange]), list(reader = readPlain))
    searchTerms <- stdCorpusCleaning(searchTerms)
    searchTerms <- tm_map(searchTerms, stemDocument)
    
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = wordGroupSize, max = wordGroupSize))
    tdmB <- TermDocumentMatrix(searchTerms, control = list(tokenize = BigramTokenizer))
    
    # if the length of text is more than 
    if (length(textSource) > 5000){
        maxCount = ceiling(length(textSource) / 5000)
        for (i in 1:(maxCount - 1)){
            
            minRange <- i*5000 + 1
            maxRange <- (i+1)*5000
            if (length(textSource) < (i+1)*5000) maxRange <- length(textSource)
            
            searchTerms <- Corpus(VectorSource(textSource[minRange:maxRange]), list(reader = readPlain))
            searchTerms <- stdCorpusCleaning(searchTerms)
            searchTerms <- tm_map(searchTerms, stemDocument)
            tdm <- TermDocumentMatrix(searchTerms, control = list(tokenize = BigramTokenizer))
            tdmB <- c(tdmB, tdm)
        }
    }
    return(tdmB)
}


# This function creates a symmetric term-term matrix (TTM) from a word pair list
# a TTM is required for network plots
# Author: Tejas Mahajan
createTTMfromWordPairList <- function(wordPairList){
    
    library("Matrix")
    # remove entries with 'NA' and get all unique words
    wordPairList <- wordPairList[!is.na(wordPairList[, 1]) | !is.na(wordPairList[, 2]), ]
    words <- c(as.character(wordPairList[, 1]), as.character(wordPairList[, 2]))
    words <- unique(words)
    
    ttm <- Matrix(0, nrow = length(words), ncol = length(words))
    rownames(ttm) <- words
    colnames(ttm) <- words
    
    for (i in 1:dim(wordPairList)[1]){
        r <- which(rownames(ttm) == wordPairList[i,1])
        c <- which(rownames(ttm) == wordPairList[i,2]) 
        ttm[r,c] <- wordPairList[i,3]
        ttm[c,r] <- wordPairList[i,3] 
    }
    
    return(ttm)
}


# This function will get the row sums for the tdm
# if colSweepFactors are specified,
# the tdm columns will be 'sweeped' with these factors before taking the row sum
# The sweepFactors are 1 dimensional arrays with the length same as the number of columns of the tdm
# the splitSize default is 500. Set it to a smaller value if the input data is very large.
# As a benchmark, for input size above 100,000 rows, use splitSize = 50
# output is a dataframe consisting of word counts and scaled word counts for all the specified colSweepFactors
# Author: Tejas Mahajan
getWordCount <- function(tdm, colSweepFactor1 = NA, colSweepFactor2 = NA, colSweepFactor3 = NA,
                         splitSize = 500){
    
    library("Matrix")
    
    rowCount <- dim(tdm)[1]
    itr <- 1
    totalCount <- NA
    if (hasArg(colSweepFactor1)) totalFactor1 <- NA
    if (hasArg(colSweepFactor2)) totalFactor2 <- NA
    if (hasArg(colSweepFactor3)) totalFactor3 <- NA
    
    # NOTE: Profiling revealed that sweep and rowSums are the two most
    # time intensive operations.
    
    # break down the tdm into sets of lesser number of terms and get word count for each group
    while (itr <= rowCount){
        
        upperBound <- itr + splitSize - 1
        if (upperBound > rowCount){
            upperBound <- rowCount
        }
        
        sub_tdm <- tdm[itr:upperBound,]
        grpCount <- rowSums(as.matrix(sub_tdm))
        totalCount <- append(totalCount, grpCount)
        
        #multiply tdms by sweepFactors
        if (!is.na(colSweepFactor1[1]) &!missing(colSweepFactor1)){
            sub_tdmc <- sub_tdm
            sub_tdmc <- sweep(sub_tdmc, 2, as.numeric(colSweepFactor1), "*")
            grpFactor <- rowSums(as.matrix(sub_tdmc))
            totalFactor1 <- append(totalFactor1, grpFactor)
        }
        if (!is.na(colSweepFactor2[1]) & !missing(colSweepFactor2)){
            sub_tdmc <- sub_tdm
            sub_tdmc <- sweep(sub_tdmc, 2, as.numeric(colSweepFactor2), "*")
            grpFactor <- rowSums(as.matrix(sub_tdmc))
            totalFactor2 <- append(totalFactor2, grpFactor)
        }
        if (!is.na(colSweepFactor3[1]) & !missing(colSweepFactor3)){
            sub_tdmc <- sub_tdm
            sub_tdmc <- sweep(sub_tdmc, 2, as.numeric(colSweepFactor3), "*")
            grpFactor <- rowSums(as.matrix(sub_tdmc))
            totalFactor3 <- append(totalFactor3, grpFactor)
        }
        
        itr <- itr + splitSize
        
    }
    
    #totalCount <- as.matrix(totalCount[-1, ], drop = F)
    totalCount <- as.matrix(totalCount[2:length(totalCount)], drop = F)
    #  data <- totalCount
    #   if (!missing(colSweepFactor1)){
    #     totalFactor1 <- as.matrix(totalFactor1[2:length(totalFactor1)])
    #     data <- cbind(data, totalFactor1)  
    #   }
    #   if (!missing(colSweepFactor2)){
    #     totalFactor2 <- as.matrix(totalFactor2[2:length(totalFactor2)])
    #     data <- cbind(data, totalFactor2)  
    #   }
    #   if (!missing(colSweepFactor3)){
    #     totalFactor3 <- as.matrix(totalFactor3[2:length(totalFactor3)])
    #     data <- cbind(data, totalFactor3)  
    #   }
    
    data <- totalCount
    if (!is.na(colSweepFactor1[1]) & !missing(colSweepFactor1)){
        totalFactor1 <- as.numeric(totalFactor1[2:length(totalFactor1)])
        data <- cbind(data, totalFactor1)  
    }
    if (!is.na(colSweepFactor2[1]) & !missing(colSweepFactor2)){
        totalFactor2 <- as.numeric(totalFactor2[2:length(totalFactor2)])
        data <- cbind(data, totalFactor2)  
    }
    if (!is.na(colSweepFactor3[1]) & !missing(colSweepFactor3)){
        totalFactor3 <- as.numeric(totalFactor3[2:length(totalFactor3)])
        data <- cbind(data, totalFactor3)  
    }
    colnames(data)[1] <- "totalCount"
    
    return(data)
}

# This function takes a 2-dimensional frame as input (typically the output of a word count)
# Finds duplicates in the words (column number specified by wordCol. If the names are in rowNames, enter 'rownames' here)
# Removes the duplicates and adds up the values from duplicates (if 'Add' is TRUE)
# The columns where addition is to be performed can be specified in the form of list of column numbers
# Author: Tejas Mahajan
# NOTE: THE 'ADD' PARAMETERS IS NOW USELESS AND WILL BE REMOVED IN LATER VERSIONS
removeDuplicatesInWordCount <- function(data, wordCol, add = TRUE, addColList){
    
    # Get the data from the specified column
    if (wordCol == tolower('rownames')) names <- rownames(data)
    else names <- data[, wordCol]
    names <- do.call(rbind, str_split(names, ' '))
    
    # Order individual words within the word column 
    orderedNames <- NULL
    for(i in 1:dim(names)[1]){
        orderedNames[i] <- paste(sort(names[i,]), collapse='-')
    }
    
    # combine the names with the data and then sort data by names
    data <- cbind(data, orderedNames)
    data <- data[order(orderedNames), ]
    ordCol <- which(colnames(data) %in% "orderedNames")
    
#     for (i in 1:(nrow(data)-1)){
#         # if the names in consequtive columns is the same, 
#         if (data[c, ordCol] == data[c+1, ordCol]){
#             # if add = TRUE, add up the specified columns
#             if (add == TRUE){
#                 for (j in addColList){
#                     data[c, j] <- as.numeric(data[c, j]) + 
#                         as.numeric(data[(c+1), j])
#                 }
#             }
#             # remove the repeated row
#             data <- data[-(c+1),]
#         } else{
#             c <- c + 1
#         }
#     }

    # The above for loop has been replaced by the function call below    
    data <- combineRepeatedRows(data, ordCol, addColList, NA)
    # remove the extra column and return 'data'
    data <- as.data.frame(data[, -ordCol])
}


# This function takes an array of text, a 1-dimensional array
# Returns another array with the count of words per element
# Author: Tejas Mahajan
countWords <- function(textArray){
    textArray <- gsub(' {2,}', ' ', textArray) # replaces 2 or more spaces with a single space
    wordCounts <- sapply(strsplit(textArray, ' '), length)
}


# DATA SUMMARIZING ----

# This function will combine values from rows that have the same text in consequtive
# rows of the specified column.
# Text values of the specified columns would be appended. Numerical values from
# the specified columns would be added.
# Inputs required
# 1. data - the data frame which is to be treated
# 2. wordCol - the column in which consecutive values are to be checked for similarity. This is a column number.
# 3. textColToAppend - the values from these columns would be appended. This is an array of column numbers/names.
# 4. numColToAdd - the values from these columns would be added. This is an array of column numbers/names.
# Author: Tejas Mahajan
combineRepeatedRows <- function(data, wordCol, numColToAdd = NA, textColToAppend = NA) {
    
    # order the data based on the word column
    data <- data[order(data[, wordCol]), ]
    # if only one element is passed, return data
    if (nrow(data) == 1) return(data)
    
    counter <- 1
    
    for (i in 1:(nrow(data)-1)){
        # if the names in consequtive columns is the same, 
        # using as.character before comparing it is making this comparison run orders of magnitudes faster
        if (as.character(data[counter, wordCol]) == as.character(data[counter+1, wordCol])) {
            # add values from the numerical columns
            if (!is.na(numColToAdd[1])) {
                for (j in numColToAdd){
                    data[counter, j] <- as.numeric(data[counter, j]) + 
                                  as.numeric(data[(counter+1), j])
                }
            }
            # append values from the text columns
            if (!is.na(textColToAppend[1])) {
                for (j in textColToAppend) {
                    data[counter, textColToAppend] <- as.character(paste(data[counter, textColToAppend],
                                                                   data[counter+1, textColToAppend], sep = ''))   
                }
            }
            # remove the repeated row
            data <- data[-(counter+1),]
        } else{
            counter <- counter + 1
        }
    }
    return(data)
}


# FORMAT DATA ----

# This function takes a text array in one of the columns has words are separated by space. 
# Returns new array with separated word Columns
# Number of columns returned is equal to wordGroupSize
# Author : Indrajit Patil
splitColumn <- function(textArray, textColumn, wordGroupSize){
    
    library(stringr)
    # if rownames is textColumn
    if(textColumn == tolower('rownames')){ 
        column <- row.names(textArray)
    }else{
        column <- textArray[ , textColumn]
        
        textArray <- textArray[-which(colnames(textArray) %in% c(textColumn))]
    }
    word.Columns <- NULL
    word.Columns <- do.call(rbind, str_split(column, ' '))
    word.Columns <- as.data.frame(word.Columns)
    
    if(wordGroupSize == 1){ #only one word
        colnames(word.Columns) <- "Word"
    }else if(wordGroupSize == 2){ # two words are separated
        colnames(word.Columns) <- c("Word1", "Word2")
    }else if(wordGroupSize == 3){ # Three words separated
        colnames(word.Columns) <- c("Word1", "Word2", "Word3")
    }
    
    data <- cbind(word.Columns, textArray)
}


# PHRASE EXTRACTION ----

# This function takes inputs:
# 1. data - Data containing the phraseList column
# 2. textColumn - Column to be searched for phrases
# 3. Match.Whole.Word - Boolean (TRUE/FALSE). TRUE if the whole word is to be matched.
# For example, "laptop" will not match "laptops" if Match.Whole.Word is TRUE.
# word1, word2 (optional), word3 (optional) - words to be searched
# Returns an array of rows whith indexes of Phrases that contain given words
# Author: Indrajit Patil
extractPhraseIndex<- function(data, textColumn, Match.Whole.Word=TRUE, word1, word2='', word3='' ){
    
    data <- data 
    inputColumn <- data[, textColumn]
    #For exact word matching
    if(Match.Whole.Word == TRUE){
        word1 <- paste('\\b', word1, '\\b', sep='')
        word2 <- paste('\\b', word2, '\\b', sep='')
        word3 <- paste('\\b', word3, '\\b', sep='')
    }
    # 3 Words can appear in 6 combinations in data; we find phrases for each  and bind them together
    wordList1<- paste('.*', word1, '.*', word2, '.*', word3, sep='')
    wordIndex1 <- as.data.frame(grep(wordList1, ignore.case = TRUE, inputColumn, value=FALSE))
    
    wordList2<- paste('.*', word1, '.*', word3, '.*', word2, sep='')
    wordIndex2 <- as.data.frame(grep(wordList2, ignore.case = TRUE, inputColumn, value=FALSE))
    
    wordList3<- paste('.*', word2, '.*', word1, '.*', word3, sep='')
    wordIndex3 <- as.data.frame(grep(wordList3, ignore.case = TRUE, inputColumn, value=FALSE))
    
    wordList4<- paste('.*', word2, '.*', word3, '.*', word1, sep='')
    wordIndex4 <- as.data.frame(grep(wordList4, ignore.case = TRUE, inputColumn, value=FALSE))
    
    wordList5<- paste('.*', word3, '.*', word1, '.*', word2, sep='')
    wordIndex5 <- as.data.frame(grep(wordList5, ignore.case = TRUE, inputColumn, value=FALSE))
    
    wordList6<- paste('.*', word3, '.*', word2, '.*', word1, sep='')
    wordIndex6 <- as.data.frame(grep(wordList6, ignore.case = TRUE, inputColumn, value=FALSE))
    colnames(wordIndex1) <- colnames(wordIndex2) <- colnames(wordIndex3) <- 
        colnames(wordIndex4) <- colnames(wordIndex5) <- colnames(wordIndex6) <- "PhraseIndexes"
    #Bind without duplicates  
    phraseIndexes <- unique(rbind(wordIndex1, wordIndex2, wordIndex3, wordIndex4, wordIndex5, wordIndex6))
    return(phraseIndexes)
}

# This function takes inputs:
# 1. data - Data containing the phraseList column
# 2. textColumn - Column to be searched for phrases
# 3. Match.Whole.Word - Boolean (TRUE/FALSE). TRUE if the whole word is to be matched.
# For example, "laptop" will not match "laptops" if Match.Whole.Word is TRUE.
# word1, word2 (optional), word3 (optional) - words to be searched
# Returns an array of rows whith Phrases that contain given words
# Author: Indrajit Patil
extractPhrasesForWords <- function(data, textColumn, Match.Whole.Word=TRUE, word1, word2='', word3='' ){
    
    phraseIndexes <- extractPhraseIndex(data, textColumn, Match.Whole.Word, word1, word2, word3 )
    phrases <- data[phraseIndexes[,1], ]
    return(phrases)
}


# WORD ASSOCIATIONS FROM MULTI-WORD COUNT LISTS ----

# This function takes input as wordPair list which contains pair of "words" and "columnName" of frequency
# This function returns a data frame list in which first column will be given "word1" and 
#         second column contains associated word came with given word 
#         and third column contains frequency of that associated word
# Author: Indrajit Patil
assocFromPairs <- function(word1, wordPairList, columnName){
    
    word_match1 <- wordPairList[which(wordPairList$Word1 == word1 | wordPairList$Word2 == word1),]
    if (dim(word_match1)[1] != 0){
        word_match <- data.frame(Word1 = word1,
                                 Word2=sapply(1:nrow(word_match1),
                                              function(i){
                                                  x <- as.character(word_match1[i,1:2])
                                                  res <- x[ !x %in% c(word1)]
                                                  
                                                  if(length(res) == 0){
                                                      res <- aggregate(x,list(x),length)
                                                      res[ res$x == 2, 1]
                                                  }else res
                                              }),
                                 columnName = word_match1[columnName])
    }else{
        word_match<-data.frame(Word1=word1, Word2='', columnName=1)
    }
    
    colnames(word_match) <- c('Word1', 'Word2', columnName)
    return(word_match)
}

# This function takes input as word Triplet list which contains triplets of "words" and "columnName" of frequency
# This function returns a data frame list in which first 2 columns will be given words "word1" and "word2";
#         third column contains associated word came with given words 
#         and fourth column contains frequency of that associated word
# Author: Indrajit Patil
assocFromTriplets <- function(word1, word2, wordTripletList, columnName){
    
    word_match1<-wordTripletList[which(wordTripletList$Word1 == word1 | wordTripletList$Word2 == word1 |
                                           wordTripletList$Word3 == word1),]
    word_match1<-word_match1[which(word_match1$Word1 == word2 | word_match1$Word2 == word2 |
                                       word_match1$Word3 == word2),]
    word_match <- NULL
    if(dim(word_match1)[1] != 0){
        word_match<- data.frame( Word1=word1, Word2=word2,
                                 Word3=sapply(1:nrow(word_match1),
                                              function(i){
                                                  x <- as.character(word_match1[i,1:3])
                                                  res <- x[ !x %in% c(word1,word2)]
                                                  
                                                  if(length(res) == 0){
                                                      res <- aggregate(x,list(x),length)
                                                      res[ res$x == 2, 1]
                                                  }else res
                                              }),
                                 columnName = word_match1[columnName])
    }else{
        word_match<-data.frame(Word1=word1, Word2=word2, Word3='', columnName=1)
    }
    colnames(word_match) <- c('Word1', 'Word2', 'Word3', columnName)
    return(word_match)
}

# This function takes input as word Quadruplet list which contains quadruplets of "words" and "columnName" of frequency
# This function returns a data frame list in which first 2 columns will be given words "word1", "word2", "word3";
#         Fourth column contains associated word came with given words 
#         and last column contains frequency of that associated word
# Author: Indrajit Patil
assocFromQuaduplets <- function(word1, word2, word3, wordQuadrupletList, columnName){
    
    word_match1<-wordQuadrupletList[which(wordQuadrupletList$Word1 == word1 | wordQuadrupletList$Word2 == word1 |
                                              wordQuadrupletList$Word3 == word1 | wordQuadrupletList$Word4 == word1),]
    word_match1<-word_match1[which(word_match1$Word1 == word2 | word_match1$Word2 == word2 |
                                       word_match1$Word3 == word2 | word_match1$Word4 == word2),]
    word_match1<-word_match1[which(word_match1$Word1 == word3 | word_match1$Word2 == word3 |
                                       word_match1$Word3 == word3 | word_match1$Word4 == word3),]
    word_match <- NULL
    if(nrow(word_match1)!=0)
    {
        word_match<-data.frame(
            Word1=word1,  Word2=word2, Word3=word3,
            Word4=sapply(1:nrow(word_match1),
                         function(i){
                             x <- as.character(word_match1[i,1:4])
                             res <- x[ !x %in% c(word1,word2,word3)]
                             
                             if(length(res) == 0){
                                 res <- aggregate(x,list(x),length)
                                 res[ res$x == 2 |3, 1]
                             }else res
                         }),
            columnName = word_match1[columnName])
    }else{
        word_match<-data.frame(Word1=word1, Word2=word2, Word3=word3, Word4='', columnName=1)
    }
    colnames(word_match) <- c('Word1', 'Word2', 'Word3', 'Word4', columnName)
    
    return(word_match)
}

# This function returns a list of associated words of the given word with their co-occurance frequency
# Inputs - word(s) (whole associations are to be found)
#  - Path of wordPairCount file, tripletFile, quadrupletFile to extract the associations from.
#  - column name of 'frequency' column 
# If only one word is given, output is a list of words associated with it.
# The output format has the given word in column1, the associated words in column2 and the frequency in column3
# similarly, if two words are given as input, output will have 4 columns
# for three input words, output will have 5 columns
# Author : Indrajit Patil
getAssoForWord <- function(word1, wordPairFileName, columnName, word2='', word3='', tripletFile='', quadrupletFile=''){
    
    if(word1!=''& word2 == ''& word3 == ''){
        word_input <- read.csv(file=wordPairFileName, header = TRUE, stringsAsFactors = FALSE)
        word_input <- word_input[c('Word1', 'Word2', columnName)]
        
        # Use of subfunction getting association and making data frame of it.
        word_match <- assocFromPairs(word1=word1, wordPairList=word_input, columnName=columnName)
        
    }else if(word1!=''& word2!=''& word3 == ''){
        
        word_input<- read.csv(file=tripletFile, header=TRUE, stringsAsFactors=FALSE)
        word_input <- word_input[c("Word1", "Word2", "Word3", columnName)]
        
        # Use of subfunction getting association and making data frame of it.
        word_match <- assocFromTriplets(word1=word1, word2=word2, wordTripletList=word_input, columnName=columnName)
        
    }else if(word1!=''& word2!=''& word3!=''){
        
        word_input <- read.csv(file=quadrupletFile,header=TRUE, stringsAsFactors=FALSE)
        word_input <- word_input[c("Word1", "Word2", "Word3", "Word4", columnName)]
        
        # Use of subfunction getting association and making data frame of it.
        word_match <- assocFromQuaduplets(word1=word1, word2=word2, word3=word3, wordQuadrupletList=word_input, columnName=columnName) 
    }
    
    return(word_match)
}

# This function takes inputs:
#                             1. wordList file 
#                             2. wordPair File
#                             3. Column Name of which count is to be shown in output
#                             4. minFreq- minimum count of above column
#                             5. Number of associations 
#                             6. Whether you want semi-colon seperated output
# Returns an array which contains associated words for wordList
# Author : Indrajit Patil
getAssoForWordList <- function( listFileName, wordPairFileName, columnName,
                                minFreq, numAsso, semiColonSepOutput){
    
    # read the list of words
    wordsForAsso <- read.csv(file=listFileName, header = TRUE, stringsAsFactors = FALSE)
    
    # read the pair file
    word_pairs <- read.csv(file=wordPairFileName, header = TRUE, stringsAsFactors = FALSE)
    word_pairs <- word_pairs[ c("Word1", "Word2", columnName)]
    
    # get associations for the words and write to file
    
    associations <- NULL
    
    for (i in 1:dim(wordsForAsso)[1]){
        
        word1 <- wordsForAsso[i, 1]
        
        # Use of "getAssoForWord()" Function
        word_match <- getAssoForWord(word1=word1, wordPairFileName=wordPairFileName, columnName=columnName)
        #order data by frequency column
        word_match <- word_match[order(-word_match[, 3]), ]
        # Take maximum associations equal to "numAsso"
        word_match <- word_match[1 : numAsso, ]
        # Take associations greatter than "minFreq" 
        word_match <- word_match[which(word_match[ , columnName] > minFreq), ]
        
        if (semiColonSepOutput == TRUE) {
            # Remove first column of word1
            word_match <- word_match[, c(-1)]
            word_matchStr <- bindWithSemicolon(data=word_match)
            associations <- rbind(associations, c(word1, word_matchStr))
        }else associations <- rbind(associations, word_match)
        
    }
    colnames(associations) <- c("Word", paste("Associated words", '(',columnName, ')', sep=''))
    if (semiColonSepOutput == FALSE) {
        colnames(associations) <- c("Word", "Associated words", columnName)
    }
    
    return(associations)
}

# This function takes inputs:
#                             1. wordList file with pairs 
#                             2. wordTriplet File
#                             3. Column Name of which count is to be shown in output
#                             4. minCount- minimum count of above column
#                             5. Number of associations 
# Returns an array which contains associated words for wordList
# Author : Indrajit Patil
getAssoForWordPairList <- function( listFileName, wordPTripletFileName, 
                                    columnName, minFreq, numAsso){
    # read the list of words
    wordPairList <- read.csv(file=listFileName, header = TRUE, stringsAsFactors = FALSE)
    
    # read the triplet file
    word_triplets <- read.csv(file=wordPTripletFileName, header = TRUE, stringsAsFactors = FALSE)
    word_triplets <- word_triplets[ c("Word1", "Word2", "Word3", columnName)]
    
    associations <- NULL
    for (i in 1:dim(wordPairList)[1]){
        
        word1 <- wordPairList[i,1]
        word2 <- wordPairList[i,2]
        # Use of "assocFromTriplets" function
        word_match <- assocFromTriplets(word1=word1, word2=word2, wordTripletList=word_triplets, columnName="Total_Impressions")
        
        word_match <- word_match[order(-word_match[, 4]), ]
        word_match <- word_match[1 : numAsso, ]
        
        word_match <- word_match[, c(-1, -2)]
        word_match <- word_match[which(word_match[ , columnName] > minFreq), ]
        
        word_matchStr <- bindWithSemicolon(data=word_match)
        associations <- rbind(associations, c(word1, word2, word_matchStr))
    }
    
    colnames(associations) <- c("Word1", "Word2", paste("Associated words", '(',columnName, ')', sep=''))
    return(associations)
}

# This function takes two words and wordTriplet Count and quadruplet Count File  as a input
# (WARNING: Two words should not identical)
# Returns an array which contains associated word tree
# Author : Indrajit Patil
getAssoTreeForWord <- function(word1, word2, tripletFile, quadrupletFile, columnName){
    
    if(word2  ==  ''| word1 == '')
    {
        stop("Please specify word1 and word2")
    }
    
    word_input<- read.csv(file=tripletFile, header=TRUE, stringsAsFactors=FALSE)
    word_input <- word_input[c("Word1", "Word2", "Word3", columnName)]
    
    # Use of subfunction getting association and making data frame of it.
    word_match <- assocFromTriplets(word1=word1, word2=word2, wordTripletList=word_input, columnName= columnName)
    
    Words3<-as.character(word_match$Word3)
    
    assocTree <- NULL
    for( i in 1:length(Words3))
    {
        word3=Words3[i]
        word_input <- read.csv(file=quadrupletFile,header=TRUE, stringsAsFactors=FALSE)
        word_input <- word_input[c("Word1", "Word2", "Word3", "Word4", columnName)]
        
        # Use of subfunction getting association and making data frame of it.
        word_match <- assocFromQuaduplets(word1=word1, word2=word2, word3=word3, wordQuadrupletList=word_input, columnName= columnName)
        
        assocTree <- rbind(assocTree, word_match)
        
    }
    return(assocTree)
}

# This function takes inputs: single word frequency list, word pair frequency list, triplet word frequency list
# Single word freq file (wordFreqList) had two columns - words and frequency
# wordPairFreqList has three columns. word1, word2, frequency
# wordTripletFreqList had four columns. word1, word2, word3, freq
# WARNING:
#1. Provide wordFreqList and wordPairFreqList OR wordPairFreqList and wordTripletFreqList, not all three at the same time
#2. Please pass the non-needed parameter as NULL
# Returns an array which contains confidence and support for the required words/word groups.
# Author : Indrajit Patil
getConfidenceSupport <- function(wordFreqList, wordPairFreqList, wordTripletFreqList=NULL){  
    
    if(length(wordFreqList)>0 & length(wordPairFreqList)>0 & !length(wordTripletFreqList)){
        # Removing factors/Levels from wordFreqList
        wordFreqList_word<-as.character(wordFreqList$Word1)
        # Converting frequency column to numeric 
        wordPairFrequency <- as.numeric(as.character(wordPairFreqList[, 3]))
        # Creating empty data frames of same size as wordPairFreqList
        confidence1 <- data.frame(confidence = integer(dim(wordPairFreqList)[1]))
        confidence2 <- data.frame(confidence = integer(dim(wordPairFreqList)[1]))
        
        for(i in 1:dim(wordPairFreqList)[1])
        {
            word1 <- wordPairFreqList$Word1[i]
            indexWordFreqList <- which(wordFreqList_word == word1)
            
            if(length(indexWordFreqList)!=0){
                support_word <- wordFreqList[, 2][indexWordFreqList]
                conf <- wordPairFrequency[i]/support_word 
                confidence1[i, 1] <- conf
            }else{
                confidence1[i, 1] <- NA
            }
            
            word2 <- wordPairFreqList$Word2[i]
            indexWordFreqList <- which(wordFreqList_word == word2)
            
            if(length(indexWordFreqList)!=0){
                support_word <- wordFreqList[, 2][indexWordFreqList]
                conf <- wordPairFrequency[i]/support_word 
                confidence2[i, 1] <- conf
                
            }else{
                confidence2[i, 1] <- NA
            }    
        }
        
        #Building final data file with confidence and supports
        
        finalScore <- cbind(wordPairFreqList,confidence1,confidence2)
        
        #Renaming columns 
        colnames(finalScore) <- c("Word1","Word2","Frequency(Support)",
                                  "confidence(Word1->Word2)","confidence(Word2->Word1)")
        
    }else {
        
        # Removing factors/Levels from wordPairFreqList
        wordPairFreqList_Word1 <- as.character(wordPairFreqList$Word1)
        wordPairFreqList_Word2 <- as.character(wordPairFreqList$Word2)
        
        wordTripletFreqList_Frequency <- as.numeric(as.character(wordTripletFreqList[, 4]))
        
        # defining empty data frame of same size as wordTripletFreqList
        confidence1 <- data.frame(confidence = integer(dim(wordTripletFreqList)[1]))
        confidence2 <- data.frame(confidence = integer(dim(wordTripletFreqList)[1]))
        confidence3 <- data.frame(confidence = integer(dim(wordTripletFreqList)[1]))
        
        for(i in 1 : dim(wordTripletFreqList)[1])
        {
            #conf[(W1,W2)->W3]
            word1 <- wordTripletFreqList$Word1[i]
            word2 <- wordTripletFreqList$Word2[i]
            
            indexWordPairFreqList <- which(wordPairFreqList_Word1 == word1 & wordPairFreqList_Word2 == word2)
            if (length(indexWordPairFreqList) == 0){
                indexWordPairFreqList <- which(wordPairFreqList_Word1 == word2 & wordPairFreqList_Word2 == word1)
            }
            
            if (length(indexWordPairFreqList) != 0){
                support_word <- wordPairFreqList[, 3][indexWordPairFreqList]
                conf <- wordTripletFreqList_Frequency[i] / support_word 
                confidence1[i,1] <- conf
            }else{
                confidence1[i,1] <-'NA'
            }
            
            #conf[(W2,W3)->W1]
            word2 <- wordTripletFreqList$Word2[i]
            word3 <- wordTripletFreqList$Word3[i]
            
            indexWordPairFreqList <- which(wordPairFreqList_Word1 == word2 & wordPairFreqList_Word2 == word3)
            
            if (length(indexWordPairFreqList) == 0){
                indexWordPairFreqList <- which(wordPairFreqList_Word1 == word3 & wordPairFreqList_Word2 == word2)
            }
            
            if (length(indexWordPairFreqList)!= 0){
                support_word <- wordPairFreqList[, 3][indexWordPairFreqList]
                conf <- wordTripletFreqList_Frequency[i] / support_word 
                confidence2[i,1] <- conf
            }else{
                confidence2[i,1]<-'NA'
            }   
            
            #conf[(W1,W3)->W2]
            word1 <- wordTripletFreqList$Word1[i]
            word3 <- wordTripletFreqList$Word3[i]
            
            indexWordPairFreqList <- which(wordPairFreqList_Word1 == word1 & wordPairFreqList_Word2 == word3)
            
            if (length(indexWordPairFreqList) == 0){
                indexWordPairFreqList <- which(wordPairFreqList_Word1 == word3 & wordPairFreqList_Word2 == word1)
            }
            
            if (length(indexWordPairFreqList) != 0){
                support_word <- wordPairFreqList[, 3][indexWordPairFreqList]
                conf <- wordTripletFreqList_Frequency[i] / support_word 
                confidence3[i, 1] <- conf
            }else{
                confidence3[i, 1] <- 'NA'
            }
        }
        
        #Building final data file with confidence and supports
        finalScore <- cbind(wordTripletFreqList, confidence1, confidence2, confidence3)
        
        #Renaming columns 
        colnames(finalScore) <- c("Word1","Word2","Word3","Frequency(Support)",
                                  "conf[(W1,W2)->W3]","conf[(W2,W3)->W1])","conf[(W1,W3)->W2]")
        
    }
    
    return(finalScore)
}


# WORD ASSOCIATIONS FROM TDM USING CORRELATION ----

# This function gets the associated words for all words in the word list,
# with coefficient of correlation greater than 'corr'
# INPUTS
# 1. wordList - A one dimensional vector containing the list of words which are to be looked up
# 2. tdm - the tdm containing all words
# 3. corr - the correlation value between 0 and 1
# 4. the number of associations to be extracted for each of the words from the word list
# OUTPUT
# A data frame containing the list of words and their associations
# Author: Tejas Mahajan
getAssoWordsForWordList_c <- function(wordList, tdm, corr, numOfAsso) {
    
    # WIERD BEHAVIOR - IF THE TERM IS PRESENT IN THE ROWNAMES, IT IS NOT LISTED
    # AS THE ASSOCIATED WORD.
    # FOR THIS REASON, A LOOP IS BEING USED INSTEAD OF DIRECTLY USING LISTS.
    
    # 
    l2Nodes <- NA
    for (i in 1:length(wordList)) {
        assocs <- head(findAssocs(tdm, wordList[i], corr), numOfAsso)
        if (!is.null(dim(assocs))) {
            assocs <- cbind(wordList[i], rownames(assocs), assocs[, 1])
            l2Nodes <- rbind(l2Nodes, assocs)
        }
    }
    # remove the invalid first row and remove row names
    l2Nodes <- as.data.frame(l2Nodes[-1, ])
    rownames(l2Nodes) <- NULL
    return(l2Nodes)
}


# STEMMING ----

# This function takes a data frame containing word list and columns containing statistics
# Determines the stems of these words. Returns a data frame with
# - the list of stems
# - the words from the input list belonging to that stem
# - consolidated statistics for the stems
# Inputs required
# 1. data - the data frame containing the words to be stemmed and their statistics
# 2. wordColumn - the column name of the data frame containing the words to be stemmed
# 3. wordsOfStem - Values from this column would be displayed in the wordsOfStem column
#                   Sepcify the column name.
# 4. colsToAdd - array of column numbers. The values from these columns would be added up.
# Author: Tejas Mahajan
getStemStats <- function(data, wordColumn, wordsOfStemCol, colsToAdd) {
    
    library("SnowballC")
    
    # get the stems of the word column
    words <- data[[wordColumn]]
    stems <- wordStem(words)
    
    # bind the words and their session numbers to the stems and sort stems alphabetically 
    wordsOfStem <- paste(data[[wordColumn]], "(", data[[wordsOfStemCol]], "); ", sep = '')
    data <- cbind(stems, data, wordsOfStem)
    data$wordsOfStem <- as.character(data$wordsOfStem) #remove levels
    data <- data[order(data$stems), ]
    
    # If the words have the same stem, append the words and add up the session count
    colsToAdd <- colsToAdd + 1 # This is required since the columns have shifted due to addition of stems
    
    # append/add up values from rows which have the same stems 
    data <- combineRepeatedRows(data, "stems", colsToAdd, "wordsOfStem")
    
    # remove the old word column, sort and assign column name
    data <- data[, -which(names(data) == wordColumn)]
    data <- data[order(data[[wordsOfStemCol]], decreasing = TRUE), ]
    colnames(data)[which(colnames(data) == "wordsOfStem")] <- 
        paste("wordsOfStem (", wordsOfStemCol, ")", sep = '')
    
    return(data)
}