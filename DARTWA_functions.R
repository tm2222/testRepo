# DARTWA_functions is a collection of all functions needed for DART project.
# This gets called in rMain program and returns output as per request.

# Sources the required supporting files
source(paste(RCD, "\\common.R", sep = ''))
source(paste(RCD, "\\formatData.R", sep = ''))

# Author : Indrajit Patil
# This function returns word count of given input file
wordCount <- function(inputFile, textColumn, wordGroupSize, requestID, factor1="", factor2="", factor3=""){
    
    # loading libraries
    Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7")
    library(tm) 
    library(RWeka)
    library(stringr)
    library(data.table)
    library(RODBC)
    options(stringsAsFactors = FALSE)
    
    text <- read.delim(file=inputFile, sep = ",", header=TRUE, stringsAsFactors=FALSE)
    # This will handle special character in Office depot colname.
    colnames(text)[1] <- gsub('ï..', '', colnames(text)[1])
    
    sweepFactor1 <- sweepFactor2 <- sweepFactor3 <- NA
    
    # If parameters(factor1,factor2,factor3) are blank, 'NA' is assigned
    if(is.na(factor1) |factor1=="") factor1 <- NA 
    if(is.na(factor2) |factor2=="") factor2 <- NA
    if(is.na(factor3) |factor3=="") factor3 <- NA
    
    # Create 'addColList' needed for 'removeDuplicatesInWordCount' function
    addColList <- 1
    if(!is.na(factor1)){
        sweepFactor1 <- text[ , factor1]
        sweepFactor1  <- convertToInteger(sweepFactor1)
        addColList <- c(1, 2)
    }
    if(!is.na(factor2)){
        sweepFactor2 <- text[ , factor2]
        sweepFactor2 <- convertToInteger(sweepFactor2)
        addColList <- c(1, 2, 3)
    } 
    if(!is.na(factor3)){
        sweepFactor3 <- text[ , factor3]
        sweepFactor3 <- convertToInteger(sweepFactor3)
        addColList <- c(1, 2, 3, 4)
    }
    # wordCount calculation
    bitdm <- createTDM(text[ , textColumn], wordGroupSize)
    data <-getWordCount(tdm=bitdm,colSweepFactor1=sweepFactor1, colSweepFactor2=sweepFactor2, colSweepFactor3=sweepFactor3, splitSize=500)
    data <- removeDuplicatesInWordCount(data=data, wordCol='rownames', add=TRUE, addColList=addColList) 
    
    words <- gsub(' ', '-', rownames(data))
    wordCount_output <- cbind(requestID, words, data)
    # adding default columns to wordCount_output table; These are added as Null columns if doesnt exist.
    totalFactor1 <- totalFactor2 <- totalFactor3 <-NA
    if(! "totalFactor1" %in% colnames(wordCount_output)) wordCount_output <- cbind(wordCount_output, totalFactor1)
    if(! "totalFactor2" %in% colnames(wordCount_output)) wordCount_output <- cbind(wordCount_output, totalFactor2)
    if(! "totalFactor3" %in% colnames(wordCount_output)) wordCount_output <- cbind(wordCount_output, totalFactor3)
    colnames(wordCount_output) <- c("requestID", "words", "totalCount", "totalFactor1", "totalFactor2", "totalFactor3")
    
    # All Numeric columns are converted to numeric type
    wordCount_output$totalCount <- as.numeric(wordCount_output$totalCount)
    wordCount_output$totalFactor1 <- as.numeric(wordCount_output$totalFactor1)
    wordCount_output$totalFactor2 <- as.numeric(wordCount_output$totalFactor2)
    wordCount_output$totalFactor3 <- as.numeric(wordCount_output$totalFactor3)
    wordCount_output[is.na(wordCount_output)]   <- 0
    
    # create temporary file for bulk inserting into DB
    outputFileName <- paste("wordCount_output", "-", requestID, ".csv", sep='')
    outputFile <- paste(DbInsertTempDir,"\\", outputFileName, sep='')
    write.csv(wordCount_output, file=outputFile, row.names=FALSE, quote=FALSE)
    rm(outputFile)
    
    # create query for DB
    query <- paste("BULK \n
    INSERT [dbo].[wordCount_output] \n
    FROM 'F:\\Data Transfer\\", outputFileName, "'", "\n
    WITH \n
    ( \n
    FIRSTROW = 2, \n
    FIELDTERMINATOR = ',', \n
    ROWTERMINATOR = '\\n' \n
    )", sep = '')
    
    # Write to database
    dbhandle <- odbcDriverConnect(dbDriver)
    sqlQuery(channel=dbhandle, query)
    odbcClose(dbhandle) 
    
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: wordCount_output\n')
    
    invisible(outputFileName) # passing a small dummy variable, which is anyway invisible
}

# This function does long tail analysis by phrase length
longTailPhraseLength <- function(inputFile, textColumn, requestID, countCol=NA){
    
    library(RODBC)
    
    data <- read.delim(inputFile, header = TRUE, sep = ",", stringsAsFactors=FALSE)
    # This will handle special character in Office depot colname.
    colnames(data)[1] <- gsub('ï..', '', colnames(data)[1])
    text <- data[ , textColumn]
    
    countOfWords <- countWords(text)
    
    if(is.na(countCol) | countCol=="") countCol <- NA
    
    # if the countCol is not NULL or blank, set it to the countCol contents, else to 1
    colValues <- 1
    if (!is.null(countCol) & !is.na(countCol) & countCol != "") colValues <- data[[countCol]]
    
    colValues <- countOfWords * as.numeric(gsub(",", "", colValues))
    
    # sum all elements by word length
    output <- as.data.frame(tapply(colValues, countOfWords, FUN = sum))
    # generate a sequence to fill any gaps that exist in the output sequence  
    index <- data.frame(index = seq(max(as.numeric(rownames(output)))))
    matches <- match(rownames(index), rownames(output))
    output <- cbind(as.numeric(rownames(index)), as.numeric(output[matches, ]))
    output[is.na(output)] <- 0
    
    # add a column with cumulative percentage
    percent <- output[, 2] / sum(output[, 2])
    longTail_phraseLength <- cbind(requestID, output, cumsum(percent)*100)
    
    colnames(longTail_phraseLength) <- c("requestID", "Number.Of.Words", "Number.Of.Search.Phrases", "Cumulative.Percentage")
        
    #For database table creation
    dbhandle <- odbcDriverConnect(dbDriver)
    sqlSave(dbhandle, as.data.frame(longTail_phraseLength),rownames=FALSE, fast=TRUE, append=TRUE)
    odbcClose(dbhandle) 
    
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: longTail_phraseLength\n')
    
    invisible(longTail_phraseLength)
}

# This function does long tail analysis by keyword count
longTailKeywordCount <- function(inputFile, numericColumn, requestID, groupSize){
    
    library(RODBC)
    
    text <- read.delim(inputFile, header = TRUE, sep = ",", stringsAsFactors=FALSE)
    text[[numericColumn]] <- as.numeric(gsub(',', '', text[[numericColumn]]))
    text <- text[order(text[[numericColumn]], decreasing = T), ]
    
    # TAKE THE SUM OF GROUPS, CALCULATE CUMULATIVE VALUE----
    kwCountAnalysis <- tapply(text[[numericColumn]], ceiling(1:length(text[[numericColumn]]) / as.numeric(groupSize)),
                              FUN = sum, simplify = T)
    # add a column with cumulative percentage
    percent <- kwCountAnalysis / sum(kwCountAnalysis)
    kwCountAnalysis <- as.data.frame(cbind(requestID, (1:length(kwCountAnalysis)) * as.numeric(groupSize),
                                           kwCountAnalysis, cumsum(percent)*100))
    
    colnames(kwCountAnalysis) <- c("requestID", "Count", "SumOfColumns", "CumulativePercentage")
    
    #For database table creation
    dbhandle <- odbcDriverConnect(dbDriver)
    sqlSave(dbhandle, kwCountAnalysis, fast=TRUE, rownames = FALSE, append=TRUE)
    odbcClose(dbhandle) 
    
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: kwCountAnalysis\n')
    
    invisible(kwCountAnalysis)
}

# This function creates word cloud and stores image of it in specified folder
wordCloud <- function(requestID, freqColumn, outputPath){
    
    library(tm) 
    library(wordcloud)
    library(RColorBrewer)
    library(RODBC)
    
    # Get word count data
    dbhandle <- odbcDriverConnect(dbDriver)
    query <- paste ("select * from wordCount_output where requestID=", requestID, sep='')
    tableData <-  sqlQuery(channel=dbhandle, query)
    odbcClose(dbhandle) 
    
    # Get words and their frequencies
    words <- tableData[ "words"]
    freq <- tableData[freqColumn]
    
    #make the word cloud
    pal <- brewer.pal(8,"Dark2")
    filepath <- paste(outputPath, "wordcloud.png", sep = '\\')
    png(file=filepath, width=720, height=720)
    wordcloud(words[ , 1], freq[ , 1], c(4, 1), max.words = 200, random.order = F, rot.per = 0.15, 
              random.color = F, color = pal, fixed.asp=TRUE)
    dev.off()
    
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: ', filepath,'\n')
    
    invisible(filepath)
}

# This function converts the word pair frequency file
# into json format required by the dynamic association visualization
# returns a string that contains the data in the required format
dynamicNetMap_json <- function(requestID, outputPath) {
    
    library(RODBC)
    
    # Get word pair list
    dbhandle <- odbcDriverConnect(dbDriver)
    query <- paste ("select * from wordCount_output where requestID=", requestID, sep='')
    wordPairList <- sqlQuery(channel=dbhandle, query)
    odbcClose(dbhandle) 
    
    # get required columns and order them
    wordPairList <- wordPairList[ , c("words","totalFactor1")]
    wordPairList <- wordPairList[order(wordPairList[["totalFactor1"]], decreasing = T), ]
    wordPairList <- wordPairList[1:200, ]
    
    # seperate words by spaces
    wordPairList$words <- gsub('-', ' ', wordPairList$words)
    
    wordPairList <- splitColumn(textArray = wordPairList, textColumn = "words", wordGroupSize = 2)
    
    # Import formatData functions
    json_data <- convertToAssoVizJson(wordPairList)
    
    filepath <- paste(outputPath, "Json_file_Dynamic_nw_map", sep = '\\')
    filepath <- paste(filepath, "-", requestID, ".json", sep='' )
    write(json_data, file=filepath)
    
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: ', filepath,'\n')
    invisible(filepath)
}

# This function extracts phrases which contains given words.
phraseExtract <- function(requestID, inputFile, outputPath, textColumn, Match.Whole.Word, word1, word2, word3){
    
    if(is.na(word2)) word2 <- ''
    if(is.na(word3)) word3 <- ''
    data <- read.delim(file=inputFile, sep = ",", header=TRUE, stringsAsFactors=FALSE)
    colnames(data)[1] <- gsub('ï..', '', colnames(data)[1]) # This will handle special character in Office depot colname.
    
    phraseIndexes <- extractPhraseIndex(data, textColumn,
                                        Match.Whole.Word, word1, word2, word3)
    phrases <- data[phraseIndexes[ , 1], ]
    
    # Writing to file
    filepath <- paste(outputPath, "Phrases.csv", sep = '\\')
    write.csv(phrases, file=filepath, row.names=F)
    
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: ', filepath,'\n')
    invisible(filepath)
}

# Rare words
rareWords <- function(inputFile, textColumn, wordGroupSize, requestID, numericColumn, 
                      rareWordFrequency, popularWordPercent, rareWordFreqMoreThan, 
                      wordCount_output_requestId, factor1="", factor2="", factor3=""){
    
    library(tm) 
    library(RWeka)
    library(stringr)
    library(data.table)
    library(RODBC)
    options(stringsAsFactors = FALSE)
    
    text <- read.delim(file=inputFile, sep = ",", header=TRUE, stringsAsFactors=FALSE)
    # This will handle special character in Office depot colname.
    colnames(text)[1] <- gsub('ï..', '', colnames(text)[1])
    
    sweepFactor1 <- sweepFactor2 <- sweepFactor3 <- NA
    # If parameters(factor1,factor2,factor3) are blank, 'NA' is assigned
    if(is.na(factor1) |factor1=="") factor1 <- NA 
    if(is.na(factor2) |factor2=="") factor2 <- NA
    if(is.na(factor3) |factor3=="") factor3 <- NA
    # create 'addColList' needed by 'removeDuplicatesInWordCount' function
    addColList <- 1
    if(!is.na(factor1)){
        sweepFactor1 <- text[ , factor1]
        sweepFactor1  <- text[ , factor1] <- convertToInteger(sweepFactor1)
        addColList <- c(1, 2)
    }
    if(!is.na(factor2)){
        sweepFactor2 <- text[ , factor2]
        sweepFactor2 <- text[ , factor2] <- convertToInteger(sweepFactor2)
        addColList <- c(1, 2, 3)
    } 
    if(!is.na(factor3)){
        sweepFactor3 <- text[ , factor3]
        sweepFactor3 <- text[ , factor3] <- convertToInteger(sweepFactor3)
        addColList <- c(1, 2, 3, 4)
    }
    
    # Get textData less than rareWordFrequency of column numericColumn
    text <- text[which(text[ , numericColumn] < rareWordFrequency), ]
    
    # wordCount calculation
    bitdm <- createTDM(text[ , textColumn], wordGroupSize)
    data <-getWordCount(tdm=bitdm,colSweepFactor1=sweepFactor1, colSweepFactor2=sweepFactor2, colSweepFactor3=sweepFactor3, splitSize=500)
    data <- removeDuplicatesInWordCount(data=data, wordCol='rownames', add=TRUE, addColList=addColList) 
    
    words <- gsub(' ', '-', rownames(data))
    wordCount_output <- cbind(requestID, words, data)
    # adding default columns to wordCount_output table; These are added as Null columns if doesnt exist.
    totalFactor1 <- totalFactor2 <- totalFactor3 <-NA
    if(! "totalFactor1" %in% colnames(wordCount_output)) wordCount_output <- cbind(wordCount_output, totalFactor1)
    if(! "totalFactor2" %in% colnames(wordCount_output)) wordCount_output <- cbind(wordCount_output, totalFactor2)
    if(! "totalFactor3" %in% colnames(wordCount_output)) wordCount_output <- cbind(wordCount_output, totalFactor3)
    colnames(wordCount_output) <- c("requestID", "words", "totalCount", "totalFactor1", "totalFactor2", "totalFactor3")
    
    # All Numeric columns are converted to numeric type
    wordCount_output$totalCount <- as.numeric(wordCount_output$totalCount)
    wordCount_output$totalFactor1 <- as.numeric(wordCount_output$totalFactor1)
    wordCount_output$totalFactor2 <- as.numeric(wordCount_output$totalFactor2)
    wordCount_output$totalFactor3 <- as.numeric(wordCount_output$totalFactor3)
    wordCount_output[is.na(wordCount_output)]   <- 0
    
    wordCount_output_rare <- wordCount_output 
    # FETCHING TABLE DATA OF WORDS WITH FACTOR GREATER THAN GIVEN PERCENT(SAY 10%)
    # wordCount_output_requestId, words
    dbhandle <- odbcDriverConnect(dbDriver)
    query <-paste("select * from wordCount_output where requestID=", wordCount_output_requestId, sep='')
    wordCountFull <-  sqlQuery(channel=dbhandle, query)
    
    # Renaming colnames as per given columns of file
    names(wordCountFull)[4] <- names(wordCount_output_rare)[4] <-  factor1
    names(wordCountFull)[5] <- names(wordCount_output_rare)[5] <- factor2
    names(wordCountFull)[6] <- names(wordCount_output_rare)[6] <- factor3
    
    # Get the specified % of top words
    wordCountFull <- wordCountFull[order(wordCountFull[[numericColumn]], decreasing = T), ]
    popularWordNum <- round(dim(wordCountFull)[1]* as.numeric(popularWordPercent)/100)
    wordCountFull <- wordCountFull[1: popularWordNum, ]
    wordCountFull <- wordCountFull[with(wordCountFull, order(words)), ]
    
    # remove top words from the roare word list
    wordCount_output_rare <- wordCount_output_rare[with(wordCount_output_rare, order(words)), ]
    wordCount_output_rare <- wordCount_output_rare[-which(wordCount_output_rare$words %in% wordCountFull$words), ]
    
    # keep rare words with frequency more than 'rareWordFreqMoreThan'
    wordCount_output_rare <- wordCount_output_rare[which(wordCount_output_rare[ , numericColumn] > rareWordFreqMoreThan), ]
    
    # create a temp file to write to DB
    outputFileName <- paste("wordCount_output", "-", requestID, ".csv", sep='')
    outputFile <- paste(DbInsertTempDir,"\\", outputFileName, sep='')
    write.csv(wordCount_output_rare, file=outputFile, row.names=FALSE, quote=FALSE)
    rm(outputFile)
    
    query <- paste("BULK \n
                   INSERT [dbo].[wordCount_output] \n
                   FROM 'F:\\Data Transfer\\", outputFileName, "'", "\n
                   WITH \n
                   ( \n
                   FIRSTROW = 2, \n
                   FIELDTERMINATOR = ',', \n
                   ROWTERMINATOR = '\\n' \n
                   )", sep = '')
    
    sqlQuery(channel=dbhandle, query)
    
    odbcClose(dbhandle) 
    cat('\nJobStatus: Success\n')
    cat('\nJobOutputPath: wordCount_output\n')
    
    invisible(outputFileName) # passing a small dummy variable, which is anyway invisible
}

# Clustering
cluster <- function(inputFile, textColumn, numericColumn, numberOfClusters, numberOfKeywords){
    
    text <- read.delim(file=inputFile, sep = ",", header=TRUE, stringsAsFactors=FALSE)
    # This will handle special character in Office depot colname.
    colnames(text)[1] <- gsub('ï..', '', colnames(text)[1]) 
    
    searchTermCorpus <- Corpus(VectorSource(text[ , textColumn]),
                               readerControl = list(language = "en")) 
    searchTermCorpus <- stdCorpusCleaning(searchTermCorpus)
    tdm <- createTDM(text[ , textColumn], wordGroupSize = 1)
    
    tdm <- TermDocumentMatrix(searchTermCorpus, control =  list(stopwords=TRUE, wordLengths=c(0, Inf)))
    Sessions <- text$Sessions
    
    Sessions <- as.numeric(gsub(',', '', Sessions))
    tdm <- sweep(tdm, 2, Sessions, "*")
    
    # CLUSTERING ------------------
    
    no_of_rows <- dim(text)[1]
    
    #no_of_clusters <- round(sqrt(no_of_rows/2),0) # standard approach
    no_of_clusters <- round(no_of_rows^(1/3),0) # my approach
    
    #dissimilarity matrix calculation is memory and CPU intensive
    #takes more than 10 minutes on ~4000 line file
    dissimTime <- Sys.time()
    tdm_dissim <- dissimilarity(tdm, method = "cosine")
    Sys.time() - dissimTime
    clust <- hclust(tdm_dissim, method = "ward")
    
    #summary(clust)
    #plot(clust)
    #plot(rect.hclust(clust, k = no_of_clusters)) #has to be run after the above command
    
    searchTerms <- text$On.Site.Search.Term
    clusmemb <- as.numeric(cutree(clust, k = no_of_clusters))
    clusmembMap <- cbind(searchTerms, Sessions, as.numeric(clusmemb))
    #clusmembMap <- clusmembMap[with(clusmembMap, order(clusmemb)), ]
    colnames(clusmembMap)[3] <- c("cluster membership")
    
    clusmembMap <- clusmembMap[order(clusmembMap[,3]), ]
    write.csv(clusmembMap,file=clusMapFile) # Saves the search term to cluster mapping to file
    
    # CLUSTER LEVEL CALCULATIONS ------------------
    
    # pairFreqData <- read.delim(file=wordPairFreqFile, sep = ",", 
    #                            header=TRUE, stringsAsFactors=FALSE)
    
    
    keyword.column <- NULL
    for(i in 1:numberOfKeywords){
        keyword.column <- cbind(keyword.column, paste('Keyword',i, sep=''))
    }
    cat("Cluster#", "Total search requests (Sessions)",keyword.column,
        "\n", sep = ',', file = clusterFile)
    
    # IDENTIFY (numberOfKeywords)KEYWORDS PER CLUSTER
    i<-1
    for (i in 1:no_of_clusters){
        
        clusText <- clusmembMap[which(clusmembMap[,3]==i),]
        clusCorpus <- NULL
        clusCorpus <- Corpus(VectorSource(clusText[, 1]), readerControl = list(language = "en"))
        
        #transform/clean data
        clusCorpus <- tm_map(clusCorpus, tolower)
        #   clusCorpus <- tm_map(clusCorpus, removeWords, stopwords("english")) //indra
        clusCorpus <- tm_map(clusCorpus, removePunctuation)
        clusCorpus <- tm_map(clusCorpus, stripWhitespace)
        
        clustdm <- TermDocumentMatrix(clusCorpus, control =  list(stopwords=TRUE, wordLengths=c(0, Inf)))
        clusImpressions <- as.numeric(clusText[,2])
        clustdm <- sweep(clustdm, 2, clusImpressions, "*")
        
        allFreqTerms <- sort(rowSums(as.matrix(clustdm)), decreasing = TRUE)
        #keywords  
        clusTopics <- head(allFreqTerms, numberOfKeywords)
        
        totalClusImpr <- sum(clusImpressions)
        
        # Creating Cluster file with specified no of keywords  
        names.of.keywords <- NULL
        for(j in 1:numberOfKeywords){
            percentPresence <- round(100 * clusTopics[j] / totalClusImpr, digits = 2)
            names.of.keywords <- cbind(names.of.keywords, paste(names(clusTopics)[j]," (", percentPresence, "%)", sep = ''))
        }
        cluster.for.keywords <- cbind(i, totalClusImpr, names.of.keywords)
        cat(cluster.for.keywords,
            "\n", 
            file = clusterFile, sep = ',',append = TRUE)
        
        
    }
}