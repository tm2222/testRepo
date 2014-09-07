# THIS FILE HOLDS ALL THE FUNCTIONS THAT WILL FORMAT THE DATA IN THE DESIRED FORMAT

# This function converts the word pair frequency list
# into json format required by the dynamic association visualization
# returns a string that contains the data in the required format
convertToAssoVizJson <- function(wordPairList) {
    
    #init
    library("stringr")
    source(paste(Sys.getenv("VSSHOME"), "\\DARTWA\\R\\common.R", sep = ''))
    options(stringsAsFactors = F)
    
    ttm <- createTTMfromWordPairList(wordPairList)
    
    # format the first part - node list
    nodeList <- "\t[\n"
    
    for (i in 1:dim(ttm)[1]){
        nodeData <- str_c("\t\t", "{\n",
                          "\t\t", "\"id\": ", i, ",\n",
                          "\t\t", "\"cluster\": 0,", "\n",# fixing as 0 for now
                          "\t\t", "\"text\": ", "\"", rownames(ttm)[i], "\"", ",\n",
                          "\t\t", "\"size\": ", sum(ttm[i, ]), ",\n",
                          "\t\t", "\"count\": ", sum(ttm[i, ]), "\n", # implement count here
                          "\t\t", "},\n")
        #     # Add a comma except for the last entry
        #     if (i != dim(ttm)[1]) nodeData <- str_c(nodeData, ",")
        #     nodeData <- str_c(nodeData, "\n")
        nodeList <- str_c(nodeList, nodeData)
    }
    nodeList <- substr(nodeList, 1, nchar(nodeList)-2)
    nodeList <- str_c(nodeList, "\n\t", "],", "\n")
    
    # format the second part - node association
    assoList <- "\t[\n"
    for (r in 1:nrow(ttm)){
        for (c in r:ncol(ttm)){
            if (ttm[r, c] != 0){
                assoList <- str_c(assoList, "\t\t", "[", r, ", ", c, "],\n")
            }
        }
    }
    assoList <- substr(assoList, 1, nchar(assoList)-2)
    assoList <- str_c(assoList, "\n\t", "];", "\n")
    
    # return the 2 parts combined
    return(str_c("[\n", nodeList, "\n", assoList, "]\n"))
    
}


# This function reads the word list and word pair frequency list
# The word list, if specified, should have all the words present in the word pair list
# The wordColorList (optional) is a list of words with their desired colors
# Output is a string in json format required by the dynamic association visualization
convertToAssoVizJson1 <- function(wordPairList, wordList = NULL, wordColorList = NULL) {
    
}

# This function reads the data which contains 2 columns 
#This function puts the element of the second column within paranthesis
# and then combines the two columns into a string separated by semicolon

# Author : Indrajit Patil
bindWithSemicolon <- function(data){
    
    data <- paste(data[, 1], "(", data[,  2], ")", sep = '')
    word_matchStr <- paste(data, collapse='; ')
    
    return(word_matchStr)
}

# This function does the formating of a Vector which contains ',', '$'.
# This function removes these symbols and convert it into numeric type
convertToInteger <- function(colVector){
    
    colVector <- gsub('$', '', colVector)
    colVector <- gsub(',', '', colVector)
    colVector <- as.numeric(colVector)
}