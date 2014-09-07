# This is Main file which is called by .Net and process request. 

# This is "R code directory".
# This is where all the R code files will reside.
# This path is used internally by the R code for inter-file function calling.
# This path should be set when the DART-WA backend gets installed.
RCD <- "C:\\Users\\indrajit.patil\\Desktop\\VSSHOME\\DARTWA\\R"

# for hiding warnings

options(warn = -1)
args <- commandArgs(trailingOnly = TRUE)

source(paste(RCD, "\\Rconfig.R", sep = ''))
source(paste(RCD, "\\DARTWA_functions.R", sep = '')) 

switch(args[1],
       "Word Count"={
           if(length(args)>4)
           {
               wordCount(args[2], gsub(' ', '.', args[3]), args[4], 
                         args[5], gsub(' ', '.', args[6]), gsub(' ', '.', args[7]), gsub(' ', '.', args[8]))
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for word count Creation.\n')
           }
           
       },
       
       "Long tail phrase length"={
           if(length(args)>=3)
           {
               longTailPhraseLength(args[2], gsub(' ', '.', args[3]), args[4], gsub(' ', '.', args[5]))
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Long tail analysis Creation.\n')
           }
           
       },
       
       "Long tail keyword count"={
           if(length(args)>3)
           {
               longTailKeywordCount(args[2], gsub(' ', '.', args[3]), args[4],args[5])
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Long tail analysis Creation.\n')
           }
           
       },
       
       "Phrase extraction"={
           if(length(args)>6)
           {
               phraseExtract(args[2], args[3],
                             args[4], gsub(' ', '.', args[5]), args[6], args[7], args[8], args[9])
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Long tail analysis Creation.\n')
           }
           
       },
       
       "Dynamic word association"={
           if(length(args)==3)
           {
               dynamicNetMap_json(args[2], args[3])
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Dynamic Network map Creation.\n')
           }
           
       },
       
       "wordcloud"={
           if(length(args)==4)
           {
               wordCloud(args[2], args[3], args[4])
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Dynamic Network map Creation.\n')
           }
           
           
       },
       
       "Rare words" ={
           if(length(args)==13)
           {
               rareWords(args[2], gsub(' ', '.', args[3]), args[4], 
                     args[5], gsub(' ', '.', args[6]), args[7], args[8], args[9], args[10], 
                    gsub(' ', '.', args[11]), gsub(' ', '.', args[12]), gsub(' ', '.', args[13]))
                            
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Dynamic Network map Creation.\n')
           }
           
       },
       
       "clustering" ={
           if(length(args)==4)
           {
               
           }else
           {
               cat('\nJobStatus: Fail\n')
               cat('\nReason: Pass required arguments for Dynamic Network map Creation.\n')
           }
           
       },
       
       
       # More processes to go
       
       #default
{
    cat('\nJobStatus: Fail\n')
    cat('\nReason: Pass required arguments to process the request\n') 
}

)
