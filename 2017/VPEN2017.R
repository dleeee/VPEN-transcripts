library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(readr)
library(data.table)


##########
###2017###
##########
setwd("E:/DataSets/VisualPolitik EN transcripts//2017")

#Sort Files by Date Modified
details_2017 = file.info(list.files(pattern="*.txt"))
details_2017 = details_2017[with(details_2017, order(as.POSIXct(mtime))), ]
files_2017 = rownames(details_2017)

#works
VPEN2017 = lapply(files_2017, function(x)read_table(x, col_names = F, skip = 1)) 
VPEN2017 <- do.call(rbind, VPEN2017)
VPEN2017 <- data.frame(matrix(VPEN2017$X1, ncol = 3, byrow = TRUE, dimnames = list(NULL, c('count', 'timestamp', 'text'))))
VPEN2017$text <- as.character(VPEN2017$text)
  #This line is necessary when we want to do sentimental analysis through the get_sentences() function


#################
##TEXT ANALYSIS##
#################
# https://towardsdatascience.com/easy-text-analysis-on-abc-news-headlines-b434e6e3b5b8

library(udpipe)
#Loading R package and Getting Language Model ready
#model <- udpipe_download_model(language = "english")
#Downloaded, no need to run it again
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.4-190531.udpipe')


#Annotate Input Text Data for Visual Politik EN 2017
s <- udpipe_annotate(udmodel_english, VPEN2017$text)
#annotate(object returned from 'udpipe_load_model', text data)
x <- data.frame(s)
#Incredibly long to load, proceed with caution


#Universal Parts of Speech
library(lattice)
#To produce more plots, notably the 'barchart()' function

upo_freq <- txt_freq(x$upos)
  # extract frequency of the 'upo' column of 'x'
upo_freq$key <- factor(upo_freq$key, levels = rev(upo_freq$key))
  # convert the 'key' column as factor, and sort them by descending order in terms of their frequency
  # Otherwise, the next command will print a plot that is sorted  in descending by name
barchart(key ~ freq_pct, data = upo_freq, col = "5", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")

#Most Occuring Nouns
nouns <- subset(x, upos %in% c("NOUN")) 
nouns <- txt_freq(nouns$token)
nouns$key <- factor(nouns$key, levels = rev(nouns$key))
barchart(key ~ freq_pct, data = head(nouns, 30), col = "6", 
         main = "Most Occurring Nouns 2017", xlab = "Freq")

## ADJECTIVES
adjectives <- subset(x, upos %in% c("ADJ")) 
adjectives <- txt_freq(adjectives$token)
adjectives$key <- factor(adjectives$key, levels = rev(adjectives$key))
barchart(key ~ freq_pct, data = head(adjectives, 30), col = "7", 
         main = "Most occurring adjectives 2017", xlab = "Freq")

## VERBS
verbs <- subset(x, upos %in% c("VERB")) 
verbs <- txt_freq(verbs$token)
verbs$key <- factor(verbs$key, levels = rev(verbs$key))
barchart(key ~ freq_pct, data = head(verbs, 30), col = "8", 
         main = "Most occurring Verbs 2017", xlab = "Freq")
#How do you print 10th to 30th results? Tip: Not head()

#Automated Keywords Extraction with RAKE
#RAKE is one of the most popular (unsupervised) algorithms for extracting keywords 
#in Information retrieval. RAKE short for Rapid Automatic Keyword Extraction algorithm

## Using RAKE
RAKE <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                      relevant = x$upos %in% c("NOUN", "ADJ"))
RAKE$key <- factor(RAKE$keyword, levels = rev(RAKE$keyword))
barchart(key ~ rake, data = head(subset(RAKE, freq >4), 20), col = "9", 
         main = "Keywords identified by RAKE (by Nouns and Adj) 2017", 
         xlab = "Rake")

## Using RAKE 2
RAKE <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                      relevant = x$upos %in% c("NOUN", "VERB"))
RAKE$key <- factor(RAKE$keyword, levels = rev(RAKE$keyword))
barchart(key ~ rake, data = head(subset(RAKE, freq >2), 20), col = "10", 
         main = "Keywords identified by RAKE (by Nouns and Verb) 2017", 
         xlab = "Rake")

#collocation
kcol <- keywords_collocation(x = x, term = "lemma", group = "doc_id")
kcol$key <- factor(kcol$keyword, levels = rev(kcol$keyword))
barchart(key ~ pmi, data = head(subset(subset(kcol, freq >3), pmi <10), 20), col = "10", 
         main = "Keywords identified by collocation 2017", 
         xlab = "Pointwise mutual information (PMI)")
  # a PMI of <10 is chosen, otherwise the list would be names of political figures


################################
#### ON SENTIMENT ANALYSIS #####
#################################
# https://github.com/trinker/sentimentr

library(sentimentr)
library(magrittr)
library(dplyr)

VPEN2017 %>%
  filter(count %in% sample(unique(count), 2)) %>%
  mutate(text = get_sentences(text)) %$%
  sentiment_by(text, count) %>%
  highlight()
  #Seed is not set, so you will get the sentence from 2 random nth row of each Visual Politik EN episodes


###########################################
######### END OF SENTIMENT WORKFLOW #######
############################################





