library(dplyr)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

text <- dfgeod %>%
    select(clinicalSigns) %>%
    mutate_all(na_if,"") %>%
    na.omit()


ncols <- max(stringr::str_count(dfgeod$clinicalSigns, ",")) + 1

ncols

# colmn <- paste("col", 1:ncols)
#
#
# cs <-
#     tidyr::separate(
#         data = dfgeod,
#         col = clinicalSigns,
#         sep = ",",
#         into = colmn,
#         remove = FALSE
#     )
#
# require(dplyr)
# head(cs) %>% knitr::kable()

#Number of papers that reported clinical signs
dfgeod %>%
    select(refID, agent, agent.subtype, clinicalSigns) %>%
    mutate(CSbool = case_when(clinicalSigns == "" ~ 0,
                              TRUE ~ 1)) %>%
    summarise(sum(CSbool))

#Create a vector containing only the text
text <- dfgeod %>%
    select(clinicalSigns) %>%
    mutate_all(na_if,"") %>%
    na.omit()

#This also shows the number of papers that reported clinical signs
nrow(text)

# Create a corpus
docs <- Corpus(VectorSource(text))

#Remove everything we don't want in the cloud
sub

docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE)
df <- data.frame(word = names(words),freq=words)

#From n_row(text) papers that provided clinical signs, x (df$freq) reported df$word
#for loop

set.seed(1234)
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


wordcloud2(df, color = "random-light", backgroundColor = "grey")
