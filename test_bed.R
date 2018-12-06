```{r data_tables}
corpus_file <- "processedCorpus.rds"
dtm_file <- "dtm.rds"
tdm_file <- "tdm.rds"

if (file.exists(corpus_file)) {
  processedCorpus <- readRDS(file = corpus_file)
} else {
  processedCorpus <- en_us %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation) %>%
    tm_map(stemDocument) %>%
    tm_map(removeWords, stopwords("english"))
  saveRDS(processedCorpus, corpus_file)
}

if (file.exists(dtm_file)) {
  dtm <- readRDS(file = dtm_file)
} else {
  dtm <- DocumentTermMatrix(processedCorpus)
  saveRDS(dtm, dtm_file)
}

if (file.exists(tdm_file)) {
  tdm <- readRDS(file = tdm_file)
} else {
  tdm <- TermDocumentMatrix(processedCorpus)
  saveRDS(tdm, tdm_file)
}
```

### Word counts for each file

```{r word_count}

```