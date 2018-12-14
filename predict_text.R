get_content <- function(f, n_samples = NULL) {
  con <- file(f)
  all <- readLines(con, -1, encoding="UTF-8")
  close(con)
  if (is.null(n_samples) || n_samples > length(all)) {
    output <- all
  } else{
    set.seed(1)
    output <- sample(all, n_samples)
  }
  tibble(text = output, book = f)
}

clean_blogtext <- function(text) {
  
  profanities <- tryCatch(read.table(file.path("data", "swearWords.csv"), header = FALSE, stringsAsFactors = FALSE) %>%
                            unlist #%>% data.frame(word = .)
                          ,
                          error = function(e) {
                            warning(e)
                            c("")
                          })
  
  text %>% 
    tolower %>%
    str_replace_all(",", " ") %>% 
    str_replace_all("(?<=\\w)[\\u2019']d(?!\\w)", " would") %>%
    str_replace_all("(?<!\\w)[Tt]here[\\u2019']s(?!\\w)", "there is") %>%
    str_replace_all("(?<!\\w)[hH]ere[\\u2019']s(?!\\w)", "here is") %>%
    str_replace_all("(?<!\\w)[tT]hat[\\u2019']s(?!\\w)", "that is") %>%
    str_replace_all("(?<!\\w)[wW]hat[\\u2019']s(?!\\w)", "what is") %>%
    str_replace_all("(?<!\\w)[wW]ho[\\u2019']s(?!\\w)", "who is") %>%
    str_replace_all("(?<!\\w)[hH]e[\\u2019']s(?!\\w)", "he is") %>%
    str_replace_all("(?<!\\w)[Ss]he[\\u2019']s(?!\\w)", "she is") %>%
    str_replace_all("(?<!\\w)[iI]t[\\u2019']s(?!\\w)", "it is") %>%
    str_replace_all("(?<!\\w)[lL]et[\\u2019']s(?!\\w)", "let us") %>%
    
    str_replace_all("(?<=\\w)n[\\u2019']t(?!\\w)", " not") %>%
    str_replace_all("(?<=\\w)[\\u2019']ve(?!\\w)", " have") %>%
    str_replace_all("(?<=\\w)[\\u2019']l{1,2}(?!\\w)", " will") %>%
    str_replace_all("(?<=\\w)[\\u2019']re{0,1}(?!\\w)", " are") %>%
    str_replace_all("(?<=\\w)[\\u2019']m(?!\\w)", " am") %>%
    
    str_replace_all("(?<=\\w)[\\u2019']s(?!\\w)", "") %>%
    
    str_replace_all("[^a-zA-Z ]+", ".") %>%
    removeWords(profanities)
}

predict_text_options <- function(input_text, words_to_use = 3) {
  if (!exists("ngrams")) stop("Please create ngrams variable")
  input_text <- str_replace(input_text, "[^\\w]+", " ")
  n_words <- str_count(input_text, "\\w+")
  
  if (words_to_use > 3) stop("prediction can only take place for words_to_use of 3 and below")
  
  if (n_words > words_to_use) {
    input_text <- input_text %>% 
      str_match_all("\\w+") %>% 
      unlist %>% 
      tail(words_to_use) %>% 
      paste(collapse = " ")
  } else if (words_to_use > n_words) {
    words_to_use <- n_words
  }
  
  ngrams[[words_to_use]] %>%
    filter(str_detect(gram, paste0("^", input_text, " "))) %>%
    mutate(prediction = str_match(gram, "\\w+$"))
}

get_corr <- function(input_text) {
  if (!exists("word_cors")) stop("Please build word_cors variable")
  wordlist <- input_text %>% 
    tolower %>%
    removeWords(stop_words$word) %>%
    str_match_all("\\w+") %>%
    unlist
  
  word_cors %>%
    filter(item1 %in% wordlist)  %>%
    group_by(item2) %>%
    summarise(correlation = sum(correlation)) %>%
    arrange(desc(correlation))
}

predict_text <- function(input_text, words_to_use = 3, show_alt = FALSE) {
  input_text <- clean_blogtext(input_text)
  correlations <- get_corr(input_text)
  preds <- predict_text_options(input_text, words_to_use) 
  
  reduced_stop_words <- c("a", "the", "and", "i", "of")
  
  results <- suppressWarnings(left_join(preds, correlations, by = c("prediction" = "item2"))) %>%
    arrange(desc(correlation), desc(n)) %>%
    filter(! prediction %in% reduced_stop_words)
  
  if (nrow(results) == 0) {
    if (words_to_use > 1) {
      results <- predict_text(input_text, words_to_use = words_to_use - 1, show_alt = TRUE)
    } else {
      results <- NULL
    }
  }
  
  if (show_alt) {
    results
  } else {
    head(results, 1) %>%
      .$prediction
  }
}

# Initialize results

if (file.exists(file.path("data", "word_cors.rds")) & file.exists(file.path("data", "ngrams.rds"))) {
  word_cors <- readRDS(file.path("data", "word_cors.rds"))
  ngrams <- readRDS(file.path("data", "ngrams.rds"))
} else {
  clean <- rbind(get_content(file.path("data", "en_us", "en_US.blogs.txt"), 100000), 
                 get_content(file.path("data", "en_us", "en_US.news.txt"), 100000), 
                 get_content(file.path("data", "en_us", "en_US.twitter.txt"), 100000)) %>% 
    mutate(clean_text = clean_blogtext(text),
           section = 1:n())
  
  temp <- str_split(clean$clean_text, "\\.") 
  
  split_sentences <- lapply(1:length(temp), function(x) {data.frame(text = unlist(temp[[x]]), section = x, stringsAsFactors = FALSE)}) %>% 
    bind_rows %>%
    filter(str_count(text, "\\w+") > 3) %>%
    as.tibble
  
  rm(temp)
  
  system.time(
    ngrams <- lapply(c(2,3,4), function(x) {
      split_sentences %>% 
        unnest_tokens(gram, text, token = "ngrams", n = x) %>%
        count(gram, sort = TRUE) %>%
        filter(n > 1)
    })
  )
  
  words_df <- clean %>% 
    unnest_tokens(word, clean_text) %>%
    filter(!word %in% stop_words$word) %>%
    select(section, word)
  
  #word_pairs <- words_df %>%
  #  pairwise_count(word, section, sort = TRUE)
  system.time(
    word_cors <- words_df %>%
      group_by(word) %>%
      filter(n() >= 40) %>%
      pairwise_cor(word, section, sort = TRUE) %>%
      filter(abs(correlation) > 0.001)  ## keep ~25% of results #quantile(abs(correlation), 0.75 
    #.[seq(1, nrow(word_cors), 2),] # Remove duplicates
  )
  
  saveRDS(word_cors, "word_cors.rds")
  saveRDS(ngrams, "ngrams.rds")
}
