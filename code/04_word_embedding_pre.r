## ----------------------------------------------------
# Import pkgs
pacman::p_load(
  data.table,
  tidyverse,
  purrr,
  furrr,
  tm,
  here,
  tidytext, # text analysis 
  SnowballC, # stemming 
  textclean, # text preprocessing 
  ggthemes,
  ggpubr,
  text2vec, # word embedding 
  widyr,
  patchwork, # arranging ggplots
  glue,
  reactable
)

# for publication-friendly theme 
theme_set(theme_pubr())

# Custom functions
file.list <- glue("{here('functions')}/{ 
     list.files(here('functions'))}")

purrr::walk(file.list, source)


## ----eval = FALSE------------------------------------
## df <- fread(here("processed_data", "text_ready.csv"))
## 
## df <- df %>%
##   filter(wuhan_virus == 1 | chinese_virus == 1 |
##          kung_flu == 1)
## 
## df <- subset(df, date >= as.Date(c("2020-03-16")))


## ----eval = FALSE------------------------------------
## # Call stop words dictionary
## data("stop_words")
## 
## # Tidying and filtering
## tidy_df <- df %>%
##   # Tokenize
##   unnest_tokens(bigram, full_text,
##                 token = "ngrams", n = 2)
## 
## filtered_df <- tidy_df %>%
##   separate(bigram, into = c("first","second"), sep = " ") %>%
##   anti_join(stop_words, by = c("first" = "word")) %>%
##   anti_join(stop_words, by = c("second" = "word")) %>%
##   filter(str_detect(first, "[a-z]") &
##          str_detect(second, "[a-z]")) %>%
##   unite(word, first, second)
## 
## nested_df <- filtered_df %>%
##   nest(words = c(word))


## ----eval = FALSE------------------------------------
## plan(multisession)  ## for parallel processing
## 
## unnested_df <- nested_df %>%
##   mutate(words = future_map(words, slide_windows, 4L)) %>%
##   unnest(words)


## ----eval = FALSE------------------------------------
## tidy_pmi <- unnested_df %>%
##   unite(window_id, date, window_id) %>%
##   pairwise_pmi(word, window_id)
## 
## save(unnested_df, tidy_pmi, file = here("processed_data", "tidy_pmi_post.Rdata"))


## ----eval = FALSE------------------------------------
## # 100 dimensions using Singular Value Composition
## tidy_word_vectors <- tidy_pmi %>%
##   widely_svd(
##     item1, item2, pmi,
##     nv = 100, maxit = 1000
##   )
## 
## saveRDS(tidy_word_vectors, file = here("processed_data", "tidy_word_vectors_post.Rdata"))


## ----eval = FALSE------------------------------------
## # Application
## 
## tidy_word_vectors <- readRDS(file = here("processed_data", "tidy_word_vectors_post.Rdata"))
## 
## tidy_word_vectors %>%
##   nearest_neighbors("chinese_virus") %>%
##   filter(item1 != "chinese_virus") %>%
##   filter(!str_detect(item1, "http")) %>%
##   top_n(10, abs(value)) %>%
##   mutate(value = round(value,2)) %>%
##   rename(word = item1,
##          similarity = value) %>%
##   kableExtra::kable()


## ----eval = FALSE------------------------------------
## knitr::purl("04_word_embedding_post.Rmd",
##             "04_word_embedding_post.r")

