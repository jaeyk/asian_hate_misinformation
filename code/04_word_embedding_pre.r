## ---------------------------------------------
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


## ---------------------------------------------
df <- fread(here("processed_data", "text_ready.csv"))

df <- subset(df, date < as.Date(c("2020-03-16")))


## ---------------------------------------------
# Remove non word characters 
df$full_text <- textclean::strip(df$full_text)

# Call stop words dictionary 
data("stop_words")

# Tidying and filtering 
tidy_df <- df %>%
  # Tokenize
  unnest_tokens("word", full_text) %>%
  # Remove stop words 
  anti_join(get_stopwords(), by = "word") 
  # Add count 
  add_count(word) %>%
  # Filter
  filter(n >= 1000) %>%
  # Drop the variable 
  select(-n)

nested_df <- tidy_df %>%
  nest(words = c(word))


## ----eval = FALSE-----------------------------
## plan(multisession)  ## for parallel processing
## 
## unnested_df <- nested_df %>%
##   mutate(words = future_map(words, slide_windows, 4L)) %>%
##   unnest(words)
## 
## tidy_pmi <- unnested_df %>%
##   unite(window_id, date, window_id) %>%
##   pairwise_pmi(word, window_id)
## 
## save(unnested_df, tidy_pmi, file = here("processed_data", "tidy_pmi.Rdata"))


## ---------------------------------------------
load(here("processed_data", "tidy_pmi.Rdata"))


## ---------------------------------------------
# 100 dimensions using Singular Value Composition
tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )

saveRDS(tidy_word_vectors, file = here("processed_data", "tidy_word_vectors_pre.Rdata"))


## ----eval = FALSE-----------------------------
## # Application
## 
## tidy_word_vectors %>%
##   nearest_neighbors("visible") %>%
##   filter(item1 != "visible") %>%
##   top_n(30, abs(value)) %>%
##   mutate(value = round(value,2)) %>%
##   rename(word = item1,
##          similarity = value)
## 
## tidy_word_vectors %>%
##   nearest_neighbors("chinese virus")


## ----eval = FALSE-----------------------------
## knitr::purl("04_word_embedding_pre.Rmd",
##             "04_word_embedding_pre.r")

