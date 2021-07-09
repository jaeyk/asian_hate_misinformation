## -------------------------------------
pacman::p_load(data.table, # fast data manipulation
               tidyverse, # tidyverse 
               ggpubr, # arranging ggplots   
               ggthemes, # fancy ggplot themes
               here, # reproducibility 
               patchwork, # easy ggarrange
               ggsci, # pubs 
               ggrepel, # annotate ggplot   
               glue, # string + object 
               purrr, # functional programming 
               udpipe,
               tm,
               tidytext,
               future,
               furrr)
               
# for publication-friendly theme 
theme_set(theme_pubr())

# Custom functions
file.list <- glue("{here('functions')}/{ 
     list.files(here('functions'))}")

purrr::walk(file.list, source)


## ----eval = FALSE---------------------
## df <- fread(here("processed_data", "text_ready.csv"))
## 
## df <- df %>%
##   filter(wuhan_virus == 1 | chinese_virus == 1 |
##          kung_flu == 1)
## 
## df <- subset(df, date >= as.Date(c("2020-03-16")))


## ----eval = FALSE---------------------
## df <- df %>%
##   mutate(trump = if_else(str_detect(tolower(full_text), "trump"), "trump", "non-trump")) %>%
##   group_by(trump)


## ----eval = FALSE---------------------
## df$full_text <- tm::removePunctuation(df$full_text)
## df$full_text <- tm::removeNumbers(df$full_text)
## 
## saveRDS(df, file = here("processed_data", "cleaned_text.Rdata"))


## ----eval = FALSE---------------------
## cleaned_text <- readRDS(here("processed_data", "cleaned_text.Rdata"))
## 
## #udpipe_download_model(language = "english")
## 
## no_cores <- availableCores() - 1
## plan(multicore, workers = no_cores)
## 
## corpus_splitted <- split(cleaned_text$full_text, seq(1, nrow(df), by = 100))
## 
## annotation <- furrr::future_map_dfr(corpus_splitted, annotate_splits, .options = furrr_options(seed = T))
## 
## saveRDS(annotation, file = here("processed_data", "annotation.Rdata"))


## -------------------------------------
x <- readRDS(here("processed_data", "annotation.Rdata")) %>% data.frame()

stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))

data(stop_words)

stats <- stats %>%
  anti_join(stop_words, by = c("key" = "word"))

stats %>%
  top_n(10, freq) %>%
  ggplot(aes(x = key, y = freq)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 10 most occuring verbs",
         x = "",
         y = "")

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")

stats2 <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)

stats2 <- subset(stats2, ngram > 1 & freq > 3)

stats2$key <- factor(stats2$keyword, levels = rev(stats2$keyword))

stats2 %>%
  filter(str_detect(key, "call"))


## -------------------------------------
asian_text <- df %>%
  filter(str_detect(full_text, "asian"))


## -------------------------------------
knitr::purl(here("code", "05_inspection.Rmd"),
            here("code", "05_inspection.r"))

