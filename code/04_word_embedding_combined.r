# Install packages
if (!require(pacman)) install.packages("pacman")

# Load packages 
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
  reactable,
  kableExtra
)

# for publication-friendly theme 
theme_set(theme_pubr())

# Custom functions
file.list <- glue("{here('functions')}/{ 
     list.files(here('functions'))}")

purrr::walk(file.list, source)

# Load files
tidy_word_vectors_pre <- readRDS(file = here("processed_data", "tidy_word_vectors_pre.Rdata"))

tidy_word_vectors_post <- readRDS(file = here("processed_data", "tidy_word_vectors_post.Rdata"))

# Word embedding analysis 
pre_table <- tidy_word_vectors_pre %>%
  nearest_neighbors("chinese_virus") %>%
  filter(item1 != "chinese_virus") %>%
  filter(!str_detect(item1, "http")) %>%
  top_n(20, abs(value)) %>%
  mutate(value = round(value,2)) %>%
  rename(Word = item1,
         Smilarity = value) %>%
  rowid_to_column("Rank")

post_table <- tidy_word_vectors_post %>%
  nearest_neighbors("chinese_virus") %>%
  filter(item1 != "chinese_virus") %>%
  filter(!str_detect(item1, "http")) %>%
  top_n(20, abs(value)) %>%
  mutate(value = round(value,2)) %>%
  rename(Word = item1,
         Similarity = value) 

long_table <- cbind(pre_table, post_table)

kbl(long_table, booktabs = T, format = "latex") %>%
  kable_styling(position = "center", full_width = F) %>%
  add_header_above(c("Pre-Trump speech" = 3, 
                     "Post-Trump speech" = 2)) 