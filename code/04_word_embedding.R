## -------------------------------------

# Import pkgs
pacman::p_load(
  tidyverse,
  lubridate, 
  purrr,
  furrr,
  tm,
  quanteda, 
  here,
  tidytext, # text analysis 
  ggthemes,
  text2vec, # word embedding 
  widyr,
  patchwork, # arranging ggplots
  glue,
  # deep learning 
  text2vec,
  # network graph
  igraph,
  ggraph,
  # structural breakpoint
  strucchange,
  pbapply
)

devtools::install_github("prodriguezsosa/conText")
library(conText)
theme_set(theme_clean())

source(here("functions", "utils.R"))



## -------------------------------------
load(here("processed_data", "df_ready.Rdata"))


## -------------------------------------
local_glove <- df2vec(df_usa)
saveRDS(local_glove, file = here("processed_data/local_glove.Rdata"))

rm(local_glove)

fcm_cr <- df2cm(df_usa)

saveRDS(fcm_cr,  file = here("processed_data/fcm_cr.Rdata"))

rm(fcm_cr)

load(file = here("processed_data/local_glove.Rdata"))

load(file = here("processed_data/fcm_cr.Rdata"))

local_transform <- df2ltm(df_usa, fcm_cr, local_glove)

saveRDS(local_transform,
     file = here("processed_data/lcoal_transform.Rdata"))



## ----eval = FALSE---------------------
## knitr::purl(input = here::here("code", "04_word_embedding.Rmd"),
##             output = here::here("code", "04_word_embedding.R"))

