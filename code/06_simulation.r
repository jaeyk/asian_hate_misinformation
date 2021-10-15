## ---------------------------------

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
  textclean,
  textstem,
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


## ---------------------------------
load(here("processed_data", "df_ready.Rdata"))


## ---------------------------------
df_usa$org_label <- df_usa$Label
corpus <- df_usa %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  mutate(Label = if_else(Label == "Hate", 1, 0))
rm(df_usa)


## ---------------------------------
corpus$date <- as.POSIXct(corpus[["created_at"]], tz = "UTC", format = "%a %b %d %H:%M:%S %z %Y") %>% word(1)

corpus <- corpus %>%
  mutate(trump_speech = if_else(date >= as.Date(c("2020-03-16")), 1, 0)) %>%
  select(-c("created_at"))

corpus$date <- as.Date(corpus$date)
corpus$clean_text <- clean_text(corpus$full_text)

load(here("processed_data/context_bg.Rdata"))

iter_embed <- function(corpus) {

    mod <- conText(formula = chinesevirus ~ trump_speech + Label,
          data = corpus,
          text_var = 'clean_text',
          pre_trained = local_glove,
          transform = TRUE,
          transform_matrix = local_transform,
          bootstrap = TRUE,
          num_bootstraps = 20,
          stratify_by = c('trump_speech', 'Label'),
          permute = TRUE,
          num_permutations = 100,
          window = 6,
          valuetype = 'fixed',
          case_insensitive = TRUE,
          hard_cut = FALSE,
          verbose = FALSE)

    out <- mod$normed_betas$Normed_Estimate[1]/mod$normed_betas$Normed_Estimate[2]

    return(out)
}

res <- rerun(1000, iter_embed(corpus))

write_rds(res, here("processed_data", "iter.rds"))

res_df <- data.frame(out = unlist(res),
                     num = 1:1000)

mean(res_df$out)
sd(res_df$out)