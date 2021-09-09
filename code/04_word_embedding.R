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

rm(df_usa)

corpus <- df %>%
  filter(country == "us" | country == "US") %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  mutate(Label = if_else(Label == "Hate", 1, 0))

corpus$date <- as.Date(corpus$date)
corpus$clean_text <- clean_text(corpus$full_text)
rm(df)


## ---------------------------------
local_glove <- df2vec(corpus)
local_transform <- df2ltm(corpus, local_glove)

save(local_glove, local_transform, 
     file = here("processed_data/context_bg.Rdata"))


## ---------------------------------
mod <- conText(formula = chinese ~ trump_speech + Label, 
          data = corpus, 
          text_var = 'clean_text', 
          pre_trained = local_glove,
          transform = TRUE, 
          transform_matrix = local_transform, 
          bootstrap = TRUE, 
          num_bootstraps = 20, 
          stratify_by = c('trump_speech'),
          permute = TRUE, 
          num_permutations = 100, 
          window = 6, 
          valuetype = 'fixed', 
          case_insensitive = TRUE, 
          hard_cut = FALSE, 
          verbose = FALSE)


## ---------------------------------
plot_tibble <- mod$normed_betas %>% 
  mutate(Coefficient = c("Trump speech", "Hate label")) %>% 
  mutate(Coefficient = factor(Coefficient, levels = Coefficient))

plot_tibble


## ---------------------------------
reg_plot <- plot_tibble %>%
  ggplot(aes(x = Coefficient, y = Normed_Estimate)) + 
  geom_bar(position = position_dodge(), 
    stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = Normed_Estimate - 
    1.96 * Std.Error, ymax = Normed_Estimate + 1.96 * Std.Error), size = 0.75, width = 0.15, 
    position = position_dodge(0.9)) + 
  ylab("Norm of beta hats") + 
  geom_text(aes(label = c("***", "***")), position = position_dodge(width = 0.9), hjust = 0.5, vjust = -1, size = 8) + 
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 10)) +
  ylim(c(0,1.4)) +
  labs(title = "Keyword: Chinese")

reg_plot
ggsave(here("outputs", "embed_reg.png"))


## ---------------------------------
hate_post_bt <- get_bt_terms(1, 1, "chinese", 100)
hate_pre_bt <- get_bt_terms(0, 1, "chinese", 200)
counterhate_post_bt <- get_bt_terms(1, 0, "chinese", 100)
counterhate_pre_bt <- get_bt_terms(0, 0, "chinese", 200)


## ---------------------------------
terms2plot(hate_pre_bt, counterhate_pre_bt, "Chinese", "Pre-Trump Speech") + terms2plot(hate_post_bt, counterhate_post_bt, "Chinese", "Post-Trump Speech")

ggsave(here("outputs", "bt_chinese.png"), width = 10, height = 10)

terms2plot_sep(hate_pre_bt, counterhate_pre_bt, "Chinese", "Pre-Trump Speech") + terms2plot_sep(hate_post_bt, counterhate_post_bt, "Chinese", "Post-Trump Speech")

ggsave(here("outputs", "bt_chinese_sep.png"), width = 10, height = 10)


## ---------------------------------
nns_hate <- (con2nplot(subset(corpus, trump_speech == 0), c("chinese"), local_glove, local_transform) + labs(title = "Pre-Trump speech", subtitle = "Keyword: Chinese")) +
(con2nplot(subset(corpus, trump_speech == 1), c("chinese"), local_glove, local_transform) + labs(title = "Post-Trump speech", subtitle = "Keyword: Chinese"))

nns_hate 

ggsave(here("outputs", "nns_hate.png"),
       width = 15, height = 10
       )


## ---------------------------------
latino_pre_black_ex <- get_examples(1, 1, c("black", "african_american", "negro"), 20)
asian_pre_black_ex <- get_examples(0, 1, c("black", "african_american", "negro"), 20)
latino_post_black_ex <- get_examples(1, 0, c("black", "african_american", "negro"), 20)
asian_post_black_ex <- get_examples(0, 0, c("black", "african_american", "negro"), 20)
latino_pre_disc_ex <- get_examples(1, 1, "discrimination", 20)
asian_pre_disc_ex <- get_examples(0, 1, "discrimination", 20)
latino_post_disc_ex <- get_examples(1, 0, "discrimination", 20)
asian_post_disc_ex <- get_examples(0, 0, "discrimination", 20)


## ----eval = FALSE-----------------
## knitr::purl(input = here::here("code", "04_word_embedding.Rmd"),
##             output = here::here("code", "04_word_embedding.R"))

