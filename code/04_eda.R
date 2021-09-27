# Tidyverse
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, here, ggthemes, patchwork)

# Load us dataframe
load(here("processed_data", "df_ready.Rdata"))

# Load world data
parsed <- readRDS(here("processed_data", "parsed.rds"))

theme_set(theme_clean())

# Convert date variable
df_usa$date <- as.POSIXct(df_usa[["created_at"]], tz = "UTC", format = "%a %b %d %H:%M:%S %z %Y") %>% word(1)

df_usa$date <- as.Date(df_usa$date)

# Plot
non_log <- df_usa %>%
  # filter(wuhan_virus == 1) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(date, Label) %>%
  count(chinese_virus) %>%
  ggplot(aes(x = date,
             y = n,
             col = Label)) +
  geom_point() +
  geom_line(alpha = 0.7) +
  geom_vline(xintercept = as.Date("March 16 2020",
                                  format = "%b %d %Y"
  ), linetype = "dashed") +
  ggtitle("Hate and Counterhate Mentions of \nChinese Virus Over Time") +
  xlab("Date") +
  ylab("Count") +
  facet_wrap(~Label)

log <- df_usa %>%
  # filter(wuhan_virus == 1) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(date, Label) %>%
  count(chinese_virus) %>%
  ggplot(aes(x = date,
         y = log(n),
         col = Label)) +
  geom_point() +
  geom_line(alpha = 0.7) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  ), linetype = "dashed") +
  ggtitle("Hate and Counterhate Mentions of \nChinese Virus Over Time") +
  xlab("Date") +
  ylab("Logged Count") +
  facet_wrap(~Label)

non_log / log
ggsave(here("outputs", "chinavirusprepostlabel.png"))
