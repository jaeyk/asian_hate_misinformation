# Tidyverse
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, here, ggthemes)

# Load us dataframe
load(here("processed_data", "df_ready.Rdata"))

# Load world data
parsed <- readRDS(here("processed_data", "parsed.rds"))

theme_set(theme_clean())

## Label

### Base label
df_usa %>%
  ggplot() +
  geom_bar(aes(
    x = Label,
    fill = Label
  )) +
  ggtitle("Proportion of Labels in Twitter Dataset") +
  xlab("Label") +
  ylab("Number of Tweets")

ggsave(here("outputs", "prop_label.png"))

### wuhan_virus ==1 by label

df_usa %>%
  filter(wuhan_virus == 1) %>%
  ggplot() +
  geom_bar(aes(x = as.factor(wuhan_virus), fill = Label),
    position = "dodge"
  ) +
  ggtitle("Mentions of Wuhan Virus by Label") +
  xlab("Hate Speech Content") +
  ylab("Number of Tweets") +
  theme_clean() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(here("outputs", "mention wuhan label.png"))

df_usa %>%
  filter(chinese_virus == 1) %>%
  ggplot() +
  geom_bar(aes(x = as.factor(chinese_virus), fill = Label),
    position = "dodge"
  ) +
  ggtitle("Mentions of Chinese Virus by Label") +
  xlab("Hate Speech Content") +
  ylab("Number of Tweets") +
  theme_clean() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
ggsave(here("outputs", "mention china virus label.png"))

df_usa %>%
  filter(kung_flu == 1) %>%
  ggplot() +
  geom_bar(aes(x = as.factor(kung_flu), fill = Label),
    position = "dodge"
  ) +
  ggtitle("Mentions of Kung Flu by Label") +
  xlab("Hate Speech Content") +
  ylab("Number of Tweets") +
  theme_clean() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
ggsave(here("outputs", "mention kung flu label.png"))

## Count of virus mentions over time

### Mutate date format
df_usa <- df_usa %>%
  mutate(created_at_date = as.Date(paste(
    str_sub(created_at,
      start = 5,
      end = 10
    ),
    "2020"
  ),
  format = "%b %d %Y"
  ))

df_usa %>%
  group_by(created_at_date) %>%
  mutate(wuhan_count = sum(wuhan_virus)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, wuhan_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Wuhan Virus Tweets Before and After Trump's Tweet") +
  xlab("Date") +
  ylab("Count of Tweets")

ggsave(here("outputs", "wuhan tweets prepost.png"))

df_usa %>%
  group_by(created_at_date) %>%
  mutate(chinese_count = sum(chinese_virus)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, chinese_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Chinese Virus Tweets Before and After Trump's Tweets") +
  xlab("Date") +
  ylab("Count of Tweets")

ggsave(here("outputs", "china virus tweets prepost.png"))

df_usa %>%
  group_by(created_at_date) %>%
  mutate(kung_count = sum(kung_flu)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, kung_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Kung Flu Tweets Before and After Trump's Tweets") +
  xlab("Date") +
  ylab("Count of Tweets")

ggsave(here("outputs", "kung flu tweets prepost.png"))


### Count over time by label

df_usa %>%
  # filter(wuhan_virus == 1) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(created_at_date, Label) %>%
  count(wuhan_virus) %>%
  ggplot() +
  geom_point(aes(
    x = created_at_date,
    y = n,
    color = Label
  )) +
  geom_line(aes(
    x = created_at_date,
    y = n,
    color = Label
  )) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Hate and Counterhate Mentions of \nWuhan Virus Over Time") +
  xlab("Date") +
  ylab("Count")

ggsave(here("outputs", "wuhan tweets prepost label.png"))

df_usa %>%
  # filter(wuhan_virus == 1) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(created_at_date, Label) %>%
  count(chinese_virus) %>%
  ggplot() +
  geom_point(aes(
    x = created_at_date,
    y = n,
    color = Label
  )) +
  geom_line(aes(
    x = created_at_date,
    y = n,
    color = Label
  )) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Hate and Counterhate Mentions of \nChinese Virus Over Time") +
  xlab("Date") +
  ylab("Count") +
  facet_wrap(~Label)

ggsave(here("outputs", "china virus tweets prepost label.png"))


df_usa %>%
  # filter(wuhan_virus == 1) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(created_at_date, Label) %>%
  count(kung_flu) %>%
  ggplot() +
  geom_point(aes(
    x = created_at_date,
    y = n,
    color = Label
  )) +
  geom_line(aes(
    x = created_at_date,
    y = n,
    color = Label
  )) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Hate and Counterhate Mentions of \nKung Flu Over Time") +
  xlab("Date") +
  ylab("Count") +
  theme_clean()
ggsave(here("outputs", "kung flu tweets prepost label.png"))


## Map

parsed_us_state <- parsed %>%
  filter(country_code == "US") %>%
  filter(str_detect(location, gsub(",", "|", toString(state.abb))) == TRUE) %>%
  mutate(abbr = str_sub(location, -2)) %>%
  filter(abbr %in% state.abb)

parsed_us_state_demo <- parsed_us_state %>%
  left_join(statepop) %>%
  inner_join(df_usa %>% mutate(id = tweet_id)) %>%
  group_by(abbr) %>%
  count(wuhan_virus) %>%
  filter(wuhan_virus == 1) %>%
  right_join(statepop) %>%
  ungroup() %>%
  replace_na(list(n = 0))

us_map_data <- map_data("state") %>%
  left_join(statepop %>% mutate(region = tolower(full)))

wuhan_tweets_by_state <- parsed_us_state_demo %>%
  inner_join(us_map_data)

p <- ggplot()
p <- p + geom_polygon(
  data = wuhan_tweets_by_state,
  aes(x = long, y = lat, group = group, fill = n),
  color = "white", size = 0.2
)
p

ggsave(here("outputs", "map of wuhan tweets.png"))

# Comparative Plots

parsed <- parsed %>%
  mutate(
    chinese_virus = as.numeric(str_detect(full_text, "chinavirus|china virus|chinese flu|chineseflu|chinese virus|chinesevirus")),
    kung_flu = as.numeric(str_detect(full_text, "kung flu|kungflu")),
    wuhan_virus = as.numeric(str_detect(full_text, "wuhan virus|wuhanvirus"))
  )


parsed <- parsed %>%
  mutate(created_at_date = as.Date(paste(
    str_sub(created_at,
      start = 5,
      end = 10
    ),
    "2020"
  ),
  format = "%b %d %Y"
  ))

parsed %>%
  mutate(usa_tweet = ifelse(country_code == "US", "US", "Non-US")) %>%
  filter(!is.na(usa_tweet)) %>%
  group_by(created_at_date, usa_tweet) %>%
  mutate(wuhan_count = sum(wuhan_virus)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, wuhan_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Wuhan Virus Tweets Before and After Trump's Tweet") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean() +
  facet_wrap(~usa_tweet)

ggsave(here("outputs", "wuhan comparison.png"))

parsed %>%
  mutate(usa_tweet = ifelse(country_code == "US", "US", "Non-US")) %>%
  filter(!is.na(usa_tweet)) %>%
  group_by(created_at_date, usa_tweet) %>%
  mutate(chinese_count = sum(chinese_virus)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, chinese_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Chinese Virus Tweets Before and After Trump's Tweets") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean() +
  facet_wrap(~usa_tweet)

ggsave(here("outputs", "chinese virus comparison.png"))

parsed %>%
  mutate(usa_tweet = ifelse(country_code == "US", "US", "Non-US")) %>%
  filter(!is.na(usa_tweet)) %>%
  group_by(created_at_date, usa_tweet) %>%
  mutate(kung_count = sum(kung_flu)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, kung_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Kung Flu Tweets Before and After Trump's Tweets") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean() +
  facet_wrap(~usa_tweet)

ggsave(here("outputs", "kung flu comparison.png"))

## More plots
# Hate speech v. misinformation over time - bat/propaganda
bat_soup <- df_usa %>%
  mutate(bat = as.numeric(str_detect(full_text, "bat|bat soup|batsoup"))) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(created_at_date) %>%
  count(bat)

propaganda <- df_usa %>%
  mutate(propaganda = as.numeric(str_detect(full_text, "propaganda|chinese propaganda|chinesepropaganda"))) %>%
  filter(Label %in% c("Hate", "Counterhate")) %>%
  group_by(created_at_date, Label) %>%
  count(propaganda)

ggplot() +
  geom_point(
    data = bat_soup,
    aes(
      x = created_at_date,
      y = n
    ),
    color = "blue"
  ) +
  geom_line(
    data = bat_soup,
    aes(
      x = created_at_date,
      y = n
    ),
    color = "blue"
  ) +
  geom_point(
    data = propaganda,
    aes(
      x = created_at_date,
      y = n
    ),
    color = "red",
    show.legend = TRUE
  ) +
  geom_line(
    data = propaganda,
    aes(
      x = created_at_date,
      y = n
    ),
    color = "red"
  ) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("Mentions of Bat Soup and Propaganda Over Time") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean()

ggsave(here("outputs", "bat propaganda.png"))

# US v. UK/Canada/India/Australia

parsed %>%
  filter(country_code %in% c("US", "GB", "IN", "AU", "CA", "NZ")) %>%
  filter(!is.na(country_code)) %>%
  group_by(created_at_date, country_code) %>%
  mutate(wuhan_count = sum(wuhan_virus)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, wuhan_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("International Comparison of \nWuhan Virus Tweets Before and After Trump's Tweet") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean() +
  facet_wrap(~country_code)

ggsave(here("outputs", "intl comparison wuhan.png"))

parsed %>%
  filter(country_code %in% c("US", "GB", "IN", "AU", "CA", "NZ")) %>%
  filter(!is.na(country_code)) %>%
  group_by(created_at_date, country_code) %>%
  mutate(chinese_count = sum(chinese_virus)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, chinese_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("International Comparison of \nChinese Virus Tweets Before and After Trump's Tweets") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean() +
  facet_wrap(~country_code)
ggsave(here("outputs", "intl comparison chinese virus.png"))

parsed %>%
  filter(country_code %in% c("US", "GB", "IN", "AU", "CA", "NZ")) %>%
  filter(!is.na(country_code)) %>%
  group_by(created_at_date, country_code) %>%
  mutate(kung_flu_count = sum(kung_flu)) %>%
  ggplot() +
  geom_point(aes(x = created_at_date, kung_flu_count)) +
  geom_vline(xintercept = as.Date("March 16 2020",
    format = "%b %d %Y"
  )) +
  ggtitle("International Comparison of \nKung Flu Virus Tweets Before and After Trump's Tweets") +
  xlab("Date") +
  ylab("Count of Tweets") +
  theme_clean() +
  facet_wrap(~country_code)

ggsave(here("outputs", "intl comparison kung flu.png"))
