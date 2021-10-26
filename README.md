# Replication Data and Code for Misinformation and Hate Speech: The Case of Anti-Asian Hate Speech During the COVID-19 Pandemic

Authors: Jae Yeon Kim (corresponding author, jkim638@jhu.edu) and Aniket Kesari 

**Session information**

1. Programming languages

* R version 4.0.4 (2021-02-15)
* Python 3.8.8
* Bash 5.1.4(1)-release

2. Operation system

* Platform: x86_64-pc-linux-gnu (64-bit)
* Running under: Ubuntu 21.04

## Data collection 

### Raw data: tweet_ids

* [download.sh](https://github.com/jaeyk/asian_hate_misinformation/raw/master/raw_data/download.sh): The data source is [COVID-Hate tweet data](http://claws.cc.gatech.edu/covid/#dataset) created by [Ziems et al (2020)](http://claws.cc.gatech.edu/covid). The original dataset only provided tweet IDs, not tweets, following Twitter's [developer terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases). We turned these tweet IDs back into a JSON file (tweets) using [Twarc](https://github.com/DocNow/twarc). This process is called hydrating and is very time-consuming. We used [tidytweetjson](https://github.com/jaeyk/tidytweetjson), an R package developed by Jae Yeon Kim (one of the co-authors), to parses this large JSON file into a tidyverse-ready data frame. 

* [tweet_ids](https://github.com/jaeyk/asian_hate_misinformation/raw/master/raw_data/tweet_ids): To help replication, We also saved the IDs of the tweets by typing the following command in the terminal: `grep "INFO archived" twarc.log | awk '{print $5}' > tweet_ids` (saved in [`\raw_data`](https://github.com/jaeyk/asian_hate_misinformation/blob/master/raw_data/tweet_ids)).

### Processed data: word embedding 

* [context_bg.Rdata](https://github.com/jaeyk/asian_hate_misinformation/raw/master/processed_data/context_bg.Rdata): R data file containing a set of (GloVe) pre-trained embeddings (`local glove`) and a transformation matrix specific to the pre-trained embeddings (`local_transform`)

### Replication code

#### Data collection and wrangling 

* [01_filter_geocoded_tweet_ids.Rmd](https://github.com/jaeyk/asian_hate_misinformation/blob/master/code/01_filter_geocoded_tweet_ids.Rmd): R markdown for filtering the geocoded tweets from the original dataset

* [02_parse.r](https://github.com/jaeyk/asian_hate_misinformation/blob/master/code/02_parse.r): R script for parsing the large JSON file (19.6 GB)

* [03_wrangle.Rmd](https://github.com/jaeyk/asian_hate_misinformation/blob/master/code/03_wrangle.Rmd): R markdown file for wrangling the tidied JSON file 

#### Data analysis 

* [04_eda.R](https://github.com/jaeyk/asian_hate_misinformation/blob/master/code/04_eda.R) this R script file produced [Figure 1](https://github.com/jaeyk/asian_hate_misinformation/blob/master/outputs/chinavirusprepostlabel.png) 

* [05_word_embedding.Rmd](https://github.com/jaeyk/asian_hate_misinformation/blob/master/code/05_word_embedding.Rmd) this R markdown file produced Figures [2](https://github.com/jaeyk/asian_hate_misinformation/blob/master/outputs/propchange.png), [3](https://github.com/jaeyk/asian_hate_misinformation/blob/master/outputs/embedreg.png), and [4](https://github.com/jaeyk/asian_hate_misinformation/blob/master/outputs/btchinesesep.png)

* [06_simulation.r](https://github.com/jaeyk/asian_hate_misinformation/blob/master/code/06_simulation.r) This R file was used to obtain the stable point estimate from the word embedding regression. 
