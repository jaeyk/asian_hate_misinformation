
# Import packages 

pacman::p_load(data.table, # for fast data manipulation
               jsonlite, # for importing json data
               tidyverse, # for tidyverse
               here, # for reproducibility
               tidyjson, # for json data manipulation
               purrr, # for functional programming
               tictoc) # for performance test 

devtools::install_github("jaeyk/tidytweetjson",
                         dependencies = TRUE)

library(tidytweetjson)

# Parse all 

future::plan("multiprocess")

tictoc::tic()
df <- jsonl_to_df_all(dir_path = here("processed_data", "splitted_data"), simplify = TRUE)
tictoc::toc()

# Save it.

saveRDS(df, here("processed_data", "parsed.rds"))