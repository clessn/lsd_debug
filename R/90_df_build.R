library(dplyr)

df <- readRDS("~/Dropbox/Shared/_SharedFolder_article_syrie-ukraine/Data/analysis/dataset_prepped_pipeline.rds")

## Sample 1000 rows from df
df_sample <- df %>%
  as.data.frame() %>%
  sample_n(1000)

saveRDS(df_sample, "data/data.rds")
