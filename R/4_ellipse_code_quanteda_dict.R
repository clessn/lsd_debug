library(tidyverse)
library(quanteda)

df_raw <- readRDS("~/Dropbox/Shared/_SharedFolder_article_syrie-ukraine/Data/analysis/dataset_prepped_pipeline.rds")

df <- df_raw %>%
  as.data.frame() %>%
  mutate(
    country = ifelse(grepl("syr", batch, ignore.case = TRUE), "syria", "ukraine"),
    year = lubridate::year(date),
    doc_id = paste(id, id_sentence, sep = "_")  # Create unique document IDs
  ) %>%
  select(id, id_sentence, media_id, date, year, country, body_prepped)

results <- ellipsetxt::run_dictionary(df, body_prepped, data_dictionary_LSD2015 , verbose = TRUE)

df_sentiments <- cbind(df, results) 

df_sentiments <- df_sentiments %>%
  mutate(
    total_words = str_count(body_prepped, "\\S+"),
    proportion_positive = (positive + neg_negative) / total_words,
    proportion_negative = (negative + neg_positive) / total_words,
    tone_index = proportion_positive - proportion_negative
  )

df_print <- df_selected %>%
  group_by(country) %>%
  summarise(
    mean_tone_index = mean(tone_index, na.rm = TRUE),
    combo = "ellipse_code_quanteda_dict"
  )

print(df_print)
saveRDS(df_print, "data/ecqd.rds")
