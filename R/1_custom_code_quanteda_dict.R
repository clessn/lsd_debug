# Load necessary libraries
library(dplyr)
library(quanteda)
library(furrr)
library(stringr)

# Enable parallel processing
plan(multisession)  # Use all available CPU cores

# 1. Load and prepare data ------------------------------------------------
df <- readRDS("~/Dropbox/Shared/_SharedFolder_article_syrie-ukraine/Data/analysis/dataset_prepped_pipeline.rds")

df_selected <- df %>%
  mutate(
    country = ifelse(grepl("syr", batch, ignore.case = TRUE), "syria", "ukraine"),
    year = lubridate::year(date),
    doc_id = paste(id, id_sentence, sep = "_")  # Create unique document IDs
  ) %>%
  select(doc_id, id, id_sentence, media_id, date, year, country, body_prepped)

# 2. Split data into chunks for parallel processing ----------------------
chunk_size <- 1000  # Adjust based on your system's memory
chunks <- split(df_selected, (seq(nrow(df_selected)) - 1) %/% chunk_size)

# 3. Define processing function for each chunk ---------------------------
process_chunk <- function(chunk) {
  texts <- setNames(chunk$body_prepped, chunk$doc_id)  # Named text vector
  dfm <- tokens(texts) %>%
    tokens_compound(data_dictionary_LSD2015) %>%
    dfm() %>%
    dfm_lookup(data_dictionary_LSD2015)
  
  # Convert to data.frame and ensure document IDs are preserved
  results <- convert(dfm, to = "data.frame")
  return(results)
}

# 4. Process chunks in parallel ------------------------------------------
processed_chunks <- future_map(chunks, process_chunk, .progress = TRUE)

# 5. Combine results -----------------------------------------------------
final_results <- bind_rows(processed_chunks)

# 6. Merge results back into the original data ---------------------------
df_selected <- df_selected %>%
  left_join(final_results, by = "doc_id")

df_selected <- df_selected %>%
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
    combo = "custom_code_quanteda_dict"
  )

print(df_print)
saveRDS(df_print, "data/ccqd.rds")
