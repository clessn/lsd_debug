source("R/1_custom_code_quanteda_dict.R")
source("R/2_custom_code_ellipse_dict.R")
source("R/3_ellipse_code_ellipse_dict.R")
source("R/4_ellipse_code_quanteda_dict.R")
source("R/5_lo_code_quanteda_dict.R")
source("R/6_lo_code_ellipse_dict.R")

df_1 <- readRDS("data/ccqd.rds")
df_2 <- readRDS("data/cced.rds")
df_3 <- readRDS("data/eced.rds")
df_4 <- readRDS("data/ecqd.rds")
df_5 <- readRDS("data/lcqd.rds")
df_6 <- readRDS("data/lced.rds")

df_all <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6)

print(df_all)
