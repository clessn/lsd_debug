library(tube)
library(tidyverse)
library(clessnize)

condwd <- ellipse_connect(env = "DEV")
con <- ellipse_connect(env = "PROD")

ellipse_discover(con, "r-media-headlines")

# AWS timestamps are in UTC
to_localtime <- function(dt, tz = "America/Toronto") {
  lubridate::with_tz(dt, tzone = tz)
}


date_cible <- as.Date("2024-11-07")

# Modifier la requête
df <- ellipse_query(con, "r-media-headlines") |>
  dplyr::mutate(
    extraction_date = as.Date(extraction_date)  # S'assurer que extraction_date est de type Date
  ) |>
  dplyr::filter(media_id %in% c("FXN", "CNN")) |>
  dplyr::filter(extraction_date >= (date_cible - days(60)) &  # 60 jours avant le 7 novembre
                  extraction_date <= date_cible) |>  # jusqu'au 7 novembre
  dplyr::collect() |>
  dplyr::mutate(extraction_localtime =
                  to_localtime(ymd_hms(paste0(as.character(extraction_date),
                                              extraction_time))),
                extraction_localdate = date(extraction_localtime)) |>
  rename(date = extraction_localdate)

# Dictionnaire de sentiments

df_dict_sent <- tube::ellipse_query(con, "dict-sentiments") |>
  dplyr::collect() |>
  dplyr::select(-id, -metadata_lake_item_key, -metadata_url, -version)

####################################################################
####### ÉTAPE 3: APPLIQUER LES DICTIONNAIRE ########################
####################################################################

# Pour installer le package ellipsetxt, rouler la fonction suivante :
# > devtools::install_github("ellipse-science/ellipsetxt")

## Désagréger les articles par phrases

pattern <- "(?<!\\bM)\\.\\s+(?=[A-Z])|\\.\\s*$"

df_split <- df |>
  mutate(body = strsplit(as.character(body), pattern, perl = TRUE)) |>
  unnest_longer(col = body)

df_split <- df_split |>
  mutate(body = trimws(body))

## Appliquer le dictionnaire

quanteda_dict_sent <-
  split(df_dict_sent$item, df_dict_sent$category) |>
  quanteda::dictionary()

dictionary_sent <- quanteda_dict_sent

result_sent <- ellipsetxt::run_dictionary(df_split, body, dictionary_sent, verbose = TRUE)

# Combiner les dataframes

df_sent <- cbind(df_split, result_sent) 
df_sent <- df_sent %>% 
  select(-doc_id)

####################################################################
####### SCORE DE TON ######################
####################################################################

df_sent <- df_sent |>
  mutate(
    total_mots_phrase = str_count(body, "\\w+"),
    net_tone_phrase = (positive + neg_positive - negative - neg_negative) / total_mots_phrase
  )

####################################################################
####### ÉVOLUTION NETTE DU TON DANS LE TEMPS ######################
####################################################################

df_summary <- df_sent %>%
  group_by(date, media_id) %>%
  summarise(average_tone = mean(net_tone_phrase, na.rm = TRUE))

long_data <- df_summary %>%
  pivot_longer(cols = -c(date, media_id), 
               names_to = "tone", 
               values_to = "tone_average")

ggplot(long_data, aes(x = date, y = tone_average, color = media_id)) + 
  geom_line(size = 1.2, alpha = 0.3, linetype = "dashed") +  # Trace les lignes avec plus de transparence
  geom_point(size = 3, alpha = 0.3) +   # Ajoute des points avec plus de transparence
  geom_smooth(se = FALSE, size = 2) +  # Ajoute un smooth plus épais
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +  # Ligne horizontale au 0
  scale_y_continuous(limits = c(-0.05, 0.05), expand = expansion(mult = c(0, 0.1)), 
                     breaks = c(-0.05, 0, 0.05),  # Définit les positions des labels
                     labels = c("Negative tone", "Neutral tone", "Positive tone")) +  # Remplace les labels
  scale_color_manual(values = c("CNN" = "red", "FXN" = "blue")) +  # Modifie les couleurs
  labs(title = "",
       x = "",
       y = NULL,
       color = "Media") + 
  theme_classic() +  # Applique ton thème personnalisé
  theme(
    plot.title = element_text(hjust = 0.3, size = 18),  # Increase title size
    axis.text = element_text(size = 45),  # Increase axis labels size
    axis.title = element_text(size = 45),  # Increase axis title size
    legend.text = element_text(size = 45),  # Increase legend text size
    legend.title = element_text(size = 45)  # Increase legend title size
  )  # Centrer le titre
ggsave("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/mpsa_media_polarization/graphs/net_tone_graph.png", width = 30, height = 17, units = c("cm"), bg = "white")

####################################################################
####### COMPARAISON TRUMP/HARRIS ######################
####################################################################

global_avg <- df_sent %>% 
  group_by(media_id) %>%
  summarise(global_mean_tone = mean(net_tone_phrase, na.rm = TRUE))


df_filtered <- df_sent %>%
  filter(grepl("Harris|Trump", body, ignore.case = TRUE)) %>%
  mutate(person = case_when(
    grepl("Harris", body, ignore.case = TRUE) ~ "Harris",
    grepl("Trump", body, ignore.case = TRUE) ~ "Trump"
  ))

df_summary_candidate <- df_filtered %>%
  group_by(date, media_id, person) %>%
  summarise(average_tone = mean(net_tone_phrase, na.rm = TRUE))


# Jointure du ton global avec les données spécifiques à Trump et Harris
data_adjusted <- df_summary_candidate %>%
  filter(person %in% c("Trump", "Harris")) %>%  # Filtrer uniquement Trump et Harris
  left_join(global_avg, by = "media_id") %>%  # Joindre le ton global
  mutate(adjusted_tone = average_tone - global_mean_tone)  # Ajuster le ton par rapport au ton global

# Calcul du ton moyen ajusté pour chaque combinaison média-personne
data_aggregated <- data_adjusted %>%
  group_by(media_id, person) %>%
  summarise(mean_adjusted_tone = mean(adjusted_tone, na.rm = TRUE))

# Création du graphique à barres

ggplot(data_aggregated, aes(x = media_id, y = mean_adjusted_tone, fill = person)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, alpha = 0.7) +
  scale_fill_manual(values = c("Trump" = "#FF0000", "Harris" = "#0076CE")) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + # Ligne de référence à 0
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_x_discrete(labels = c("CNN" = "CNN", 
                              "FXN" = "Fox News")) +
  scale_y_continuous(breaks = c(-0.002, 0, 0.002),  # Définstainit les positions des labels
                     labels = c("Negative Tone", "Neutral Tone", "Positive Tone"),
                     expand = expansion(mult = c(0.1, 0.2))) +  # Expand y-axis space
  theme_classic() +  # Applique ton thème personnalisé
  theme(
    plot.title = element_text(hjust = 0.4, size = 80),
    axis.text.x = element_text(size = 60),
    axis.text.y = element_text(size = 50),  # Adjust Y label size for more space
    axis.title.y = element_text(size = 60),
    legend.text = element_text(size = 70),
    legend.title = element_blank(),
    plot.caption = element_text(
      size = 50,
      family = "PixelOperatorSC",
      hjust = 0,  # Justifié à gauche
      vjust = -3,
      color = colorspace::darken("black", 0.3),  # Couleur assombrie pour le caption
      margin = margin(t = 30, b = 5, l = -75, r = 20),
      lineheight = 0.3
    ))

ggsave("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/mpsa_media_polarization/graphs/comparative_tone_graph.png", width = 30, height = 17, units = c("cm"), bg = "white")




####################################################################
####### COMPARAISON TRUMP/HARRIS DANS LE TEMPS #####################
####################################################################

# Filter data for each media
data_fox <- data_adjusted %>% filter(media_id == "FXN")
data_cnn <- data_adjusted %>% filter(media_id == "CNN")

# Function to create the plot
plot_tone <- function(data, media_id) {
  ggplot(data, aes(x = date, y = adjusted_tone, color = person)) +
    geom_line(size = 1.2, alpha = 0.3, linetype = "dashed") +  # Trace les lignes avec plus de transparence
    geom_point(size = 3, alpha = 0.3) +   # Ajoute des points avec plus de transparence
    geom_smooth(se = FALSE, size = 2) +  # Ajoute un smooth plus épais
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1)+
    labs(
      title = paste("Aggregated Tone of Trump and Harris on", media_id),
      x = NULL,
      y = NULL,
      color = "Candidate"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.3, size = 50),  # Increase title size
      axis.text = element_text(size = 35),  # Increase axis labels size
      axis.title = element_text(size = 35),  # Increase axis title size
      legend.text = element_text(size = 35),  # Increase legend text size
      legend.title = element_text(size = 35)  # Increase legend title size
    )+
    scale_color_manual(values = c("Trump" = "red", "Harris" = "blue"))
}

# Generate plots
plot_fox <- plot_tone(data_fox, "FXN")
plot_cnn <- plot_tone(data_cnn, "CNN")

# Display plots
print(plot_fox)
print(plot_cnn)

library(patchwork)

combined_plot <- plot_fox | plot_cnn

combined_plot

ggsave("/Users/jeremiedrouin/Dropbox/Travail/Universite_Laval/CLESSN/Publications/mpsa_media_polarization/graphs/comparative_timetrend_graph.png", width = 30, height = 17, units = c("cm"), bg = "white")
