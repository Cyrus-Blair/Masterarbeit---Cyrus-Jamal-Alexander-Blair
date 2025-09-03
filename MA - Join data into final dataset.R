# ------------------------------------------------------------------------------
# MA - Transforming Top Products per Keyword 
# ------------------------------------------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(fixest)
library(purrr)
library(broom)
library(tidyr)



rm(list= ls())
# options(scipen = 999)

setwd("C:/Users/Cyrus Blair/Documents/Uni/Master/Masterarbeit/Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon/Code/Getting Sistrix Data via API/Data")



# ------------------------------------------------------------------------------
# Daten einlesen
# ------------------------------------------------------------------------------

## Details zu Produkten
Top_Products_per_Key_Word_with_Details_sampled_new <- read.csv("Top_Products_per_Key_Word_with_Details_sampled_new.csv", sep = ";")


## Sistrixmetric per Product per Domain per week
de_partial_1 <- read.csv("de_partial_week_1-15.csv", sep = ";")
de_partial_2 <- read.csv("de_partial_week_16-31.csv", sep = ";")
de_partial_3 <- read.csv("de_partial_week_32-35.csv", sep = ";")

fr_partial_1 <- read.csv("fr_partial_week_1-14.csv", sep = ";")
fr_partial_2 <- read.csv("fr_partial_week_15-22.csv", sep = ";")
fr_partial_3 <- read.csv("fr_partial_week_23-35.csv", sep = ";")

uk_partial_1 <- read.csv("uk_partial_week_1-26.csv", sep = ";")
uk_partial_2 <- read.csv("uk_partial_week_27-35.csv", sep = ";")

it_partial_1 <- read.csv("it_partial_week_1-27.csv", sep = ";")
it_partial_2 <- read.csv("it_partial_week_28-35.csv", sep = ";")

es_partial_1 <- read.csv("es_partial_week_1-26.csv", sep = ";")
es_partial_2 <- read.csv("es_partial_week_27-33.csv", sep = ";")
es_partial_3 <- read.csv("es_partial_week_34-35.csv", sep = ";")



# ------------------------------------------------------------------------------
# Daten zusammenführen
# ------------------------------------------------------------------------------

# Sichtbarkeitsdaten kombinieren
sistrix_all <- bind_rows(de_partial_1, de_partial_2, de_partial_3,
                         fr_partial_1, fr_partial_2, fr_partial_3,
                         uk_partial_1, uk_partial_2,
                         it_partial_1, it_partial_2,
                         es_partial_1, es_partial_2, es_partial_3,
                         )


# Spalten auslassen, die du nicht brauchst und asin umbenennen für einfacheren Join 
products_filtered <- Top_Products_per_Key_Word_with_Details_sampled_new %>%
  select(
    -country,     # Schon vorhanden
    -keyword,
    -search_date, 
    -closest_date, 
    -organic_position, 
    -keywords_count
  ) %>%
  rename(
    product.asin = asin,
    product.title = title)

sistrix_all <- sistrix_all %>% select(-product.title)


# Join über ASIN
sistrix_enriched <- sistrix_all %>%
  left_join(products_filtered, by = "product.asin")

glimpse(sistrix_enriched)
names(sistrix_enriched)
summary(sistrix_enriched)




# ------------------------------------------------------------------------------
# Neuen Datansatz speichern
# ------------------------------------------------------------------------------

# Save the dataset with semicolon separator and . as decimal seperator and ignores ; in product title
write.table(sistrix_enriched, 
            "MA_complete_dataset.csv", 
            sep = ";", 
            dec = ".", 
            row.names = FALSE, 
            quote = TRUE)












