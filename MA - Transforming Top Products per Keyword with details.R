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

data <- read.csv("Top_Products_per_Key_Word_with_Details.csv", sep = ";")




# ------------------------------------------------------------------------------
# Amazon First-Party Brand List (extend as needed)
# ------------------------------------------------------------------------------

amazon_brands <- c("206 Collective", "28 Palms", "365 Everyday Value", "Aequator", "Alkove", 
                   "Amazing Baby", "Amazon Basics", "Amazon Collection", "Amazon Elements", 
                   "Amazon Essentials", "AmazonBasics", "AmazonCommercial", "Amcar-Ambike", 
                   "Arabella", "ARCANITE", "Athlyt", "Aurique", "Auto Companion", "Basic Care", 
                   "BB AutoSports", "Beauty Bar", "Belei", "Bruzzzler", "Buttoned Down", 
                   "Buy Box Experts", "Byron Statics", "Cable Stitch", "CarFashion", "CarPlus", 
                   "Certeo", "Chicreat", "Coastal Blue", "Comfort Zone", "Compass Road", "Core 10", 
                   "CZON SPORTS", "Daily Ritual", "Denali", "DIY Doctor", "Downcy", "Ella Moon", 
                   "EONO", "FA Sports", "Feature Home", "Fée", "find.", "Fly-Bye", "Formegolose", 
                   "Franklin & Freeman", "Glart", "Goodthreads", "Gözze", "HALE", "Happy Belly", 
                   "Happy Belly Select", "HERMO", "HIKARO", "Hislon", "Homecall", "Illuminate", 
                   "iMW", "Iris & Lilly", "Iris Ohyama", "James & Erin", "Kid Nation", "Kitchen Stories", 
                   "Lark & Ro", "LEVIVO", "LNIEGE", "Luby", "Luxum", "LYCANDER", "Mad Monkey", 
                   "Madeline Kelly", "Mae", "Mama Bear", "MAXTOOLS", "McPaint", "Meraki", "Mono", 
                   "Moon and Back", "Mountain Falls", "Movian", "Myhabit", "Nature's Wonder", 
                   "North Eleven", "novasmart", "NuPro", "Office hippo", "Oversteel", "Paris Sunday", 
                   "Peak Velocity", "Pet Craft Supply", "Petper", "Pike Street", "Pinzon (by Amazon)", 
                   "Pinzon by Amazon", "Plumberry", "Presto!", "Probus", "Quality Durables Co.", 
                   "Rally and Roar", "Red Wagon", "Rhino Racking", "Rivet", "RTS", "Scout + Ro", 
                   "Simple Joys by Carter's", "SimpliSafe", "Single Cow Burger", "Small Parts", 
                   "Smart is Beautiful", "Smart is Beautiful.", "Solimo", "SoundHealth", "Spotted Zebra", 
                   "Starter", "Stone & Beam", "Store&Style", "Strandgut07", "Strathwood", "T-Equip", 
                   "Tenten", "The Drop", "The Fix", "Timbus", "TouchGuard", "Trek Support", "Truth & Fable", 
                   "Tummy Tub", "Ultrakidz", "Ultranatura", "Ultrasport", "Ultratec", "UMI", "Venga!", 
                   "Vida Designs", "Wag", "WellWear", "Whole Foods Market", "Wickedly Prime", 
                   "Windsoroyal", "XAED", "Amazon-Marke")




# ------------------------------------------------------------------------------
# Clean Data and Add Flags
# ------------------------------------------------------------------------------

# Ensure numeric type
data_clean <- data %>%
  mutate(organic_position = as.numeric(as.character(organic_position)))

data_clean <- data_clean %>%
  distinct(asin, .keep_all = TRUE) %>%                            # Remove duplicate ASINs
  filter(!is.na(seller_id) & seller_id != "" & seller_id != " ")  # Remove rows with missing or empty seller_id

# Add 'private_label_amazon' flag: 1 if product name contains any Amazon brand, else 0
pattern <- paste0("\\b(", paste0(tolower(amazon_brands), collapse = "|"), ")\\b")

data_flagged <- data_clean %>%
  mutate(
    private_label_amazon = if_else(
      str_detect(tolower(title), pattern),
      1, 0
    ),
    amazon_seller = if_else(seller_name == "Amazon", 1, 0)
  )

# Add new flag: 1 if "amazon" appears in title, else 0
data_flagged_check <- data_flagged %>%
  mutate(
    title_mentions_amazon = if_else(str_detect(tolower(title), "amazon"), 1, 0)
  )

# Show all mismatched observations
mismatches <- data_flagged_check %>%
  filter(private_label_amazon != title_mentions_amazon)

# View how many mismatches there are
nrow(mismatches)


# ------------------------------------------------------------------------------
# Descriptive Results
# ------------------------------------------------------------------------------

summary(data_flagged)

# Number and percentage of Amazon sellers
amazon_stats <- data_flagged %>%
  summarise(
    n_amazon_sellers = sum(amazon_seller == 1, na.rm = TRUE),
    percentage_amazon = mean(amazon_seller == 1, na.rm = TRUE) * 100
  )

# Number and percentage of Private Label Amazon sellers
private_label_stats <- data_flagged %>%
  summarise(
    n_private_label_amazon = sum(private_label_amazon == 1, na.rm = TRUE),
    percentage_private_label_amazon = mean(private_label_amazon == 1, na.rm = TRUE) * 100
  )

print("Number of observations in data_flagged:")
print(nrow(data_flagged))

print("Amazon sellers stats:")
print(amazon_stats)

print("Private label Amazon sellers stats:")
print(private_label_stats)

print("Number of sellers:")
print(n_distinct(data_flagged$seller_name))





# ------------------------------------------------------------------------------
# Balance Test: Compare Means of Covariates by Amazon Seller Flag
# ------------------------------------------------------------------------------

# Define covariates to compare
covariates <- c("price", "avg_review", "reviews", "keywords_count")

# Create balance table with covariates as rows, Amazon vs. Non-Amazon as columns
balance_table <- data_flagged %>%
  group_by(amazon_seller) %>%
  summarise(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  # Transpose and convert to data.frame
  t() %>%
  as.data.frame()

# Clean column names and rows (exclude redundant 'Variable' column)
colnames(balance_table) <- c("Non-Amazon", "Amazon")  # assuming 0 = Non-Amazon, 1 = Amazon
balance_table <- balance_table[-1, , drop = FALSE]    # Remove 'amazon_seller' row if still present
rownames(balance_table) <- gsub("\\.\\d+", "", rownames(balance_table))  # Optional cleanup of row names
balance_table <- balance_table[, c("Non-Amazon", "Amazon")]  # Drop redundant column if previously added

# Print the balance table
print(balance_table)




# ------------------------------------------------------------------------------
# Neuen Datansatz kreieren mit Stratified Sampling (Private Label und Seller)
# ------------------------------------------------------------------------------

# 1. Include all private label = 1
pl_data <- data_flagged %>%
  filter(private_label_amazon == 1)

# 2. Count private label Amazon sellers
n_total <- 400
n_pl <- nrow(pl_data)

pl_amazon <- pl_data %>% filter(amazon_seller == 1) %>% nrow()
pl_non_amazon <- n_pl - pl_amazon

# 3. Determine how many to sample to reach 50/50 seller split
target_per_group <- n_total / 2
needed_amazon <- target_per_group - pl_amazon
needed_non_amazon <- target_per_group - pl_non_amazon

# 4. Sample from private_label_amazon == 0 to fill up to 400, maintaining seller balance
fill_amazon <- data_flagged %>%
  filter(private_label_amazon == 0, amazon_seller == 1) %>%
  sample_n(needed_amazon)

fill_non_amazon <- data_flagged %>%
  filter(private_label_amazon == 0, amazon_seller == 0) %>%
  sample_n(needed_non_amazon)

# 5. Combine all
sample_data <- bind_rows(pl_data, fill_amazon, fill_non_amazon)

# 6. Check results
nrow(sample_data)  # Should be 400
table(sample_data$amazon_seller)  # Should be 200/200
table(sample_data$private_label_amazon)
table(sample_data$amazon_seller, sample_data$private_label_amazon)






# ------------------------------------------------------------------------------
# Balance Test: Compare Means of Covariates by Amazon Seller Flag (sample_data)
# ------------------------------------------------------------------------------

# Define covariates to compare
covariates <- c("price", "avg_review", "reviews", "keywords_count")

# Create balance table with covariates as rows, Amazon vs. Non-Amazon as columns
balance_table_sampled <- sample_data %>%
  group_by(amazon_seller) %>%
  summarise(across(all_of(covariates), mean, na.rm = TRUE)) %>%
  # Transpose and convert to data.frame
  t() %>%
  as.data.frame()

# Clean column names and rows (exclude redundant 'Variable' column)
colnames(balance_table_sampled) <- c("Non-Amazon", "Amazon")  # assuming 0 = Non-Amazon, 1 = Amazon
balance_table_sampled <- balance_table_sampled[-1, , drop = FALSE]    # Remove 'amazon_seller' row if still present
rownames(balance_table_sampled) <- gsub("\\.\\d+", "", rownames(balance_table_sampled))  # Optional cleanup of row names
balance_table_sampled <- balance_table_sampled[, c("Non-Amazon", "Amazon")]  # Drop redundant column if previously added

# Print the balance table
print(balance_table_sampled)




# ------------------------------------------------------------------------------
# Neuen Datansatz speichern
# ------------------------------------------------------------------------------

# Construct the filename with the number included
output_filename <- paste0("Top_Products_per_Key_Word_with_Details_sampled_new.csv")

# Save the dataset with semicolon separator
write.csv2(sample_data, output_filename, row.names = FALSE)
