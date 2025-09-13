
# ------------------------------------------------------------------------------
# MA - Analyse der Sistrix Daten
# ------------------------------------------------------------------------------



# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, ggplot2, dplyr, stringr, lubridate, fixest, purrr, broom, modelsummary, kableExtra, stargazer, webshot2)

# Clean environment
rm(list = ls())

# Set working directory
setwd("C:/Users/Cyrus Blair/Documents/Uni/Master/Masterarbeit/Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon/Code/Analyse der Sistrix Daten/Data")

# Create output directories
dir.create("plots_robustness", showWarnings = FALSE)
dir.create("plots_eventstudy", showWarnings = FALSE)

# Set clean white theme for saved plots globally
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_line(color = "gray90"),
      strip.background = element_rect(fill = "gray90", color = NA),
      strip.text = element_text(face = "bold")
    )
)

# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

data_raw <- read_delim(
  "MA_complete_dataset.csv", 
  delim = ";", 
  locale = locale(decimal_mark = ".", grouping_mark = ","), 
  quote = "\"", 
  escape_double = TRUE
)



# ------------------------------------------------------------------------------
# Transform Dataset
# ------------------------------------------------------------------------------

# Convert columns into correct format and add column post for DiD-model
data <- data_raw %>%
  mutate(
    date = ymd(date),
    listed_since = ymd(listed_since),
    years_since_listed = as.numeric(difftime(date, listed_since, units = "days")) / 365.25,
    sichtbarkeitsindex = as.numeric(sichtbarkeitsindex),
    price = as.numeric(str_replace(price, ",", ".")),
    num_reviews = as.integer(reviews),
    avg_review = as.numeric(str_replace(avg_review, ",", ".")),
    avg_review_scaled = avg_review/ 10,
    private_label_amazon = as.integer(private_label_amazon),
    amazon_seller = as.integer(amazon_seller),
    post = if_else(date >= as.Date("2023-09-06"), 1, 0)
  )


# New Dataset - only keeps ASINs with at least one non-zero visibility value
data_nonzero_asins <- data %>%
  group_by(product.asin) %>%
  filter(any(sichtbarkeitsindex > 0)) %>%
  ungroup()



# ------------------------------------------------------------------------------
# Descriptive Analysis
# ------------------------------------------------------------------------------

str(data_nonzero_asins)
summary(data_nonzero_asins)


### General setup

threshold <- 20

plot_histogram <- function(df, var, title, binwidth = 1, fill = "steelblue") {
  ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(binwidth = binwidth, fill = fill, color = "white") +
    labs(title = title, x = var, y = "Number of Products") +
    theme_minimal()
}

# Create contingency table
tbl <- table(data$private_label_amazon, data$amazon_seller)

# Compute percentages of total
group_share_percent <- prop.table(tbl) * 100

# Add readable dimension names
dimnames(group_share_percent) <- list(
  `Private Label` = c("No", "Yes"),
  `Amazon Seller` = c("No", "Yes")
)

group_share_percent




### Histograms All Data

# Histogram: All data
plot_histogram(data, "sichtbarkeitsindex", "Distribution of Visibility Index")


# Separating Inliers and Outliers (All Data)
data_inliers <- filter(data, sichtbarkeitsindex <= threshold)
data_outliers <- filter(data, sichtbarkeitsindex > threshold)

# Histogramm Inliers
plot_histogram(data_inliers, "sichtbarkeitsindex", "Inliers: Visibility Index", binwidth = 0.5)

# Histogramm Outliers
plot_histogram(data_outliers, "sichtbarkeitsindex", "Outliers: Visibility Index", binwidth = 0.5)



### Histograms Nonzero Data

# Histogram: Nonzero Data
plot_histogram(
  data_nonzero_asins,
  "sichtbarkeitsindex",
  "",
  binwidth = 1
) +
  labs(x = "Sichtbarkeitsindex", y = "Anzahl der Beobachtungen")


# Separating Inliers and Outliers (Nonzero Data)
data_nonzero_inliers <- filter(data_nonzero_asins, sichtbarkeitsindex <= threshold)
data_nonzero_outliers <- filter(data_nonzero_asins, sichtbarkeitsindex > threshold)

# Histogramm Inliers (Nonzero Data)
plot_histogram(
  data_nonzero_inliers,
  "sichtbarkeitsindex",
  "",
  binwidth = 0.5
) +
  labs(x = "Sichtbarkeitsindex", y = "Anzahl der Beobachtungen")

# Histogramm Outliers (Nonzero Data)
plot_histogram(
  data_nonzero_outliers,
  "sichtbarkeitsindex",
  "",
  binwidth = 0.5
) +
  labs(x = "Sichtbarkeitsindex", y = "Anzahl der Beobachtungen")


# ------------------------------------------------------------------------------
# Balance Tabel
# ------------------------------------------------------------------------------


# Balance Tabel mit t-Test

summarise_with_ttest_and_means <- function(df, group_var) {
  
  # Liste der Variablen, die getestet werden sollen
  vars_to_test <- c("years_since_listed", "price", "avg_review_scaled", "num_reviews")
  
  # Leere Liste, um die Ergebnisse zu speichern
  results_list <- list()
  
  # Schleife über jede Variable
  for (var in vars_to_test) {
    
    # Sicherstellen, dass die Daten für den Test bereit sind
    valid_data <- df[!is.na(df[[var]]) & df[[var]] >= 0, ]
    
    # Berechne die Mittelwerte
    mean_group_1 <- mean(valid_data[[var]][valid_data[[group_var]] == 1])
    mean_group_0 <- mean(valid_data[[var]][valid_data[[group_var]] == 0])
    
    # Berechne die Mittelwertsdifferenz (Gruppe 1 - Gruppe 0)
    diff_of_means <- mean_group_1 - mean_group_0
    
    # Führe den Welch-t-Test durch
    test_result <- t.test(valid_data[[var]] ~ valid_data[[group_var]], data = valid_data)
    
    # Extrahiere den p-Wert
    p_value <- test_result$p.value
    
    # Speichere die Ergebnisse in einem tibble (gerundet auf 2 Nachkommastellen)
    results_list[[var]] <- tibble(
      Variable = var,
      Mean_Group_1 = round(mean_group_1, 2),
      Mean_Group_0 = round(mean_group_0, 2),
      Difference  = round(diff_of_means, 2),
      P_Value     = p_value
    )
  }
  
  # Kombiniere alle Ergebnisse zu einem einzigen Dataframe
  bind_rows(results_list)
}


# Balance Tabel mit Mittelwert + Standardabweichung

clean_and_summarise_with_sd <- function(df, group_var) {
  df %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      mean_years_since_listed = round(mean(years_since_listed[years_since_listed >= 0], na.rm = TRUE), 2),
      sd_years_since_listed   = round(sd(years_since_listed[years_since_listed >= 0], na.rm = TRUE), 2),
      
      mean_price = round(mean(price[price >= 0], na.rm = TRUE), 2),
      sd_price   = round(sd(price[price >= 0], na.rm = TRUE), 2),
      
      mean_avg_review = round(mean(avg_review_scaled[avg_review_scaled >= 0], na.rm = TRUE), 2),
      sd_avg_review   = round(sd(avg_review_scaled[avg_review_scaled >= 0], na.rm = TRUE), 2),
      
      mean_num_reviews = round(mean(num_reviews[num_reviews >= 0], na.rm = TRUE), 2),
      sd_num_reviews   = round(sd(num_reviews[num_reviews >= 0], na.rm = TRUE), 2),
      
      .groups = "drop"
    )
}

# Mittelwerte nach Gruppen

means_by_groups <- data_nonzero_asins %>%
  filter(
    years_since_listed >= 0,
    price >= 0,
    avg_review_scaled >= 0,
    num_reviews >= 0
  ) %>%
  group_by(private_label_amazon, amazon_seller) %>%
  summarise(
    mean_years_since_listed = round(mean(years_since_listed, na.rm = TRUE), 2),
    mean_price              = round(mean(price, na.rm = TRUE), 2),
    mean_avg_review         = round(mean(avg_review_scaled, na.rm = TRUE), 2),
    mean_num_reviews        = round(mean(num_reviews, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Ergebnisse

balance_private_ttest <- summarise_with_ttest_and_means(data_nonzero_asins, "private_label_amazon")
balance_seller_ttest  <- summarise_with_ttest_and_means(data_nonzero_asins, "amazon_seller")
balance_post_ttest    <- summarise_with_ttest_and_means(data_nonzero_asins, "post")

balance_private <- clean_and_summarise_with_sd(data_nonzero_asins, "private_label_amazon")
balance_seller  <- clean_and_summarise_with_sd(data_nonzero_asins, "amazon_seller")
balance_post    <- clean_and_summarise_with_sd(data_nonzero_asins, "post")

# Ausgabe
print(balance_private_ttest)
print(balance_seller_ttest)
print(balance_post_ttest)

print(balance_private)
print(balance_seller)
print(balance_post)

print(means_by_groups)



# ------------------------------------------------------------------------------
# Korrelationsmatrix
# ------------------------------------------------------------------------------

# Korrelationsmatrix für private_label_amazon und amazon_seller
cor_matrix <- data_nonzero_asins %>%
  select(private_label_amazon, amazon_seller, post) %>%
  cor(method = "pearson", use = "complete.obs")

cor_matrix



# ------------------------------------------------------------------------------
# Trend Analysis by Group (APA 7 Style, monthly ticks + gridlines)
# ------------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(stringr)

### General setup

# Function to compute and plot trends (APA styled)
plot_trend <- function(df, group_var, group_labels, title, color_label) {
  trend <- df %>%
    group_by(date, .data[[group_var]]) %>%
    summarize(mean_vis = mean(sichtbarkeitsindex, na.rm = TRUE), .groups = "drop") %>%
    mutate(group = recode(.data[[group_var]], !!!group_labels))
  
  ggplot(trend, aes(x = date, y = mean_vis, color = group)) +
    geom_line(size = 1) +
    geom_vline(xintercept = as.Date("2023-09-06"), linetype = "dashed") +
    scale_x_date(
      date_breaks = "1 month",    # monthly breaks
      date_labels = "%b %Y"       # format = "Jan 2023"
    ) +
    labs(
      title = str_to_title(title),
      x = "Datum",
      y = "Durchschnittlicher Sichtbarkeitsindex",
      color = color_label
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "italic", size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      legend.position = "right",
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      panel.grid.minor = element_line(color = "grey92", size = 0.2),
      axis.text.x = element_text(angle = 45, hjust = 1) # tilt labels so they don’t overlap
    )
}

# 4-group combination
trend_4group <- data_nonzero_asins %>%
  group_by(date, private_label_amazon, amazon_seller) %>%
  summarize(mean_vis = mean(sichtbarkeitsindex, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    group = case_when(
      private_label_amazon == 1 & amazon_seller == 1 ~ "Amazon-Eigenmarke + Amazon-Retail",
      private_label_amazon == 1 & amazon_seller == 0 ~ "Amazon-Eigenmarke + Drittanbieter-Retail",
      private_label_amazon == 0 & amazon_seller == 1 ~ "Fremdmarke + Amazon-Retail",
      private_label_amazon == 0 & amazon_seller == 0 ~ "Fremdmarke + Drittanbieter-Retail"
    )
  )

### Plots

# Plot of all 4 group trends
ggplot(trend_4group, aes(x = date, y = mean_vis, color = group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = as.Date("2023-09-06"), linetype = "dashed") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  labs(
    title = "",
    x = "Datum",
    y = "Durchschnittlicher Sichtbarkeitsindex",
    color = "Gruppe"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "italic", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "right",
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey85", size = 0.3),
    panel.grid.minor = element_line(color = "grey92", size = 0.2),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Private Label vs Non-private Label
plot_trend(
  data_nonzero_asins,
  group_var = "private_label_amazon",
  group_labels = c(`0` = "Fremdmarke", `1` = "Amazon-Eigenmarke"),
  title = "",
  color_label = "Marke"
)

# Amazon Seller vs Non
plot_trend(
  data_nonzero_asins,
  group_var = "amazon_seller",
  group_labels = c(`0` = "Drittanbieter-Retail", `1` = "Amazon-Retail"),
  title = "",
  color_label = "Verkäufertyp"
)




# ------------------------------------------------------------------------------
# Regression
# ------------------------------------------------------------------------------

### Settings

# Labels für Variablen
coef_labels <- c(
  "post" = "Post",
  "private_label_amazon" = "PrivateLabel",
  "amazon_seller" = "AmazonSeller"
)

# GOF-Statistiken
gof_custom <- data.frame(
  raw = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("Num.Obs.", "R²", "Adj. R²"),
  fmt = c(0, 3, 3),
  stringsAsFactors = FALSE
)

# Regionen definieren (alle Länder + EU)
countries <- unique(data_nonzero_asins$country)
regions <- c(countries, "EU")


### Function

# Hilfsfunktion: Modelle pro Region bauen
run_models_by_region <- function(df) {
  control_data <- df %>% filter(private_label_amazon == 0, amazon_seller == 0)
  
  models <- list(
    "Modell 1" = lm(sichtbarkeitsindex ~ post, data = df),
    "Modell 2" = lm(sichtbarkeitsindex ~ private_label_amazon, data = df),
    "Modell 3" = lm(sichtbarkeitsindex ~ amazon_seller, data = df),
    "Modell 4" = lm(sichtbarkeitsindex ~ post + private_label_amazon + amazon_seller, data = df),
    "Variation Modell 1" = lm(sichtbarkeitsindex ~ post, data = control_data)
  )
  
  return(models)
}





### Run Fuction

# Modelle pro Region schätzen
ols_results <- lapply(regions, function(region) {
  if (region == "EU") {
    df <- data_nonzero_asins %>% filter(country != "uk")
  } else {
    df <- data_nonzero_asins %>% filter(country == region)
  }
  run_models_by_region(df)
})
names(ols_results) <- regions

# Zusätzliche Modelle für den gesamten Datensatz
ols_results[["Gesamt"]] <- run_models_by_region(data_nonzero_asins)


### Speichern

# Funktion zum Speichern
save_ols_tables <- function(ols_results,
                            output_type = c("single", "multiple"),
                            file = "ols_models_all.html",
                            folder = "ols_models_by_region") {
  
  output_type <- match.arg(output_type)
  
  if (output_type == "single") {
    html_tables <- lapply(names(ols_results), function(region) {
      models <- ols_results[[region]]
      
      modelsummary(
        models,
        title = paste("OLS-Modelle für", region),
        output = "html",
        stars = TRUE,
        coef_map = coef_labels,
        coef_omit = "(Intercept)",
        gof_map = gof_custom
      )
    })
    
    sink(file)
    for (tbl in html_tables) {
      cat(as.character(tbl), "\n\n")
    }
    sink()
    
    # Auch als PNG speichern
    webshot(file, sub("\\.html$", ".png", file))
    
  } else {
    if (!dir.exists(folder)) dir.create(folder)
    
    for (region in names(ols_results)) {
      models <- ols_results[[region]]
      tbl <- modelsummary(
        models,
        title = paste("OLS-Modelle für", region),
        output = "html",
        stars = TRUE,
        coef_map = coef_labels,
        coef_omit = "(Intercept)",
        gof_map = gof_custom
      )
      
      html_content <- paste0(
        "<html><head><meta charset='UTF-8'><title>OLS Modelle - ", region, "</title></head><body>",
        as.character(tbl),
        "</body></html>"
      )
      
      html_file <- file.path(folder, paste0("ols_models_", region, ".html"))
      png_file  <- file.path(folder, paste0("ols_models_", region, ".png"))
      
      writeLines(html_content, html_file, useBytes = TRUE)
      
      # HTML -> PNG konvertieren
      webshot(html_file, png_file)
    }
  }
}

# Alle OLS-Modelle (inkl. Gesamt) in eine einzige Datei + PNG
save_ols_tables(ols_results, output_type = "single", file = "ols_models_all.html")

# ODER: pro Region einzelne Dateien + PNGs (inkl. Gesamt)
save_ols_tables(ols_results, output_type = "multiple", folder = "ols_models_by_region")





# ------------------------------------------------------------------------------
# DID ANALYSIS — By Country + EU + Gesamt
# ------------------------------------------------------------------------------

### Settings

# Create a list of countries to loop over
countries <- unique(data_nonzero_asins$country)

# Add "EU" as a synthetic region excluding UK
regions <- c(countries, "EU")



### Functions 

# Helper: Run DiD model
run_did <- function(df, formula_rhs, model_name) {
  feols(
    as.formula(paste("sichtbarkeitsindex ~", formula_rhs, "| product.asin + date")),
    cluster = ~product.asin,
    data = df
  )
}

# Wrapper: Run all 3 DiD models per country/region
run_models_by_region <- function(region_name) {
  if (region_name == "EU") {
    df <- data_nonzero_asins %>% filter(country != "uk")
  } else if (region_name == "Gesamt") {
    df <- data_nonzero_asins
  } else {
    df <- data_nonzero_asins %>% filter(country == region_name)
  }
  
  models <- list(
    private_label = run_did(df, "private_label_amazon * post", "PrivateLabel"),
    amazon_seller = run_did(df, "amazon_seller * post", "AmazonSeller"),
    interaction   = run_did(df, "amazon_seller * private_label_amazon * post", "Interaction")
  )
  
  return(models)
}



### Run Function

# Run for each region
did_results <- map(regions, run_models_by_region)
names(did_results) <- regions

# Zusätzlich: Modelle für den Gesamtdatensatz
did_results[["Gesamt"]] <- run_models_by_region("Gesamt")



### Save HTML + PNG Tables

save_html_tables <- function(did_results, 
                             output_type = c("single", "multiple"), 
                             file = "did_models_all.html", 
                             folder = "did_models_by_region") {
  
  output_type <- match.arg(output_type)
  
  if (output_type == "single") {
    html_tables <- lapply(names(did_results), function(region) {
      models <- did_results[[region]]
      modelsummary(
        list(
          "Model 5" = models$private_label,
          "Model 6" = models$amazon_seller,
          "Model 7" = models$interaction
        ),
        title = paste("DID-Modelle für", region),
        output = "html",
        stars = TRUE,
        coef_map = c(
          "private_label_amazon:post" = "PrivateLabel × Post",
          "amazon_seller:post" = "AmazonSeller × Post",
          "amazon_seller:private_label_amazon:post" = "PrivateLabel × AmazonSeller × Post"
        ),
        gof_omit = "IC|Log|RMSE",
        statistic = "({std.error})"
      )
    })
    
    sink(file)
    for (tbl in html_tables) {
      cat(as.character(tbl), "\n\n")
    }
    sink()
    
    # PNG auch für die "single" Datei speichern
    webshot(file, gsub("\\.html$", ".png", file))
    
  } else {
    if (!dir.exists(folder)) dir.create(folder)
    
    for (region in names(did_results)) {
      models <- did_results[[region]]
      tbl <- modelsummary(
        list(
          "Model 5" = models$private_label,
          "Model 6" = models$amazon_seller,
          "Model 7" = models$interaction
        ),
        title = paste("DID-Modelle für", region),
        output = "html",
        stars = TRUE,
        coef_map = c(
          "private_label_amazon:post" = "PrivateLabel × Post",
          "amazon_seller:post" = "AmazonSeller × Post",
          "amazon_seller:private_label_amazon:post" = "PrivateLabel × AmazonSeller × Post"
        ),
        gof_omit = "IC|Log|RMSE",
        statistic = "({std.error})"
      )
      
      html_file <- file.path(folder, paste0("did_models_", region, ".html"))
      png_file  <- file.path(folder, paste0("did_models_", region, ".png"))
      
      html_content <- paste0(
        "<html><head><meta charset='UTF-8'><title>DID Modelle - ", region, "</title></head><body>",
        as.character(tbl),
        "</body></html>"
      )
      
      writeLines(html_content, html_file, useBytes = TRUE)
      
      # Direkt auch PNG speichern
      webshot(html_file, png_file)
    }
  }
}

# Save all models in one single file (+ PNG)
save_html_tables(did_results, output_type = "single", file = "did_models_all.html")

# Save all models as multiple files (+ PNGs)
save_html_tables(did_results, output_type = "multiple", folder = "did_models_by_region")




# ------------------------------------------------------------------------------
# SETTINGS Robustness Analysis
# ------------------------------------------------------------------------------

countries <- unique(data_nonzero_asins$country)
regions <- c(countries, "EU")
cutoff_dates <- seq.Date(as.Date("2023-07-07"), as.Date("2024-02-24"), by = "7 days")
treatments <- c("private_label_amazon", "amazon_seller")




# ------------------------------------------------------------------------------
# ROBUSTNESS TESTS — for multiple treatments
# ------------------------------------------------------------------------------

### Function

run_robustness_by_region <- function(region_name, treat_var) {
  if (region_name == "EU") {
    df_region <- data_nonzero_asins %>% filter(country != "uk")
  } else if (region_name == "Gesamt") {
    df_region <- data_nonzero_asins
  } else {
    df_region <- data_nonzero_asins %>% filter(country == region_name)
  }
  
  map_df(cutoff_dates, function(cut) {
    df_cut <- df_region %>%
      mutate(post_alt = if_else(date >= cut, 1, 0))
    
    mod <- feols(
      as.formula(paste("sichtbarkeitsindex ~", treat_var, "* post_alt | product.asin + date")),
      data    = df_cut,
      cluster = ~product.asin
    )
    
    # Build the expected interaction term pattern
    # Replace " * " with ":" so it matches how fixest names interaction terms
    treat_pattern <- gsub(" \\* ", ":", treat_var)
    # Allow for the possibility of triple interactions in multi-treatment case
    interaction_pattern <- paste0(treat_pattern, ".*:post_alt")
    
    # Extract results and filter for the correct interaction term(s)
    tidy(mod, se = "cluster") %>%
      filter(str_detect(term, interaction_pattern)) %>%
      transmute(
        region    = region_name,
        cutoff    = cut,
        estimate  = estimate,
        std.error = std.error,
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        treatment = treat_var
      )
  })
}



### Run Function

robust_results_all <- map_df(treatments, function(tr) {
  map_df(regions, function(reg) {
    run_robustness_by_region(reg, tr)
  })
})

# Zusätzlich: Modelle für den Gesamtdatensatz anhängen
robust_results_all <- bind_rows(
  robust_results_all,
  map_df(treatments, function(tr) {
    run_robustness_by_region("Gesamt", tr)
  })
)



### SAVE INDIVIDUAL ROBUSTNESS PLOTS — per region & treatment

robust_results_all %>%
  split(list(.$region, .$treatment)) %>%
  walk(function(df_region) {
    region_name <- unique(df_region$region)
    treat_var <- unique(df_region$treatment)
    p <- ggplot(df_region, aes(x = cutoff, y = estimate)) +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.4) +
      labs(
        title = paste("Robustness:", region_name, "-", treat_var),
        x = "Post-period start",
        y = expression(hat(beta)~"(Treatment × post_alt)")
      )
    safe_region <- gsub(" ", "_", region_name)
    safe_treat <- gsub(" ", "_", treat_var)
    safe_treat <- gsub(":", "X", safe_treat)
    
    ggsave(
      filename = paste0("plots_robustness/robustness_", safe_region, "_", safe_treat, ".png"),
      plot = p, width = 8, height = 5
    )
  })



### Save COMBINED ROBUSTNESS PLOTS — one per treatment

for (tr in treatments) {
  df_treat <- robust_results_all %>% filter(treatment == tr)
  
  # Reihenfolge der Regionen festlegen: Länder zuerst, EU zweitletztes, Gesamt letztes
  df_treat <- df_treat %>%
    mutate(
      region = factor(
        region,
        levels = c(
          setdiff(unique(region), c("EU", "Gesamt")),  # alle Länder zuerst
          "EU",    # EU zweitletztes
          "Gesamt" # Gesamt als letztes
        )
      )
    )
  
  p <- ggplot(df_treat, aes(x = cutoff, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.4) +
    facet_wrap(~ region, scales = "free_y") +  # jetzt sollte Faktor-Level-Reihenfolge greifen
    labs(
      title = paste("Robustness of DiD Estimate to Cutoff Date —", tr),
      x = "Post-period start",
      y = expression(hat(beta)~"(Treatment × post_alt)")
    ) +
    theme_bw()
  
  safe_treat <- gsub(" ", "_", tr)
  ggsave(
    filename = paste0("plots_robustness/robustness_all_regions_", safe_treat, ".png"),
    plot = p, width = 12, height = 8
  )
}






# ------------------------------------------------------------------------------
# Robustnes trippel Interaction
# ------------------------------------------------------------------------------

### Function

run_robustness_by_region_tripel <- function(region_name){
  if (region_name == "EU") {
    df_region <- data_nonzero_asins %>% filter(country != "uk")
  } else if (region_name == "Gesamt") {
    df_region <- data_nonzero_asins
  } else {
    df_region <- data_nonzero_asins %>% filter(country == region_name)
  }
  
  map_df(cutoff_dates, function(cut) {
    df_cut <- df_region %>%
      mutate(post_alt = if_else(date >= cut, 1, 0))
    
  mod <- feols(
    as.formula(paste("sichtbarkeitsindex ~ private_label_amazon*amazon_seller*post_alt | product.asin + date")),
    data    = df_cut,
    cluster = ~product.asin
  )
  
  td <- broom::tidy(mod, se = "cluster")
  
  patterns <- c(
    "private_label_amazon:post_alt",
    "amazon_seller:post_alt",
    "private_label_amazon:amazon_seller:post_alt"
  )
  
  out <- purrr::map_dfr(patterns, function(pat) {
    row <- td %>% filter(stringr::str_detect(term, fixed(pat))) %>% slice_head(n = 1)
    tibble(
      region    = region_name,
      cutoff    = cut,
      estimate  = row$estimate,
      std.error = row$std.error,
      conf.low  = row$estimate - 1.96 * row$std.error,
      conf.high = row$estimate + 1.96 * row$std.error,
      treatment = pat
    )
  })
  
  out
  })
}

  

### Run Function

robust_results_all_triple <- map_df(regions, function(reg) {
    run_robustness_by_region_tripel(reg)
  })

# Zusätzlich: Modelle für den Gesamtdatensatz anhängen
robust_results_all_triple <- bind_rows(
  robust_results_all_triple, run_robustness_by_region_tripel("Gesamt")
)



### Save INDIVIDUAL ROBUSTNESS PLOTS — per region & treatment


robust_results_all_triple %>%
  split(list(.$region, .$treatment)) %>%
  walk(function(df_region) {
    region_name <- unique(df_region$region)
    treat_var <- unique(df_region$treatment)
    
    p <- ggplot(df_region, aes(x = cutoff, y = estimate)) +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.4) +
      labs(
        title = paste("Robustness:", region_name, "-", treat_var),
        x = "Post-period start",
        y = expression(hat(beta)~"(Treatment × post_alt)")
      )
    
    safe_region <- gsub(" ", "_", region_name)
    safe_treat <- gsub(" ", "_", treat_var)
    safe_treat <- gsub(":", "X", safe_treat)
    
    ggsave(
      filename = paste0("plots_robustness_triple/robustness_", safe_region, "_", safe_treat, ".png"),
      plot = p, width = 8, height = 5
    )
  })



### Save COMBINED ROBUSTNESS PLOTS — one per treatment

treatments_triple <- c("private_label_amazon:post_alt",
                       "amazon_seller:post_alt",
                       "private_label_amazon:amazon_seller:post_alt")


for (tr in treatments_triple) {
  df_treat <- robust_results_all_triple %>% filter(treatment == tr)

  # Reihenfolge der Regionen festlegen: Länder zuerst, EU zweitletztes, Gesamt letztes
  df_treat <- df_treat %>%
    mutate(
      region = factor(
        region,
        levels = c(
          setdiff(unique(region), c("EU", "Gesamt")),  # alle Länder zuerst
          "EU",    # EU zweitletztes
          "Gesamt" # Gesamt als letztes
        )
      )
    )
  
  p <- ggplot(df_treat, aes(x = cutoff, y = estimate)) +
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.4) +
    facet_wrap(~ region, scales = "free_y") +  # jetzt sollte Faktor-Level-Reihenfolge greifen
    labs(
      title = paste("Robustness of DiD Estimate to Cutoff Date —", tr),
      x = "Post-period start",
      y = expression(hat(beta)~"(Treatment × post_alt)")
    ) +
    theme_bw()
  
  safe_treat <- gsub(" ", "_", tr)
  safe_treat <- gsub(":", "X", safe_treat)
  
  ggsave(
    filename = paste0("plots_robustness_triple/robustness_all_regions_", safe_treat, ".png"),
    plot = p, width = 12, height = 8
  )
}




# ------------------------------------------------------------------------------
# EVENT-STUDY — triple Interaction
# ------------------------------------------------------------------------------

###Settings 
Treatments <-  c("PrivateLabel", "AmazonSeller", "PrivateLabel×AmazonSeller")
countries <- unique(data_nonzero_asins$country)
regions <- c(countries, "EU")


### Pre-process the data for the event study

# Create a 'week' variable and `rel_week` (weeks relative to treatment)
data_nonzero_asins_ES <- data_nonzero_asins %>%
  mutate(
    date = as.Date(date),
    cutoff_date = as.Date("2023-09-06"),
    week = as.integer(floor((date - min(date)) / 7)),
    rel_week = as.integer(round(difftime(date, cutoff_date, units = "weeks"))),
    rel_week = if_else(rel_week == -1, -1L, rel_week) # Set week 0 to -1 as a common baseline
  )

# Create the triple interaction term
data_nonzero_asins_ES <- data_nonzero_asins_ES %>%
  mutate(
    PrivateLabel_AmazonSeller = private_label_amazon * amazon_seller
  )


### Function

# The key change is in the `feols` model specification.
run_event_study_triple <- function(region_name) {
  if (region_name == "EU") {
    df_region <- data_nonzero_asins_ES %>% filter(country != "uk")
  } else if (region_name == "Gesamt") {
    df_region <- data_nonzero_asins_ES
  } else {
    df_region <- data_nonzero_asins_ES %>% filter(country == region_name)
  }
  
  # Specify the model with the three treatment interactions
  # `i()` is used to create and include the time-relative interactions,
  # setting `ref = -1` as the reference week.
  es_mod <- feols(sichtbarkeitsindex ~ i(rel_week, private_label_amazon, ref = -1) + 
                    i(rel_week, amazon_seller, ref = -1) + 
                    i(rel_week, PrivateLabel_AmazonSeller, ref = -1) | 
                    product.asin + week, 
                  data = df_region)
  
  # Extract and tidy the results for all three interactions
  tidy(es_mod, conf.int = TRUE) %>%
    filter(str_detect(term, "rel_week::")) %>%
    mutate(
      evt_week = as.integer(str_extract(term, "(?<=rel_week::)-?\\d+")),  # Zahl nach rel_week::
      treatment = case_when(
        str_detect(term, "private_label_amazon") ~ "PrivateLabel",
        str_detect(term, "amazon_seller") ~ "AmazonSeller",
        str_detect(term, "PrivateLabel_AmazonSeller") ~ "PrivateLabel×AmazonSeller",
        TRUE ~ NA_character_
      ),
      region = region_name
    )
}


#### Run Function
event_study_results <- map_df(regions, run_event_study_triple)

event_study_results <- bind_rows(
  event_study_results, run_event_study_triple("Gesamt")
)


### Save plots for each treatment group separately

# This loop generates one plot for each of the three treatment groups,
# showing all regions on a single plot.

Treatments <- unique(event_study_results$treatment)

for (tr in Treatments) {
  df_plot <- event_study_results %>% 
    filter(treatment == tr)
  
  # Set the order of regions: countries first, EU second to last, Gesamt last
  df_plot <- df_plot %>%
    mutate(
      region = factor(
        region,
        levels = c(
          setdiff(unique(region), c("EU", "Gesamt")),  # all countries first
          "EU",    # EU second to last
          "Gesamt" # Gesamt as last
        )
      )
    )
  
  p <- ggplot(df_plot, aes(x = evt_week, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "black") + 
    geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.4) +
    facet_wrap(~ region, scales = "free_y") +
    labs(
      title = paste("Event-Study Estimates —", tr),
      x = "Weeks Since Treatment", # Corrected to English for clarity
      y = "Estimated Effect on Sichtbarkeitsindex" # Corrected to English for clarity
    ) +
    theme_bw()
  
  safe_treat <- gsub(" ", "_", tr)
  ggsave(
    filename = paste0("plots_eventstudy_triple/eventstudy_all_", safe_treat, ".png"),
    plot = p, width = 14, height = 10
  )
}

