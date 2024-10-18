
## Data:      Wikipedia - Bob's Burgers, Episode list
## Seasons    1 - 14
## Link:      https://en.wikipedia.org/wiki/List_of_Bob%27s_Burgers_episodes#Episodes

## Focus      Scrape data from web pages using rvest and polite.

## Author:    Steven Ponce
## Date:      2024-09-12

## 1. LOAD PACKAGES & SETUP ----
pacman::p_load(
  tidyverse,   # Easily Install and Load the 'Tidyverse'
  ggtext,      # Improved Text Rendering Support for 'ggplot2'
  showtext,    # Using Fonts More Easily in R Graphs
  janitor,     # Simple Tools for Examining and Cleaning Dirty Data
  skimr,       # Compact and Flexible Summaries of Data
  scales,      # Scale Functions for Visualization
  lubridate,   # Make Dealing with Dates a Little Easier
  glue,        # Interpreted String Literals
  rvest,       # Easily Harvest (Scrape) Web Pages
  polite       # Be Nice on the Web
)


## 2. SCRAPE THE DATA ----

# Initialize an empty dataframe
final_df <- tibble()

# The base URL remains the same for all seasons
base_url <- 'https://en.wikipedia.org/wiki/List_of_Bob%27s_Burgers_episodes'

# Bow to the website once since all data is on a single page
session <- polite::bow(base_url,
                       user_agent = "Steven Ponce (steven_ponce@yahoo.com)",
                       delay = 2,
                       force = FALSE,
                       verbose = FALSE
)

# Scrape the web page once and get all tables
all_tables <- polite::scrape(session) |>
  rvest::html_table(fill = TRUE)

# Filter the list to keep only tables 2 through 15 (equivalent to seasons 1 - 14)
season_tables <- all_tables[2:15]

# Function to clean up tables: Clean column names and remove superscripts
clean_table <- function(df) {
  # Rename columns by removing superscripts and cleaning unwanted characters
  clean_names <- colnames(df) |>
    str_replace_all("\\[.*?\\]", "") |> # Remove any superscript numbers in column names
    str_trim() |>                       # Trim white spaces
    make_clean_names()                  # Make names syntactically valid

  colnames(df) <- clean_names

  # Drop columns that are entirely NA
  df <- df |>
    select_if(~!all(is.na(.)))

  return(df)
}

# Use the first table's columns as a reference structure
reference_colnames <- colnames(clean_table(season_tables[[1]]))

# Process each table
for (i in seq_along(season_tables)) {
  print(paste("Processing table", i + 1))

  # Convert each table from the list to a tibble and clean it
  season_df <- as_tibble(season_tables[[i]], .name_repair = "unique")

  # Clean column names and drop unnecessary columns
  season_df <- clean_table(season_df)

  # Align missing columns with the reference table
  missing_cols <- setdiff(reference_colnames, colnames(season_df))
  for (col in missing_cols) {
    season_df[[col]] <- NA
  }

  # Reorder columns to match the reference structure
  season_df <- season_df[, reference_colnames]

  # Ensure No.overall and No. inseason are treated as character
  if ("no_overall" %in% names(season_df)) {
    season_df <- season_df |>
      mutate(no_overall = as.character(no_overall))
  }

  if ("no_inseason" %in% names(season_df)) {
    season_df <- season_df |>
      mutate(no_inseason = as.character(no_inseason))
  }

  # Clean U.S. viewers column
  if ("u_s_viewers_millions" %in% names(season_df)) {
    season_df <- season_df |>
      mutate(u_s_viewers_millions = str_replace_all(u_s_viewers_millions, "\\[.*?\\]", ""),
             u_s_viewers_millions = as.numeric(u_s_viewers_millions))
  }

  # Add the current season to the dataframe
  season_df <- mutate(season_df, season = i)

  # Append this season's data to the final dataframe
  final_df <- bind_rows(final_df, season_df)
}

# Print final summary
print(paste("Total episodes loaded:", nrow(final_df)))

# Show the final dataframe structure
glimpse(final_df)



## 3. TIDY DATA ----
wikipedia_data <- final_df |>
  clean_names() |>
  # Drop unwanted columns
  select(
    -contains("title_234"),
    -contains("original_air_date_234"),
    -contains("title_256"),
    -contains("original_air_date_256"),
    -contains("prod_code ")
  ) |>
  mutate(
    # Clean title column
    title = str_replace_all(title, "\\[.*?\\]", ""),
    title = str_trim(title),
    # Remove digits within brackets
    u_s_viewers_millions = str_replace_all(u_s_viewers_millions, "\\[\\d+\\]", ""),
    u_s_viewers_millions = as.numeric(u_s_viewers_millions),
    # Extracts dates in the format YYYY-MM-DD
    original_air_date = str_extract(original_air_date, "\\d{4}-\\d{2}-\\d{2}"),
    # Converts the string to a date
    original_air_date = ymd(original_air_date),
    year = year(original_air_date),
    ) |>
  rename(
    episode_overall = no_overall,
    episode = no_inseason,
    aired_date = original_air_date,
    us_viewers_millions = u_s_viewers_millions
  ) |>
  select(
    episode_overall, aired_date, year, season, episode, everything(), -prod_code
  )  |>
  filter(!is.na(year))



## 4. CHECK FOR DUPLICATES ----

# Check for duplicates in the 'title' column
duplicates <- wikipedia_data |>
  group_by(title) |>
  filter(n() > 1) |>
  arrange(title)

# View the duplicates
print(duplicates)



## 5. SAVE ----
write.csv(
  wikipedia_data,
  "data-raw/wikipedia_Bobs_Burgers_Data.csv"
)



## 6. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.1 (2024-06-14 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-09-12
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
#
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ! package     * version date (UTC) lib source
# P assertthat    0.2.1   2019-03-21 [?] CRAN (R 4.4.0)
# V base        * 4.4.1   2024-04-24 [2] local (on disk 4.4.0)
# base64enc     0.1-3   2015-07-28 [1] CRAN (R 4.4.0)
# cachem        1.1.0   2024-05-16 [1] CRAN (R 4.4.1)
# cli           3.6.3   2024-06-21 [1] CRAN (R 4.4.0)
# colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.4.1)
# P compiler      4.4.0   2024-04-24 [?] local
# crayon        1.5.3   2024-06-20 [1] CRAN (R 4.4.0)
# curl          5.2.1   2024-03-01 [1] CRAN (R 4.4.1)
# P datasets    * 4.4.0   2024-04-24 [?] local
# digest        0.6.35  2024-03-11 [1] CRAN (R 4.4.1)
# dplyr       * 1.1.4   2023-11-17 [1] CRAN (R 4.4.1)
# fansi         1.0.6   2023-12-08 [1] CRAN (R 4.4.1)
# fastmap       1.2.0   2024-05-15 [1] CRAN (R 4.4.1)
# forcats     * 1.0.0   2023-01-29 [1] CRAN (R 4.4.1)
# fs            1.6.4   2024-04-25 [1] CRAN (R 4.4.1)
# generics      0.1.3   2022-07-05 [1] CRAN (R 4.4.1)
# ggplot2     * 3.5.1   2024-04-23 [1] CRAN (R 4.4.1)
# ggtext      * 0.1.2   2022-09-16 [1] CRAN (R 4.4.1)
# glue        * 1.7.0   2024-01-09 [1] CRAN (R 4.4.1)
# P graphics    * 4.4.0   2024-04-24 [?] local
# P grDevices   * 4.4.0   2024-04-24 [?] local
# P grid          4.4.0   2024-04-24 [?] local
# gridtext      0.1.5   2022-09-16 [1] CRAN (R 4.4.1)
# gtable        0.3.5   2024-04-22 [1] CRAN (R 4.4.1)
# hms           1.1.3   2023-03-21 [1] CRAN (R 4.4.1)
# htmltools     0.5.8.1 2024-04-04 [1] CRAN (R 4.4.1)
# httr          1.4.7   2023-08-15 [1] CRAN (R 4.4.1)
# janitor     * 2.2.0   2023-02-02 [1] CRAN (R 4.4.1)
# jsonlite      1.8.8   2023-12-04 [1] CRAN (R 4.4.1)
# knitr         1.47    2024-05-29 [1] CRAN (R 4.4.1)
# lifecycle     1.0.4   2023-11-07 [1] CRAN (R 4.4.1)
# lubridate   * 1.9.3   2023-09-27 [1] CRAN (R 4.4.1)
# magrittr      2.0.3   2022-03-30 [1] CRAN (R 4.4.1)
# memoise       2.0.1   2021-11-26 [1] CRAN (R 4.4.1)
# P methods     * 4.4.0   2024-04-24 [?] local
# mime          0.12    2021-09-28 [1] CRAN (R 4.4.0)
# munsell       0.5.1   2024-04-01 [1] CRAN (R 4.4.1)
# P pacman        0.5.1   2019-03-11 [?] CRAN (R 4.4.0)
# pillar        1.9.0   2023-03-22 [1] CRAN (R 4.4.1)
# pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.4.1)
# P polite      * 0.1.3   2023-06-30 [?] CRAN (R 4.4.1)
# purrr       * 1.0.2   2023-08-10 [1] CRAN (R 4.4.1)
# R6            2.5.1   2021-08-19 [1] CRAN (R 4.4.1)
# P ratelimitr    0.4.1   2018-10-07 [?] CRAN (R 4.4.1)
# Rcpp          1.0.12  2024-01-09 [1] CRAN (R 4.4.1)
# readr       * 2.1.5   2024-01-10 [1] CRAN (R 4.4.1)
# renv          1.0.7   2024-04-11 [1] CRAN (R 4.4.0)
# repr          1.1.7   2024-03-22 [1] CRAN (R 4.4.1)
# rlang         1.1.4   2024-06-04 [1] CRAN (R 4.4.1)
# P robotstxt     0.7.13  2020-09-03 [?] CRAN (R 4.4.1)
# rstudioapi    0.16.0  2024-03-24 [1] CRAN (R 4.4.1)
# rvest       * 1.0.4   2024-02-12 [1] CRAN (R 4.4.1)
# scales      * 1.3.0   2023-11-28 [1] CRAN (R 4.4.1)
# sessioninfo   1.2.2   2021-12-06 [1] CRAN (R 4.4.1)
# showtext    * 0.9-7   2024-03-02 [1] CRAN (R 4.4.1)
# showtextdb  * 3.0     2020-06-04 [1] CRAN (R 4.4.1)
# skimr       * 2.1.5   2022-12-23 [1] CRAN (R 4.4.1)
# snakecase     0.11.1  2023-08-27 [1] CRAN (R 4.4.1)
# P spiderbar     0.2.5   2023-02-11 [?] CRAN (R 4.4.1)
# P stats       * 4.4.0   2024-04-24 [?] local
# stringi       1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
# stringr     * 1.5.1   2023-11-14 [1] CRAN (R 4.4.1)
# sysfonts    * 0.8.9   2024-03-02 [1] CRAN (R 4.4.1)
# tibble      * 3.2.1   2023-03-20 [1] CRAN (R 4.4.1)
# tidyr       * 1.3.1   2024-01-24 [1] CRAN (R 4.4.1)
# tidyselect    1.2.1   2024-03-11 [1] CRAN (R 4.4.1)
# tidyverse   * 2.0.0   2023-02-22 [1] CRAN (R 4.4.1)
# timechange    0.3.0   2024-01-18 [1] CRAN (R 4.4.1)
# P tools         4.4.0   2024-04-24 [?] local
# tzdb          0.4.0   2023-05-12 [1] CRAN (R 4.4.1)
# usethis       2.2.3   2024-02-19 [1] CRAN (R 4.4.1)
# utf8          1.2.4   2023-10-22 [1] CRAN (R 4.4.1)
# P utils       * 4.4.0   2024-04-24 [?] local
# vctrs         0.6.5   2023-12-01 [1] CRAN (R 4.4.1)
# withr         3.0.0   2024-01-16 [1] CRAN (R 4.4.1)
# xfun          0.45    2024-06-16 [1] CRAN (R 4.4.1)
# xml2          1.3.6   2023-12-04 [1] CRAN (R 4.4.1)
#
# [1] C:/Users/poncest/OneDrive - Bristol Myers Squibb/RStudio/Bobs_Burguers/renv/library/windows/R-4.4/x86_64-w64-mingw32
# [2] C:/Users/poncest/AppData/Local/R/cache/R/renv/sandbox/windows/R-4.4/x86_64-w64-mingw32/d6ee0ff8
#
#
# ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# >
