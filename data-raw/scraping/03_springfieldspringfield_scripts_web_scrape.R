
## Data:      springfieldspringfield.co.uk - Bob's Burgers Wiki, Transcripts
## Seasons    1 - 14
## Link:      https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=bobs-burgers

## Focus      Scrape data from web pages using rvest and polite.

## Author:    Steven Ponce
## Date:      2024-06-30


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
  polite,      # Be Nice on the Web
  xml2         # Parse XML
)


# Base URL for the show's script page
base_url <- "https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=bobs-burgers"


## 2. SCRAPE EPISODE LINKS ----

# Bow to the base page
session_base <- polite::bow(base_url,
                            user_agent = "Steven Ponce (steven_ponce@yahoo.com)",
                            delay = 5,
                            force = FALSE,
                            verbose = FALSE
                            )

is.polite(session_base)

# Scrape the base page
base_page <- session_base |> polite::scrape()

# Extract episode links
episode_links <- base_page |>
  html_nodes("a.season-episode-title") |>
  html_attr("href") |>
  purrr::map_chr(~ paste0("https://www.springfieldspringfield.co.uk/", .))

# Initialize the dataframe before the loop
transcript_data <- tibble(
  Season   = character(),
  Episode  = character(),
  Title    = character(),
  Line     = integer(),
  Raw_Text = character(),
  Dialogue = character()
  )

## 3. LOOP THROUGH EPISODE LINKS TO EXTRACT DATA ----

# Iterate over each episode link
for (link in episode_links) {
  cat("Processing:", link, "\n")                                                # Print the link being processed

  # Set up a session for each episode link
  session_episode <- polite::bow(link)

  # Scrape the episode page
  episode_page <- session_episode |> polite::scrape()

  # Extract episode details such as season and episode number from the link
  season_episode <- str_extract(link, "s(\\d+)e(\\d+)")
  season  <- str_sub(season_episode, 2, 3)
  episode <- str_sub(season_episode, 5, 6)

  # Extract the title
  title <- episode_page |>
    html_nodes("h3.text-center.mb-2") |>
    html_text() |>
    str_trim()

  # Extract the transcript HTML to split on <br>
  transcript_html <- episode_page |>
    html_node(".scrolling-script-container") |>
    as.character()  # Get raw HTML content

  # Replace <br> tags with newlines to facilitate splitting
  transcript_text <- gsub("<br>", "\n", transcript_html)
  transcript_text <- gsub("<.*?>", "", transcript_text)                         # Remove remaining HTML tags

  # Split transcript into lines
  transcript_lines <- str_split(transcript_text, "\n")[[1]]

  if (length(transcript_lines) > 0) {
    for (i in seq_along(transcript_lines)) {
      line_text <- transcript_lines[i]
      dialogue_text <- str_squish(line_text)  # Clean up whitespace
      dialogue_text <- str_replace_all(dialogue_text, "\\[.*?\\]", "")          # Remove bracketed text

      # Append data to the dataframe
      transcript_data <- transcript_data |>
        add_row(
          Season   = season,
          Episode  = episode,
          Title    = title,
          Line     = i,
          Raw_Text = line_text,
          Dialogue = dialogue_text
        )
    }
  }
}

# Print summary for debugging
print(paste("Total transcripts processed:", nrow(transcript_data)))

# Clean column names
transcript_data <- transcript_data |> clean_names()

# Glimpse the final dataframe
glimpse(transcript_data)

# Save data
write.csv(
  transcript_data,
  "data-raw/Transcript_Bobs_Burgers_Data.csv",
  row.names = FALSE
)

# 4. SESSION INFO ----
sessioninfo::session_info(include_base = TRUE)

# ─ Session info ──────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.4.0 (2024-04-24 ucrt)
# os       Windows 10 x64 (build 19045)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/New_York
# date     2024-07-12
# rstudio  2024.04.2+764 Chocolate Cosmos (desktop)
# pandoc   NA
#
# ─ Packages ──────────────────────────────────────────────────────────────────────────────
# ! package     * version date (UTC) lib source
# P assertthat    0.2.1   2019-03-21 [?] CRAN (R 4.4.0)
# base        * 4.4.0   2024-04-24 [2] local
# base64enc     0.1-3   2015-07-28 [1] CRAN (R 4.4.0)
# cachem        1.1.0   2024-05-16 [1] CRAN (R 4.4.1)
# cli           3.6.3   2024-06-21 [1] CRAN (R 4.4.0)
# colorspace    2.1-0   2023-01-23 [1] CRAN (R 4.4.1)
# P compiler      4.4.0   2024-04-24 [?] local
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
# selectr       0.4-2   2019-11-20 [1] CRAN (R 4.4.1)
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
# xml2        * 1.3.6   2023-12-04 [1] CRAN (R 4.4.1)
#
#
# P ── Loaded and on-disk path mismatch.
#
# ─────────────────────────────────────────────────────────────────────────────────────────
# >

