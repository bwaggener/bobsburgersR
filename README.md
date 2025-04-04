
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bobsburgersR <img src="img/bobsburgersR.png" align="right" height="240"/>

[![R-CMD-check](https://github.com/poncest/bobsburgersR/workflows/R-CMD-check/badge.svg)](https://github.com/poncest/bobsburgersR/actions)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange)
![License](https://img.shields.io/badge/license-MIT-blue.svg)

A collection of datasets on the [Bob’s
Burgers](https://www.fox.com/bobs-burgers/) American animated sitcom.
This package aims to provide easy access to data about the show,
allowing for analysis of trends in ratings, character dialogue, and
more. Included in the package are 2 datasets detailed below for seasons
1-14.

# Installation

Install from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("poncest/bobsburgersR")
```

# Data Dictionary

## `imdb_wikipedia_data`

| Column Name | Data Type | Description |
|----|----|----|
| `episode_overall` | `dbl` | The overall episode number in the entire Bob’s Burgers series (starting from episode 1). |
| `imdb_aired_date` | `date` | The date the episode originally aired, according to IMDb. Format: YYYY-MM-DD. |
| `year` | `dbl` | The year the episode aired. |
| `season` | `dbl` | The season number of the episode within the Bob’s Burgers TV show. |
| `episode` | `dbl` | The episode number within the specific season of the Bob’s Burgers TV show. |
| `imdb_title` | `chr` | The title of the episode, as listed on IMDb. |
| `rating` | `dbl` | The IMDb user rating of the episode (on a scale from 1 to 10). |
| `synopsis` | `chr` | A brief description or synopsis of the episode, summarizing the key plot points, according to IMDb. |
| `wikipedia_directed_by` | `chr` | The name(s) of the director(s) of the episode, as listed on Wikipedia. |
| `wikipedia_written_by` | `chr` | The name(s) of the writer(s) of the episode, as listed on Wikipedia. |
| `wikipedia_viewers` | `dbl` | The number of US viewers (in millions) who watched the episode when it first aired, according to Wikipedia. |

### Notes:

- **IMDb vs. Wikipedia**: `imdb_aired_date`, `imdb_title`, `rating`, and
  `synopsis` are from IMDb, while `wikipedia_directed_by`,
  `wikipedia_written_by`, and `wikipedia_viewers` are from Wikipedia.
  Both sources provide complementary data for the same episodes.

- Viewer numbers (`wikipedia_viewers`) represent the viewership in
  millions, so 9.38 means 9.38 million viewers.

## `transcript_data`

| Column Name | Data Type | Description |
|----|----|----|
| `season` | `dbl` | The season number in which the episode is part of the Bob’s Burgers TV show. |
| `episode` | `dbl` | The episode number within the specific season of Bob’s Burgers. |
| `title` | `chr` | The title of the episode in which the dialogue line appears. |
| `line` | `dbl` | The line number of the dialogue in the episode (the order in which it appears). |
| `raw_text` | `chr` | The original raw text of the dialogue, possibly including formatting or special characters. |
| `dialogue` | `chr` | Cleaned-up version of the `raw_text`, containing the actual dialogue spoken by the characters in the episode. |

------------------------------------------------------------------------

### Notes:

- The `raw_text` column contains the unprocessed version of the
  dialogue, while `dialogue` is the cleaned-up version.

# Examples

## IMDb Ratings by Season

This plot shows the distribution of IMDb ratings for each season, with
individual episode ratings represented as jittered points.

``` r
data("imdb_wikipedia_data")
head(imdb_wikipedia_data)
```

    ## # A tibble: 6 × 11
    ##   episode_overall imdb_aired_date  year season episode imdb_title         rating
    ##             <dbl> <date>          <dbl>  <dbl>   <dbl> <chr>               <dbl>
    ## 1               1 2011-01-08       2011      1       1 Human Flesh           7.7
    ## 2               2 2011-01-15       2011      1       2 Crawl Space           8.1
    ## 3               3 2011-01-22       2011      1       3 Sacred Cow            7.5
    ## 4               4 2011-02-12       2011      1       4 Sexy Dance Fighti…    7.4
    ## 5               5 2011-02-19       2011      1       5 Hamburger Dinner …    7.5
    ## 6               6 2011-03-05       2011      1       6 Sheesh! Cab, Bob?     8.3
    ## # ℹ 4 more variables: synopsis <chr>, wikipedia_directed_by <chr>,
    ## #   wikipedia_written_by <chr>, wikipedia_viewers <dbl>

``` r
# Box Plot with Jitter: IMDb Ratings by Season

ggplot(imdb_wikipedia_data, aes(x = as.factor(season), y = rating)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.shape = NA) + # Avoid duplicate points by not showing boxplot outliers
  geom_point(alpha = 0.6, color = "darkred", position = position_jitter(seed = 42, width = 0.2)) +
  labs(
    title = "IMDb Ratings by Season",
    x = "Season",
    y = "IMDb Rating"
  ) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Heatmap: Lines Spoken by Season and Episode

The following heatmap shows the number of dialogue lines spoken in each
episode across different seasons of Bob’s Burgers.

``` r
data("transcript_data")
head(transcript_data)
```

    ## # A tibble: 6 × 6
    ##   season episode title        line raw_text                         dialogue    
    ##    <dbl>   <dbl> <chr>       <dbl> <chr>                            <chr>       
    ## 1      1       1 Human Flesh     1 <NA>                             <NA>        
    ## 2      1       1 Human Flesh     2 <NA>                             <NA>        
    ## 3      1       1 Human Flesh     3 <NA>                             <NA>        
    ## 4      1       1 Human Flesh     4 Listen, pep talk.                Listen, pep…
    ## 5      1       1 Human Flesh     5 Big day today.                   Big day tod…
    ## 6      1       1 Human Flesh     6 It's our grand re-re-re-opening. It's our gr…

``` r
## Heatmap: Lines Spoken by Season and Episode

# Summarize number of lines per episode per season
heatmap_data <- transcript_data |>
  filter(!is.na(dialogue)) |>  
  group_by(season, episode) |>
  summarize(total_lines = n(), .groups = "drop") 

# Heatmap: Lines Spoken by Season and Episode
ggplot(heatmap_data, aes(x = as.factor(episode), y = as.factor(season), fill = total_lines)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  coord_equal() +
  labs(
    title = "Lines Spoken by Season and Episode",
    x = "Episode",
    y = "Season",
    fill = "Total Lines"
  ) +
  theme_minimal()
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Question/Contribute

If there is any data you would like to include or if you have
suggestions, please get in touch. You can contact me at
<steven_ponce@yahoo.com> or open an issue on the [GitHub
repository](https://github.com/poncest/bobsburgersR/issues).

# References

1.  IMDb: [Episodes
    List](https://www.imdb.com/title/tt1561755/episodes/?season=1)
2.  Wikipedia (episodes): [List of Bob’s Burgers
    episodes](https://en.wikipedia.org/wiki/List_of_Bob%27s_Burgers_episodes#Episodes)
3.  Springfield! Springfield! (episode scripts): [Springfield
    Springfield - Bob’s Burgers
    scripts](https://www.springfieldspringfield.co.uk/episode_scripts.php?tv-show=bobs-burgers)
