# load the necessary packages
library(hexSticker) # hexSticker generator
library(magick)     # Advanced image processing
library(sysfonts)   # font selection
library(tidyverse)

# Sticker function---------------------------

sticker(
  subplot, #              * image/ggplot object
  s_x = 0.8, #            * subplot x-position (left/right position)
  s_y = 0.75, #           * subplot y-position (up/down position)
  s_width = 0.3, #        * subplot width
  s_height = 0.4, #       * subplot height
  package, #              * package name to be displayed in hexSticker
  p_x = 1, #              * package name x-position
  p_y = 1.4, #            * package name y-position
  p_color = "#FFFFFF", #  * package name color
  p_family = "Aller_Rg", #* package name font family
  p_fontface = "plain", # * package name font face
  p_size = 8, #           * package name font size
  h_size = 1.2, #         * hexSticker size
  h_fill = "#1881C2", #   * hexSticker background color
  h_color = "#87B13F", #  * hexsticker border color
  spotlight = FALSE, #    * add spotlight effect to hexSticker
  l_x = 1, #              * spotlight effect x-position
  l_y = 0.5, #            * spotlight effect y-position
  l_width = 3, #          * spotlight effect width
  l_height = 3, #         * spotlight effect height
  l_alpha = 0.4, #        * spotlight effect level
  url = "", #             * url to add to the hexSticker
  u_x = 1, #              * url x-position
  u_y = 0.08, #           * url y-position
  u_color = "black", #    * url font color
  u_family = "Aller_Rg", #* url font family
  u_size = 1.5, #         * url font size
  u_angle = 30, #         * url angle
  white_around_sticker = FALSE, # * add white blocks around sticker
  filename = paste0(package, ".png"), # * save hexSticker to file
  asp = 1, # * adjust aspect ratio
  dpi = 320 # * Dots Per Inch resolution
)

# Create your first sticker------------------
cheeseburger_img <- image_read('cheeseburger.png')

fonts_dataset <- font_files()

# Manually add the font from the specific file location
font_add("BobsBurgers",
         "../../../Downloads/Bob__039_s_Burgers.ttf")

fonts_dataset |>
  as_tibble() |>
  filter(str_detect(str_to_lower(family), pattern = "bob"))



# font_add("Old English", "OLDENGL.TTF")

sticker(
  subplot = cheeseburger_img,
  package = "bobsburgersR",
  s_width = .8,
  s_height = .8,
  s_x = 1,
  s_y = .85,
  p_size = 25,
  h_fill = 'gold',
  h_color = '#f40a26',
  h_size = 1.5,
  url = "poncest/bobsburgersR",
  u_size = 4,
  u_color = '#242424',
  spotlight = T,
  l_y = 3,
  l_x = 1,
  l_width = 5,
  l_height = 3,
  l_alpha = 0.3,
  p_color = '#f40a26',
  p_family = "BobsBurgers"
) |> print()




# -------------------------------------------------------------------------




usethis::use_data("data/IMDb_Wikipedia_Bobs_Burgers_Data_Clean.csv",
                  overwrite = FALSE)




library(tidyverse)

imdb_wikipedia_data <- read_csv("data/IMDb_Wikipedia_Bobs_Burgers_Data_Clean.csv")
usethis::use_data(imdb_wikipedia_data, overwrite = TRUE)

transcript_data <- read_csv("data/Transcript_Bobs_Burgers_Data_Clean.csv")
usethis::use_data(transcript_data, overwrite = TRUE)


usethis::use_mit_license()


usethis::use_readme_rmd()




devtools::document()

devtools::load_all()

data("imdb_wikipedia_data")
head(imdb_wikipedia_data)


data("transcript_data")
head(transcript_data)



# -------------------------------------------------------------------------

# 1. Run devtools::check()
devtools::check()

# 2. Test Dataset Accessibility

library(bobsburgersR)

# Load datasets to ensure they are accessible
data("imdb_wikipedia_data")
head(imdb_wikipedia_data)

data("transcript_data")
head(transcript_data)

# 3. Check Package Documentation
devtools::document()


devtools::document()

?imdb_wikipedia_data
?transcript_data


# -------------------------------------------------------------------------

names(imdb_wikipedia_data)
names(transcript_data)

library(tidyverse)

data("imdb_wikipedia_data")
glimpse(imdb_wikipedia_data)



library(bobsburgersR)
library(ggplot2)
library(dplyr)


### Step 2: Example Charts


#### Chart 2: Distribution of IMDb Ratings



# Box Plot with Points: IMDb Ratings by Season
data("imdb_wikipedia_data")

ggplot(imdb_wikipedia_data, aes(x = as.factor(season), y = rating)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5, color = "black", outlier.shape = NA) + # Outlier.shape = NA to avoid duplicate points
  geom_point(alpha = 0.6, color = "darkred",position = position_jitter(seed = 42)) +
  labs(
    title = "IMDb Ratings by Season",
    x = "Season",
    y = "IMDb Rating"
  ) +
  theme_minimal()


data("imdb_wikipedia_data")

ggplot(imdb_wikipedia_data, aes(x = as.factor(season), y = rating)) +
  geom_boxplot(fill = "lightblue", alpha = 0.5, color = "black", outlier.shape = NA) + # Avoid duplicate points by not showing boxplot outliers
  geom_point(alpha = 0.6, color = "darkred", position = position_jitter(seed = 42, width = 0.2)) +
  labs(
    title = "IMDb Ratings by Season",
    x = "Season",
    y = "IMDb Rating"
  ) +
  theme_minimal()

data("transcript_data")
head(transcript_data)

glimpse(transcript_data)



## Heatmap: Lines Spoken by Season and Episode

# Summarize number of lines per episode per season
heatmap_data <- transcript_data |>
  filter(!is.na(dialogue)) |>
  group_by(season, episode) |>
  summarize(total_lines = n()) |>
  ungroup()

# Heatmap: Lines Spoken by Season and Episode
ggplot(heatmap_data, aes(x = as.factor(episode), y = as.factor(season), fill = total_lines)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(
    title = "Lines Spoken by Season and Episode",
    x = "Episode",
    y = "Season",
    fill = "Total Lines"
  ) +
  theme_minimal()



rm(average_lines_per_episode, character_dialogue, character_summary,dialogue_summary, episodes_summary, heatmap_data, top_characters
   )


# -------------------------------------------------------------------------

devtools::install()


# -------------------------------------------------------------------------


library(tidyverse)
library(bobsburgersR)


data("imdb_wikipedia_data")
glimpse(imdb_wikipedia_data)



library(ggplot2)
ggplot(imdb_wikipedia_data, aes(x = factor(season), y = rating)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#92C5DE") +
  labs(x = "Season", y = "Average IMDb Rating", title = "Average IMDb Rating by Season")


ggplot(imdb_wikipedia_data, aes(x = imdb_aired_date, y = rating)) +
  geom_line(color = "#0571B0") +
  labs(x = "Aired Date", y = "IMDb Rating", title = "IMDb Ratings Over Time")

ggplot(imdb_wikipedia_data, aes(x = factor(season), y = rating)) +
  geom_boxplot(fill = "#CA0020") +
  labs(x = "Season", y = "IMDb Rating", title = "Distribution of IMDb Ratings by Season")


library(reshape2)
imdb_wikipedia_data$episode_combined <- paste(imdb_wikipedia_data$season, imdb_wikipedia_data$episode, sep = "-")
heatmap_data <- dcast(imdb_wikipedia_data, episode_combined ~ imdb_aired_date, value.var = "rating")
heatmap(as.matrix(heatmap_data[,-1]), Colv = NA, scale = "column", col = heat.colors(256))

ggplot(imdb_wikipedia_data, aes(x = wikipedia_directed_by, fill = wikipedia_written_by)) +
  geom_bar() +
  labs(x = "Director", fill = "Writer", title = "Episodes Directed and Written By")





s










