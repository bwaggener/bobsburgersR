#' IMDb Wikipedia Bob's Burgers Data
#'
#' A dataset containing information about Bob's Burgers episodes from IMDb and Wikipedia.
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{x1}{Row index or unique identifier for each episode in the dataset.}
#'   \item{episode_overall}{The overall episode number in the entire Bob's Burgers series (starting from episode 1).}
#'   \item{imdb_aired_date}{The date the episode originally aired, according to IMDb. Format: YYYY-MM-DD.}
#'   \item{year}{The year the episode aired.}
#'   \item{season}{The season number of the episode within the Bob's Burgers TV show.}
#'   \item{episode}{The episode number within the specific season of the Bob's Burgers TV show.}
#'   \item{imdb_title}{The title of the episode, as listed on IMDb.}
#'   \item{rating}{The IMDb user rating of the episode (on a scale from 1 to 10).}
#'   \item{synopsis}{A brief description or synopsis of the episode, summarizing the key plot points, according to IMDb.}
#'   \item{wikipedia_directed_by}{The name(s) of the director(s) of the episode, as listed on Wikipedia.}
#'   \item{wikipedia_written_by}{The name(s) of the writer(s) of the episode, as listed on Wikipedia.}
#'   \item{wikipedia_viewers}{The number of US viewers (in millions) who watched the episode when it first aired, according to Wikipedia.}
#' }
#' @source IMDb and Wikipedia
"imdb_wikipedia_data"

#' Transcript Data from Bob's Burgers
#'
#' A dataset containing the transcripts of Bob's Burgers episodes.
#'
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{x1}{Row index or unique identifier for each line of dialogue in the dataset.}
#'   \item{season}{The season number in which the episode is part of the Bob's Burgers TV show.}
#'   \item{episode}{The episode number within the specific season of Bob's Burgers.}
#'   \item{title}{The title of the episode in which the dialogue line appears.}
#'   \item{line}{The line number of the dialogue in the episode (the order in which it appears).}
#'   \item{raw_text}{The original raw text of the dialogue, possibly including formatting or special characters.}
#'   \item{dialogue}{Cleaned-up version of the `raw_text`, containing the actual dialogue spoken by the characters in the episode.}
#' }
#' @source Springfield! Springfield!
"transcript_data"
