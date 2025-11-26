library(tidyverse)
library(stringr)
library(lubridate)

url <- "https://kenpom.com/cbbga26.txt"
lines <- read_lines(url)

lines <- lines[lines != ""]

pattern <- "^([0-9/]+)\\s+([A-Za-z.&'() ]+?)\\s+([0-9]+)\\s+([A-Za-z.&'() ]+?)\\s+([0-9]+)"

games <- str_match(lines, pattern)

df <- tibble(date = mdy(games[,2]),
  away_team = str_trim(games[,3]),
  away_score = as.numeric(games[,4]),
  home_team = str_trim(games[,5]),
  home_score = as.numeric(games[,6]))

df <- df %>% drop_na()

daily <- df %>%
  group_by(date) %>%
  summarize(total_away = sum(away_score),
    total_home = sum(home_score),
    games = n(),
    avg_points = (sum(away_score) + sum(home_score)) / n())

output <- "kenpom_daily.csv"

if (!file.exists(output)) {
  write_csv(daily, output)
} else {
  existing <- read_csv(output)
  new_rows <- anti_join(daily, existing, by = "date")
  updated <- bind_rows(existing, new_rows)
  write_csv(updated, output)
}
