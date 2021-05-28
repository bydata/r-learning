library(tidyverse)

# tuesdata <- tidytuesdayR::tt_load("2020-01-21")
tuesdata <- tidytuesdayR::tt_load(2020, week = 4)
str(tuesdata, max.level = 1)

a_tibble <- tuesdata[[1]]
class(a_tibble)
a_list <- tuesdata[1]
class(a_list)

names(tuesdata)
spotify_songs <- tuesdata$spotify_songs
# spotify_songs2 <- tuesdata[["spotify_songs"]]
# rm(spotify_songs2)

# Top 10 der Songs (genreübergreifend)
df <- 
  spotify_songs %>% # %M% = Pipe. Tastenkombination: STRG - Shift - M
  # Auswahl relevanter Variablen
  select(track_name, track_artist, track_popularity) %>% 
  # Sortieren der Daten nach absteigender (desc) Track Popularity (aufsteigende Sortierung: ohne desc())
  arrange(desc(track_popularity)) %>% 
  # Eliminieren von Duplikaten (über alle Variablen hinweg, sofern keine Variablen in den Klammern angegeben werden)
  distinct() %>% 
  # Auswahl (Slicing) der Zeilen 1 bis 10 (jeweils inklusive)
  slice(1:10)

# Ohne Pipe wirkt der(selbe) Code weniger lesbar:
slice(distinct(arrange(select(spotify_songs, track_name, track_artist, track_popularity), desc(track_popularity))), 1:10)

# Gleiches Ergebnis (in Dataframe df4), aber viele Zwischenergebnisse im Environment
df1 <- select(spotify_songs, track_name, track_artist, track_popularity)
df2 <- arrange(.data = df1, desc(track_popularity))
df3 <- distinct(df2)
df4 <- slice(df3, 1:10)
rm(df1)
rm(list = c("df2", "df3"))


select(spotify_songs, track_name, track_artist, track_popularity)

spotify_songs %>% # STRG - Shift - M
  # select(track_name, track_artist, track_popularity) %>% 
  arrange(desc(track_popularity))


# Wie viele Duplikate je Titel?
spotify_songs %>% 
  count(track_name, track_artist, sort = TRUE) %>% 
  filter(n > 1) %>%
  # filter(n > 1 & n <= 4) %>%
  ggplot(aes(n)) +
  geom_histogram()

# wie viele Zeilen enthält der Datensatz und wie viele unique Titel-Artist-Kombinationen?
nrow(spotify_songs)
spotify_songs %>% 
  summarize(n_distinct(track_name, track_artist))


## Gruppieren
spotify_songs %>% 
  group_by(playlist_genre) %>% 
  summarize(danceability_mean = mean(danceability),
            danceability_median = median(danceability)
            ) %>% 
  arrange(desc(danceability_median))


count(spotify_songs, playlist_genre)
