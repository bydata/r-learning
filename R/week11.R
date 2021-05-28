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

genre.stats

class(spotify_songs)
typeof(spotify_songs)
print(spotify_songs)
print.tbl_df(spotify_songs)


square_number <- function(x) {
  x ^ 2
}
square_number(2)
squared <- square_number(2)
squared

genre_stats <- spotify_songs %>% 
  group_by(playlist_genre) %>% 
  summarize(danceability_mean = mean(danceability),
            danceability_median = median(danceability)
  ) %>% 
  arrange(desc(danceability_median))

count(spotify_songs, playlist_genre)

max(spotify_songs$track_popularity)


## Gruppieren
spotify_songs %>% 
  group_by(playlist_genre) %>% 
  summarize(tempo_mean = mean(tempo),
            tempo_median = median(tempo)
  ) %>% 
  arrange(desc(tempo_median))

summary(spotify_songs)

set.seed(123)
songs <- sample_n(spotify_songs, 1000)

songs %>% 
  ggplot(aes(x = valence, y = danceability)) +
  #  ggplot(aes(valence, danceability)) +
  geom_point(aes(color = playlist_genre), alpha = 0.3, size = 2) +
  facet_wrap(vars(playlist_genre)) +
  theme_minimal()
  
songs %>%
  ggplot(aes(x = valence, y = danceability)) +
  #  ggplot(aes(valence, danceability)) +
  geom_point(aes(color = playlist_genre), alpha = 0.3, size = 2, shape = "diamond") +
  # geom_smooth(method = "lm") +
  # colorspace::scale_color_discrete_qualitative() +
  scale_color_manual(values = c("darkgreen", 
                                "#1234FF", 
                                rgb(40, 140, 200, maxColorValue = 255),
                                "red",
                                "brown",
                                "orange")) +
  facet_wrap(vars(playlist_genre)) +
  theme_minimal()


iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_point(alpha = 0.3)
  

iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_jitter(alpha = 0.6)


iris %>% 
  ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_jitter(alpha = 0.6, width = 0.1, height = 0.1)

iris %>% 
  ggplot(aes(Species, Sepal.Length)) +
  geom_boxplot() +
  geom_point()

iris %>% 
  ggplot(aes(Species, Sepal.Length)) +
  geom_boxplot(width = 0.4) +
  geom_jitter(width = 0.2, height = 0, shape = 21, fill = "grey10", color = "green")


iris %>% 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot(width = 0.4, alpha = 0.25, size = 0.2, show.legend = FALSE, color = "grey90") +
  geom_jitter(width = 0.2, height = 0, shape = 21, color = "white", alpha = 0.7, show.legend = FALSE) +
  labs(title = "Kelchblattlänge nach Spezies",
       x = NULL) +
  theme_minimal() +
  theme(plot.background = element_rect(color = NA, fill = "grey8"),
        text = element_text(color = "grey90"),
        axis.text = element_text(color = "grey70"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey40"))

ggsave("plots/plot_iris_boxplot_dark.png", width = 6, height = 4, type = "cairo")



iris %>% 
  ggplot(aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot(width = 0.4, alpha = 0.25, size = 0.2, show.legend = FALSE, color = "grey90") +
  geom_jitter(width = 0.2, height = 0, shape = 21, color = "white", alpha = 0.7, show.legend = FALSE) +
  labs(title = "Kelchblattlänge nach Spezies",
       x = NULL) +
  colorspace::scale_fill_discrete_sequential(palette = "Blues 3", aesthetics = c("fill", "color")) +
  scale_x_discrete(labels = str_to_upper) +
  theme_minimal() +
  theme(plot.background = element_rect(color = NA, fill = "grey8"),
        text = element_text(color = "grey90"),
        axis.text = element_text(color = "grey70"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey40"))

ggsave("plots/plot_iris_boxplot_dark-2.png", width = 6, height = 4, type = "cairo")
