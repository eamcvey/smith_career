
# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(plotly)

# Read data ---------------------------------------------------------------

# Song characteristics from Spotify
# spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# Lyrics from all Billboard yearly Top 100
bb_df <- readr::read_csv('../musiclyrics/billboard_lyrics_1964-2015.csv')

# Explore -----------------------------------------------------------------

bb_df %>%
  filter(Year == max(Year)) %>%
  slice(1) %>%
  pull(Lyrics)

token_df <- bb_df %>%
  tidytext::unnest_tokens(word, Lyrics)



# Words per song per year -------------------------------------------------

words_per_song <- token_df %>%
  group_by(Year, Song) %>%
  summarize(total_words = n(),
            unique_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(pct_unique_words = 100 * round(unique_words / total_words, 1))

avg_words_per_song <- words_per_song %>%
  group_by(Year) %>%
  summarize_at(vars(total_words:pct_unique_words), list(mean = ~mean(.)))

# Number of total words per song has gone up (then down)
avg_words_per_song %>%
  ggplot(aes(x = Year, y = total_words_mean)) +
  geom_line() +
  theme_bw()

# Number of unique words per song has gone up (then down)
avg_words_per_song %>%
  ggplot(aes(x = Year, y = unique_words_mean)) +
  geom_line() +
  theme_bw()

# Uniqueness of word use is declining
avg_words_per_song %>%
  ggplot(aes(x = Year, y = pct_unique_words_mean)) +
  geom_line() +
  theme_bw()

# Word use by year --------------------------------------------------------

word_count_year_df <- token_df %>%
  group_by(Year, word) %>%
  count() 

top_wc_year_df <- word_count_year_df %>%
  group_by(Year) %>%
  arrange(desc(n)) %>%
  slice(1:20)

word_count_year_df %>%
  filter(word %in% unique(top_wc_year_df$word)) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = n, group = word)) +
  geom_line() +
  facet_wrap( ~ word)

# Remove stop words -------------------------------------------------------

data(stop_words)

token_clean_df <- token_df %>%
  anti_join(stop_words)

x_df <- tibble(word = c('shit', 'fuck', 'bitch', 'thong', 'la', 'aah', 'ah', 'da', 'id', 'im', 'ive', 'na', 'ill',''
                        'nigga', 'niggaz', 'ooh', 'shes', 'theyre', 'tu', 'uh', 'ur', 'yo', 'youll', 'youre'))

token_clean_df <- token_clean_df %>%
  anti_join(x_df)

# Word use by year --------------------------------------------------------

word_count_year_df <- token_clean_df %>%
  group_by(Year, word) %>%
  count() 

top_wc_year_df <- word_count_year_df %>%
  group_by(Year) %>%
  arrange(desc(n)) %>%
  slice(1:20)

word_count_year_df %>%
  filter(word %in% unique(top_wc_year_df$word)) %>%
  ungroup() %>%
  ggplot(aes(x = Year, y = n, group = word)) +
  geom_line() +
  facet_wrap( ~ word)

