library(tidyverse)
library(scales)
library("ggplot2")
library("corpus")
library("NLP")
library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("stringi")
library("topicmodels")
library("syuzhet")
library("ROAuth")
IMDB_ratings <- read.csv(file.choose(), na = "\\N", quote = '')
IMDB_ratings %>% head()
windows()
ggplot(IMDB_ratings, aes(x = numVotes, y = averageRating)) +
  geom_point()

ggsave("imdb-0.png", plot, width = 4, height = 3)
windows()
ggplot(IMDB_ratings, aes(x = numVotes, y = averageRating)) +
  geom_bin2d() +
  scale_x_log10(labels = comma) +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_viridis_c(labels = comma)
IMDB_basics <- read.csv(file.choose(), na = "\\N", quote = '')
IMDB_basics %>% head()
IMDB_ratings <- IMDB_ratings %>% left_join(IMDB_basics)
head(IMDB_ratings)
windows()
ggplot(IMDB_ratings %>% filter(runtimeMinutes>180, numVotes >= 10), aes(x = runtimeMinutes, y = averageRating)) +
  geom_bin2d() +
  scale_x_continuous(breaks = seq(0, 180, 60), labels = 0:3) +
  scale_y_continuous(breaks = 0:10) +
  scale_fill_viridis_c(option = "inferno", labels = comma) +
  theme_minimal(base_family = "Source Sans Pro", base_size = 8) +
  labs(title = "Relationship between Movie Runtime and Average Mobie Rating",
       subtitle = "Data from IMDb retrieved Sep 7th, 2020",
       x = "Runtime (Hours)",
       y = "Average User Rating",
       caption = "Max Woolf - minimaxir.com",
       fill = "# Movies")

ggplot(IMDB_ratings %>% filter(titleType == "movie", numVotes >= 10), aes(x = startYear, y = averageRating)) +
  geom_bin2d() +
  geom_smooth(color="black") +
  scale_x_continuous() +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_viridis_c(option = "plasma", labels = comma, trans = 'log10')
IMDB_actors <- read_tsv('name.basics.tsv', na = "\\N", quote = '') %>%
  filter(str_detect(primaryProfession, "actor|actress"))  %>%
  select(nconst, primaryName, birthYear)
IMDB_principals <- read_tsv('title.principals.tsv', na = "\\N", quote = '') %>%
  filter(str_detect(category, "actor|actress")) %>%
  select(tconst, ordering, nconst, category) %>%
  group_by(tconst) %>%
  filter(ordering == min(ordering))
IMDB_principals <- IMDB_principals %>% left_join(IMDB_actors)
IMDB_ratings <- IMDB_ratings %>% left_join(IMDB_principals)
IMDB_ratings_movies <- IMDB_ratings %>%
  filter(titleType == "movie", !is.na(birthYear), numVotes >= 10) %>%
  mutate(age_lead = startYear - birthYear) 
IMDB_actor_ages <- IMDB_ratings_movies %>%
  group_by(startYear) %>%
  summarize(low_age = quantile(age_lead, 0.25, na.rm=T),
            med_age = quantile(age_lead, 0.50, na.rm=T),
            high_age = quantile(age_lead, 0.75, na.rm=T))
plot <- ggplot(IMDB_actor_ages %>% filter(startYear >= 1920) , aes(x = startYear)) +
  geom_ribbon(aes(ymin = low_age, ymax = high_age), alpha = 0.2) +
  geom_line(aes(y = med_age))
IMDB_actor_ages_lead <- IMDB_ratings_movies %>%
  group_by(startYear, category) %>%
  summarize(low_age = quantile(age_lead, 0.25, na.rm = T),
            med_age = quantile(age_lead, 0.50, na.rm = T),
            high_age = quantile(age_lead, 0.75, na.rm = T))

plot <- ggplot(IMDB_actor_ages_lead %>% filter(startYear >= 1920), aes(x = startYear, fill = category, color = category)) +
  geom_ribbon(aes(ymin = low_age, ymax = high_age), alpha = 0.2) +
  geom_line(aes(y = med_age)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")
IMDB_ratings_movies_nth <- IMDB_ratings_movies %>%
  group_by(nconst) %>%
  arrange(startYear) %>%
  mutate(nth_lead = row_number())

