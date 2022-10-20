# Tidy Tuesday Week 42: Stranger Things Dialogue
# How does sentiment change across episodes for each season?
# Load libraries

library(tidytuesdayR)
library(readr)
library(tidytext)
library(ggplot2)

# Get data

tuesdata <- tidytuesdayR::tt_load('2022-10-18')
tuesdata <- tidytuesdayR::tt_load(2022, week = 42)
episodes <- tuesdata$episodes
dialogue <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")

# clean
dialogue_clean <- dialogue %>%
  select(c(season, episode, line, dialogue)) %>%
  unnest_tokens(word, dialogue)

dialogue_clean <- dialogue_clean %>%
  anti_join(stop_words)

dialogue_clean <- dialogue_clean %>%
  filter(!is.na(word))

episode_sentiment <- dialogue_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(season, episode, word, sentiment)

# plot
season.labs <- c("Season 1", "Season 2", "Season 3","Season 4")
names(season.labs) <- c("1", "2", "3","4")

episode_sentiment %>%
  group_by(season, episode) %>%
  count(sentiment) %>%
  ggplot(aes(x = episode, y = n, fill=sentiment)) +
  geom_col() +
  facet_wrap(~season, ncol = 1, labeller = labeller(season = season.labs)) +
  labs(title = "Sentiment Frequency Across Each Episode of Stranger Things",
       y = "Word Frequency", x = "Episode",
       subtitle = "Positive and negative word sentiment frequency in the dialogue across every episode of stranger things.\nSeason 4 seems pretty rough.",
       caption = "Visualisation: Naomi W. Holt | Data source: 8flix.com | #TidyTuesday 2022 week 42") +
  scale_x_continuous(n.breaks =8) +
  scale_fill_manual(values = c("#810000","#073e1e")) +
  theme(text = element_text(color="Black", family="Bell MT", size=12),
        strip.background = element_rect(
          color="black", fill="black", size=1.5, linetype="solid"
        ),
        strip.text = element_text(color="white", family = "Benguiat"),
        panel.grid=element_blank(),
        panel.background = element_rect(fill="#EEEBDD"),
        legend.background = element_rect(fill="#EEEBDD"),
        legend.position = "bottom",
        plot.background = element_rect(fill="#EEEBDD"),
        plot.title = element_text(size = 18, family ="Benguiat", color="#CE1212"),
        plot.caption = element_text(size = 10, hjust = 0.5,
                                    margin = margin(20, 0, 5, 0)),
        plot.subtitle = element_text(size=14, hjust=0.5))
