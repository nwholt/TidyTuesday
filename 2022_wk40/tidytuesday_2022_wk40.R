# Tidy Tuesday - 2022 Wk 40

# load data
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(wesanderson)

tuesdata <- tidytuesdayR::tt_load('2022-10-04')

product_hunt <- tuesdata$product_hunt

font_add_google(name = "Recursive", family = "recursive")

# data wrangling
clean <- product_hunt %>%
  mutate(Year = format(release_date, format = "%Y")) %>%
  dplyr::mutate(cat = stringr::str_remove_all(category_tags, c("\\["))) %>%
  dplyr::mutate(cat = stringr::str_remove_all(cat, c("\\]"))) %>%
  tidytext::unnest_tokens(category, cat,token = "regex", pattern = ",") %>%
  dplyr::mutate(category = stringr::str_remove_all(category, "'")) %>%
  dplyr::mutate(category = stringr::str_trim(category)) %>%
  select(c(Year,category))

year <- clean %>%
  group_by(Year, category) %>%
  summarise(n = n())

dat <- year %>%
  group_by(Year) %>%
  arrange(desc(n), .by_group=T) %>%
  filter(!category %in% c("ipad","iphone","mac","windows","android","web app")) %>%
  top_n(5, n)

# plotting

dat %>%
  group_by(Year) %>%
  ggplot(aes(x = Year, y = n, fill=category)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(y = "Count", title="Top 5 Product Categories for Each Year",
       subtitle="Most common category tags for each year",
       caption = "Visualisation: Naomi W. Holt | Data source: components.one | #TidyTuesday 2022 week 40") +
  scale_fill_manual(values = wes_palette("Zissou1", n = 8, type="continuous")) +
  theme(plot.title = element_text(family ="recursive", size = 45),
        legend.position = "bottom",
        legend.title = element_text(family = "recursive", size=20),
        legend.text = element_text(family ="recursive", size = 15),
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_text(family="recursive", size =12),
        axis.text = element_text(family="recursive", size =12),
        plot.subtitle = element_text(size =25, family="recursive", color="grey"),
        plot.caption = element_text(size = 10, hjust = 0.5,
                                    margin = margin(20, 0, 5, 0)))


# save