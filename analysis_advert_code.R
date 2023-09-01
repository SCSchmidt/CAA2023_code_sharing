# few analyses of Sophie to add to the CAA 2023 presentation
## 2023-03-24

# How many projects have an attached publication?
# the first condition is most important --> if sth has a publication it overwrites blog or website

load("./analysis/data/derived_data/oarch.RData")

library(dplyr)

oarch3 <- oarch |>
  mutate(
    pub = case_when(
    !is.na(publication) ~ "publication",
    !is.na(website) ~ "website",
    !is.na(blogpost) ~ "blogpost",
    !is.na(github) ~ "github",
    !is.na(gitlab) ~ "gitlab",
    !is.na(bitbucket) ~ "bitbucket",
    !is.na(gist) ~ "gist",
    !is.na(codeberg) ~ "codeberg"),
    advertisement = case_when(
      pub == "publication" ~ "yes",
      pub == "website" ~ "yes",
      pub == "blogpost" ~ "yes"))

pubs <- colnames(oarch[,3:16])

library(ggplot2)
library(forcats) # easy ordering of column bars

# Highest frequency to lowest frequency

oarch3 %>%
ggplot(aes(x = fct_infreq(pub), fill = advertisement))+
  geom_bar()+
  guides(fill = "none") +
  scale_x_discrete(name="Publication",
                  labels=c("Github", "Website", "Paper", "Codeberg", "Blogpost", "Gist", "Gitlab", "Bitbucket",  "Other"))+
  labs(title = "How is the software distributed?",
       caption = "Data: open-archaeo.info\n (no double mentions)",
       x = NULL, y = NULL)+
  theme_minimal()+
  theme(text = element_text(size = 20)) +
  coord_flip()

ggsave("./analysis/figures/publication_software.png", width = 200, height = 127, units = "mm")


table(oarch3$platform)
# 27/196 R-Skripte als pakete auf CRAN

library(tidyr)

oarch5 <- oarch3 %>% pivot_longer(cols=colnames(oarch[,3:16]),
                                  names_to='publication',
                                  values_to='pub_form')

oarch5 <- oarch5 |>
  filter(!is.na(pub_form))


oarch5 %>%
  ggplot(aes(x = fct_infreq(publication), fill = advertisement))+
  geom_bar()+
  guides(fill = "none") +
 # scale_x_discrete(name="Publication",
  #                 labels=c("Github", "Website", "Paper", "Codeberg", "Blogpost", "Gist", "Gitlab", "Bitbucket",  "Other"))+
  labs(title = "How is the software distributed?",
       caption = "Data: open-archaeo.info (double mentions)",
       x = NULL, y = NULL)+
  theme_minimal()+
  theme(text = element_text(size = 20)) +
  coord_flip()

ggsave("./analysis/figures/publication_software_several_mentions_new.png", dpi = 300, width = 250, height = 125, units = "mm")


table(oarch5$publication)


# code stolen from Joe/Zack:

oarch <- mutate(oarch,
                category = recode(category,
                                  "Specifications, protocols and schemas" = "Specifications etc.",
                                  .default = category))
oarch %>%
  drop_na(category) %>%
  group_by(category) %>%
  summarise(n = n()) %>%
  ggplot(aes(fct_reorder(category, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most popular categories",
    subtitle = "By number of projects",
    caption = "Data: open-archaeo.info",
    x = NULL, y = NULL
  ) +
  theme_minimal()+
  theme(text = element_text(size = 20))

ggsave("./analysis/figures/categories.png", dpi = 300, width = 250, height = 125, units = "mm")

