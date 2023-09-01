## analysis code for JCAA research papers and case studies
## Sophie C. Schmidt
## last changes: 2023-08-22


# data from https://journal.caa-international.org/articles/?f=1&f=3&order=date_published&app=100
# papers of a theoretical nature have been excluded from the table, because no programming expected to take place

## load JCAA table data

library(here)
path <- here("analysis", "data", "articles_code_JCAA_2018-2023.ods" )

d <- readODS::read_ods(path = path)

library(ggplot2)

table(d$explanation)

table(d$as)
# package != standalone software, but an extension
# program = standalone
# compendium not necessarily a package, but a folder structure for reproducibility
# script: just that, sometimes several, but not so evolved as a compendium


library(dplyr)
library(stringr)

# quick check, wie häufig vmtl gecoded

d |>
  mutate(b = case_when(
    str_detect(Code, "no", F) ~ "no",
    Code == "?" ~ "?",
    TRUE ~ "yes")) |>
  ggplot()+
    geom_bar(aes(x = b))

# -> 32

# quick check, how many have code available:

d1 <- d |>
  mutate(a = case_when(
    str_detect(available, "https|printed", F) ~ "yes",
    TRUE ~ "no"
  )) |>
  filter(a == "yes",
         str_detect(Code, "no", T))

d1 |>
ggplot()+
  geom_bar(aes(x = a))+
  coord_flip()

# -> 22


### for comparison: load data of all papers in JCAA

path <- here("analysis", "data", "JCAA_all_papers_per_year.ods" )

JCAA <- readODS::read_ods(path = path)

jcaa1 <- JCAA |>
  group_by(Year) |>
  summarise(Frequency = sum(n))

## compare number of articles with code with nr alltogether

#jcaa1$Year[jcaa1$Year == "2023 (until August)"] <- "2023"

#jcaa1$Year <- as.numeric(jcaa1$Year)

d1 |>
  ggplot()+
  geom_bar(aes(x = as.numeric(year),
               fill = pid))+
  geom_line(data = jcaa1,
            aes(x = as.numeric(Year),
                y = Frequency))+
  theme_bw()


# aim of the software over time
d1 |>
  ggplot()+
  geom_bar(aes(x = as.numeric(year),
               fill = aim))+
  geom_line(data = jcaa1,
            aes(x = as.numeric(Year),
                y = Frequency))+
  theme_bw()

# kind of software

d1 |>
  ggplot()+
  geom_bar(aes(x = as.numeric(year),
               fill = as))+
  geom_line(data = jcaa1,
            aes(x = as.numeric(Year),
                y = Frequency))+
  theme_bw()


# documentation
## comments = ONLY comments, README (may also contain comments) as main info and manuals as extra on top
d1 |>
  ggplot()+
  geom_bar(aes(x = as.numeric(year),
               fill = documentation))+
  geom_line(data = jcaa1,
            aes(x = as.numeric(Year),
                y = Frequency))+
  theme_bw()

## how is which kind of program documentated?

d1 |>
  ggplot()+
  geom_bar(aes(x = as,
               fill = documentation))+
theme_bw()


## make length of documentation to ordered factor

d1$doc_length <- factor(d1$doc_length, ordered = T,
                        levels = c("little", "some", "good"))

ggplot()+
  geom_bar(data = d1, aes(x = as,
                   fill = doc_length))

