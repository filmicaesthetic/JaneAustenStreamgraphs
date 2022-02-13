###########
# Jane Austen sentiment streamgraphs
###########

library(plyr)
library(stringr)
library(data.table)
library(ggplot2)
library(ggstream)
library(tidytext)
library(janeaustenr)
library(wesanderson)
library(extrafont)
library(gganimate)
library(showtext)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)

font_add_google("Noto Serif", "Noto Serif")

showtext_auto() 

nrc_all <- get_sentiments("nrc")

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

unique(tidy_books$book)

choice <- "Pride & Prejudice"

sense <- tidy_books %>% filter(book == choice)

sentiments <- unique(nrc_all$sentiment)

sense <- tidy_books %>% 
  filter() %>%
  mutate(joy = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "joy"]),
         trust = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "trust"]),
         surprise = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "surprise"]),
         anticipation = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "anticipation"]),
         sadness = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "sadness"]),
         fear = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "fear"]),
         anger = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "anger"]),
         disgust = as.numeric(word %in% nrc_all$word[nrc_all$sentiment == "disgust"]))

sense_long <- melt(setDT(sense), id.vars = c("book","linenumber","chapter","word"), variable.name = "emotion")
sense_long_10 <- sense_long %>% mutate(linenumber = round_any(linenumber, 30, f = ceiling))

sense_byline <- sense_long_10 %>% 
  group_by(book, linenumber, emotion) %>% 
  summarise(value = sum(value)) %>% 
  filter(value > 0) %>% 
  group_by(book, linenumber) %>% 
  mutate(test = value / sum(value) * n())
sense_byline <- sense_byline %>% group_by(book) %>% mutate(id = row_number())

sense_byline_dt <- sense_byline %>% mutate(book_line = paste0(book, linenumber))
sense_byline_dt <- as.data.table(sense_byline_dt)
sense_byline_dt <- sense_byline_dt[sense_byline_dt[, .I[value >= max( value[value!=max(value)] )], by=book_line]$V1]
sense_byline_dt$value <- 1
sense_byline_dt <- sense_byline_dt %>% 
  group_by(book, emotion) %>% 
  mutate(test = n() / value)

pal <- c("joy" = "#6b9a3e", 
         "trust" = "#25654d", 
         "surprise" = "#efd041", 
         "anticipation" = "#34478b", 
         "sadness" = "#4175a5", 
         "fear" = "#ed9953", 
         "anger" = "#9b262c", 
         "disgust" = "#807094")



emma_p <- sense_byline_dt %>% filter(book == "Emma") %>%
  ggplot(aes(x = id, y = test)) +
  geom_stream(bw = 0.6, n_grid = 16561, aes(fill = emotion), alpha = 0.96) +
  scale_fill_manual(values = pal) +
  ggtitle("E M M A") +
  theme(panel.background = element_rect(fill = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#9e7f60", size = 14, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#80664d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family = "Noto Serif", face = "bold", color = "#9e7f60"))

plot <- plot_grid(sense_p, emma_p, north_p, mansfield_p, pride_p, nrow = 5, rel_heights = c(1, 1, 1, 1, 1.5))
plot

ggsave("JaneAusten.png", plot = plot, dpi = 300, width = 10, height = 20, units = "cm")
