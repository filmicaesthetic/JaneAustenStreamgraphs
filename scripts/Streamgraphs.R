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
library(extrafont)
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


sense_p <- sense_byline_dt %>% filter(book == "Sense & Sensibility") %>%
  ggplot(aes(x = id, y = test)) +
  geom_stream(bw = 0.6, n_grid = 16561, aes(fill = emotion), alpha = 0.96) +
  scale_fill_manual(values = pal) +
  ggtitle("S E N S E   &   S E N S I B I L I T Y") +
  theme(panel.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#69543f", size = 18, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#69543f"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family = "Noto Serif", face = "bold", color = "#69543f"))

emma_p <- sense_byline_dt %>% filter(book == "Emma") %>%
  ggplot(aes(x = id, y = test)) +
  geom_stream(bw = 0.6, n_grid = 16561, aes(fill = emotion), alpha = 0.96) +
  scale_fill_manual(values = pal) +
  ggtitle("E M M A") +
  theme(panel.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 24),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#69543f", size = 18, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#69543f"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family = "Noto Serif", face = "bold", color = "#69543f"))

north_p <- sense_byline_dt %>% filter(book == "Northanger Abbey") %>%
  ggplot(aes(x = id, y = test)) +
  geom_stream(bw = 0.6, n_grid = 16561, aes(fill = emotion), alpha = 0.96) +
  scale_fill_manual(values = pal) +
  ggtitle("N O R T H A N G E R   A B B E Y") +
  theme(panel.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(size = 24),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#69543f", size = 18, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#69543f"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family = "Noto Serif", face = "bold", color = "#69543f"))

mansfield_p <- sense_byline_dt %>% filter(book == "Mansfield Park") %>%
  ggplot(aes(x = id, y = test)) +
  geom_stream(bw = 0.6, n_grid = 16561, aes(fill = emotion), alpha = 0.96) +
  scale_fill_manual(values = pal) +
  ggtitle("M A N S F I E L D   P A R K") +
  theme(panel.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 24),
        legend.title = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#69543f", size = 18, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#69543f"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family = "Noto Serif", face = "bold", color = "#69543f"))

pride_p <- sense_byline_dt %>% filter(book == "Pride & Prejudice") %>%
  ggplot(aes(x = id, y = test)) +
  geom_stream(bw = 0.6, n_grid = 16561, aes(fill = emotion), alpha = 0.96) +
  scale_fill_manual(values = pal) +
  ggtitle("P R I D E   &   P R E J U D I C E") +
  labs(caption = "Sentiment Analysis Data: NRC Word-Emotion Association Lexicon / saifmohammad.com | Visualisation: @filmicaesthetic") +
  theme(panel.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#69543f", size = 18, hjust = 0.5),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#69543f"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family = "Noto Serif", face = "bold", color = "#69543f"))

# build legend with bar chart
leg <- data.frame(EMOTION = as.factor(c("joy", "trust", "surprise", "anticipation", "sadness", "fear", "anger", "disgust")), value = c(1, 1, 1, 1, 1, 1, 1, 1))
leg$EMOTION <- factor(leg$EMOTION, levels = leg$EMOTION)

# legend plot
hist <- ggplot(leg, aes(x = EMOTION, y = value, fill = EMOTION)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.502, label = toupper(EMOTION)), angle = 90, family = "Noto Serif", color = c("#b07e13", "#f2ddb1", "#b07e13", "#f2ddb1", "#b07e13", "#b07e13", "#f2ddb1", "#b07e13"),  size = 8) +
  geom_text(aes(y = 0.498, label = toupper(EMOTION)), angle = 90, family = "Noto Serif", color = c("#403020", "#b07e13", "#403020", "#b07e13", "#403020", "#403020", "#b07e13", "#403020"), size = 8) +
  geom_text(aes(y = 0.95, label = "JANE AUSTEN"), family = "Noto Serif", color = "#f2ddb1", size = 1.5) +
  geom_text(aes(y = 0.95001, label = "JANE AUSTEN"), family = "Noto Serif", color = "#291d12", size = 1.5) +
  geom_text(aes(y = 0.05, label = "SIMON & SCHUSTER"), family = "Noto Serif", color = "#f2ddb1", size = 1.1) +
  geom_text(aes(y = 0.05001, label = "SIMON & SCHUSTER"), family = "Noto Serif", color = "#291d12", size = 1.1) +
  scale_fill_manual(values = pal) +
  ggtitle(toupper("The Emotions of Jane Austen"), subtitle = "A visual interpretation of the emotion of language used in Jane Austen novels.") + 
  theme(panel.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        plot.background = element_rect(fill = "#f2f0ed", color = "#f2f0ed"),
        panel.grid.major = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        plot.margin = margin(r = 4, l = 4, unit = "cm"),
        legend.position = "none",
        legend.box.margin = margin(r = 0, l = 0),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size= 18, margin = margin(t = 5, b = 5, unit = "pt")),
        plot.title = element_text(angle = 0, family = "Noto Serif", color = "#69543f", size = 24, face="bold", hjust = 0.5, margin = margin(t = 12, unit = "pt")),
        axis.text = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color = "#80664d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text=element_text(family="Noto Serif", face = "bold", color = "#69543f"))

plot <- plot_grid(hist, sense_p, emma_p, north_p, mansfield_p, pride_p, nrow = 6, rel_heights = c(1.2, 1, 1, 1, 1, 1))

ggsave("JaneAusten.png", plot = plot, dpi = 300, width = 13, height = 20, units = "cm")
