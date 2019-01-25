### -----------------------------------------------------------------------------------------------
### Analysis of offensive line yards created by men in the box
###
### -----------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
require(theme538)

five_mib <- read_csv("data/5mib-18.csv")
six_mib <- read_csv("data/6mib-18.csv")
seven_mib <- read_csv("data/7mib-18.csv")
eight_mib <- read_csv("data/8mib-18.csv")
nine_mib <- read_csv("data/9mib-18.csv")
six_mib_17 <- read_csv("data/mib6-17.csv")
seven_mib_17 <- read_csv("data/mib7-17.csv")
eight_mib_17 <- read_csv("data/mib8-17.csv")
six_mib_16 <- read_csv("data/mib6-16.csv")
seven_mib_16 <- read_csv("data/mib7-16.csv")
eight_mib_16 <- read_csv("data/mib8-16.csv")

### roll it all up --------------------------------------------------------------------------------

mib <- five_mib %>%
   bind_rows(six_mib,
             seven_mib,
             eight_mib,
             nine_mib)


### Also let's combine 5 man boxes into 6 and fewer, and 9 into 8 plus ----------------------------

mib_grouped1 <- five_mib %>%
   mutate(DefendersInBox = 6) %>%
   bind_rows(six_mib)

mib_grouped2 <- nine_mib %>%
   mutate(DefendersInBox = 8) %>%
   bind_rows(eight_mib)

mib_grouped <- mib_grouped1 %>%
   bind_rows(seven_mib,
             mib_grouped2,
             six_mib_17,
             seven_mib_17,
             eight_mib_17,
             six_mib_16,
             seven_mib_16,
             eight_mib_16) %>%
   mutate(season = year(date))

### What are the league average YPC by MIB in 2018?  ----------------------------------------------

mib_ypc <- mib %>%
   filter(playType == "RUSH") %>%
   group_by(DefendersInBox) %>%
   summarise(ypc = mean(yards))

### Let's adjust for open field, between the 20s. ?  ----------------------------------------------

mib_ypc_open_field <- mib %>%
   filter(playType == "RUSH",
          goalDistStart >= 20 & goalDistStart <= 80) %>%
   group_by(DefendersInBox) %>%
   summarise(ypc = mean(yards))

### How about goal line runs inside the 5? --------------------------------------------------------

mib_goal_line <- mib %>%
   filter(playType == "RUSH",
          goalDistStart <= 5) %>%
   group_by(DefendersInBox) %>%
   summarise(ypc = mean(yards),
             count = n())

### Let's go nuts and see how YPC changes with field position -------------------------------------

mib_grouped$yard_bin <- cut(mib_grouped$goalDistStart,
                            breaks = seq(0, 100, by = 10),
                            right = FALSE)

mib_ypc_by_yardline <- mib_grouped %>%
   filter(playType == "RUSH") %>%
   group_by(DefendersInBox, yard_bin) %>%
   summarise(ypc = mean(yards),
             count = n())

### This is amazing. Let's make a heatmap that looks like a football field, sort of. --------------

ggplot(mib_ypc_by_yardline, aes(x = DefendersInBox, y = yard_bin, fill = ypc)) +
   geom_tile() +
   theme_538 +
   scale_fill_distiller(palette="Greens",
                        direction = 1) +
   geom_tile(color = "black") +
   geom_text(aes(label = round(ypc, 1)),
             color = "black") +
   guides(fill = F) +
   coord_equal() +
   labs(x = "Defenders in the Box", y = "Distance from Goal (10 yard buckets)",
        subtitle = "2016-2018 reg season",
        caption = "Source: ESPN")

ggsave("YPC-heatmap.png")

### Make a model to predict YPC -------------------------------------------------------------------

model_ypc <- lm(data = mib_ypc_by_yardline, ypc ~ yard_bin + DefendersInBox)
summary(model_ypc)
