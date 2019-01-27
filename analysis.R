### -----------------------------------------------------------------------------------------------
### Analysis of offensive line yards created by men in the box
###
### -----------------------------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
require(theme538)

six_mib_18 <- read_csv("data/mib6-18.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_18 <- read_csv("data/mib7-18.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_18 <- read_csv("data/mib8-18.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_17 <- read_csv("data/mib6-17.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_17 <- read_csv("data/mib7-17.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_17 <- read_csv("data/mib8-17.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_16 <- read_csv("data/mib6-16.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_16 <- read_csv("data/mib7-16.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_16 <- read_csv("data/mib8-16.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_15 <- read_csv("data/mib6-15.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_15 <- read_csv("data/mib7-15.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_15 <- read_csv("data/mib8-15.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_14 <- read_csv("data/mib6-14.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_14 <- read_csv("data/mib7-14.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_14 <- read_csv("data/mib8-14.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_13 <- read_csv("data/mib6-13.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_13 <- read_csv("data/mib7-13.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_13 <- read_csv("data/mib8-13.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_12 <- read_csv("data/mib6-12.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_12 <- read_csv("data/mib7-12.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_12 <- read_csv("data/mib8-12.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_11 <- read_csv("data/mib6-11.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_11 <- read_csv("data/mib7-11.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_11 <- read_csv("data/mib8-11.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_10 <- read_csv("data/mib6-10.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_10 <- read_csv("data/mib7-10.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_10 <- read_csv("data/mib8-10.csv") %>%
   mutate(DefendersInBox = 8)
six_mib_09 <- read_csv("data/mib6-09.csv") %>%
   mutate(DefendersInBox = 6)
seven_mib_09 <- read_csv("data/mib7-09.csv") %>%
   mutate(DefendersInBox = 7)
eight_mib_09 <- read_csv("data/mib8-09.csv") %>%
   mutate(DefendersInBox = 8)

### roll it all up --------------------------------------------------------------------------------

mib_grouped <- six_mib_18 %>%
   bind_rows(seven_mib_18,
             eight_mib_18,
             six_mib_17,
             seven_mib_17,
             eight_mib_17,
             six_mib_16,
             seven_mib_16,
             eight_mib_16,
             six_mib_15,
             seven_mib_15,
             eight_mib_15,
             six_mib_14,
             seven_mib_14,
             eight_mib_14,
             six_mib_13,
             seven_mib_13,
             eight_mib_13,
             six_mib_12,
             seven_mib_12,
             eight_mib_12,
             six_mib_11,
             seven_mib_11,
             eight_mib_11,
             six_mib_10,
             seven_mib_10,
             eight_mib_10,
             six_mib_09,
             seven_mib_09,
             eight_mib_09) %>%
   mutate(season = year(date))

check <- mib_grouped %>%
   filter(playType == "RUSH",
          season == 2018) %>%
   group_by(teamName) %>%
   summarize(count = n())

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
             epa = mean(epa),
             count = n())

### This is amazing. Let's make a heatmap that looks like a football field, sort of. --------------

ggplot(mib_ypc_by_yardline, aes(x = DefendersInBox, y = yard_bin, fill = ypc)) +
   geom_tile() +
   scale_fill_distiller(palette="Greens",
                        direction = 1) +
   geom_tile(color = "black") +
   geom_text(aes(label = round(ypc, 1)),
             color = "black") +
   guides(fill = F) +
   coord_equal() +
   labs(x = "Defenders in the Box", y = "Distance from Goal (10 yard buckets)",
        subtitle = "2009-2018 reg season",
        caption = "Source: ESPN") +
   theme_538

ggsave("YPC-heatmap.png")

ggplot(mib_ypc_by_yardline, aes(x = DefendersInBox, y = yard_bin, fill = epa)) +
   geom_tile() +
   theme_538 +
   scale_fill_distiller(palette="Greens",
                        direction = 1) +
   geom_tile(color = "black") +
   geom_text(aes(label = round(epa, 3)),
             color = "black") +
   guides(fill = F) +
   coord_equal() +
   labs(x = "Defenders in the Box", y = "Distance from Goal (10 yard buckets)",
        subtitle = "2009-2018 reg season",
        caption = "Source: ESPN")

ggsave("EPA-heatmap.png")

### Make a model to predict YPC using field position and men in the box----------------------------

model_ypc <- lm(data = mib_ypc_by_yardline, ypc ~ yard_bin + DefendersInBox)
summary(model_ypc)

predicted <- predict(model_ypc, mib_ypc_by_yardline)

mib_ypc_by_yardline$predict <- predicted

ggplot(data = mib_ypc_by_yardline, aes(x = predict, y = ypc)) +
   geom_smooth(method = "lm",
               alpha = .1,
               color = "grey") +
   geom_point(alpha = .4,
              size = 4) +
   theme_538 +
   labs(x = "Predicted Yards per Carry", y = "Actual Yards per Carry",
        title = "Defenders in the Box and Field Position Explain 96% of rushing.",
        subtitle = "2009-2018 reg season",
        caption = "Source: ESPN")

ggsave("YPC-model-predict.png")

###

team_yards_over_expected <- mib_grouped %>%
   left_join(mib_ypc_by_yardline, by = c("DefendersInBox", "yard_bin")) %>%
   mutate(yards_over_expected = yards - predict,
          scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)) %>%
   filter(playType == "RUSH") %>%
   select(yard_bin, teamName, epa.x, yards_over_expected, season) %>%
   na.omit() %>%
   group_by(teamName, season) %>%
   summarise(yards_over_expected = round(mean(yards_over_expected), 2),
             epa = mean(epa.x),
             count = n())

yards_over_expected_stability <- team_yards_over_expected %>%
   mutate(season2 = season + 1) %>%
   inner_join(team_yards_over_expected, by = c("season2" = "season", "teamName"))

yoe_model <- lm(data = yards_over_expected_stability, yards_over_expected.y ~ yards_over_expected.x)
summary(yoe_model)

team_counts_by_mib <- mib_grouped %>%
   left_join(mib_ypc_by_yardline, by = c("DefendersInBox", "yard_bin")) %>%
   mutate(yards_over_expected = yards - predict,
          scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)) %>%
   filter(playType == "RUSH",
          scramble == 0) %>%
   select(DefendersInBox, teamName, epa.x, yards_over_expected, season) %>%
   na.omit() %>%
   group_by(DefendersInBox, teamName, season) %>%
   summarise(yards_over_expected = round(mean(yards_over_expected), 2),
             epa = mean(epa.x),
             count = n()) %>%
   group_by(teamName, season) %>%
   mutate(freq = round(count / sum(count), 2))

team_yards_over_expected_18 <- team_yards_over_expected %>%
   filter(season == 2018)

