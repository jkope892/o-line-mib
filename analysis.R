### -----------------------------------------------------------------------------------------------
### Analysis of offensive line yards created by men in the box
###
### -----------------------------------------------------------------------------------------------

library("tidyverse")
library("lubridate")
library("Cairo")
require("theme538")

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

eleven_per_09 <- read_csv("data/2009-11per.csv")
eleven_per_10 <- read_csv("data/2010-11per.csv")
eleven_per_11 <- read_csv("data/2011-11per.csv")
eleven_per_12 <- read_csv("data/2012-11per.csv")
eleven_per_13 <- read_csv("data/2013-11per.csv")
eleven_per_14 <- read_csv("data/2014-11per.csv")
eleven_per_15 <- read_csv("data/2015-11per.csv")
eleven_per_16 <- read_csv("data/2016-11per.csv")
eleven_per_17 <- read_csv("data/2017-11per.csv")
eleven_per_18 <- read_csv("data/2018-11per.csv")

### roll it all up --------------------------------------------------------------------------------

mib_grouped <- six_mib_18 %>%
   bind_rows(
      seven_mib_18,
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
      eight_mib_09
   ) %>%
   mutate(season = year(date),
          aly_mib = ifelse(yards < 0, yards * 1.2,
                           ifelse(
                              yards %in% c(0:4), yards,
                              ifelse(yards %in% c(5:10), 4 + ((yards - 4) * 0.5),
                                     ifelse(yards > 10, 7, 0))
                           )),
          scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)) %>%
   filter(playType == "RUSH",
          scramble == 0)

write_csv(mib_grouped, "data_check.csv")

eleven_personnel <- eleven_per_09 %>%
   bind_rows(
      eleven_per_10,
      eleven_per_11,
      eleven_per_12,
      eleven_per_13,
      eleven_per_14,
      eleven_per_15,
      eleven_per_16,
      eleven_per_17,
      eleven_per_18
   ) %>%
   mutate(
      season = year(date),
      personnel = 11,
      scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)
   ) %>%
   filter(playType == "RUSH",
          scramble == 0) %>%
   group_by(season) %>%
   summarize(count = n()) %>%
   mutate(Metric = "11 Personnel")

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

write_csv(mib_grouped, "data_check.csv")

mib_ypc_by_yardline <- mib_grouped %>%
   filter(playType == "RUSH") %>%
   group_by(DefendersInBox, yard_bin) %>%
   summarise(
      ypc = mean(yards),
      epa = mean(epa),
      aly = mean(aly_mib),
      count = n()
   )

### This is amazing. Let's make a heatmap that looks like a football field, sort of. --------------

ggplot(mib_ypc_by_yardline,
       aes(x = DefendersInBox, y = yard_bin, fill = ypc)) +
   geom_tile() +
   scale_fill_distiller(palette = "Greens",
                        direction = 1) +
   geom_tile(color = "black") +
   geom_text(aes(label = round(ypc, 1)),
             color = "black") +
   guides(fill = F) +
   coord_equal() +
   labs(
      x = "Defenders in the Box",
      y = "Distance from Goal (10 yard buckets)",
      subtitle = "2009-2018 reg season",
      caption = "Source: ESPN"
   ) +
   theme_538

ggsave("YPC-heatmap.png")

ggplot(mib_ypc_by_yardline,
       aes(x = DefendersInBox, y = yard_bin, fill = epa)) +
   geom_tile() +
   theme_538 +
   scale_fill_distiller(palette = "Greens",
                        direction = 1) +
   geom_tile(color = "black") +
   geom_text(aes(label = round(epa, 3)),
             color = "black") +
   guides(fill = F) +
   coord_equal() +
   labs(
      x = "Defenders in the Box",
      y = "Distance from Goal (10 yard buckets)",
      subtitle = "2009-2018 reg season",
      caption = "Source: ESPN"
   )

ggsave("EPA-heatmap.png")

ggplot(mib_ypc_by_yardline,
       aes(x = DefendersInBox, y = yard_bin, fill = aly)) +
   geom_tile() +
   scale_fill_distiller(palette = "Greens",
                        direction = 1) +
   geom_tile(color = "black") +
   geom_text(aes(label = round(aly, 1)),
             color = "black") +
   guides(fill = F) +
   coord_equal() +
   labs(
      x = "Defenders in the Box",
      y = "Distance from Goal (10 yard buckets)",
      subtitle = "2009-2018 reg season",
      caption = "Source: ESPN"
   ) +
   theme_538

ggsave("aly-heatmap.png")

### Make a model to predict YPC using field position and men in the box----------------------------

model_ypc <-
   lm(data = mib_ypc_by_yardline, ypc ~ 0 + yard_bin + DefendersInBox)
summary(model_ypc)

model_aly <-
   lm(data = mib_ypc_by_yardline, aly ~ yard_bin + DefendersInBox)
summary(model_aly)

predicted <- predict(model_ypc, mib_ypc_by_yardline)

mib_ypc_by_yardline$predict <- predicted

ggplot(data = mib_ypc_by_yardline, aes(x = predict, y = ypc)) +
   geom_smooth(method = "lm",
               alpha = .1,
               color = "grey") +
   geom_point(alpha = .4,
              size = 4) +
   theme_538 +
   labs(
      x = "Predicted Yards per Carry Using Just Defenders in the Box and Field Position",
      y = "Actual Yards per Carry",
      title = "Defenders in the Box and Field Position Explain 96% of rushing.",
      subtitle = "2009-2018 reg season",
      caption = "Source: ESPN"
   )

ggsave("YPC-model-predict.png")

predicted_aly <- predict(model_aly, mib_ypc_by_yardline)

mib_ypc_by_yardline$predict_aly <- predicted_aly

ggplot(data = mib_ypc_by_yardline, aes(x = predicted_aly, y = aly)) +
   geom_smooth(method = "lm",
               alpha = .1,
               color = "grey") +
   geom_point(alpha = .4,
              size = 4) +
   theme_538 +
   labs(
      x = "Predicted Yards per Carry Using Just Defenders in the Box and Field Position",
      y = "Actual Yards per Carry",
      title = "Defenders in the Box and Field Position Explain 96% of rushing.",
      subtitle = "2009-2018 reg season",
      caption = "Source: ESPN"
   )

ggsave("ALY-model-predict.png")

### Look at team level YPC by MIB and Yards Over Expected------------------------------------------

team_yards_over_expected <- mib_grouped %>%
   left_join(mib_ypc_by_yardline, by = c("DefendersInBox", "yard_bin")) %>%
   mutate(
      yards_over_expected = yards - predict,
      aly_over_expected = aly_mib - aly,
      scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)
   ) %>%
   filter(playType == "RUSH") %>%
   select(
      yard_bin,
      teamName,
      yards,
      predict,
      epa.x,
      yards_over_expected,
      aly_mib,
      aly_over_expected,
      season
   ) %>%
   na.omit() %>%
   group_by(teamName, season) %>%
   summarise(
      ypc = round(mean(yards), 1),
      expected_yards = round(mean(predict), 1),
      yards_over_expected = round(mean(yards_over_expected), 2),
      my_aly = round(mean(aly_mib), 2),
      aly_over_expected = round(mean(aly_over_expected), 2),
      epa = mean(epa.x),
      count = n()
   )

yards_over_expected_stability <- team_yards_over_expected %>%
   mutate(season2 = season + 1) %>%
   inner_join(team_yards_over_expected,
              by = c("season2" = "season", "teamName"))

### Test the stability of yards per carry over expected -------------------------------------------

yoe_model <- lm(data = yards_over_expected_stability,
                yards_over_expected.y ~ yards_over_expected.x)
summary(yoe_model)

### Tables ----------------------------------------------------------------------------------------

team_counts_by_mib <- mib_grouped %>%
   left_join(mib_ypc_by_yardline, by = c("DefendersInBox", "yard_bin")) %>%
   mutate(yards_over_expected = yards - predict,
          scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)) %>%
   filter(playType == "RUSH",
          scramble == 0) %>%
   select(DefendersInBox,
          teamName,
          yards,
          epa.x,
          yards_over_expected,
          season) %>%
   na.omit() %>%
   group_by(DefendersInBox, teamName, season) %>%
   summarise(
      rushing_yards = sum(yards),
      ypc = round(mean(yards), 1),
      yards_over_expected = round(mean(yards_over_expected), 2),
      epa = mean(epa.x),
      count = n()
   ) %>%
   group_by(teamName, season) %>%
   mutate(freq = round(count / sum(count), 2))

league_rush_pct_by_mib <- mib_grouped %>%
   left_join(mib_ypc_by_yardline, by = c("DefendersInBox", "yard_bin")) %>%
   mutate(yards_over_expected = yards - predict,
          scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0)) %>%
   filter(playType == "RUSH",
          scramble == 0) %>%
   select(DefendersInBox,
          teamName,
          yards,
          epa.x,
          yards_over_expected,
          season) %>%
   na.omit() %>%
   group_by(DefendersInBox, season) %>%
   summarise(
      ypc = round(mean(yards), 1),
      yards_over_expected = round(mean(yards_over_expected), 2),
      epa = mean(epa.x),
      count = n()
   ) %>%
   group_by(season) %>%
   mutate(freq = round(count / sum(count), 2),
          DefendersInBox = as.factor(DefendersInBox))

### Plot number of rushes with 11 personnel vs # of rush plays with 6 MIB -------------------------

six_defenders_and_eleven <- league_rush_pct_by_mib %>%
   filter(DefendersInBox == 6) %>%
   select(season, count) %>%
   mutate(Metric = "Defenders in the Box") %>%
   bind_rows(eleven_personnel)

CairoPDF(file = "11-personnel-and-6-mib.pdf",
         width = 11.7,
         height = 15.4)

ggplot(six_defenders_and_eleven,
       aes(
          x = season,
          y = count,
          group = Metric,
          color = Metric
       )) +
   geom_line(size = 2) +
   geom_point(size = 3) +
   theme_538 +
   theme(legend.position = "none") +
   labs(
      x = "Season",
      y = "Number of Rushing Plays (excluding QB scrambles)",
      title = "11 Personnel and 6 Defenders in the Box move together",
      subtitle = "2009-2018 reg season (Red = 11 Personnel, Blue = 6 Defenders in the Box)",
      caption = "Source: ESPN"
   )

dev.off()

ggsave("11-personnel-and-6-mib.png")


### Table of 2018 teams ---------------------------------------------------------------------------

teams <- mib_grouped %>%
   left_join(mib_ypc_by_yardline, by = c("DefendersInBox", "yard_bin")) %>%
   mutate(
      yards_over_expected = yards - predict,
      scramble = ifelse(str_detect(playDesc, "scramble") == 1, 1, 0),
      aly_mib = ifelse(yards < 0, yards * 1.2, ifelse(
         yards %in% seq(0:4), yards, ifelse(yards %in% seq(5:10), yards * 0.5, 0)
      ))
   ) %>%
   filter(playType == "RUSH",
          scramble == 0) %>%
   select(DefendersInBox,
          teamName,
          yards,
          epa.x,
          aly_mib,
          yards_over_expected,
          season) %>%
   na.omit() %>%
   group_by(teamName, season) %>%
   summarise(
      rushing_yards = sum(yards),
      ypc = round(mean(yards), 1),
      ypc_over_expected = round(mean(yards_over_expected), 2),
      yards_over_expected = sum(yards_over_expected),
      epa = mean(epa.x),
      count = n(),
      aly_mib = round(mean(aly_mib), 1)
   ) %>%
   group_by(teamName, season) %>%
   mutate(freq = round(count / sum(count), 2))

aly <- read_csv("data/aly.csv")

aly_stability <- aly %>%
   mutate(season2 = season + 1) %>%
   left_join(aly, by = "team", "season2 = season")

aly_stability_model <- lm(data = aly_stability, aly.y ~ aly.x)
summary(aly_stability_model)

comparing_aly <- team_yards_over_expected %>%
   left_join(aly, by = c("season", "teamName" = "team")) %>%
   select(teamName, season, my_aly, aly_over_expected, aly)

aly_compare <- lm(data = comparing_aly, aly ~ my_aly)
summary(aly_compare)

write_csv(teams, "teams.csv")
write_csv(comparing_aly, "teams_aly.csv")
