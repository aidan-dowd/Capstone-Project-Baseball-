require(car)
require(tidyverse)
require(dplyr)
require(ggplot2)
require(patchwork)

df <- read.csv("/Users/aidandowd/Desktop/Combined Capstone Data.csv")

# Code for Pitch number throughout game
df <- df %>%
  arrange(game_date, player_name, at_bat_number, pitch_number) %>%
  group_by(game_date) %>%
  mutate(pitch_number_in_game = row_number())
df$one <- 1

# Dataframe for just Gerrit Cole
GC <- subset(df, grepl("Cole, Gerrit", player_name))

#Year Group for Gerrit Cole
Y2022 <- subset(GC, grepl(2022, game_year))
Y2023 <- subset(GC, grepl(2023, game_year))

#histogram of pitch type
x1 <- ggplot(Y2022, aes(x=pitch_name)) + 
  geom_bar(stat="count")

x2 <- ggplot(Y2023, aes(x=pitch_name)) + 
  geom_bar(stat="count")

x1 + x2

#scatter plot of the position of his pitches
ggplot(GC, aes(x = plate_x, y = plate_z, color = description)) +
  geom_point() + facet_grid(~pitch_name)

# boxplots of release velocity
ggplot(GC, aes(x = pitch_name, y = release_speed, color = as.factor(game_year))) +
  geom_boxplot() + theme_bw()

ggplot(GC, aes(x = release_pos_x, y = release_speed, color = as.factor(game_year))) +
  geom_point() + facet_grid(~pitch_name)


# Release Speed by Year and Pitch Type
year_performance <- GC %>% 
  group_by(pitch_name, game_year) %>%
  summarise(avg = mean(release_speed))

# How many games did Gerrit Cole play each year  
total_games_year <- GC %>% 
  group_by(game_year) %>%
  summarise(total_pitches = sum(one))

# How many pitches of each pitch type did Gerrit Cole throw each year 
total_pitches_year <- GC %>% 
  group_by(pitch_name, game_year) %>%
  summarise(total_pitches = sum(one))

#Finding proportion because there are an uneven number of games played in both years
one <- total_pitches_year$total_pitches[1] /3274
two <- total_pitches_year$total_pitches[1] /2873
three <- total_pitches_year$total_pitches[1] /3274
four <- total_pitches_year$total_pitches[1] /2873
five <- total_pitches_year$total_pitches[1] /3274
six <- total_pitches_year$total_pitches[1] /2873
seven <- total_pitches_year$total_pitches[1] /3274
eight <- total_pitches_year$total_pitches[1] /2873
nine <- total_pitches_year$total_pitches[1] /3274
ten <- total_pitches_year$total_pitches[1] /2873

list1 <- c(one, two, three, four, five, six, seven, eight, nine, ten)
total_pitches_year$ratio <- list1

# compare pitch types by year and inning
year_and_type <- df %>%
  group_by(game_year, pitch_name) %>%
  summarise(total_count = n())

#Subsetting the most common pitches
dx <- subset(year_and_type, total_count > 10000)

#How many games were played each year
year <- df %>%
  group_by(game_year) %>%
  summarise(count = sum(one))

# Subset data by year
Y2022 <- subset(dx, grepl(2022, game_year))
Y2023 <- subset(dx, grepl(2023, game_year))

# find proportion of pitches thrown each year
Y2022$proportion <- (Y2022$total_count / year$count) * 100
Y2023$proportion <- (Y2023$total_count / year$count) * 100

# Finding the difference betwee years
merged <- merge(x = Y2022, y = Y2023, by = "pitch_name")
merged$difference <- merged$proportion.y - merged$proportion.x



# inning examination
inning_and_type <- df %>%
  group_by(inning, game_year, pitch_name) %>%
  summarise(total_count = n(), mean_release = mean(release_speed))

# Taking the most common pitches from the previous analysis
inning_and_type <- subset(inning_and_type, pitch_name == "4-Seam Fastball" | 
                            pitch_name == "Changeup" | 
                            pitch_name == "Curveball" | pitch_name == "Cutter" | 
                            pitch_name == "Knuckle Curve" | pitch_name == "Sinker" | 
                            pitch_name == "Slider" | 
                            pitch_name == "Split-Finger" | 
                            pitch_name == "Sweeper")

# Further cleaning
inning_and_type <- subset(inning_and_type, inning <= 9)

## Finding proportions for each year
Yi2022 <- subset(inning_and_type, grepl(2022, game_year))
Yi2023 <- subset(inning_and_type, grepl(2023, game_year))
Yi2022$proportion <- (Yi2022$total_count / sum(Yi2022$total_count)) * 100
Yi2023$proportion <- (Yi2023$total_count / sum(Yi2023$total_count)) * 100
mergedi <- merge(x = Yi2022, y = Yi2023, by = "pitch_name")

#Combining the proportions to find the difference each year
mergedi <- subset(mergedi, inning.x == inning.y)
mergedi$difference_release <- mergedi$mean_release.y - mergedi$mean_release.x

prop_array <- c(Yi2022$proportion, Yi2023$proportion)
inning_and_type$proportion <- prop_array
inning_and_type <- inning_and_type %>%
  arrange(game_year, pitch_name, inning)

# Release Speed drop off by inning for each pitch type
inning_and_type$mean_release_diff <- c(0, inning_and_type$mean_release[-1] - 
                                         inning_and_type$mean_release[-nrow(inning_and_type)])
inning_and_type$mean_release_diff[c(10,19,28,37,46,55,64,73,82,91,100,109,
                                    118,127,136,145,154)] <- 0


ggplot(inning_and_type, aes(x = as.factor(inning), y = mean_release, color = as.factor(game_year))) +
  geom_point() + theme_bw() + facet_wrap(~pitch_name) + 
  ylab("Average Release Speed Difference") +
  xlab("Inning") + ggtitle("Analysis of Pitch Speed by Inning")

## pitches thrown each year analysis
pitch_y_totals <- df %>%
  group_by(game_year) %>%
  summarise(totals = n())
# Total pitches by year
total_pitches_year <- df %>% 
  group_by(pitch_name, game_year) %>%
  summarise(total_pitches = sum(one))

#Subsetting the previously used most common pitches
total_pitches_year <- subset(total_pitches_year, pitch_name == "4-Seam Fastball" | 
                               pitch_name == "Changeup" | 
                               pitch_name == "Curveball" | pitch_name == "Cutter" | 
                               pitch_name == "Knuckle Curve" | pitch_name == "Sinker" | 
                               pitch_name == "Slider" | 
                               pitch_name == "Split-Finger" | 
                               pitch_name == "Sweeper")

merge_count <- merge(x = total_pitches_year, y = pitch_y_totals, by = "game_year")

merge_count$prop <- merge_count$total_pitches / merge_count$totals
merge1 <- subset(merge_count, game_year == 2022)
merge2 <- subset(merge_count, game_year == 2023)

merge_count2 <- merge(x = merge1, y = merge2, by = "pitch_name")
merge_count2$prop_diff <- (merge_count2$prop.y - merge_count2$prop.x)*100

## visuals
plot_pitch_speed_diff <- ggplot(merged, aes(x = pitch_name, y = difference, fill = pitch_name)) +
  geom_bar(stat = "identity", position = "dodge") + theme_bw() + 
  ylab("Release Speed Difference") +
  xlab("Pitch Type") + ggtitle("Analysis of Pitch Speed Difference Between 2022 & 2023")


# Looking at inning falloff (not ultimately used)
mergedi_short <- subset(mergedi, inning.x <= 5)
plot_inning_falloff <- ggplot(mergedi_short, aes(x = as.factor(inning.x), y = difference_release, color = pitch_name)) +
  geom_line(group=1) + theme_bw() + facet_wrap(~pitch_name) + theme(legend.position = "None") + 
  ylab("Average Release Speed Difference") +
  xlab("Inning") + ggtitle("Analysis of Pitch Speed by Inning")

# pitch type by inning
pitch_by_inning <- df %>%
  group_by(pitch_name, game_year, inning) %>%
  summarise(total_pitches = n())

pitch_by_inning <- subset(pitch_by_inning, pitch_name == "4-Seam Fastball" | 
                            pitch_name == "Changeup" | 
                            pitch_name == "Curveball" | pitch_name == "Cutter" | 
                            pitch_name == "Knuckle Curve" | pitch_name == "Sinker" | 
                            pitch_name == "Slider" | 
                            pitch_name == "Split-Finger" | 
                            pitch_name == "Sweeper")


pitch_by_inning <- subset(pitch_by_inning, inning <= 9)
merge11 <- subset(pitch_by_inning, game_year == 2022)
merge22 <- subset(pitch_by_inning, game_year == 2023)
merge11$sum <- sum(merge11$total_pitches)
merge22$sum <- sum(merge22$total_pitches)
pitch_by_inning2 <- merge(x = merge11, y = merge22, by = c("pitch_name", "inning"))

xx <- rbind(merge11, merge22)
xx$prop <- xx$total_pitches / xx$sum

yy <- xx %>%
  group_by(pitch_name, inning) %>%
  summarise(diff = diff(prop)*100)
# visual
########################
# Grid of the difference in pitch types being thrown by inning
ggplot(yy, aes(x = as.factor(inning), y = diff, fill = pitch_name)) + 
  geom_bar(stat = "identity", position = "dodge") + facet_wrap(~pitch_name) +
  ylab("Percentage Difference") +
  xlab("Inning") + ggtitle("Analysis of Pitch Choices by Inning from 2022 to 2023") +
  theme(legend.position = "None")

# Total pitches proportion difference by year
plot_pitch_tot_diff <- ggplot(merge_count2, aes(x = pitch_name, y = prop_diff, fill = pitch_name)) + 
  geom_bar(stat = "identity", position = "dodge") +theme(legend.position = "None") +
  ylab("Percentage Difference") +
  xlab("Pitch Type") + ggtitle("Analysis of the Difference in Pitch Types by Year") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
########################
