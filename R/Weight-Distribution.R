library(ggplot2)
library(dplyr)
library(magrittr)
source("R/Functions.R")

members = read.csv("Data/members.csv") 
teams = read.csv("Data/teams.csv")

p = members %>% 
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  group_by(Sex, Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "MN High School Distribution by Weight Class", subtitle = "Grades 9th - 12th", x = "Weight Classes") +
  facet_wrap(Sex~., nrow = 2, scales = "free_y")

SavePlot(p)


p = members %>% 
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  group_by(Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(as.character(Weight), -Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "MN High School Distribution by Weight Class Ordered", subtitle = "Grades 9th - 12th", x = "Weight Classes") +
  theme_gray(base_size = 14)

SavePlot(p)

p = members %>% 
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  mutate(Grade = factor(Grade, levels = c("Fr.", "So.", "Jr.", "Sr."))) %>%
  group_by(Grade, Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  facet_wrap(Grade~., nrow = 2) +
  labs(title = "MN High School Grades Distribution by Weight Class and Grade", subtitle = "Grades 9th - 12th", x = "Weight Classes") +
  theme_gray(base_size = 14)
SavePlot(p)
 
read.csv("Sandbox/Data/members.csv") %>% 
  filter(Grade %in% c("7th", "8th")) %>%
  group_by(Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Minnesota 7th/8th Grade (Wrestling High School) Distribution by Weight Class", x = "Weight Classes")

read.csv("Sandbox/Data/members.csv") %>% 
  filter(Sex == "M") %>%
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  pull(Weight) %>%
  summary()


read.csv("Sandbox/Data/members.csv") %>% 
  #filter(Sex == "M") %>%
  filter(Grade %in% c("So.")) %>%
  pull(Weight) %>%
  summary()

read.csv("Sandbox/Data/members.csv") %>% 
  inner_join(read.csv("Sandbox/Data/teams.csv")) %>%
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  group_by(Team) %>%
  summarise(AverageWeight = mean(Weight), MedianWeight = median(Weight), TeamSize = n()) %>%
  arrange(AverageWeight) %>% 
  print(n = 250)
