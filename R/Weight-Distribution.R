library(ggplot2)
library(dplyr)

read.csv("Data/members.csv") %>% 
  filter(Grade %in% c("7th", "8th", "Fr.", "So.", "Jr.", "Sr.")) %>%
  filter(Sex == "M") %>%
  group_by(Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
    geom_bar(stat = "identity") +
    labs(title = "All Grades Distribution by Weight Class")

read.csv("Data/members.csv") %>% 
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  filter(Sex == "M") %>%
  group_by(Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "All Minnesota High School Grades Distribution by Weight Class", x = "Weight Classes")

read.csv("Sandbox/Data/members.csv") %>% 
  filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  filter(Sex == "M") %>%
  mutate(Grade = factor(Grade, levels = c("Fr.", "So.", "Jr.", "Sr."))) %>%
  group_by(Grade, Weight) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  facet_wrap(Grade~., nrow = 2) +
  labs(title = "All Minnesota High School Grades Distribution by Weight Class",  x = "Weight Classes")

 
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
