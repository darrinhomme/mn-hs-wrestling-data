read.csv("Sandbox/Data/members.csv") %>% 
  dplyr::filter(Grade %in% c("7th", "8th", "Fr.", "So.", "Jr.", "Sr.")) %>%
  dplyr::filter(Sex == "M") %>%
  dplyr::group_by(Weight) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
    geom_bar(stat = "identity") +
    labs(title = "All Grades Distribution by Weight Class")

read.csv("Sandbox/Data/members.csv") %>% 
  dplyr::filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  dplyr::filter(Sex == "M") %>%
  dplyr::group_by(Weight) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "All Minnesota High School Grades Distribution by Weight Class", x = "Weight Classes")

read.csv("Sandbox/Data/members.csv") %>% 
  dplyr::filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  dplyr::filter(Sex == "M") %>%
  dplyr::mutate(Grade = factor(Grade, levels = c("Fr.", "So.", "Jr.", "Sr."))) %>%
  dplyr::group_by(Grade, Weight) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  facet_wrap(Grade~., nrow = 2) +
  labs(title = "All Minnesota High School Grades Distribution by Weight Class",  x = "Weight Classes")

 
read.csv("Sandbox/Data/members.csv") %>% 
  dplyr::filter(Grade %in% c("7th", "8th")) %>%
  dplyr::group_by(Weight) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot(aes(x = as.character(Weight), y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Minnesota 7th/8th Grade (Wrestling High School) Distribution by Weight Class", x = "Weight Classes")

read.csv("Sandbox/Data/members.csv") %>% 
  dplyr::filter(Sex == "M") %>%
  dplyr::filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  dplyr::pull(Weight) %>%
  summary()


read.csv("Sandbox/Data/members.csv") %>% 
  #dplyr::filter(Sex == "M") %>%
  dplyr::filter(Grade %in% c("So.")) %>%
  dplyr::pull(Weight) %>%
  summary()

read.csv("Sandbox/Data/members.csv") %>% 
  dplyr::inner_join(read.csv("Sandbox/Data/teams.csv")) %>%
  dplyr::filter(Grade %in% c("Fr.", "So.", "Jr.", "Sr.")) %>%
  dplyr::group_by(Team) %>%
  dplyr::summarise(AverageWeight = mean(Weight), MedianWeight = median(Weight), TeamSize = n()) %>%
  dplyr::arrange(AverageWeight) %>% 
  print(n = 250)
