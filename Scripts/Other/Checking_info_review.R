#Adding extra information to adress reviewers concerns

#1st check the percentage of the different life forms
library(dplyr)
dat %>% 
mutate(life_form = recode_factor(life_form, "vine" = "shrub")) %>% 
group_by(life_form) %>% 
summarise(Count = n(), Percentage = round(n() / nrow(dat), 2))

#2nd check the percentage of the different insect pollinators
#in relation to the other groups

#this data is generated in script 1_Merge_[..]
d = all_long_poll_names

groupped_data = d %>% 
group_by(guild) %>% 
summarise(Count = n())

non_insect = groupped_data %>% filter(guild == "Birds" | guild == "Lizards")

sum(non_insect$Count) / sum(groupped_data$Count) * 100
(sum(groupped_data$Count)-sum(non_insect$Count)) / sum(groupped_data$Count) * 100

non_insect_bird = groupped_data %>% filter(guild == "Birds")
sum(non_insect_bird$Count) / sum(groupped_data$Count) * 100

non_insect_lizard = groupped_data %>% filter(guild == "Lizards")
sum(non_insect_lizard$Count) / sum(groupped_data$Count) * 100

