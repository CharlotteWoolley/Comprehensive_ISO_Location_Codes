library(tidyverse)
library(zoo)

eQuest <- read_csv('eQuest.csv', col_names = FALSE) %>%
        rename(country_code = X1,
               region_code = X2,
               sub_code = X3,
               name = X4) %>%
        mutate(country_code = na.locf(country_code)) %>%
        group_by(country_code) %>%
        mutate(region_code = na.locf(region_code, na.rm = FALSE)) %>%
        ungroup() %>%
        separate(region_code, c("RC1", "region_code_short"), "-", remove = FALSE) %>%
        separate(sub_code, c("SC1", "sub_code_short"), "-", remove = FALSE) %>%
        select(country_code, region_code, region_code_short, sub_code, sub_code_short, name) 
 
subdiv <- read_csv('2017-1 SubdivisionCodes.csv', col_names = FALSE) %>%
        rename(country_code = X1,
               region_code_short = X2,
               name = X3,
               region_type = X4)

all_divisions <- full_join(eQuest, subdiv)

part1_name1 <- read_csv('2017-1 UNLOCODE CodeListPart1.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               region_code_short = X3,
               name = X4) %>%
        select(country_code, region_code_short, name)

part1_name2 <- read_csv('2017-1 UNLOCODE CodeListPart1.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               region_code_short = X3,
               name = X5) %>%
        select(country_code, region_code_short, name)

part2_name1 <- read_csv('2017-1 UNLOCODE CodeListPart2.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               region_code_short = X3,
               name = X4) %>%
        select(country_code, region_code_short, name)

part2_name2 <- read_csv('2017-1 UNLOCODE CodeListPart2.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               region_code_short = X3,
               name = X5) %>%
        select(country_code, region_code_short, name)

part3_name1 <- read_csv('2017-1 UNLOCODE CodeListPart3.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               region_code_short = X3,
               name = X4)%>%
        select(country_code, region_code_short, name)

part3_name2 <- read_csv('2017-1 UNLOCODE CodeListPart3.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               region_code_short = X3,
               name = X5)%>%
        select(country_code, region_code_short, name)

new_data <- full_join(all_divisions, rbind(part1_name1, part1_name2, part2_name1, 
                           part2_name2, part3_name1, part3_name2)) 

new_data2 <- unique(new_data)

sum(duplicated(new_data$name))

%>%
        mutate(name = case_when(is.na(name1) == FALSE ~ name1,
                                is.na(name1) == TRUE & is.na(name4) == FALSE ~ name4,
                                is.na(name1) == TRUE & is.na(name2) == FALSE ~ name2,
                                is.na(name1) == TRUE & is.na(name3) == FALSE ~ name3))
               
               
               mtcars %>% 
                       mutate(cg = case_when(carb <= 2 ~ "low",
                                             carb > 2  ~ "high"))
        
        


