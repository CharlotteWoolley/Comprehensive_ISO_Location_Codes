library(tidyverse)
library(zoo)
library(stringr)

eQuest <- read_csv('eQuest.csv', col_names = FALSE) %>%
        rename(country_code = X1,
               principality_code = X2,
               region_code = X3,
               name = X4) %>%
        mutate(country_code = na.locf(country_code)) %>%
        group_by(country_code) %>%
        mutate(principality_code = na.locf(principality_code, na.rm = FALSE)) %>%
        ungroup() %>%
        separate(principality_code, c("RC1", "principality_code_short"), "-", remove = FALSE) %>%
        separate(region_code, c("SC1", "region_code_short"), "-", remove = FALSE) %>%
        select(country_code, principality_code, principality_code_short, region_code, region_code_short, name) 
 
subdiv <- read_csv('2017-1 SubdivisionCodes.csv', col_names = FALSE) %>%
        rename(country_code = X1,
               region_code_short = X2,
               name = X3) %>%
        select(country_code, region_code_short, name) 

all_divisions <- full_join(eQuest, subdiv)

part1_name1 <- read_csv('2017-1 UNLOCODE CodeListPart1.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               sub_code_short = X3,
               region_code_short = X6, 
               name = X4) %>%
        select(country_code, region_code_short, sub_code_short, name)

part1_name2 <- read_csv('2017-1 UNLOCODE CodeListPart1.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               sub_code_short = X3,
               region_code_short = X6, 
               name = X5) %>%
        select(country_code, region_code_short, sub_code_short, name)

part2_name1 <- read_csv('2017-1 UNLOCODE CodeListPart2.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               sub_code_short = X3,
               region_code_short = X6, 
               name = X4) %>%
        select(country_code, region_code_short, sub_code_short, name)

part2_name2 <- read_csv('2017-1 UNLOCODE CodeListPart2.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               sub_code_short = X3,
               region_code_short = X6, 
               name = X5) %>%
        select(country_code, region_code_short, sub_code_short, name)

part3_name1 <- read_csv('2017-1 UNLOCODE CodeListPart3.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               sub_code_short = X3,
               region_code_short = X6, 
               name = X4)%>%
        select(country_code, region_code_short, sub_code_short, name)

part3_name2 <- read_csv('2017-1 UNLOCODE CodeListPart3.csv', col_names = FALSE) %>%
        rename(country_code = X2,
               sub_code_short = X3,
               region_code_short = X6, 
               name = X5)%>%
        select(country_code, region_code_short, sub_code_short, name)

new_data <- full_join(all_divisions, rbind(part1_name1, part1_name2, part2_name1, 
                           part2_name2, part3_name1, part3_name2)) 

new_data2 <- unique(new_data) %>%
        filter(!is.na(name))

new_data2 <- new_data2 %>%
        mutate(region_code = case_when(is.na(region_code_short) == TRUE ~ region_code,
                                       is.na(region_code_short) == FALSE ~ paste(country_code, "-", region_code_short, sep = "")),
               sub_code = case_when(is.na(sub_code_short) == TRUE ~ sub_code_short,
                                       is.na(sub_code_short) == FALSE ~ paste(country_code, "-", sub_code_short, sep = "")),
               name = str_to_upper(sub("\\.", "", name)),
               contains_symbols = grepl('[^[:alnum:]]',name)) %>%
        select(country_code, principality_code, principality_code_short, region_code, 
               region_code_short, sub_code, sub_code_short, name, contains_symbols)

new_data3 <- unique(new_data2)

get_duplications <- function(X) {
        X <- X %>%
                mutate(dups_ID = group_indices_(X, .dots=c("country_code", "region_code", "region_code_short", "sub_code", "sub_code_short")),
                       duplications = (duplicated(dups_ID) |  duplicated(dups_ID, fromLast = TRUE)))
        print(c("Duplications", sum(X$duplications)))
        return(X)
}

new_data3 <- get_duplications(new_data3) 



        
        


