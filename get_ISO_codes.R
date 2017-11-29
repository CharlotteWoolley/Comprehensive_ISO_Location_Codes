#This script was intended to join together the location codes from 2 different 
#data sources so that as many as possible can be compiled together into a simple format. 
#For example, certain data sources don't contain the principalities of the 
#United Kingdom (England, Scotland, Wales, Nothern Ireland etc) and other
#data sources have ambiguity between regional and sub-regional codes

#Packages Used
        library(tidyverse)
        library(zoo)
        library(stringr)

#Data sources

#eQuest data containing ISO 3166-2 subdivisions is taken from
#https://github.com/olahol/iso-3166-2.json 
#Filename: 'eQuest.csv'

#All 4 other data sets, which contain codes for trade and transport locations
#https://www.unece.org/cefact/codesfortrade/codes_index.html
#Fileneames: '2017-1 SubdivisionCodes.csv', '2017-1 UNLOCODE CodeListPart1.csv',
        #'2017-1 UNLOCODE CodeListPart2.csv', '2017-1 UNLOCODE CodeListPart3.csv'


#Read in 'eQuest' data 
        eQuest <- read_csv('eQuest.csv', col_names = FALSE) %>%
                #Name columns sensibly
                rename(country_code = X1,
                       principality_code = X2,
                       region_code = X3,
                       name = X4) %>%
                #Fill in unneccesary NAs in country and principality codes
                mutate(country_code = na.locf(country_code)) %>% 
                group_by(country_code) %>%
                mutate(principality_code = na.locf(principality_code, 
                                                   na.rm = FALSE)) %>%
                ungroup() %>%
                #Seperate the principality and region codes from the country 
                #codes so that they can be easily joined to other datasets
                separate(principality_code, c("RC1", "principality_code_short"), 
                         "-", remove = FALSE) %>%
                separate(region_code, c("SC1", "region_code_short"), 
                         "-", remove = FALSE) %>%
                #Order columns and remove columns that aren't needed
                select(country_code, principality_code, principality_code_short, 
                       region_code, region_code_short, name) 
         
#Read in '2017-1 SubdivisionCodes' data
        subdiv <- read_csv('2017-1 SubdivisionCodes.csv', col_names = FALSE) %>%
        #Name columns to match previous dataset so they can be easily joined
        rename(country_code = X1,
               region_code_short = X2,
               name = X3) %>%
        #Remove columns that aren't needed
        select(country_code, region_code_short, name) 


        #Join the 'eQuest' data to the '2017-1 SubdivisionCodes' data
        all_divisions <- full_join(eQuest, subdiv)
        
#The next section of codes extracts the data out of each part of the 
#'2017-1 UNLOCODE CodeList'
#Both name columns are extarcted seperately because there are spelling/character 
#discrepancies and keeping both makes it easier to join to other data

        part1_name1 <- read_csv('2017-1 UNLOCODE CodeListPart1.csv', col_names = FALSE) %>%
                #Name columns to match previous datasets
                rename(country_code = X2,
                       sub_region_code_short = X3,
                       region_code_short = X6, 
                       name = X4) %>% #This extracts data from the left hand name column
                #Order columns and remove columns that aren't needed
                select(country_code, region_code_short, sub_region_code_short, name)
        
        part1_name2 <- read_csv('2017-1 UNLOCODE CodeListPart1.csv', col_names = FALSE) %>%
                #Name columns to match previous datasets
                rename(country_code = X2,
                       sub_region_code_short = X3,
                       region_code_short = X6, 
                       name = X5) %>% #This extracts data from the right hand name column
                #Order columns and remove columns that aren't needed
                select(country_code, region_code_short, sub_region_code_short, name)
        
        part2_name1 <- read_csv('2017-1 UNLOCODE CodeListPart2.csv', col_names = FALSE) %>%
                #Name columns to match previous datasets
                rename(country_code = X2,
                       sub_region_code_short = X3,
                       region_code_short = X6, 
                       name = X4) %>% #This extracts data from the left hand name column
                #Order columns and remove columns that aren't needed
                select(country_code, region_code_short, sub_region_code_short, name)
        
        part2_name2 <- read_csv('2017-1 UNLOCODE CodeListPart2.csv', col_names = FALSE) %>%
                #Name columns to match previous datasets
                rename(country_code = X2,
                       sub_region_code_short = X3,
                       region_code_short = X6, 
                       name = X5) %>% #This extracts data from the right hand name column
                #Order columns and remove columns that aren't needed
                select(country_code, region_code_short, sub_region_code_short, name)
        
        part3_name1 <- read_csv('2017-1 UNLOCODE CodeListPart3.csv', col_names = FALSE) %>%
                #Name columns to match previous datasets
                rename(country_code = X2,
                       sub_region_code_short = X3,
                       region_code_short = X6, 
                       name = X4)%>% #This extracts data from the left hand name column
                #Order columns and remove columns that aren't needed
                select(country_code, region_code_short, sub_region_code_short, name)
        
        part3_name2 <- read_csv('2017-1 UNLOCODE CodeListPart3.csv', col_names = FALSE) %>%
                #Name columns to match previous datasets
                rename(country_code = X2,
                       sub_region_code_short = X3,
                       region_code_short = X6, 
                       name = X5)%>% #This extracts data from the right hand name column
                #Order columns and remove columns that aren't needed
                select(country_code, region_code_short, sub_region_code_short, name)

#Bind each part of the '2017-1 UNLOCODE CodeList' together in a long format       
        all_parts <- rbind(part1_name1, part1_name2, part2_name1, 
                           part2_name2, part3_name1, part3_name2)
        
#Join the compiled '2017-1 UNLOCODE CodeLists' to the previously joined
#eQuest' and '2017-1 SubdivisionCodes' data
        new_data <- full_join(all_divisions, all_parts) %>%
                #Join all region and sub-region codes back to the country code so 
                #that they are in the desired ouput e.g 'XX-XXX'
                mutate(region_code = 
                               case_when(is.na(region_code_short) == TRUE ~ 
                                                 region_code,
                                       is.na(region_code_short) == FALSE ~ 
                                               paste(country_code, "-", 
                                                     region_code_short, sep = "")),
                       sub_region_code = 
                               case_when(is.na(sub_region_code_short) == TRUE ~ 
                                                 sub_region_code_short,
                                        is.na(sub_region_code_short) == FALSE ~ 
                                                paste(country_code, "-", 
                                                      sub_region_code_short, sep = "")),
                       #Form a new column 'sub code' that summarises the 
                       #principality, region and sub-region codes into one column. 
                       #Generally, the region code is used as the sub code. When
                       #there isn't a region code but there is a sub-region code,
                       #the sub-region code is used. When neither the region code
                       #nor the sub-region code is avaiable, the principality
                       #code is used. When none of these codes exist, a blank
                       #data entry is given because this is when a country is 
                       #being described rather than a location within a country.
                       sub_code = 
                               case_when(is.na(region_code) == FALSE ~ 
                                                 region_code,
                                            is.na(region_code) == TRUE & 
                                                 is.na(sub_region_code) == FALSE ~ 
                                                 sub_region_code,
                                            is.na(region_code) == TRUE & 
                                                 is.na(sub_region_code) == TRUE & 
                                                    is.na(principality_code) == FALSE ~ 
                                                 principality_code,
                                            is.na(region_code) == TRUE & 
                                                 is.na(sub_region_code) == TRUE & 
                                                    is.na(principality_code) == TRUE ~ 
                                                 ""),
                #Convert string to upper case and remove full stops
                #from the beginning of the string
                       name = str_to_upper(sub("\\.", "", name))) %>%
                #Select only the relevant columns
                select(country_code, sub_code, name)
        
#Finally, remove duplicated rows and rows without any location names
        ISO_codes <- unique(new_data) %>%
                filter(!is.na(name))

#write the data to a csv file
        write_csv(ISO_codes, 'ISO_codes.csv')

#The output of this script is a dataframe (with 122635 observations) which contains 
#a country code, a sub-code and the location name. The sub-code may be missing 
#if the location is a country. 

        
        