library(plfs)
library(dplyr)
library(sf)
library(duckdb)
library(stringr)

con <- duckdb::dbConnect(duckdb::duckdb(), plfs::plfs_file_path())

dbListTables(con)

d2122 <- tbl(con, "2021-22-perv1") %>%
    select(
        state = state_perv1,
        gender = b4q5_perv1,
        age = b4q6_perv1,
        education = b4q8_perv1,
        sector = b1q3_perv1,
        salaried_wage = b6q9_perv1,
        selfemployed_wage = b6q10_perv1,
        economic_status = b5pt2q3_perv1
    ) %>%
    collect() %>%
    mutate(year = "2021-22")

d2223 <- tbl(con, "2022-23-perv1") %>%
    select(
        state = state_perv1,
        gender = b4q5_perv1,
        age = b4q6_perv1,
        education = b4q8_perv1,
        sector = b1q3_perv1,
        salaried_wage = b6q9_perv1,
        selfemployed_wage = b6q10_perv1,
        economic_status = b5pt2q3_perv1
    ) %>%
    collect() %>%
    mutate(year = "2022-23")

d2324 <- tbl(con, "2023-24-perv1") %>%
    select(
        state = state_perv1,
        gender = b4q5_perv1,
        age = b4q6_perv1,
        education = b4q8_perv1,
        sector = b1q3_perv1,
        salaried_wage = b6q9_perv1,
        selfemployed_wage = b6q10_perv1,
        economic_status = b5pt2q3_perv1
    ) %>%
    collect() %>%
    mutate(year = "2023-24")

dbDisconnect(con, shutdown = TRUE)

d <- d2122 %>%
    bind_rows(d2223) %>%
    bind_rows(d2324)

state_codes <- readxl::read_excel("documentation/District_codes_PLFS_Panel_4_202324_2024.xlsx", range = "A4:D698") %>%
    janitor::clean_names() %>%
    distinct(state_code, state_name)

d <- d %>%
  left_join(state_codes, by = c("state" = "state_code"))

# Type of employment
# Status Code (b5pt2q3_perv1)
# 12, 12, 31, 41, 51 - employed
# 21, 91, 92, 93, 94, 95, 97 - inactive
# 81 - unemployed
# Earnings For Regular Salaried/Wage Activity (b6q9_perv1)
# Earnings For Self Employed (b6q10_perv1)
d <- d %>%
    mutate(
        employed = case_when(
            economic_status %in% c("12", "31", "41", "51") ~ "employed",
            economic_status %in%
                c("21", "91", "92", "93", "94", "95", "97") ~ "inactive",
            economic_status == "81" ~ "unemployed",
            TRUE ~ NA_character_
        ),
        employment_type = case_when(
            selfemployed_wage >= 0 & employed == "employed" ~ "self-employed",
            salaried_wage >= 0 & employed == "employed" ~ "salaried",
            TRUE ~ NA_character_
        )
    )

d <- d %>%
    mutate(
        age = case_when(
            age < 18 ~ "under 18",
            age >= 18 & age <= 25 ~ "18-25",
            age >= 26 & age <= 35 ~ "26-35",
            age >= 36 & age <= 45 ~ "36-45",
            age >= 46 & age <= 60 ~ "46-60",
            age > 60 ~ "60+",
            TRUE ~ NA_character_
        )
    )

# Gender (b4q5_perv1)
# 1 = male
# 2 = female
# 3 = transgender
d <- d %>%
    mutate(
        gender = case_when(
            gender == "1" ~ "male",
            gender == "2" ~ "female",
            gender == "3" ~ "transgender",
            TRUE ~ NA_character_
        )
    )

# General Educaion Level (b4q8_perv1)
# 01 	not literate
# 02 	literate without formal schooling
# 03 	literate without formal schooling
# 04 	others
# 05 	below primary
# 06 	primary
# 07 	middle
# 08 	secondary
# 10 	higher secondary
# 11 	diploma/certificate course
# 12 	graduate
# 13 	postgraduate and above
d <- d %>%
    mutate(
        education = case_when(
            education %in% c("01", "02") ~ "not literate",
            education %in% c("03", "04") ~ "literate",
            education %in% c("05") ~ "below primary",
            education %in% c("06") ~ "primary",
            education %in% c("07") ~ "middle",
            education %in% c("08") ~ "secondary",
            education %in% c("10") ~ "higher secondary",
            education %in% c("11") ~ "diploma/certificate course",
            education %in% c("12") ~ "graduate",
            education %in% c("13") ~ "postgraduate and above",
            TRUE ~ NA_character_
        )
    )

# Sector (b1q3_perv1)
# 1 = rural
# 2 = urban
d <- d %>%
    mutate(
        sector = case_when(
            sector == "1" ~ "rural",
            sector == "2" ~ "urban",
            TRUE ~ NA_character_
        )
    )

d <- d %>%
    select(state_name, year, gender, age, sector, education, employed, employment_type)

d %>%
    group_by(employed) %>%
    summarise(count = n())

d <- d %>%
    filter(employed == "employed")

d <- d %>%
    select(-employed) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(state_name = as.character(state_name))

d2 <- d3po::subnational %>%
    filter(country == "India") %>%
    select(region, geometry) %>%
    mutate(region = as.character(region))

sort(unique(d$state_name))

#  [1] "A & N ISLANDS"              "ANDHRA PRADESH"            
#  [3] "ARUNACHAL PRADESH"          "ASSAM"                     
#  [5] "BIHAR"                      "CHANDIGARH"                
#  [7] "CHHATTISGARH"               "DAMAN & DIU & D & N HAVELI"
#  [9] "DELHI"                      "GOA"                       
# [11] "GUJARAT"                    "HARYANA"                   
# [13] "HIMACHAL PRADESH"           "JAMMU & KASHMIR"           
# [15] "JHARKHAND"                  "KARNATAKA"                 
# [17] "KERALA"                     "LADAKH"                    
# [19] "LAKSHADWEEP"                "MADHYA PRADESH"            
# [21] "MAHARASHTRA"                "MANIPUR"                   
# [23] "MEGHALAYA"                  "MIZORAM"                   
# [25] "NAGALAND"                   "ODISHA"                    
# [27] "PUDUCHERRY"                 "PUNJAB"                    
# [29] "RAJASTHAN"                  "SIKKIM"                    
# [31] "TAMIL NADU"                 "TELANGANA"                 
# [33] "TRIPURA"                    "UTTAR PRADESH"             
# [35] "UTTARAKHAND"                "WEST BENGAL"   

sort(unique(d2$region))

#  [1] "Andaman and Nicobar"                     
#  [2] "Andhra Pradesh"                          
#  [3] "Arunachal Pradesh"                       
#  [4] "Assam"                                   
#  [5] "Bihar"                                   
#  [6] "Chandigarh"                              
#  [7] "Chhattisgarh"                            
#  [8] "Dadra and Nagar Haveli and Daman and Diu"
#  [9] "Delhi"                                   
# [10] "Goa"                                     
# [11] "Gujarat"                                 
# [12] "Haryana"                                 
# [13] "Himachal Pradesh"                        
# [14] "Jammu and Kashmir"                       
# [15] "Jharkhand"                               
# [16] "Karnataka"                               
# [17] "Kerala"                                  
# [18] "Ladakh"                                  
# [19] "Lakshadweep"                             
# [20] "Madhya Pradesh"                          
# [21] "Maharashtra"                             
# [22] "Manipur"                                 
# [23] "Meghalaya"                               
# [24] "Mizoram"                                 
# [25] "Nagaland"                                
# [26] "Odisha"                                  
# [27] "Puducherry"                              
# [28] "Punjab"                                  
# [29] "Rajasthan"                               
# [30] "Sikkim"                                  
# [31] "Tamil Nadu"                              
# [32] "Telangana"                               
# [33] "Tripura"                                 
# [34] "Uttar Pradesh"                           
# [35] "Uttarakhand"                             
# [36] "West Bengal"

# normalize names
norm <- function(x) {
  x %>%
    toupper() %>%
    str_replace_all("&", "AND") %>%
    str_replace_all("&", "AND") %>%
    str_replace_all("[^A-Z0-9]", "") %>%
    str_squish()
}

d <- d %>%
  mutate(state_norm = norm(state_name))

d2 <- d2 %>%
  mutate(region_norm = norm(region))

match_idx <- stringdist::amatch(d$state_norm,
  d2$region_norm, method = "jw", maxDist = 0.18)

unmatched <- which(is.na(match_idx))

if (length(unmatched) > 0) {
  distinct(d[unmatched, "state_norm"])
}

# # A tibble: 2 × 1
#   state_norm               
#   <chr>                    
# 1 AANDNISLANDS             
# 2 DAMANANDDIUANDDANDNHAVELI

unique(d2$region_norm)

# AANDNISLANDS -> ANDAMANANDNICOBAR
# DAMANANDDIUANDDANDNHAVELI -> DADRAANDNAGARHAVELIANDDAMANANDDIU

d <- d %>%
    mutate(
        state_norm = case_when(
        state_norm == "AANDNISLANDS" ~ "ANDAMANANDNICOBAR",
        state_norm == "DAMANANDDIUANDDANDNHAVELI" ~ "DADRAANDNAGARHAVELIANDDAMANANDDIU",
        TRUE ~ state_norm
        )
    )

d <- d %>%
    left_join(
        d2 %>%
            select(region, region_norm, geometry),
        by = c("state_norm" = "region_norm")
    )

d2 <- d %>%
    sf::st_drop_geometry()

saveRDS(d2, "processed_employment_data.rds")

d <- d %>%
    select(region, geometry) %>%
    distinct()

saveRDS(d, "map_of_india.rds")
