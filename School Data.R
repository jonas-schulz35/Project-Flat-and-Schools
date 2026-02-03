library(readxl)
library(readr)
library(tidyverse)


# Clenaning School Data

School_data <- read_excel("C:/Users/jonas/Documents/Data Analysis Project/school_data.xlsx")

School_data_filtered <- School_data %>% 
  select(school_ID, school_type, district, plz, city_name, ergg_1km) %>% 
  transform (school_type = as.numeric(as.character(school_type))) %>%
  filter(school_type == 02)

# Cleaning Distance to School Data

Distance_School <- read_csv("C:/Users/jonas/Documents/Data Analysis Project/distance_to_schools.csv")

 Distance_School_filtered <-Distance_School %>%
  filter(school_type == 2 & nn_order == 1)

# Cleaning School Social Index 

School_Social_Index <- read_excel("C:/Users/jonas/Documents/Data Analysis Project/2022_social_index-gereinigt.xlsx")

School_Social_Index_filtered <- School_Social_Index %>%
  select(Schulnummer, Sozialindexstufe) %>%
  rename(school_ID = Schulnummer) %>% 
  filter(Sozialindexstufe != 0) %>% 
  mutate(
   Sozialindexstufe_faktor = factor(
      Sozialindexstufe,
      levels = c( 1:9
     )
    )
  )

#Merging the Data Sets 

School_Distance_and_Social_Index <- full_join(Distance_School_filtered, School_Social_Index_filtered, by = "school_ID" )

School_Data_ges <- full_join(School_data_filtered, School_Distance_and_Social_Index, by = c("ergg_1km",
                                                                                              "school_type",
                                                                                              "school_ID"


)) %>%    

mutate(
  dist_factor = as.numeric(as.character(dist)),
  dist_kat = cut(
    dist_factor,
    breaks = c(0,0.250,0.500,0.750,1.000,1.250,1.500,1.750,2.000),
    labels = c("0-0.25","0.25-0.50","0.50-0.75","0.75-1.00","1.00-1.25",
               "1.25-1.50","1.50-1.75","1.75-2.00")
  )
)
