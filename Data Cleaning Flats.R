library(tidyverse)
library(readr)
library(fastDummies)
library(ggplot2)

#______________________________________________________________________________#
#       Ablageort                                                           ####



#______________________________________________________________________________#
#      Data wrangling                                                       ####

Flats_bought_2022 <- read_csv("C:/Users/jonas/Documents/RUB Uni/WISE 2025/Data Analysis with R/Daten_projekt/CampusFile_WK_2022.csv")
View(Flats_bought_2022)
  NRW_Flat_bought <- Flats_bought_2022 %>%
  filter( blid == "North Rhine-Westphalia" ) %>%
  filter( baujahr > 1970) %>%
filter( ergg_1km != -9 ) %>%
  mutate(price_sqm_log = log(price_sqm)) %>%
  dummy_cols(select_columns = c("balkon","keller","garten", "aufzug"), 
             remove_first_dummy = TRUE ) %>%
dummy_cols( select_columns = c("parkplatz", "rollstuhlgerecht"), 
            remove_first_dummy = TRUE, ignore_na = TRUE) %>%
  mutate(
    parkplatz_Yes = factor(parkplatz_Yes, levels = c(0,1)),
    balkon_Yes = factor(balkon_Yes, levels = c(0,1)),
    garten_Yes = factor(garten_Yes, levels = c(0,1)),
    aufzug_Yes = factor(aufzug_Yes, levels = c(0,1)),
    parkplatz_Yes = factor(parkplatz_Yes, levels = c(0,1)),
    rollstuhlgerecht_Yes = factor(rollstuhlgerecht_Yes, levels = c(0,1))
      dummy_cols( select_columns = "parkplatz", 
                  remove_first_dummy = TRUE, ignore_na = TRUE) %>%
      mutate(
        etage_num = as.numeric(as.character(etage)),
      ) %>%
      mutate(
        baujahr_num = as.numeric(as.character(baujahr)),
        baujahr_kat = cut(
          baujahr_num,
          breaks = c(1900, 1946, 1970, 1980, 1990, 2000, 2010, Inf),
          labels = 1:7,
          right = FALSE
        )
      ) %>%    
      mutate(
        wohnflaeche_num = as.numeric(as.character(wohnflaeche)),
        wohnflaeche_kat = cut(
          wohnflaeche_num,
          breaks = c(20, 40, 60, 80, 100, 120, 140, 160, 180, Inf),
          labels = 1:9,
          right = FALSE
        )
          breaks = c( 1970, 1980, 1990, 2000, 2010, Inf),
        labels = 1:5,
        right = FALSE
      )
  ) %>% 
  mutate(
    ausstattung_kat = recode(
      as.character(ausstattung),
      "Not specified" = 0,
      "Simple"        = 1,
      "Normal"        = 2,
      "sophisticated" = 3,
      "Deluxe"        = 4,
      .default = NA_real_
    )
  ) %>% 
  mutate(
    ausstattung_faktor = as.factor(ausstattung_kat)
  )

# Flats rented________________________________________________________

Flats_rented_2022 <- read_csv("C:/Users/jonas/Documents/RUB Uni/WISE 2025/Data Analysis with R/Daten_projekt/CampusFile_WM_2022.csv")
View(Flats_rented_2022)
NRW_Flat_rented <- Flats_rented_2022 %>%
  filter(  blid == "North Rhine-Westphalia" ) %>%
  filter(baujahr > 1970) %>%
  filter(ergg_1km != -9) %>%
  mutate(rent_sqm_log = log(rent_sqm)) %>%
  dummy_cols(
    select_columns = c("balkon","keller","garten","aufzug"), 
    remove_first_dummy = TRUE ) %>%
  dummy_cols( 
    select_columns = "parkplatz", 
    remove_first_dummy = TRUE, ignore_na = TRUE
  ) %>%
  mutate(
    etage_num = as.numeric(as.character(etage)),
  ) %>% 
  mutate(
    baujahr_num = as.numeric(as.character(baujahr)),
    baujahr_kat = cut(
      baujahr_num,
      breaks = c( 1970, 1980, 1990, 2000, 2010, Inf),
      labels = 1:5,
      right = FALSE
    )
  ) %>%
  mutate(
    ausstattung_kat = recode(
      as.character(ausstattung),
      "Not specified" = 0,
      "Simple"        = 1,
      "Normal"        = 2,
      "sophisticated" = 3,
      "Deluxe"        = 4,
      .default = NA_real_
    )
  ) %>% 
  mutate(
    ausstattung_faktor = as.factor(ausstattung_kat)
  ) %>% 
  mutate(
    laufzeittage_log = log(laufzeittage)
  )


Flats_rented_2022 <- read_csv("C:/Users/jonas/Documents/RUB Uni/WISE 2025/Data Analysis with R/Daten_projekt/CampusFile_WM_2022.csv")
View(Flats_rented_2022)
NRW_Flat_rented <- Flats_rented_2022 %>%
  filter(  plz >= 40000 & plz <= 60000 ) %>%
  filter(baujahr > 1900) %>%
  filter(ergg_1km != -9) %>%
  mutate(rent_sqm_log = log(rent_sqm)) %>%
  dummy_cols(
    select_columns = c("balkon","keller","garten","aufzug"), 
    remove_first_dummy = TRUE ) %>%
  dummy_cols( 
    select_columns = c("parkplatz", "rollstuhlgerecht"), 
    remove_first_dummy = TRUE, ignore_na = TRUE
  ) %>%
  mutate(
    baujahr_num = as.numeric(as.character(baujahr)),
    baujahr_kat = cut(
      baujahr_num,
      breaks = c(1900, 1946, 1970, 1980, 1990, 2000, 2010, Inf),
      labels = 1:7,
      right = FALSE
    )
  ) %>%
  mutate(
    wohnflaeche_num = as.numeric(as.character(wohnflaeche)),
    wohnflaeche_kat = cut(
      wohnflaeche_num,
      breaks = c(20, 40, 60, 80, 100, 120, 140, 160, 180, Inf),
      labels = 1:9,
      right = FALSE
    )
  ) %>% 
  mutate(
    ausstattung_kat = recode(
      as.character(ausstattung),
      "Not specified" = 0,
      "Simple"        = 1,
      "Normal"        = 2,
      "sophisticated" = 3,
      "Deluxe"        = 4,
      .default = NA_real_
    )
  )

#______________________________________________________________________________#
# Data Merge                                                                ####

City_Flat_bought <- inner_join(
  NRW_Flat_bought, School_Data_ges, by = "ergg_1km") %>%
  filter(!is.na(dist)) %>% 
  mutate(
    dist_d = ifelse(dist < 1, 1, 0)
  ) %>% 
mutate(
  wohnflaeche_kat_num = as.numeric(as.character(wohnflaeche_kat))
) %>% 
  mutate(
    baujahr_kat_num = as.numeric(as.character(baujahr_kat))
  ) %>% 
  mutate(
        Garden = as.numeric(as.character(garten_Yes)),
        Elevator = as.numeric(as.character(aufzug_Yes)),
        Balcony = as.numeric(as.character(balkon_Yes)),
        Cellar = as.numeric(as.character(keller_Yes)),
        Parking = as.numeric(as.character(parkplatz_Yes))
      )
    
    
    City_Flat_rented <- inner_join(
      NRW_Flat_rented, School_Data_ges, by = "ergg_1km")%>%
    filter(!is.na(dist)) %>% 
      mutate(
        dist_d = ifelse(dist < 1, 1, 0)
      ) %>% 
      mutate(
        Garden = as.numeric(as.character(garten_Yes)),
        Elevator = as.numeric(as.character(aufzug_Yes)),
        Balcony = as.numeric(as.character(balkon_Yes)),
        Cellar = as.numeric(as.character(keller_Yes)),
        Parking = as.numeric(as.character(parkplatz_Yes))
      )
    
    
    # Visual inspection of the Data     ####
    
    ggplot(NRW_Flat_bought, aes(kaufpreis, baujahr)) + geom_smooth(method = lm)
    ggplot(NRW_Flat_bought, aes(kaufpreis, zimmeranzahl)) + geom_smooth(method = maxlike())
    ggplot(NRW_Flat_bought, aes(zimmeranzahl)) + geom_histogram()
    ggplot(NRW_Flat_bought, aes(kaufpreis, zimmeranzahl)) + geom_area()
    
    
    ggplot(NRW_Flat_rented, aes(mietekalt, baujahr)) + geom_smooth(method = lm)
    ggplot(NRW_Flat_rented, aes(baujahr, mietekalt)) + geom_point()
    ggplot(NRW_Flat_rented, aes( mietekalt, zimmeranzahl)) + geom_point()
    ggplot(NRW_Flat_bought, aes(wohnflaeche)) + geom_histogram(binwidth = 20)
    