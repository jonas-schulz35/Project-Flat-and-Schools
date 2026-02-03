library(tidyverse)
library(modelsummary)
library(ggplot2)
library(broom)


#______________________________________________________________________________#
# Method                                                                    ####

OLS_Formula <- price_sqm_log ~ 
  dist_d + baujahr_kat + wohnflaeche  + ausstattung_faktor +
  garten_Yes + parkplatz_Yes + balkon_Yes + keller_Yes + aufzug_Yes + 
  zimmeranzahl + etage_num + Sozialindexstufe_faktor

OLS_regression_buy <- lm(
  formula = OLS_Formula,
  data = City_Flat_bought
)

summary(OLS_regression_buy) 

OLS_Formula_2<-  rent_sqm_log ~ 
  dist_d + baujahr_kat  + wohnflaeche + ausstattung_faktor +
  garten_Yes + parkplatz_Yes + balkon_Yes + keller_Yes + aufzug_Yes + 
  zimmeranzahl + etage_num 

OLS_regression_rent <- lm(
  formula = OLS_Formula_2,
  data = City_Flat_rented
)

summary(OLS_regression_rent)


#______________________________________________________________________________#
#     Table wit kableExtra                                                  ####

goef_labels  <- tibble(
  raw = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("Observations", "R²", "Adj. R²"),
  fmt = c(0, 4, 4)
)

coef_label <- c( 
  dist_d = "distance to school", baujahr_kat = "Year of Construction", wohnflaeche
  = "t", ausstattung_faktor = "zu", garten_YES = "Garten", parkplatz_Yes = "parking lot",
  balkon_YES = "balcony", keller_Yes = "Cellar", aufzug_Yes = "elevator", 
  zimmeranzahl = "No. of rooms", etage_num = "No. of Floors"
)

reg_table_old <- msummary(
  list(OLS_regression_buy, OLS_regression_rent),
  output = "kableExtra",
  escape = TRUE,
  vcov = "HC3",
  stars = TRUE,
  estimate = "{estimate} ({std.error}){stars}",
  statistic = NULL,
  coef_map = coef_label,
  coef_omit = c(-1),
  gof_map = goef_labels,
  col.names = c("", "1","2"),
  title = "OLS Regression Flats",
  notes = "Parentheses shows robust standard errors.",
  format = "latex"
) %>% 
  add_header_above(
    c("", "OLS_buy" = 1, "OLS_rent" = 1)
  )

reg_table_old


#______________________________________________________________________________#
#                     Plot                                                  ####


reg_results <- tidy(OLS_regression_buy)

Plot <- reg_results %>%
  filter(term %in% 
           c("dist_kat0.25-0.50", "dist_kat0.50-0.75", "dist_kat0.75-1.00",
             "dist_kat1.00-1.25", "dist_kat1.25-1.50", "dist_kat1.50-1.75",
             "dist_kat1.75-2.00")) %>%
  mutate(
    dist_label = gsub("dist_kat", "", term), 
    fit = estimate,
    se = std.error
  )

ggplot(Plot, aes(x = term, 
                 y = estimate, 
                 ymin = estimate - 1.96 * std.error, 
                 ymax = estimate + 1.96 * std.error)) +
  geom_crossbar(fatten = 2, fill = "grey", width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Distance to School (Categories)",
    y = "Estimated Effect on Log Flat Price",
    title = "Effect of Distance to School on Flat Prices"
  ) +
  theme_minimal()

save_path <- "C:/Users/jonas/Documents/Data Analysis Project/crossbar_buy.png"
ggsave(filename = save_path,
       units = "px",
       width = 1920,
       height = 1080)

#Plot für 

reg_results_2 <- tidy(OLS_regression_rent)

Plot <- reg_results_2 %>%
  filter(term %in% 
           c("dist_kat0.25-0.50", "dist_kat0.50-0.75", "dist_kat0.75-1.00",
             "dist_kat1.00-1.25", "dist_kat1.25-1.50", "dist_kat1.50-1.75",
              "dist_kat1.75-2.00")) %>%
  mutate(
    dist_label = gsub("dist_kat", "", term), 
    fit = estimate,
    se = std.error
  )

ggplot(Plot, aes(x = term, 
                    y = estimate, 
                    ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error)) +
  geom_crossbar(fatten = 2, fill = "grey", width = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "Distance to School (Categories)",
    y = "Estimated Effect on Log Flat Price",
    title = "Effect of Distance to School on Renting Prices"
  ) +
  theme_minimal()


save_path <- "C:/Users/jonas/Documents/Data Analysis Project/crossbar_rent.png"
ggsave(filename = save_path,
       units = "px",
       width = 1920,
       height = 1080)
