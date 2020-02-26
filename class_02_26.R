
# Load libraries

library(tidyverse)
library(rstanarm)
library(skimr)

# Load the .rda file on National Election Survey

load("nes.rda")

# Create a new dataframe to store the selected columns

nes_filter <- nes %>%
  select(year, partyid7, real_ideo, race_adj, age_discrete, educ1, female, income)

# Exploratory analysis

summary(nes_filter)
glimpse(nes_filter)
skim(nes_filter)

nes_filter %>%
  group_by(partyid7) %>%
  count() %>%
  ungroup() %>%
  ggplot(aes(x = partyid7, y = n)) +
  geom_line()

# Run regression analysis

nes_fit <- stan_glm(family = "gaussian", data = nes_filter, partyid7 ~ race_adj, refresh = 0)

# Real_ideo missing over time

# nes_filter %>%
#   group_by(year) %>%
#   mutate(ct_over_time = case_when(is.na(real_ideo) ~ n(),
#                    TRUE ~ 1.0)) %>%
#   select(year, ct_over_time) %>%
#   distinct() %>%
#   ungroup() %>%
#   filter(ct_over_time != 1.0) %>%
#   ggplot(aes(x = year, y = ct_over_time)) + 
#   geom_point()

# Drop the NAs

nes_new <- na.omit(nes_filter)

# Female and partyid7 - treating partyid7 as continuous even though stan_glm()
# is for categorical

female_partyid7_fit <- stan_glm(family = "gaussian", data = nes_new, partyid7 ~ female, refresh = 0)
print(female_partyid7_fit)

race_partyid_fit <- stan_glm(data = nes_race, partyid7 ~ race, refresh = 0)
print(race_partyid_fit)

# Use model to predict point
nes_race <- nes_new %>%
  mutate(race_adj = as.character(race_adj),
         race = case_when(race_adj == "1" ~ "White",
                              race_adj == "1.5" ~ "Other",
                              race_adj == "2" ~ "Black",
                              TRUE ~ race_adj))

party_ideo_fit <- stan_glm(data = nes_race, partyid7 ~ real_ideo, refresh = 0)
print(party_ideo_fit)

# Add a predict column

nes_race %>%
  mutate(pred_gender = predict(female_partyid7_fit, nes_race),
         pred_race = predict(race_partyid_fit, nes_race),
         pred_ideo = predict(party_ideo_fit, nes_race)) %>%
  slice(8000:8010)



