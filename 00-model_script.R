#### Preamble ####
# Purpose: To create the logistic regression model.
# Author: Raghav Bhatia 
# Date: 19 March 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(boot)
library(broom.mixed)
library(collapse)
library(dataverse)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(arrow)
library(readr)

#### Download data ####

## Downloading the demographic variable data

Voter_Data <-
  get_dataframe_by_name(
    filename = "CES20_Common_OUTPUT_vv.csv",
    dataset = "10.7910/DVN/E9N6PH",
    server = "dataverse.harvard.edu",
    .f = read_csv
  ) |>
  select(votereg, CC20_410, gender, educ, race)

write_parquet(Voter_Data, "voter_data.parquet")

## Reading and Modifying the Data

Voter_data_raw <-
  read_parquet(
    "voter_data.parquet",
    col_types =
      cols(
        votereg = col_integer(),
        CC20_410 = col_integer(),
        gender = col_integer(),
        educ = col_integer(),
        race = col_integer()
      )
  )


Voter_data_cleaned <-
  Voter_data_raw |>
  filter(votereg == 1, CC20_410 %in% c(1, 2)) |>
  mutate(
    voted_for = if_else(CC20_410 == 1, "Biden", "Trump"),
    voted_for = as_factor(voted_for),
    gender = if_else(gender == 1, "Male", "Female"),
    education = case_when(
      educ == 1 ~ "No HS",
      educ == 2 ~ "High school graduate",
      educ == 3 ~ "Some college",
      educ == 4 ~ "2-year",
      educ == 5 ~ "4-year",
      educ == 6 ~ "Post-grad"
    ),
    education = factor(education, levels = c("No HS", "High school graduate", 
                                             "Some college", "2-year", "4-year", 
                                             "Post-grad")),
    race = case_when(
      race == 1 ~ "White",
      race == 2 ~ "Black",
      race == 3 ~ "Hispanic",
      race == 4 ~ "Asian",
      race == 5 ~ "Native American",
      race == 6 ~ "Middle Eastern",
      race == 7 ~ "Two or more races",
      TRUE ~ NA_character_
    ),
    race = as_factor(race)
  ) |>
  select(voted_for, gender, education, race)

### Mathematical Model ###

## \begin{align*}

##  y_i|\pi_i &\sim \mbox{Bern}(\pi_i) \\
##  \mbox{logit(}\pi_i\mbox{) } &=  \beta_0 \, + \, \beta_1 \cdot \text{gender}_i \,
##  + \, \beta_2 \cdot \mbox{education}_i \, +  \, \beta_3 \cdot \mbox{race}_i \\
##  &\quad \, + \, \beta_4 \cdot \mbox{economic outlook}_i \,
##  + \, \beta_5 \cdot \mbox{income change}_i \\
##  \beta_0 &\sim \mbox{Normal}(0, 2.5) \\
##  \beta_1 &\sim \mbox{Normal}(0, 2.5) \\
##  \beta_2 &\sim \mbox{Normal}(0, 2.5) \\
##  \beta_3 &\sim \mbox{Normal}(0, 2.5) \\
##  \beta_4 &\sim \mbox{Normal}(0, 2.5) \\
##  \beta_5 &\sim \mbox{Normal}(0, 2.5) \\
  
##  \end{align*}


### Model data ####

## The example considers a sliced sample to improve the runtime of the model.

set.seed(853)

Voter_data_reduced <- 
  Voter_data_cleaned |> 
  slice_sample(n = 2000)

## Voter Outcomes Model

# This glm regresses voting outcome on  gender race and education

voter_outcomes_model <-
  stan_glm(
    voted_for ~ gender + race + education,
    data = Voter_data_reduced,
    family = binomial(link = "logit"),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = 
      normal(location = 0, scale = 2.5, autoscale = TRUE),
    seed = 853
  )


## Model Predictions

voter_outcomes_predictions <-
  predictions(voter_outcomes_model) |>
  as_tibble()

voter_outcomes_predictions

#### Save model ####

saveRDS(
  voter_outcomes_model,
  file = "voter_outcomes_model.rds"
)


