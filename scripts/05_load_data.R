# Load data -------------------------------------------------------------------

source(here::here("scripts", "01_helpers.R"))

dat_2023 <- read_csv(here("data", "tidy", "all_data_tidy_2023.csv")) |> 
  mutate(across(score_exam:score_lt, \(x) scale_this(x), .names = "{.col}_z")) |> 
  mutate(
    level_n = case_when(
      level == "1"      ~ 1, 
      level == "1.5"    ~ 2, 
      level == "2"      ~ 3, 
      level == "3"      ~ 4, 
      level == "4"      ~ 5, 
      level == "master" ~ 6
    )
  )

dat_2024 <- read_csv(here("data", "tidy", "all_data_tidy_2024.csv"))


# -----------------------------------------------------------------------------
