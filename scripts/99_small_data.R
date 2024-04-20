# Small data ------------------------------------------------------------------


# -----------------------------------------------------------------------------




# Source libs, helpers, and load data -----------------------------------------

source(here::here("scripts", "05_load_data.R"))

# -----------------------------------------------------------------------------




# N descriptives --------------------------------------------------------------

n_s <- bind_rows(
  dat_2023 |> 
    summarize(
      n = length(id), 
      n_scores = length(na.omit(score_exam)), 
      n_lt = length(na.omit(score_lt)), 
      missing_exams = n - n_scores, 
      missing_lt = n - n_lt
    ) |> 
    mutate(year = "2023"), 

  dat_2024 |> 
    summarize(
      n = length(id), 
      n_scores = length(na.omit(score_exam)), 
      n_lt = length(na.omit(score_lt)), 
      missing_exams = n - n_scores, 
      missing_lt = n - n_lt
    ) |> 
    mutate(year = "2024")
  )

# -----------------------------------------------------------------------------
