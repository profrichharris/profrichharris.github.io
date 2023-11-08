require(tidyverse)

setwd("C:/Users/profr/Dropbox/Research/Census2021/ethnic.change")

read_csv("harmonised_ethnicity_counts.csv") |>
  mutate(PLACE = ifelse(RGN21NM == "London", RGN21NM, LAD22NM)) |>
  mutate(Persons.11 = Persons.11 - WGYP.11) |>
  mutate(Persons.21 = Persons.21 - WGYP.21) |>
  select(-contains("WGYP")) |>
  select(PLACE, where(is.numeric)) |>
  group_by(PLACE) |>
  summarise(across(where(is.numeric), sum)) -> df


df |>
  select(PLACE, contains(".01")) |>
  mutate(across(where(is.numeric) & !contains("Persons"), ~ . / Persons.01)) |>
  select(-Persons.01) |>
  mutate(across(where(is.numeric), ~ . * log(.))) |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .))) |>
  rowwise() |>
  mutate(E.01 = sum(c_across(where(is.numeric)))) |>
  ungroup() |>
  select(PLACE, E.01) |>
  mutate(E.01 = -1 * E.01 / log(14)) |>
  left_join(
    df |>
      select(PLACE, contains(".11")) |>
      mutate(across(where(is.numeric) & !contains("Persons"), ~ . / Persons.11)) |>
      select(-Persons.11) |>
      mutate(across(where(is.numeric), ~ . * log(.))) |>
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .))) |>
      rowwise() |>
      mutate(E.11 = sum(c_across(where(is.numeric)))) |>
      ungroup() |>
      select(PLACE, E.11) |>
      mutate(E.11 = -1 * E.11 / log(14)), by = "PLACE"  
  ) |> 
  left_join(
    df |>
      select(PLACE, contains(".21")) |>
      mutate(across(where(is.numeric) & !contains("Persons"), ~ . / Persons.21)) |>
      select(-Persons.21) |>
      mutate(across(where(is.numeric), ~ . * log(.))) |>
      mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .))) |>
      rowwise() |>
      mutate(E.21 = sum(c_across(where(is.numeric)))) |>
      ungroup() |>
      select(PLACE, E.21) |>
      mutate(E.21 = -1 * E.21 / log(14)), by = "PLACE"  
  ) -> E


setwd("C:/Users/profr/Dropbox/github/MandM/data")
write_csv(E, "diversity2.csv")


