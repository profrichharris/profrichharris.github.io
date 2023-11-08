require(sf)
require(tidyverse)

try(setwd("C:/Users/profr/Dropbox/github/MandM/working"), silent = TRUE)
try(setwd("~/Dropbox/github/MandM/working"), silent = TRUE)

wards <- st_read("mdb_wards_2016.shp")

read_csv("table_2022-07-25_13-05-36.csv", skip = 9, col_select = 2:4, n_max = 65880) %>%
  separate(1, into = c("WardID", "WardNumber"), sep = " : ") %>%
  pivot_wider(names_from = `Income category`, values_from = "Count") %>%
  mutate(Total = Total - `Not applicable` - Unspecified) %>%
  mutate(No_income = round(`No income` / Total * 100, 2),
         High_income = round((`R2457601 or more` + `R 1228801 - R 2457600` + `R 614401- R 1228800`) / Total * 100, 2),
         Lower_income = round((`R 1 - R 4800` + `R 4801 - R 9600` + `R 9601 - R 19200`) / Total * 100, 2)) %>%
  select(WardID, No_income, Lower_income, High_income) %>%
  inner_join(wards, ., by = "WardID") %>%
  st_simplify(dTolerance = 50, preserveTopology = TRUE) ->
  wards


read_csv("table_2022-07-28_11-11-40.csv", skip = 11, col_names = FALSE,
         col_select = 1:8, n_max = 4277) %>%
  separate(1, into = c("WardID", "WardNumber"), sep = ": ") %>%
  rename(Employed = 3, Unemployed = 4, Unspecified = 7, Not_applicable = 8,
         Total = 9) %>%
  mutate(Total = Total - Unspecified - Not_applicable) %>%
  mutate(across(contains("ployed"), ~ round(. / Total * 100, 2))) %>%
  select(WardID, Employed, Unemployed) %>%
  left_join(wards, ., by = "WardID") -> wards


read_csv("table_2022-07-28_12-04-23.csv", skip = 11, col_names = FALSE,
         col_select = 1:10, n_max = 4277) %>%
  separate(1, into = c("WardID", "WardNumber"), sep = ": ") %>%
  rename(No_schooling = 3, Higher_edu = 8,
         Unspecified = 9, Not_applicable = 10, Total = 11) %>%
  mutate(Total = Total - Unspecified - Not_applicable) %>%
  select(WardID, No_schooling, Higher_edu, Total) %>%
  mutate(across(where(is.numeric), ~ round(. / Total * 100, 2))) %>%
  select(-Total) %>%
  left_join(wards, ., by = "WardID") -> wards


read_csv("table_2022-07-28_12-16-50.csv", skip = 11, col_names = FALSE,
         col_select = 1:8, n_max = 4277) %>%
  separate(1, into = c("WardID", "WardNumber"), sep = ": ") %>%
  select(-WardNumber) %>%
  rename(Black_African = 2, Coloured = 3, Indian_Asian = 4,
         White = 5, Other = 6, Unspecified = 7, Total = 8) %>%
  mutate(Total = Total - Unspecified) %>%
  mutate(across(where(is.numeric), ~ round(. / Total * 100, 2))) %>%
  select(-Unspecified, - Total) %>%
  left_join(wards, ., by = "WardID") -> wards


read_csv("table_2022-07-28_12-27-16.csv", skip = 11, col_names = FALSE,
         n_max = 4277) -> df
age <- vector(mode = "numeric", length = nrow(df))
count <- 0
for(i in 1: nrow(df)) {
  count <- count + 1
  if(count == 100) {
    count <- 0
    cat("\n", i)
  } 
  for(j in 2: (ncol(df)-3)) {
    age[i] <- age[i] + (j - 0.5) * df[i, j] %>% pull
  }
}

df %>%
  select(1, 123, 124) %>%
  separate(1, into = c("WardID", "WardNumber"), sep = ": ") %>%
  rename(Unspecified = 3, Total = 4) %>%
  mutate(Total = Total - Unspecified) %>%
  select(-WardNumber, -Unspecified) -> df

df$age <- age
df %>%
  mutate(age = round(age / Total,1)) %>%
  select(-Total) %>%
  left_join(wards, ., by = "WardID") -> wards

try(setwd("C:/Users/profr/Dropbox/github/MandM/workspaces"), silent = TRUE)
try(setwd("~/Dropbox/github/MandM/workspaces"), silent = TRUE)
save(wards, file = "wards.RData")
