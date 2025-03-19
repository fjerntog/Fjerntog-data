
library(readxl)
library(purrr)

file_path <- "Til Rstudo(3).xlsx"  # Endre til riktig filbane

file.exists(file_path)  # Skal returnere TRUE


sheets <- excel_sheets(file_path)  # Hent arkene
print(sheets)

data_list <- map(sheets, ~ read_excel(file_path, sheet = .x))  # Les alle arkene

names(data_list) <- sheets  # Gi navn til hver tabell i listen

list2env(data_list, envir = .GlobalEnv) #KOnvertere listen til separate data frames

install.packages("fixest")  # Hvis ikke allerede installert
library(fixest)

ls()  # Liste ut alle dataframes









