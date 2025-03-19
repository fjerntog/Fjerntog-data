
library(tidyxl)
library(dplyr)

excel_file <- "~/Desktop/Reganalyse data.xlsx"

# Hent ut alle cellene fra første ark (tilpass sheet-navn om nødvendig)
cells <- xlsx_cells(excel_file, sheets = excel_sheets(excel_file)[1])

# Finn alle celler som inneholder tekst (potensielle tabellnavn)
text_cells <- cells %>%
  filter(data_type == "character") %>%
  select(sheet, address, character)  # Beholder kun relevante kolonner

print(text_cells)  # Sjekk hvor tabellene starter

# Hent navnene på alle arkene
sheet_names <- excel_sheets(excel_file)
print(sheet_names) 

# Leser inn alle arkene i en liste
data_list <- lapply(sheet_names, function(sheet) {
  read_excel(excel_file, sheet = sheet)
})

# Gi navn til listene basert på arkene
names(data_list) <- sheet_names

str(data_list)  # Viser strukturen i listen
head(data_list[[1]])  # Viser de første radene i det første arket

library(dplyr)

# Funksjon for å filtrere ut kun rader som ser ut som en tabell
clean_table <- function(df) {
  df %>%
    filter(complete.cases(.)) %>%  # Fjerner tomme rader
    filter(!is.na(df[[1]]))  # Fjerner rader der første kolonne er NA
}

# Rens alle dataene i listen
clean_data_list <- lapply(data_list, clean_table)

str(data_list[[1]])  # Sjekker strukturen i første ark
head(data_list[[1]])  # Viser de første radene

library(dplyr)

# Se første 10 rader for å finne tabellnavn
head(data_list[[1]], 10)

# Sjekk kolonnenavn
colnames(data_list[[1]])



