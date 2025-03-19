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



# Standardiser kolonnenavn for enkel fletteprosess
colnames(`Passasjerkm Dovrebanen`)[2] <- "Passasjerkm_Dovrebanen"
colnames(`Passasjerer Dovrebanen`)[2] <- "Passasjerer_Dovrebanen"
colnames(`SeteKm Dovrebanen`)[2] <- "Setekm_Dovrebanen"
colnames(`Fyllingsgrad Dovrebanen`)[2] <- "Fyllingsgrad_Dovrebanen"

colnames(`Passasjerkm Bergensbanen`)[2] <- "Passasjerkm_Bergensbanen"
colnames(`Passasjerer Bergensbanen`)[2] <- "Passasjerer_Bergensbanen"
colnames(`SeteKm Bergensbanen`)[2] <- "Setekm_Bergensbanen"
colnames(`Fyllingsgrad Bergenbanen`)[2] <- "Fyllingsgrad_Bergensbanen"

colnames(`Passasjerkm Sørlandsbanen`)[2] <- "Passasjerkm_Sørlandsbanen"
colnames(`Passasjerer Sørlandsbanen`)[2] <- "Passasjerer_Sørlandsbanen"
colnames(`SeteKm Sørlandsbanen`)[2] <- "Setekm_Sørlandsbanen"
colnames(`Fyllingsgrad Sørlandsbanen`)[2] <- "Fyllingsgrad_Sørlandsbanen"

# Dovrebanen
Dovrebanen_df <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE), 
                        list(`Passasjerkm Dovrebanen`, `Passasjerer Dovrebanen`, 
                             `SeteKm Dovrebanen`, `Fyllingsgrad Dovrebanen`))

# Bergensbanen
Bergensbanen_df <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE), 
                          list(`Passasjerkm Bergensbanen`, `Passasjerer Bergensbanen`, 
                               `SeteKm Bergensbanen`, `Fyllingsgrad Bergenbanen`))

# Sørlandsbanen
Sørlandsbanen_df <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE), 
                           list(`Passasjerkm Sørlandsbanen`, `Passasjerer Sørlandsbanen`, 
                                `SeteKm Sørlandsbanen`, `Fyllingsgrad Sørlandsbanen`))

# Sjekk at det fungerer
head(Dovrebanen_df)
head(Bergensbanen_df)
head(Sørlandsbanen_df)


# Merge alle datasettene horisontalt basert på "Måned"
Togdata_wide <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE),
                       list(`Passasjerkm Dovrebanen`, `Passasjerer Dovrebanen`, `SeteKm Dovrebanen`, `Fyllingsgrad Dovrebanen`,
                            `Passasjerkm Bergensbanen`, `Passasjerer Bergensbanen`, `SeteKm Bergensbanen`, `Fyllingsgrad Bergenbanen`,
                            `Passasjerkm Sørlandsbanen`, `Passasjerer Sørlandsbanen`, `SeteKm Sørlandsbanen`, `Fyllingsgrad Sørlandsbanen`))

# Se på de første radene
head(Togdata_wide)


library(RcppArmadillo)
library(BMisc)
library(dplyr)
library(lubridate)
library(did)


# Standardiser kolonnenavn for Bergensbanen
Bergensbanen2 <- Bergensbanen_df %>%
  rename(
    Passasjerkm = Passasjerkm_Bergensbanen,
    Passasjerer = Passasjerer_Bergensbanen,
    Setekm = Setekm_Bergensbanen,
    Fyllingsgrad = Fyllingsgrad_Bergensbanen
  ) %>%
  mutate(
    rute = "Bergen-Oslo-Bergen", 
    behandlingstidspunkt = as.Date(NA)
  )

# Standardiser kolonnenavn for Sørlandsbanen
Sørlandsbanen2 <- Sørlandsbanen_df %>%
  rename(
    Passasjerkm = Passasjerkm_Sørlandsbanen,
    Passasjerer = Passasjerer_Sørlandsbanen,
    Setekm = Setekm_Sørlandsbanen,
    Fyllingsgrad = Fyllingsgrad_Sørlandsbanen
  ) %>%
  mutate(
    rute = "Stavanger-Oslo-Stavanger",
    behandlingstidspunkt = as.Date("2019-12-15")
  )

# Standardiser kolonnenavn for Dovrebanen
Dovrebanen2 <- Dovrebanen_df %>%
  rename(
    Passasjerkm = Passasjerkm_Dovrebanen,
    Passasjerer = Passasjerer_Dovrebanen,
    Setekm = Setekm_Dovrebanen,
    Fyllingsgrad = Fyllingsgrad_Dovrebanen
  ) %>%
  mutate(
    rute = "Trondheim-Oslo-Trondheim",
    behandlingstidspunkt = as.Date("2020-06-08")
  )

# Kombiner datasettene horisontalt
Togdata_combined <- bind_rows(Bergensbanen2, Sørlandsbanen2, Dovrebanen2)

# Sjekk resultatet
head(Togdata_combined)


# Opprett post-dummies for privatisering og COVID-kontroll
Togdata_combined <- Togdata_combined %>%
  mutate(
    post_stavanger = ifelse(rute == "Stavanger-Oslo-Stavanger" & Måned >= as.Date("2019-12-15"), 1, 0),
    post_trondheim = ifelse(rute == "Trondheim-Oslo-Trondheim" & Måned >= as.Date("2020-06-08"), 1, 0),
    
    # COVID-kontrollvariabel
    covid = ifelse(Måned >= as.Date("2020-03-01") & Måned <= as.Date("2022-01-12"), 1, 0)
  )

# Konverter "Måned" til Date-format
Togdata_combined$Måned <- as.Date(Togdata_combined$Måned)

str(Togdata_combined)

# Konverter behandlingstidspunkt til numerisk (antall dager fra første dato)
Togdata_combined$behandlingstidspunkt_numeric <- as.numeric(difftime(Togdata_combined$behandlingstidspunkt, min(Togdata_combined$behandlingstidspunkt), units = "days"))


# Opprett behandlingstidspunkt (privatiseringsdato)
Togdata_combined <- Togdata_combined %>%
  mutate(
    behandlingstidspunkt = case_when(
      rute == "Stavanger-Oslo-Stavanger" ~ as.Date("2019-12-15"),
      rute == "Trondheim-Oslo-Trondheim" ~ as.Date("2020-06-08"),
      TRUE ~ NA_Date_  # Bergen-Oslo-Bergen forblir kontrollgruppe (NA)
    )
  )

# Opprett log-transformert variabel (unngå log(0) problem)
Togdata_combined <- Togdata_combined %>%
  mutate(log_passasjerer = log(Passasjerer + 1))  # +1 for å unngå log(0)

# Sjekk at alt ser riktig ut
summary(Togdata_combined$behandlingstidspunkt)
summary(Togdata_combined$log_passasjerer)


# Konverter POSIXct til numerisk (antall måneder fra første dato)
Togdata_combined$Måned_numeric <- as.numeric(difftime(Togdata_combined$Måned, min(Togdata_combined$Måned), units = "days")) / 30
# Konverter behandlingstidspunkt til numerisk (antall måneder fra første dato)
Togdata_combined$behandlingstidspunkt_numeric <- as.numeric(difftime(Togdata_combined$behandlingstidspunkt, min(Togdata_combined$behandlingstidspunkt), units = "days")) / 30

colnames(Togdata_combined)

# Sjekk om det finnes NA-verdier i datasettet
colSums(is.na(Togdata_combined))

Togdata_combined <- Togdata_combined %>%
  mutate(
    Passasjerkm = ifelse(is.na(Passasjerkm), 0, Passasjerkm),
    Passasjerer = ifelse(is.na(Passasjerer), 0, Passasjerer),
    Setekm = ifelse(is.na(Setekm), 0, Setekm),
    Fyllingsgrad = ifelse(is.na(Fyllingsgrad), 0, Fyllingsgrad),
    log_passasjerer = ifelse(is.na(log_passasjerer), 0, log_passasjerer)
  )

Togdata_combined <- Togdata_combined %>%
  mutate(
    behandlingstidspunkt = ifelse(is.na(behandlingstidspunkt), NA_Date_, behandlingstidspunkt)
  )

Togdata_combined <- Togdata_combined %>%
  mutate(
    behandlingstidspunkt_numeric = ifelse(is.na(behandlingstidspunkt_numeric), 0, behandlingstidspunkt_numeric)
  )

Togdata_combined <- Togdata_combined %>%
  mutate (
    behandlingstidspunkt_numeric = case_when(
      rute == "Bergen-Oslo-Bergen" ~ 0,  # Bergen er aldri behandlet
      rute == "Stavanger-Oslo-Stavanger" ~ as.numeric(difftime(as.Date("2019-12-15"), min(Måned), units = "days")) / 30,  # Stavanger behandlet 15.12.2019
      rute == "Trondheim-Oslo-Trondheim" ~ as.numeric(difftime(as.Date("2020-06-08"), min(Måned), units = "days")) / 30,  # Trondheim behandlet 08.06.2020
      TRUE ~ NA_real_  # Alle andre ruter, sett NA for de som ikke er spesifisert
    )
  )



# Én vei-flydata
colnames(`Fyllingsgrad Envei`)[2] <- "Fyllingsgrad"
colnames(`Passasjerkm Envei`)[2] <- "Passasjerkm"
colnames(`Pris Flybillett Envei`)[2:4] <- c("Bergen_Oslo_NOK", "Stavanger_Oslo_NOK", "Trondheim_Oslo_NOK")
colnames(`Setekm Envei.`)[2] <- "Setekm"
colnames(`Seter Envei`)[2] <- "Seter"

# Tur/retur-flydata
colnames(`Fyllingsgrad T.R`)[2] <- "Fyllingsgrad"
colnames(`Passasjerkm T.R`)[2] <- "Passasjerkm"
colnames(`Setekm T.R`)[2] <- "Setekm"

# Fly én vei
Fly_Envei_df <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE),
                       list(`Fyllingsgrad Envei`, `Passasjerkm Envei`, 
                            `Pris Flybillett Envei`, `Setekm Envei.`, `Seter Envei`))

# Fly tur/retur
Fly_TR_df <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE),
                    list(`Fyllingsgrad T.R`, `Passasjerkm T.R`, `Setekm T.R`))

# Sjekk at det fungerer
head(Fly_Envei_df)
head(Fly_TR_df)

#Rydde opp i titler flydata
Fly_Envei_df <- as.data.frame(Fly_Envei_df)
colnames(Fly_Envei_df)[which(colnames(Fly_Envei_df) == "Setekm")] <- "Setekm èn vei BGO"
colnames(Fly_Envei_df)[which(colnames(Fly_Envei_df) == "Fyllingsgrad")] <- "Fyllingsgrad èn vei BGO"
colnames(Fly_Envei_df)[which(colnames(Fly_Envei_df) == "Passasjerkm")] <- "Passasjerkm èn vei BGO"
colnames(Fly_Envei_df)[which(colnames(Fly_Envei_df) == "Seter")] <- "Seter èn vei BGO"
colnames(Fly_Envei_df)[which(colnames(Fly_Envei_df) == "SVG")] <- "Seter én vei SVG"
colnames(Fly_Envei_df)[which(colnames(Fly_Envei_df) == "TRD")] <- "Seter én vei TRD"
colnames(Fly_TR_df)[which(colnames(Fly_TR_df) == "Fyllingsgrad")] <- "Fyllingsgrad SVG T/R"
colnames(Fly_TR_df)[which(colnames(Fly_TR_df) == "Passasjerkm")] <- "Passasjerkm SVG T/R"
colnames(Fly_TR_df)[which(colnames(Fly_TR_df) == "Setekm")] <- "Setekm SVG T/R"

library(dplyr)
library(lubridate)
library(did)


# For Bergensbanen
# For Bergensbanen
Bergensbanen2 <- Bergensbanen_df %>%
  rename(
    Passasjerkm = Passasjerkm_Bergensbanen,
    Passasjerer = Passasjerer_Bergensbanen,
    Setekm = Setekm_Bergensbanen,
    Fyllingsgrad = Fyllingsgrad_Bergensbanen
  ) %>%
  mutate(
    rute = "Bergen-Oslo-Bergen", 
    behandlingstidspunkt = as.Date(NA),  # Legg til en NA-behandlingstidspunkt hvis det ikke finnes
    behandlingstidspunkt = ifelse(is.na(behandlingstidspunkt), as.Date("1970-01-01"), behandlingstidspunkt),  # Erstatt NA med en standard dato
    Måned_numeric = as.numeric(difftime(Måned, min(Måned), units = "days")) / 30  # Konverter Måned til numerisk (antall måneder)
  ) %>%
  filter(Måned >= as.Date("2015-01-01") & Måned <= as.Date("2023-12-01"))  # Filtrer datoene


# Sjekk resultatet
head(Bergensbanen2)

#View(Bergensbanen2)
#str(Bergensbanen2)

# For Sørlandsbanen
Sørlandsbanen2 <- Sørlandsbanen_df %>%
  rename(
    Passasjerkm = Passasjerkm_Sørlandsbanen,
    Passasjerer = Passasjerer_Sørlandsbanen,
    Setekm = Setekm_Sørlandsbanen,
    Fyllingsgrad = Fyllingsgrad_Sørlandsbanen
  ) %>%
  mutate(
    rute = "Stavanger-Oslo-Stavanger",
    behandlingstidspunkt = as.Date("2019-12-15"),
    Måned_numeric = as.numeric(difftime(Måned, min(Måned), units = "days")) / 30  # Konverter Måned til numerisk (antall måneder)
  ) %>%
  filter(Måned >= as.Date("2015-01-01") & Måned <= as.Date("2023-12-01"))  # Filtrer datoene

#str(Sørlandsbanen2)
#View(Sørlandsbanen2)

# For Dovrebanen
Dovrebanen2 <- Dovrebanen_df %>%
  rename(
    Passasjerkm = Passasjerkm_Dovrebanen,
    Passasjerer = Passasjerer_Dovrebanen,
    Setekm = Setekm_Dovrebanen,
    Fyllingsgrad = Fyllingsgrad_Dovrebanen
  ) %>%
  mutate(
    rute = "Trondheim-Oslo-Trondheim",
    behandlingstidspunkt = as.Date("2020-06-08"),
    Måned_numeric = as.numeric(difftime(Måned, min(Måned), units = "days")) / 30  # Konverter Måned til numerisk (antall måneder)
  ) %>%
  filter(Måned >= as.Date("2015-01-01") & Måned <= as.Date("2023-12-01"))  # Filtrer datoene

#str(Dovrebanen2)
#View(Dovrebanen2)

str(Bergensbanen2$behandlingstidspunkt)
str(Sørlandsbanen2$behandlingstidspunkt)
str(Dovrebanen2$behandlingstidspunkt)


Bergensbanen2 <- Bergensbanen2 %>%
  mutate(behandlingstidspunkt = as.Date(NA))  # Sørg for at den er Date
str(Bergensbanen2$behandlingstidspunkt)

Togdata_combined <- bind_rows(Bergensbanen2, Sørlandsbanen2, Dovrebanen2)

#View(Togdata_combined)

str(Togdata_combined)


library(tidyverse)

#Skfiter fra dato til Måned
#`Data T.R` <- `Data T.R` %>%
  #rename(Måned = Dato)


Fly_TR_df<-Fly_TR_df %>% left_join(`Data T.R`, by = "Måned")


# Standardiser kolonnenavn for flydataene
Flydata_combined <- Fly_TR_df %>%
  rename(
    Fyllingsgrad_BGO = `Fyllingsgrad BGO T/R`,
    Fyllingsgrad_SVG = `Fyllingsgrad SVG T/R`,
    Fyllingsgrad_TRD = `Fyllingsgrad TRD T/R`,
    
    Passasjerkm_BGO = `Passasjerkm BGO T/R`,
    Passasjerkm_SVG = `Passasjerkm SVG T/R`,
    Passasjerkm_TRD = `Passasjerkm TRD T/R`,
    
    Setekm_BGO = `Setekm BGO T/R`,
    Setekm_SVG = `Setekm SVG T/R`,
    Setekm_TRD = `Setekm TRD T/R`,
    
    Flybevegelser_BGO = `Flybevegelser BGO T/R`,
    Flybevegelser_SVG = `Flybeveglser SVG T/R`,
    Flybevegelser_TRD = `Flybevegelser TRD T/R`,
    
    Passasjerer_BGO = `Passasjerer BGO T/R`,
    Passasjerer_SVG = `Passasjerer SVG T/R`,
    Passasjerer_TRD = `Passasjerer TRD T/R`,
    
    Seter_BGO = `Seter BGO T/R`,
    Seter_SVG = `Seter SVG T/R`,
    Seter_TRD = `Seter TRD T/R`
  ) %>%
  pivot_longer(cols = starts_with("Passasjerkm") | starts_with("Setekm") | starts_with("Fyllingsgrad") |
                 starts_with("Flybevegelser") | starts_with("Passasjerer") | starts_with("Seter"),
               names_to = c("variable", "rute"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(
    rute = case_when(
      rute == "BGO" ~ "Bergen-Oslo-Bergen_Fly",
      rute == "SVG" ~ "Stavanger-Oslo-Stavanger_Fly",
      rute == "TRD" ~ "Trondheim-Oslo-Trondheim_Fly"
    ),
    behandlingstidspunkt = NA,  # Fly har ingen behandlingsdato
    Måned_numeric = as.numeric(difftime(Måned, min(Måned), units = "days")) / 30  # Konverter Måned til numerisk
  ) %>%
  filter(Måned >= as.Date("2015-01-01") & Måned <= as.Date("2023-12-01"))  # Filtrer datoer


# Sjekk resultatet
head(Flydata_combined)

str(Togdata_combined)
str(Flydata_combined)

Togdata_combined <- Togdata_combined %>%
  mutate(
    Flybevegelser = NA,
    Seter = NA
  )

Flydata_combined <- Flydata_combined %>%
  mutate(behandlingstidspunkt = as.Date(NA))

Flydata_combined <- Flydata_combined %>%
  select(Måned, Passasjerkm, Passasjerer, Setekm, Fyllingsgrad, rute, behandlingstidspunkt, Måned_numeric, Flybevegelser, Seter)

Togdata_combined <- Togdata_combined %>%
  select(Måned, Passasjerkm, Passasjerer, Setekm, Fyllingsgrad, rute, behandlingstidspunkt, Måned_numeric, Flybevegelser, Seter)


# Slå sammen tog- og flydata
Transportdata_combined <- bind_rows(Togdata_combined, Flydata_combined)

# Sjekk datasettet
str(Transportdata_combined)
head(Transportdata_combined)



