
install.packages("RcppArmadillo", type = "binary")
library(RcppArmadillo)
install.packages("BMisc")
install.packages("did")



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
  mutate(
    behandlingstidspunkt_numeric = case_when(
      rute == "Bergen-Oslo-Bergen" ~ 0,  # Bergen er aldri behandlet
      rute == "Stavanger-Oslo-Stavanger" ~ as.numeric(difftime(as.Date("2019-12-15"), min(Måned), units = "days")) / 30,  # Stavanger behandlet 15.12.2019
      rute == "Trondheim-Oslo-Trondheim" ~ as.numeric(difftime(as.Date("2020-06-08"), min(Måned), units = "days")) / 30,  # Trondheim behandlet 08.06.2020
      TRUE ~ NA_real_  # Alle andre ruter, sett NA for de som ikke er spesifisert
    )
  )


att_gt_tog <- att_gt(
  yname   = "log_passasjerer",               # Avhengig variabel
  gname   = "behandlingstidspunkt_numeric",   # Bruk numerisk privatiseringsdato
  idname  = "rute_numeric",                   # Numerisk enhets-ID (rute)
  tname   = "Måned_numeric",                  # Bruk den numeriske tidsvariabelen
  data    = Togdata_combined,                 # Riktig datasett
  control_group = "notyettreated",            # Bergen behandles aldri
  panel = FALSE                              # Ikke bruk balansert panel
)



# Lag numerisk ID for rute
Togdata_combined$rute_numeric <- as.numeric(factor(Togdata_combined$rute))

# Kjør DiD-modellen på nytt
att_gt_tog <- att_gt(
  yname   = "log_passasjerer",               # Avhengig variabel
  gname   = "behandlingstidspunkt_numeric",   # Bruk numerisk privatiseringsdato
  idname  = "rute_numeric",                   # Numerisk enhets-ID (rute)
  tname   = "Måned_numeric",                  # Bruk den numeriske tidsvariabelen
  data    = Togdata_combined,                 # Riktig datasett
  control_group = "notyettreated", 
  panel = FALSE
)

Togdata_combined_clean <- Togdata_combined %>%
  mutate(
    log_passasjerer = ifelse(is.na(log_passasjerer), 0, log_passasjerer),
    behandlingstidspunkt_numeric = ifelse(is.na(behandlingstidspunkt_numeric), 0, behandlingstidspunkt_numeric),
    Måned_numeric = ifelse(is.na(Måned_numeric), 0, Måned_numeric)
  )


summary(att_gt_tog)












