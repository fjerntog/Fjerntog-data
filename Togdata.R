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

View(Bergensbanen2)
str(Bergensbanen2)

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

str(Sørlandsbanen2)
View(Sørlandsbanen2)

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

str(Dovrebanen2)
View(Dovrebanen2)

str(Bergensbanen2$behandlingstidspunkt)
str(Sørlandsbanen2$behandlingstidspunkt)
str(Dovrebanen2$behandlingstidspunkt)


Bergensbanen2 <- Bergensbanen2 %>%
  mutate(behandlingstidspunkt = as.Date(NA))  # Sørg for at den er Date
str(Bergensbanen2$behandlingstidspunkt)

Togdata_combined <- bind_rows(Bergensbanen2, Sørlandsbanen2, Dovrebanen2)

View(Togdata_combined)

str(Togdata_combined)


