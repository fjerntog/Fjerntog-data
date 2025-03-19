library(tidyverse)

`Data T.R` <- `Data T.R` %>%
  rename(Måned = Dato)


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
