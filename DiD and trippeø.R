library(dplyr)
library(did)
library(lubridate)


library(dplyr)
library(lubridate)
library(did)

# 1) Lag "train_data" med en numerisk tid for hver rad (tid = antall måneder siden start)
train_data <- Togdata_combined %>%
  mutate(
    tid = as.numeric(difftime(Måned, min(Måned), units = "days")) %/% 30,
    log_passasjerer = log(Passasjerer + 1)
  )

# 2) Finn den numeriske “behandlingsperioden” for Stavanger og Trondheim,
#    og sett 0 for Bergen (som aldri behandles).
cutoff_stavanger <- as.numeric(difftime(as.Date("2019-12-15"), min(train_data$Måned), units = "days")) %/% 30
cutoff_trondheim <- as.numeric(difftime(as.Date("2020-06-08"), min(train_data$Måned), units = "days")) %/% 30

train_data <- train_data %>%
  mutate(
    # g = "first period unit becomes treated"
    g = case_when(
      rute == "Stavanger-Oslo-Stavanger"  ~ cutoff_stavanger,
      rute == "Trondheim-Oslo-Trondheim" ~ cutoff_trondheim,
      TRUE ~ 0  # Bergen = aldri behandlet
    )
  )

# 3) Kjør did::att_gt() med tname = tid (en numeric kolonne)
#    og gname = g (også numeric).
att_gt_tog <- att_gt(
  yname   = "log_passasjerer",
  tname   = "tid",
  idname  = "rute",
  gname   = "g",        # NB: nå en numerisk kolonne i "train_data"
  data    = train_data,
  control_group = "notyettreated"
)

summary(att_gt_tog)

str(train_data)


library(dplyr)
library(did)

train_data <- train_data %>%
  # Lag en numerisk ID for hver rute
  mutate(
    id = as.numeric(factor(rute))  # eller bruk group_by() + cur_group_id()
  )

att_gt_tog <- att_gt(
  yname   = "log_passasjerer",
  tname   = "tid",
  idname  = "id",
  gname   = "g",
  data    = train_data,
  control_group = "notyettreated",
  panel = FALSE
)

library(dplyr)
library(did)

# 1) Opprett (eller behold) datasettet med numerisk tidsindeks og numerisk enhets-ID
train_data <- Togdata_combined %>%
  mutate(
    # 'tid': antall måneder siden første observasjon
    tid = as.numeric(difftime(Måned, min(Måned), units = "days")) %/% 30,
    
    # 'id': numerisk ID for rute
    id = as.numeric(factor(rute)),
    
    # log-utfall
    log_passasjerer = log(Passasjerer + 1)
    
    # g = første behandlingsperiode for hver rute (eksempel):
    # g = ...
  )

# 2) Kjør 'att_gt()' med xformla = ~ passasjerkm + setekm
att_gt_tog <- att_gt(
  yname        = "log_passasjerer",  # Avhengig variabel
  tname        = "tid",             # Numerisk tidsindeks
  idname       = "id",              # Numerisk rute-ID
  gname        = "g",               # Numerisk behandlingsperiode
  data         = train_data,
  control_group = "notyettreated",
  panel        = FALSE,             # for ubalansert panel
  # Her er kontrollvariablene:
  xformla      = ~ Passasjerkm
)

summary(att_gt_tog)






library(dplyr)
library(lfe)

# 1. Definer dummier
Transportdata_combined_1 <- Transportdata_combined %>%
  mutate(
    # Tog vs. fly
    train = ifelse(grepl("_Fly$", rute), 0, 1),
    
    # Behandlet rute (Stavanger eller Trondheim) vs. ubehandlet (Bergen)
    treated = ifelse(rute %in% c("Stavanger-Oslo-Stavanger", 
                                 "Trondheim-Oslo-Trondheim"), 1, 0),
    
    # Post = 1 for perioder ETTER privatisering for dem som blir privatisert.
    # (For Stavanger: 15.12.2019, for Trondheim: 08.06.2020)
    post = case_when(
      rute == "Stavanger-Oslo-Stavanger" & Måned >= as.Date("2019-12-15") ~ 1,
      rute == "Trondheim-Oslo-Trondheim" & Måned >= as.Date("2020-06-08") ~ 1,
      TRUE ~ 0
    ),
    
    # Lag gjerne en log-transformert outcome
    log_passasjerer = log(Passasjerer + 1)
  )

# 2. Kjør triple difference via felm
# DDD = (train × treated × post)
ddd_model <- felm(
  log_passasjerer ~ train + treated + post 
  + train:treated + train:post + treated:post
  + train:treated:post   # <--- 3-dimensjons interaksjonen
  | rute + Måned, 
  data = Transportdata_combined_1
)

summary(ddd_model)

library(fixest)

ddd_model_1 <- feols(
  log_passasjerer ~ train + treated + post 
  + train:treated + train:post + treated:post
  + train:treated:post,  
  fixed = ~ rute + Måned,  # Faste effekter for både rute og måned
  data = Transportdata_combined_1
)

summary(ddd_model)


ddd_model_2 <- felm(
  log_passasjerer ~ train + treated + post 
  + train:treated + train:post + treated:post
  + train:treated:post,  
  | Måned,  # Fjernet 'rute' for å la 'treated' bli identifiserbar
  data = Transportdata_combined_1
)
summary(ddd_model)


ddd_model_fix <- felm(
  log_passasjerer ~ train + treated + post 
  + train:treated + train:post + treated:post
  + train:treated:post  # <--- Fjernet komma før | 
  | Måned,  # Kun faste effekter for tid
  data = Transportdata_combined_1
)

summary(ddd_model_fix)


ddd_model_fix2 <- felm(
  log_passasjerer ~ train + treated + post 
  + train:treated + train:post + treated:post
  + train:treated:post  
  | rute,  # Bruker faste effekter for rute, men ikke måned
  data = Transportdata_combined_1
)

summary(ddd_model_fix2)


ddd_model_fix3 <- felm(
  log_passasjerer ~ train + treated + post 
  + train:treated + train:post + treated:post
  + train:treated:post  
  | Måned,  # Bruker faste effekter for tid, fjerner rute
  data = Transportdata_combined_1
)

summary(ddd_model_fix3)

library(fixest)


library(fixest)

ddd_model_fix4 <- feols(
  log_passasjerer ~ train * treated * post,  
  fixef = "Måned",  # Beholder faste effekter for tid
  data = Transportdata_combined_1
)

summary(ddd_model_fix4)

ddd_model_fix5 <- feols(
  log_passasjerer ~ train * treated * post,  
  fixef = c("Måned"),  # Spesifiserer faste effekter riktig
  data = Transportdata_combined_1,
  collin.tol = 1e-6  # Reduserer toleransen for kollinearitet
)

summary(ddd_model_fix5)


ddd_model_fix6 <- feols(
  log_passasjerer ~ train * treated * post,  
  fixef = "rute",  # Kun rute-faste effekter
  data = Transportdata_combined_1
)

summary(ddd_model_fix6)


library(did)

att_gt_model_1 <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",  # Numerisk tidsindeks
  idname  = "rute",
  gname   = "behandlingstidspunkt",  # Behandlingstidspunkt per rute
  data    = Transportdata_combined_1,
  control_group = "notyettreated"
)

summary(att_gt_model_1)


# Sjekk hvor mange pre-treatment observasjoner vi har
table(Transportdata_combined_2$Måned_numeric < Transportdata_combined_2$behandlingstidspunkt_numeric)

att_gt_model_covid <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",
  idname  = "rute_numeric",
  gname   = "behandlingstidspunkt_numeric",
  data    = Transportdata_combined_2,  
  xformla = ~ covid,  
  control_group = "notyettreated"
)

summary(att_gt_model_covid)



library(lubridate)  # Importer hvis ikke lastet inn

Transportdata_combined_2 <- Transportdata_combined_1 %>%
  mutate(
    covid = ifelse(Måned >= as.Date("2020-03-01") & Måned <= as.Date("2022-01-12"), 1, 0),
    
    # Sørg for at behandlingstidspunkt er en Date-verdi for alle observasjoner
    behandlingstidspunkt = case_when(
      is.na(behandlingstidspunkt) ~ as.Date(max(Måned, na.rm = TRUE)) %m+% years(3),
      TRUE ~ behandlingstidspunkt  # Behold eksisterende verdier
    ),
    
    # Konverter behandlingstidspunkt til numerisk verdi (antall måneder siden første observasjon)
    behandlingstidspunkt_numeric = as.numeric(difftime(behandlingstidspunkt, min(Måned, na.rm = TRUE), units = "days")) / 30,
    
    # Konverter rute til numerisk ID
    rute_numeric = as.numeric(factor(rute))
  )

table(Transportdata_combined_2$Måned_numeric < Transportdata_combined_2$behandlingstidspunkt_numeric)

att_gt_model_covid <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",
  idname  = "rute_numeric",
  gname   = "behandlingstidspunkt_numeric",
  data    = Transportdata_combined_2,  
  control_group = "notyettreated",
  panel = FALSE  # <- Hindrer at vi mister ubalanserte data
)

summary(att_gt_model_covid)




library(ggplot2)
library(broom)  # For å hente ATT-data strukturert

library(ggplot2)
library(broom)

# Konverter ATT-resultater til en dataframe
att_results <- tidy(att_gt_model_covid)

# Sjekk tilgjengelige kolonnenavn
colnames(att_results)

# Plott ATT over tid
plot_att <- ggplot(att_results, aes(x = time, y = estimate)) +  
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
  labs(title = "ATT (Privatiseringseffekt over tid)", 
       x = "Tid siden behandling", 
       y = "Estimert effekt (log passasjerer)") +
  theme_minimal()

print(plot_att)

sum(is.na(att_results$estimate))

head(att_results)

att_results_clean <- att_results %>% filter(!is.na(estimate))

plot_att <- ggplot(att_results_clean, aes(x = time, y = estimate)) +  
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
  labs(title = "ATT (Privatiseringseffekt over tid)", 
       x = "Tid siden behandling", 
       y = "Estimert effekt (log passasjerer)") +
  theme_minimal()

print(plot_att)

table(att_results$group)

unique(att_results$time)

table(Transportdata_combined_2$behandlingstidspunkt_numeric)

att_results$time <- round(att_results$time, 0)

summary(Transportdata_combined_2$behandlingstidspunkt_numeric)
unique(Transportdata_combined_2$behandlingstidspunkt_numeric)




library(lubridate)

library(lubridate)
set.seed(123)  # Sikrer reproduserbarhet

Transportdata_combined_2 <- Transportdata_combined_1 %>%
  mutate(
    covid = ifelse(Måned >= as.Date("2020-03-01") & Måned <= as.Date("2022-01-12"), 1, 0),
    
    # Konverter behandlingstidspunkt til Date først
    behandlingstidspunkt = as.Date(behandlingstidspunkt)
  ) %>%
  mutate(
    # Sikre at NA-verdi i behandlingstidspunkt blir en framtidig dato
    behandlingstidspunkt = case_when(
      is.na(behandlingstidspunkt) ~ as.Date(max(Måned, na.rm = TRUE)) %m+% 
        years(sample(3:5, 1)) %m+% months(sample(0:12, 1)),
      TRUE ~ behandlingstidspunkt
    )
  ) %>%
  mutate(
    # Konverter behandlingstidspunkt til antall måneder siden første observasjon
    behandlingstidspunkt_numeric = as.numeric(difftime(behandlingstidspunkt, 
                                                       min(Måned, na.rm = TRUE), 
                                                       units = "days")) / 30
  )


summary(Transportdata_combined_2$behandlingstidspunkt_numeric)
table(Transportdata_combined_2$behandlingstidspunkt_numeric)

att_gt_model_covid <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",
  idname  = "rute_numeric",
  gname   = "behandlingstidspunkt_numeric",
  data    = Transportdata_combined_2,  
  control_group = "notyettreated",
  panel = FALSE  
)
summary(att_gt_model_covid)

library(fixest)
did_model <- feols(log_passasjerer ~ treated * post | rute + Måned, 
                   data = Transportdata_combined_2)
summary(did_model)

ggplot(Transportdata_combined_2, aes(x = Måned, y = log_passasjerer, color = rute)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2019-12-15"), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.Date("2020-06-08"), linetype = "dashed", color = "blue") +
  theme_minimal()

Transportdata_filtered <- Transportdata_combined_2 %>%
  filter(Måned < as.Date("2020-03-01") | Måned > as.Date("2022-01-12"))

att_gt_model_no_covid <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",
  idname  = "rute_numeric",
  gname   = "behandlingstidspunkt_numeric",
  data    = Transportdata_filtered,  
  control_group = "notyettreated",
  panel = FALSE  
)

summary(att_gt_model_no_covid)

did_model <- feols(log_passasjerer ~ treated * post | Måned, 
                   data = Transportdata_combined_2, 
                   cluster = ~rute)
summary(did_model)

colnames(Transportdata_filtered)

Transportdata_filtered <- Transportdata_combined_2 %>%
  filter(Måned < as.Date("2020-03-01") | Måned > as.Date("2022-01-12")) %>%
  mutate(
    Måned_numeric = as.numeric(difftime(Måned, min(Måned, na.rm = TRUE), units = "days")) / 30,
    rute_numeric = as.numeric(factor(rute))
  )


att_gt_model_no_covid <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",
  idname  = "rute_numeric",
  gname   = "behandlingstidspunkt_numeric",
  data    = Transportdata_filtered,  
  control_group = "notyettreated",
  panel = FALSE  
)

sapply(Transportdata_filtered[, c("log_passasjerer", "Måned_numeric", 
                                  "rute_numeric", "behandlingstidspunkt_numeric")], 
       function(x) sum(is.na(x)))

print(colnames(Transportdata_filtered))

colnames(Transportdata_filtered) <- make.names(colnames(Transportdata_filtered))
table(Transportdata_filtered$behandlingstidspunkt_numeric)
table(Transportdata_filtered$rute_numeric)
Transportdata_filtered <- Transportdata_filtered %>%
  mutate(unique_id = row_number())

att_gt_model_no_covid <- att_gt(
  yname   = "log_passasjerer",
  tname   = "Måned_numeric",
  idname  = "rute_numeric",
  gname   = "behandlingstidspunkt_numeric",
  data    = Transportdata_filtered,  
  control_group = "notyettreated",
  panel = TRUE  
)












