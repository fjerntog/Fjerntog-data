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

# Merge alle datasettene horisontalt basert på "Måned"
Togdata_wide <- Reduce(function(x, y) merge(x, y, by = "Måned", all = TRUE),
                       list(`Passasjerkm Dovrebanen`, `Passasjerer Dovrebanen`, `SeteKm Dovrebanen`, `Fyllingsgrad Dovrebanen`,
                            `Passasjerkm Bergensbanen`, `Passasjerer Bergensbanen`, `SeteKm Bergensbanen`, `Fyllingsgrad Bergenbanen`,
                            `Passasjerkm Sørlandsbanen`, `Passasjerer Sørlandsbanen`, `SeteKm Sørlandsbanen`, `Fyllingsgrad Sørlandsbanen`))

# Se på de første radene
head(Togdata_wide)
