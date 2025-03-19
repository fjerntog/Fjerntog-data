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

