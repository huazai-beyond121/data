

## Terrestrial Acidification

recipe.TerrAcid <- readxl::read_excel("data/ReCiPe111.xlsx", sheet = "AP",
                                      skip =  5)

names(recipe.TerrAcid)[names(recipe.TerrAcid) == "Substance name (ReCiPe)"] = "Substance"

recipe.TerrAcid <- recipe.TerrAcid[recipe.TerrAcid$Substance %in% c("Sulfur oxides", "Nitrogen dioxide") ,
                c("Substance","TAP100","TAP100 End EQ")]

names(recipe.TerrAcid)[names(recipe.TerrAcid) == "TAP100"] = "Terrestrial acidification#kg SO2eq"
names(recipe.TerrAcid)[names(recipe.TerrAcid) == "TAP100 End EQ" ] = "Ecosystem|Terrestrial acidification#species.yr"

recipe.TerrAcid$Substance = plyr::mapvalues(recipe.TerrAcid$Substance,
                                            from = c("Sulfur oxides", "Nitrogen dioxide")  ,
                                            to = c("Sulfur#kg SO2", "NOx#kg NO2"))

recipe.TerrAcid <- recipe.TerrAcid %>% separate(Substance, c("Substance", "unit"), sep = "#")

# convert to long format
recipe.TerrAcid <-  gather(recipe.TerrAcid, key = impact, value = value,   -Substance, -unit)



## Particulate matter formation

recipe.PMFP <- readxl::read_excel("data/ReCiPe111.xlsx", sheet = "PMFP",
                                  skip = 5)

names(recipe.PMFP)[names(recipe.PMFP) == "Substance name (ReCiPe)"] = "Substance"

recipe.PMFP <- recipe.PMFP[recipe.PMFP$Substance %in% c("Sulfur oxides", "Nitrogen dioxide", "BC", "OC" ) ,
                                   c("Substance","PMFP100","PMFP100 End HH")]

names(recipe.PMFP)[names(recipe.PMFP) == "PMFP100"] = "Particulate matter#kg PM10 eq"
names(recipe.PMFP)[names(recipe.PMFP) == "PMFP100 End HH" ] = "HumanHealth|Particulate matter#DALY"

recipe.PMFP$Substance = plyr::mapvalues(recipe.PMFP$Substance,
                                            from = c("Sulfur oxides", "Nitrogen dioxide", "BC", "OC"  )  ,
                                            to = c("Sulfur#kg SO2", "NOx#kg NO2","BC#kg BC", "OC#kg OC" ))
recipe.PMFP <- recipe.PMFP %>% separate(Substance, c("Substance", "unit"), sep = "#")

# convert to long format
recipe.PMFP <- gather(recipe.PMFP, key = impact, value = value,   -Substance, -unit)


## Particulate matter formation

recipe.POFP <- readxl::read_excel("data/ReCiPe111.xlsx", sheet = "POFP",
                                  skip = 5)

names(recipe.POFP)[names(recipe.POFP) == "Substance name (ReCiPe)"] = "Substance"

recipe.POFP <- recipe.POFP[recipe.POFP$Substance %in% c("Nitrogen dioxide", "NMVOC, non-methane volatile organic compounds, unspecified origin", "Carbon Monoxide" ) ,
                           c("Substance","POFP100","POFP100 End HH")]

names(recipe.POFP)[names(recipe.POFP) == "POFP100"] = "Oxidant formation#kg NMVOC-eq"
names(recipe.POFP)[names(recipe.POFP) == "POFP100 End HH" ] = "HumanHealth|Oxidant formation#DALY"

recipe.POFP$Substance = plyr::mapvalues(recipe.POFP$Substance,
                                        from = c("NMVOC, non-methane volatile organic compounds, unspecified origin" , "Nitrogen dioxide", "Carbon Monoxide")  ,
                                        to = c("NMVOC#kg NMVOC", "NOx#kg NO2", "CO#kg CO"))
recipe.POFP <- recipe.POFP %>% separate(Substance, c("Substance", "unit"), sep = "#")

# convert to long format
recipe.POFP <- gather(recipe.POFP, key = impact, value = value,   -Substance, -unit)


# bind to one dataframe
recipe.factors <- rbind(recipe.TerrAcid,recipe.POFP, recipe.PMFP) %>% factor.data.frame()
