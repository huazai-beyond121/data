# THEMIS LCA impact coefficients

# write LCA coefficients to xlsx-file
temp <- NTNU.impacts.IAM %>% filter(period %in% c(2010,2030,2050),
                            scenario %in% c("ADV_WP5_Base_dyn", "ADV_WP5_P240_FullTech_dyn"),
                            !(impact %in% c("Ozone depletion",
                                            "Particulate matter formation",
                                            "Photochemical oxidant formation",
                                            "Terrestrial acidification",
                                            "Terrestrial ecotoxicity",
                                            "HumanHealth|Ozone depletion",
                                            "HumanHealth|Photochemical oxidant formation",
                                            "HumanHealth|Particulate matter formation",
                                            "HumanHealth|Cimate Change",
                                            "Ecosystem|Terrestrial acidification",
                                            "Ecosystem|Cimate Change",
                                            "Ecosystem|Terrestrial acidification",
                                            "Resource|Fossil depletion",
                                            "GHG",
                                            "Deforestation")) ) %>% mutate(model = "THEMIS") %>% spread(key = period, value = value)

temp$scenario = plyr::revalue(temp$scenario, c("ADV_WP5_Base_dyn" = "Baseline", "ADV_WP5_P240_FullTech_dyn" = "Generic Climate Policy") )

openxlsx::write.xlsx(temp, file = "output/THEMIS_perUnit_Coefficients.xlsx")

rm(temp)


# write scenarioimpacts to file

impacts.absolute.IAM %>% filter(region == "World", period %in% c(2010,2020, 2030, 2040, 2050), scenario != "LimVRENucPO",
                                !(technology %in% c("Base Grid", "VRE Grid", "Storage", "Biomass irrigation"))) %>%
    select(model, scenario, region, technology, tech.variant, phase, impact,  unit, period, value) %>%
    spread(key = period, value = value, fill = 0) %>% factor.data.frame() -> temp



temp$scenario = plyr::revalue(temp$scenario, c("NoCCSNucPO" = "NewRE", "LimVRE" = "Conv") )

openxlsx::write.xlsx(temp, file = "output/IAM_ScenarioImpacts.xlsx")

rm(temp)
