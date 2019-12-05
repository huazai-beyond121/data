
library(dplyr)
library(tidyr)
library(ggplot2)

source("factor.data.frame.R")
source("order.levels.R")

load("IAMdata.Rdata")
load("NTNU.LCA.coefficients.IAM.Rdata")
load("iLUC.Rdata")

source("transformation_list.R")

# ---- direct CO2 emissions [MtCO2/a] ----
direct.emissions <- IAMdata %>%
    filter(grepl(paste0('^Emissions\\|CO2\\|(Secondary )?Energy(\\|Supply)?',
                        '\\|Electricity\\|'), variable)) %>%
    extract(variable, 'technology', '.*\\|([^|]+\\|.+)$') %>%
    select(model, scenario, region, technology, period, value) %>%
    distinct(model, scenario, region, technology, period, value) %>%
    inner_join(
        inline.data.frame(
            'technology;        species',
            'Coal|w/o CCS;      direct fossil CO2 (non-CCS)',
            'Coal|w/ CCS;       direct fossil CO2 (CCS)',
            'Gas|w/o CCS;       direct fossil CO2 (non-CCS)',
            'Gas|w/ CCS;        direct fossil CO2 (CCS)',
            'Biomass|w/o CCS;   direct fossil CO2 (non-CCS)',
            'Biomass|w/ CCS;    BECCS'),
        'technology'
    )

# # #### upstream CH4 emissions [MtCO2eq/a] ######################################
# # CH4 emission factors from Remind
# tmp <- inner_join(
#     inner_join(
#         REMIND %>%
#             filter(grepl("^Emi\\|CH4\\|SE\\|Electricity\\|",
#                          variable)) %>%
#             extract(variable, "technology",
#                     "Emi\\|CH4\\|SE\\|Electricity\\|(.*)") %>%
#             select(-model, -unit, ch4.emi = value),
#
#         REMIND.SE.production %>%
#             filter(energy.carrier == "Electricity",
#                    !is.na(technology)) %>%
#             select(-model, -energy.carrier, -unit),
#
#         by = c("scenario", "region", "technology", "period")
#     ) %>%
#         # MtCO2eq/a / EJ/a = MtCO2eq/EJ
#         mutate(ch4.factor = ch4.emi / value) %>%
#         select(scenario, region, technology, period, ch4.factor),
#
#     TL$region,
#
#     by = c("region" = "REMIND")
# ) %>%
#     select(-region, region = NTNU) %>%
#     inner_join(
#         paste(
#             "scenario                       scen.new",
#             "ADV_WP5_Base               ADV_WP5_Base",
#             "ADV_WP5_P240_FullTech      ADV_WP5_P240_FullTech",
#             "ADV_WP5_P240_LimVRE        ADV_WP5_P240_LimVRE",
#             "ADV_WP5_P240_LimVRENucPO   ADV_WP5_P240_LimVRENucPO",
#             "ADV_WP5_P240_NoCCSNucPO    ADV_WP5_P240_NoCCSNucPO",
#             "ADV_WP5_Base               ADV_WP5_Base_dyn",
#             "ADV_WP5_P240_FullTech      ADV_WP5_P240_FullTech_dyn",
#             "ADV_WP5_P240_LimVRE        ADV_WP5_P240_LimVRE_dyn",
#             "ADV_WP5_P240_LimVRENucPO   ADV_WP5_P240_LimVRENucPO_dyn",
#             "ADV_WP5_P240_NoCCSNucPO    ADV_WP5_P240_NoCCSNucPO_dyn",
#             sep = "\n") %>%
#             textConnection() %>%
#             read.table(header = TRUE, strip.white = TRUE),
#
#         by = "scenario"
#     ) %>%
#     select(-scenario, scenario = scen.new) %>%
#     filter(!is.nan(ch4.factor))
#
# # use Gas factors for oil too
# tmp <- bind_rows(
#     tmp,
#
#     tmp %>%
#         filter(grepl('Gas', technology), !is.nan(ch4.factor)) %>%
#         mutate(technology = sub('Gas', 'Oil', technology))
# )
#
# ch4.emissions <- rbind(
#     inner_join(
#         tmp,
#
#         IAM.SE.production %>%
#             select(-unit),
#
#         by = c("scenario", "region", "technology", "period")
#     ) %>%
#         # MtCO2eq/EJ * EJ/a = MtCO2eq/a
#         mutate(value = ch4.factor * value,
#                species = "upstream CH4") %>%
#         # sum over regions, since AME and PAC are each mapped to two REMIND
#         # regions
#         group_by(model, scenario, region, technology, species, period) %>%
#         summarise(value = sum(value)) %>%
#         ungroup(),
#
#     IAM.Capacities %>%
#         filter(phase == "Capacity",
#                technology == "Hydro") %>%
#         # GW * kgCH4/MWa * 28 gCO2eq/gCH4 * 1e3 MW/GW * 1e-9 Mt/kg = MtCO2eq/a
#         mutate(value   = value * 10200 * 28 * 1e-6,
#                species = "upstream CH4") %>%
#         select(model, scenario, region, technology, period, value, species)
# )
#
# rm(tmp)

# #### life cycle CO2 emissions [MtCO2/a] ######################################

# fixed life cycle energy.use from Construction and End-of-Life
fixed.life.cycle.energy.use <- inner_join(
    NTNU.LCA.coefficients.IAM %>%
        filter(phase %in% c("Construction", "End-of-life"),
               !is.na(energy.carrier)) %>%
        group_by(scenario, region, period, technology, tech.variant,
                 energy.carrier) %>%
        summarise(specific.energy.use = sum(value)) %>%
        ungroup() %>%
        # use Gas factors for oil too
        calc_addVariable('`Oil|w/o CCS`' = '`Gas|w/o CCS`',
                         '`Oil|w/ CCS`' = '`Gas|w/ CCS`',
                         variable = technology,
                         value = specific.energy.use),

    IAM.Capacities %>%
        filter(phase == "Capacity Additions") %>%
        select(model, scenario, region, period, technology,
               new.capacity = value),

    by = c("scenario", "region", "period", "technology")
) %>%
    # GJ/MW * GW/a * 1e3 MW/GW = GJ/a
    mutate(energy.use = specific.energy.use * new.capacity * 1e3) %>%
    group_by(model, scenario, region, technology, tech.variant, period,
             energy.carrier) %>%
    summarise(energy.use = sum(energy.use)) %>%
    ungroup() %>%
    mutate(type = "Construction")

# variable life cycle energy use from Operation, based on capacity
variable.life.cycle.energy.use.capacity <- inner_join(
    NTNU.LCA.coefficients.IAM %>%
        filter(phase == "Operation",
               !is.na(energy.carrier),
               unit.denominator == "MW/yr") %>%
        group_by(scenario, region, period, technology, tech.variant,
                 energy.carrier) %>%
        summarise(specific.energy.use = sum(value)) %>%
        ungroup(),

    IAM.Capacities %>%
        filter(phase == "Capacity") %>%
        select(model, scenario, region, period, technology, capacity = value),

    by = c("scenario", "region", "period", "technology")
) %>%
    # GJ/MWa * GW * 1e3 MW/GW = GJ/a
    mutate(energy.use = specific.energy.use * capacity * 1e3) %>%
    group_by(model, scenario, region, technology, tech.variant, period,
             energy.carrier) %>%
    summarise(energy.use = sum(energy.use)) %>%
    ungroup()

# variable life cycle energy use from Operation, based on electricity production
variable.life.cycle.energy.use.production <- inner_join(
    NTNU.LCA.coefficients.IAM %>%
        filter(phase == "Operation",
               !is.na(energy.carrier),
               unit.denominator == "kWh") %>%
        group_by(scenario, region, period, technology, tech.variant,
                 energy.carrier) %>%
        summarise(specific.energy.use = sum(value)) %>%
        ungroup() %>%
        # use Gas factors for oil too
        calc_addVariable('`Oil|w/o CCS`' = '`Gas|w/o CCS`',
                         '`Oil|w/ CCS`' = '`Gas|w/ CCS`',
                         variable = technology,
                         value = specific.energy.use),

    IAM.SE.production %>%
        filter(energy.carrier == "Electricity",
               !is.na(technology)) %>%
        select(model, scenario, region, period, technology,
               production = value),

    by = c("scenario", "region", "period", "technology")
) %>%
    # GJ/kWh * EJ/a / (3.6e-12 EJ/kWh) = GJ/a
    mutate(energy.use = specific.energy.use * production / 3.6e-12) %>%
    group_by(model, scenario, region, technology, tech.variant, period,
             energy.carrier) %>%
    summarise(energy.use = sum(energy.use)) %>%
    ungroup()

# total life cycle energy use
total.life.cycle.energy.use <- rbind(
    fixed.life.cycle.energy.use,
    rbind(
        variable.life.cycle.energy.use.capacity,
        variable.life.cycle.energy.use.production
    ) %>%
        group_by(model, scenario, region, technology, tech.variant, period,
                 energy.carrier) %>%
        summarise(energy.use = sum(energy.use)) %>%
        ungroup() %>%
        mutate(type = "Operation")
) %>%
    select(model, scenario, region, technology, tech.variant,  period,
           energy.carrier, species = type, energy.use)

# cement
cement <- NTNU.LCA.coefficients.IAM %>%
    filter(quantity == "Cement",
           is.na(energy.carrier)) %>%
    group_by(scenario, region, technology, tech.variant, unit.denominator,
             period) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    # use Gas factors for oil too
    calc_addVariable('`Oil|w/o CCS`' = '`Gas|w/o CCS`',
                     '`Oil|w/ CCS`' = '`Gas|w/ CCS`',
                     variable = technology)

cement.emissions <- rbind(
    inner_join(
        IAM.Capacities %>%
            filter(phase == "Capacity Additions") %>%
            select(model, scenario, region, technology, period,
                   new.cap = value),

        cement %>%
            filter(unit.denominator == "MW") %>%
            select(-unit.denominator),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # GW/a * t Cement/MW * 1e3 MW/GW = t Cement/a
        mutate(value = value * new.cap * 1e3) %>%
        select(model, scenario, region, technology, tech.variant, period,
               value),

    inner_join(
        IAM.Capacities %>%
            filter(phase == "Capacity") %>%
            select(model, scenario, region, technology, period,
                   cap = value),

        cement %>%
            filter(unit.denominator == "MW/yr") %>%
            select(-unit.denominator),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # GW * t Cement/MW a * 1e3 MW/GW = t Cement/a
        mutate(value = value * cap * 1e3) %>%
        select(model, scenario, region, technology, tech.variant, period,
               value),

    inner_join(
        IAM.SE.production %>%
            filter(energy.carrier == "Electricity") %>%
            select(model, scenario, region, technology, period,
                   production = value),

        cement %>%
            filter(unit.denominator == "kWh") %>%
            select(-unit.denominator),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # EJ/a * t Cement/kWh * 3.6e12 kWh/EJ = t Cement/a
        mutate(value = value * production * 3.6e-12) %>%
        select(model, scenario, region, technology, tech.variant, period,
               value)
) %>%
    group_by(model, scenario, region, technology, tech.variant, period) %>%
    # t Cement * 0.49 tCO2/t Cement * 1e-6 Mt/t = MtCO2
    summarise(value = 0.49 * sum(value) * 1e-6) %>%
    ungroup() %>%
    mutate(species = "Cement")

cement.emissions <- rbind(
    cement.emissions,

    cement.emissions %>%
        group_by(model, scenario, technology, tech.variant, period, species) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(region = "World")
)

# ---- iLUC
iLUC.emissions <- inner_join(
    IAM.SE.production %>%
        filter(grepl("Biomass", technology)) %>%
        select(model, scenario, region, technology, period,
               production = value),

    iLUC %>%
        filter(variable == "iLUC",
               is.na(tech.variant),
               region == "World") %>%
        select(period, value) %>%
        mutate(value = ifelse(is.na(value), 0, value)),

    by = "period"
) %>%
    # EJ/a * MtCO2/EJ (PE) * 3 EJ (PE)/EJ (el) = MtCO2/a
    mutate(value = value * production * 3,
           species = "iLUC") %>%
    select(model, scenario, region, technology, species, period, value)

# ---- combine -----------------------------------------------------------------
all.emissions <- rbind(
    # emissions that already have technology variants
    cement.emissions,

    # emisisons that don't have technology variants
    rbind(
        direct.emissions,
        iLUC.emissions
    ) %>%
        # we'll add them
        inner_join(
            total.life.cycle.energy.use %>%
                distinct(technology, tech.variant),
            by = "technology")
)

# ---- add World region
all.emissions <- rbind(
    all.emissions %>%
        filter(region %in% levels(NTNU.LCA.coefficients.IAM$region)),

    all.emissions %>%
        filter(region %in% levels(NTNU.LCA.coefficients.IAM$region)) %>%
        group_by(model, scenario, technology, tech.variant, species,
                 period) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(region = "World")
) %>%
    factor.data.frame()

# ---- copy _dyn to _stat
all.emissions <- rbind(
    all.emissions %>%
        filter(grepl("_dyn", scenario)),
    all.emissions %>%
        filter(grepl("_dyn", scenario)) %>%
        mutate(scenario = sub("_dyn", "", scenario))
)

# ---- example plot ------------------------------------------------------------
all.emissions %>%
    filter(  (scenario == "ADV_WP5_Base_dyn" & period == 2010)
           | (scenario %in% c("ADV_WP5_Base_dyn", "ADV_WP5_P240_FullTech_dyn",
                              "ADV_WP5_P240_LimVRENucPO_dyn",
                              "ADV_WP5_P240_NoCCSNucPO_dyn") & period == 2050),
           region == "World",
           is.na(tech.variant)) %>%
    group_by(model, scenario, period, species) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() -> x

ggplot(x, aes(x = interaction(scenario, period), y = value, fill = species)) +
    geom_bar(data = x %>% filter(value >= 0), stat = "identity") +
    geom_bar(data = x %>% filter(value < 0),  stat = "identity") +
    facet_wrap(~ model, nrow = 1)
