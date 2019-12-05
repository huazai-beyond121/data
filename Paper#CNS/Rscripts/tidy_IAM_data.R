#!/usr/bin/Rscript

# #### Include libraries and scripts ###########################################
library(dplyr)
library(tidyr)
library(quitte)

source("read_quitte.R")
source("quitte_interpolate_periods.R")
# source("factor.data.frame.R")
source("technology_vintage_helper.R")

source("transformation_list.R")

# #### Load IAM data ########################################################
read.quitte(mifall, sep = ",", quote = "\"", convert.periods = FALSE) %>%
    tbl_df() -> IAMdata

# #### rename models ##########################################################
IAMdata %>%
    left_join(TL$model.names, by = c("model" = "full")) %>%
    select(-model) %>%
    rename( model=short) %>%
    filter(model %in% c("GCAM", "IMAGE", "MESSAGE", "POLES", "REMIND")) ->  IAMdata

# write scenario data to file
IAMdata %>% select(model, scenario, region, variable, unit, period, value) %>%
    filter(!(region %in% c("GROUP_A","GROUP_B")) ,period %in% seq(2010,2050,10),scenario %in% TL$scenario.advance$ADVANCE_dyn[1:4]) %>%
    spread(key = period, value = value)  -> temp


temp$scenario = plyr::mapvalues(temp$scenario, from = as.character(TL$scenario.advance$ADVANCE_dyn[1:4]), to = c("Base", "FullTech", "Conv", "NewRE"))


openxlsx::write.xlsx(temp, file = "output/IAM_ScenarioData.xlsx")



# duplicate "stat" as "_dyn" scenarios for MESSAGE
if (0 == ( IAMdata %>%
           select(model, scenario) %>%
           distinct() %>%
           filter(model == "MESSAGE", grepl("_dyn", scenario)) %>%
           nrow() )) {
    IAMdata <- rbind(
        IAMdata %>%
            filter(model == "MESSAGE") %>%
            mutate(scenario = sub("$", "_dyn", scenario)),

        IAMdata
    )
}


# ### convert RCP + major economies to NTNU regions for GCAM, WITCH
rbind(
    IAMdata,
    IAMdata %>%
        filter(model %in% c("GCAM", "WITCH")) %>%
        spread(key = "region", value = "value") %>%
        mutate(AS = ASIA - China - India) %>%
        mutate(PAC = OECD90 - EU - USA) %>%
        mutate(IN = India) %>%
        mutate(CN = China) %>%
        mutate(EIT = REF) %>%
        mutate(LA = LAM) %>%
        mutate(RER = EU) %>%
        mutate(AME = MAF) %>%
        mutate(US = USA) %>%
        mutate(RER = EU) %>%
        gather(region, value, -model, -scenario, -variable, -period, -unit,
               na.rm = TRUE) %>%
        tbl_df(),

    IAMdata %>%
        filter(model == "POLES") %>%
        spread(key = "region", value = "value") %>%
        mutate(AS = ASIA - China - India) %>%
        mutate(AME = MAF) %>%
        gather(region, value, -model, -scenario, -variable, -period, -unit,
               na.rm = TRUE) %>%
        tbl_df()
) %>%
    distinct() -> IAMdata

# #### Split IAM data set into sub sets #####################################
IAM.technologies <- c("Coal|w/o CCS",  "Coal|w/ CCS",
                      'Oil|w/o CCS', 'Oil|w/ CCS',
                      "Gas|w/o CCS", "Gas|w/ CCS",
                      "Nuclear", "Hydro",
                      "Biomass|w/o CCS", "Biomass|w/ CCS",
                      "Geothermal",
                      "Wind", "Wind|Onshore", "Wind|Offshore",
                      "Solar|CSP", "Solar|PV")


# SE production
IAM.SE.production <- IAMdata %>%
    filter(grepl("^Secondary Energy\\|(Solids|Liquids|Gases|Electricity.*)$",
                 variable)) %>%
    extract(variable, c("energy.carrier", "technology"),
            "Secondary Energy\\|([^|]+)\\|?(.*)") %>%
    mutate(technology = ifelse(technology == "", NA, technology)) %>%
    filter(technology %in% IAM.technologies |
               is.na(technology)) %>%
    factor.data.frame()

# add missing wind on/offshore numbers
IAM.SE.production <- rbind(
    IAM.SE.production %>%
        filter(!grepl("Wind", technology)),

    IAM.SE.production %>%
        filter(grepl("Wind", technology)) %>%
        spread(technology, value, fill = 0) %>%
        mutate(`Wind|Onshore` = ifelse(`Wind|Offshore` == 0,
                                       Wind,
                                       `Wind|Onshore`)) %>%
        gather(technology, value, Wind, `Wind|Onshore`, `Wind|Offshore`)
)

technology_vintage_helper(
    IAM.SE.production,
    # duplicate technology life times from Remind
    TL$lifetime %>%
        mutate(GCAM = REMIND,
               IMAGE = REMIND,
               MESSAGE = REMIND,
               POLES = REMIND,
               WITCH = REMIND)) -> technology.vintage.helper
save(technology.vintage.helper,
     file = "technology.vintage.helper.IAM.Rdata")

# ---- Capacities ----
IAM.Capacities <- IAMdata %>%
    filter(grepl("^(Capacity|Capacity Additions)\\|Electricity\\|",
                 variable)) %>%
    separate(variable, c("phase", "drop", "technology"), sep = "\\|",
             extra = "merge") %>%
    select(-drop) %>%
    filter(technology %in% IAM.technologies)

# calculate missing hydropower capacity additions for GCAM
IAM.Capacities <- IAM.Capacities %>%
    filter(model == 'GCAM',
           technology == 'Hydro',
           phase == 'Capacity') %>%
    arrange(model, scenario, region, period) %>%
    group_by(model, scenario, region) %>%
    mutate(value = (value - lag(value)) / (period - lag(period))) %>%
    ungroup() %>%
    mutate(phase = 'Capacity Additions',
           unit = 'GW/yr') %>%
    overwrite(IAM.Capacities) %>%
    factor.data.frame()


# add missiing wind on/offshore numbers
IAM.Capacities <- rbind(
    IAM.Capacities %>%
        filter(!grepl("Wind", technology)),

    IAM.Capacities %>%
        filter(grepl("Wind", technology)) %>%
        spread(technology, value, fill = 0) %>%
        mutate(`Wind|Onshore` = ifelse(`Wind|Offshore` == 0,
                                       Wind,
                                       `Wind|Onshore`)) %>%
        gather(technology, value, Wind, `Wind|Onshore`, `Wind|Offshore`)
)

# add grid and storage
source("calc_storage_grid_IAM.R")

IAM.Capacities <- rbind(
    IAM.Capacities,

    rbind(
        IAM.storage.capacity %>%
            mutate(phase = ifelse(phase == "Cap", "Capacity",
                                  "Capacity Additions")) %>%
            group_by(model, scenario, region, phase, unit, period) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            mutate(technology = "Storage"),

        IAM.grid.capacity %>%
            inner_join(
                data.frame(phase = c("Cap", "New Cap"),
                           new.phase = c("Capacity", "Capacity Additions")),
                by = "phase") %>%
            select(-phase) %>%
            rename(phase = new.phase) %>%
            mutate(technology = sub("$", " Grid", technology)) %>%
            group_by(model, scenario, region, technology, phase, unit,
                     period) %>%
            summarise(value = sum(value)) %>%
            ungroup()
    )
) %>%
    distinct(model, scenario, region, technology, phase, unit, period, value)




# #### Calculate empiric load factors ##########################################
# This data frame tells you what fraction of the year a technology was running
# with full load in a period
inner_join(
    IAM.Capacities %>%
        filter(phase == "Capacity") %>%
        select(-phase, -unit) %>%
        rename(capacity = value),

    IAM.SE.production %>%
        filter(energy.carrier == "Electricity", !is.na(technology)) %>%
        select(-energy.carrier, -unit) %>%
        rename(production = value),

    by = c("model", "scenario", "region", "technology", "period")
) %>%
    # EJ / (GW * [EJ/GW])
    mutate(value = production / (capacity * 3.153672e-2),
           unit = "h/h") %>%
    select(-capacity, -production) %>%
    arrange(model, scenario, region, technology, period) %>%
    tbl_df() -> IAM.load.factor

# Fix too high full load hours
inner_join(
    IAM.load.factor,

    TL$load.factor,

    by = "technology"
) %>%
    mutate(value = ifelse(value > 1, nu, value)) %>%
    select(model, scenario, region, technology, unit, period, value) %>%
    factor.data.frame() -> IAM.load.factor

# #### Calculate empric average load factors over lifetime ####################
# technology.vintage.helper based on REMIND. May want to make it
# model-specific in the future

inner_join(
   select(technology.vintage.helper, -model),
    IAM.load.factor,
    by = c( "technology", "period")
) %>%
    select(model, scenario, region, technology, unit, lifetime, base.year,
           period, value) %>%
    filter(!is.na(value)) %>%
    group_by(model, scenario, region, technology, unit, lifetime, base.year) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>%
    factor.data.frame() -> IAM.average.load.factor

# #### Calculate specific life-time energy production ##########################
# This data frame tells you how much electricity a GW of a technology built in
# period will produce over its life time, give a scenario
# technology.vintage.helper based on REMIND. May want to make it
# model-specific in the future

inner_join(
    select(technology.vintage.helper, -model),
    inner_join(
        IAM.load.factor,
        TL$load.factor,
        by = "technology") %>%
        mutate(value = ifelse(value > 1, nu, value)) %>%
        select(-nu),
    by = c( "technology", "period")
) %>%
    select(model, scenario, region, technology, base.year, period, lifetime,
           value) %>%
    group_by(model, scenario, region, technology, base.year, lifetime) %>%
    summarise(nu = mean(value)) %>%
    ungroup() %>%
    rename(period = base.year) %>%
    # a * h/h * 3.1536e7 s/a * 1 J/Ws * 1e-18 EJ/J * 1e9 W/GW = EJ/GW
    mutate(value = lifetime * nu * 3.1536e-2,
           unit = "EJ/GW") %>%
    select(model, scenario, region, technology, unit, period, value) %>%
    filter(!is.na(value)) %>%
    arrange(model, scenario, region, technology, unit, period) %>%
    tbl_df() -> IAM.specific.lifetime.electricity.production


#### water consumption

IAM.water <- IAMdata %>%
   filter(grepl("^(Water Withdrawal|Water Consumption)\\|Electricity\\|", variable)) %>%
   separate(variable, c("impact", "drop", "technology"), sep = "\\|",
            extra = "merge") %>%
   select(-drop) %>%
   mutate(phase = "Operation") %>%
   filter(technology %in% IAM.technologies) %>%
   factor.data.frame()





IAM.Emi <-   IAMdata %>%
        filter(grepl("^Emissions\\|(.+)\\|Energy\\|Supply\\|Electricity\\|", variable)) %>%
             extract(variable, c("species", "technology"),
                     "^Emissions\\|(.+)\\|Energy\\|Supply\\|Electricity\\|(.+)" ) %>%
        rename(impact = species) %>%
        mutate(phase = "Operation") %>%
        filter(technology %in% IAM.technologies) %>%
        factor.data.frame()






# #### Store sub sets ##########################################################
save(IAMdata,
     IAM.SE.production,
     IAM.Capacities,
     IAM.water,
     IAM.Emi,
     IAM.load.factor,
     IAM.average.load.factor,
     IAM.specific.lifetime.electricity.production,
     file = "IAMdata.Rdata")

# REMIND.direct.emissions,
# REMIND.direct.emission.factors,
# REMIND.Emissions,
# REMIND.Emissions.fossil,
# REMIND.CO2.intensities,

