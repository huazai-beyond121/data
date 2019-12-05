library(dplyr)
library(tidyr)
library(quitte)

load("NTNU.LCA.coefficients.IAM.Rdata")
load("IAMdata.Rdata")
load("IAM.storage.Rdata")

source("transformation_list.R")

# ---- combine Bio technologies across biomass scenarios -----------------------
NTNU.impacts.IAM %>%
    select(-model) %>%
    mutate(technology = ifelse(grepl("^Bio", technology),
                               sub("(^Bio.*),.*", "\\1", technology),
                               as.character(technology))) %>%
    group_by(scenario, region, technology, tech.variant, phase, impact, unit.numerator,
             unit.denominator, period) %>%
    summarise(value = mean(value)) %>%
       ungroup() %>% factor.data.frame() -> impacts

# ---- Use gas impacts as proxy for oil ----
impacts <- rbind(
    impacts,
    impacts %>%
        filter(technology == "Gas|w/ CCS" ) %>%
        mutate(technology = "Oil|w/ CCS"),
    impacts %>%
        filter(technology == "Gas|w/o CCS" ) %>%
        mutate(technology = "Oil|w/o CCS")
)

# ---- include End-of-life with Construction phase -----------------------------
impacts <- rbind(
    impacts %>%
        filter(phase == "Operation"),

    impacts %>%
        filter(phase != "Operation") %>%
        group_by(scenario, region, technology, tech.variant, impact,
                 unit.numerator, unit.denominator, period) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(phase = "Construction")
)

# ---- calculate absolute impacts ----
impacts.absolute <- rbind(
    # impacts from Construction
    inner_join(
        impacts %>%
            filter(phase == "Construction",
                   unit.denominator == 'MW'),

        IAM.Capacities %>%
            filter(phase == "Capacity Additions",
                   unit == 'GW/yr') %>%
            select(model, scenario, region, technology, period,
                   new.cap = value),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # x/MW * GW/a * 1000 MW/GW = x/a
        mutate(value = value * new.cap * 1000) %>%
        select(-new.cap, -unit.denominator, unit = unit.numerator),

    # impacts from Grid Construction
    inner_join(
        impacts %>%
            filter(phase == 'Construction',
                   unit.denominator == 'GWkm'),

        IAM.Capacities %>%
            filter(phase == 'Capacity Additions',
                   unit == 'TWkm/a') %>%
            select(model, scenario, region, technology, period,
                   new.cap = value),

        c('scenario', 'region', 'technology', 'period')
    ) %>%
        # x/GWkm * TWkm/a * 1000 GWkm/TWkm = x/a
        mutate(value = value * new.cap * 1e3) %>%
        select(-new.cap, -unit.denominator, unit = unit.numerator),

    # impacts from Storage Construction
    inner_join(
        impacts %>%
            filter(phase == 'Construction',
                   unit.denominator == 'MWh'),

        IAM.Capacities %>%
            filter(phase == 'Capacity Additions',
                   unit == 'GWh/a') %>%
            select(model, scenario, region, technology, period,
                   new.cap = value),

        c('scenario', 'region', 'technology', 'period')
    ) %>%
        # x/MWh * GWh/a * 1000 MWh/GWh = x/a
        mutate(value = value * new.cap * 1e3) %>%
        select(-new.cap, -unit.denominator, unit = unit.numerator),

    # impacts from Operation (electricity production)
    inner_join(
        impacts %>%
            filter(phase == "Operation",
                   unit.denominator == "kWh"),

        IAM.SE.production %>%
            filter(energy.carrier == "Electricity",
                   !is.na(technology)) %>%
            select(model,scenario, region, technology, period, elec.prod = value),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # x/kWh * EJ / (3.6e-12 EJ/kWh) = x
        mutate(value = value * elec.prod / 3.6e-12) %>%
        select(-elec.prod, -unit.denominator, unit = unit.numerator),

    # impacts from Operation (capacity)
    inner_join(
        impacts %>%
            filter(phase == "Operation",
                   unit.denominator == "MW/yr"),

        IAM.Capacities %>%
            filter(phase == "Capacity") %>%
            select(model, scenario, region, technology, period, cap = value),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # x/MW * GW * 1000 MW/GW = x
        mutate(value = value * cap * 1000) %>%
        select(-cap, -unit.denominator, unit = unit.numerator),

    # impacts from Grid Operation
    inner_join(
        impacts %>%
            filter(phase == "Operation",
                   unit.denominator == "GWkm"),

        IAM.Capacities %>%
            filter(phase == "Capacity") %>%
            select(model, scenario, region, technology, period, cap = value),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # x/GWkm * TWkm * 1e3 = x
        mutate(value = value * cap * 1000) %>%
        select(-cap, -unit.denominator, unit = unit.numerator),

    # impacts from Storage Operation
    inner_join(
        impacts %>%
            filter(phase == "Operation",
                   unit.denominator == "MWh"),

        IAM.storage.work %>%
            group_by(model, scenario, region, period) %>%
            # EJ/a / (3.6e-9 EJ/MWh) = MWh/a
            summarise(work = sum(value) / 3.6e-9) %>%
            ungroup() %>%
            mutate(technology = "Storage"),

        by = c("scenario", "region", "technology", "period")
    ) %>%
        # x/MWh * MWh/a = x/a
        mutate(value = value * work) %>%
        select(-work, -unit.denominator, unit = unit.numerator)
) %>%
    factor.data.frame()

# ---- additional land occupation for wind farm ----
dummy <- impacts %>%
    filter(impact == "Land occupation",
           technology == "Wind",
           tech.variant == 1 )

# data from NTNU_LCA_2016-04-23.csv lines
# "Additional land occupation if entire wind farm area is counted|
# Wind, onshore|Operation"
dummy.wind.farm <- rbind(
    dummy %>%
        mutate(tech.variant = NA) %>%
        mutate(value = 345000),
    dummy %>%
        mutate(tech.variant = "default") %>%
        mutate(value = 345000),
    dummy %>%
        mutate(tech.variant = "upper") %>%
        mutate(value = 413000),
    dummy %>%
        mutate(tech.variant = "lower") %>%
        mutate(value = 103000)
    )

impacts.wind.farm <- inner_join(
    dummy.wind.farm %>%
        filter(phase == "Operation",
               unit.denominator == "MW/yr") %>%
        character.data.frame(),

    IAM.Capacities %>%
        filter(phase == "Capacity") %>%
        select(model, scenario, region, technology, period, cap = value) %>%
        character.data.frame(),

    by = c("scenario", "region", "technology", "period")) %>%
    mutate(value = value * cap * 1000) %>%
    mutate(technology = "Wind farm") %>%
    select(-cap, -unit.denominator, unit = unit.numerator) %>%
    factor.data.frame()

impacts.absolute <- rbind(impacts.absolute, impacts.wind.farm)

rm(dummy, dummy.wind.farm, impacts.wind.farm)


# ---- rename scenario names to simpler version ----
levels(impacts.absolute$scenario) <-  plyr::mapvalues(
    levels(impacts.absolute$scenario),
    as.character(TL$scenario.advance$ADVANCE_dyn),
    to = as.character(TL$scenario.advance$legend ))

# ---- water impacts absolute ----
water.absolute <- rbind(
    IAM.water %>%
        filter(scenario %in% as.character(TL$scenario.advance$ADVANCE_stat),
               region %in%  levels(impacts.absolute$region)) %>%
        mutate(tech.variant = 1),

    IAM.water %>%
        filter(scenario %in% as.character(TL$scenario.advance$ADVANCE_dyn),
               region %in%  levels(impacts.absolute$region)) %>%
        mutate(tech.variant = 2))  %>%
    droplevels()

levels(water.absolute$scenario) <-  plyr::mapvalues(
    levels(water.absolute$scenario),
    as.character(TL$scenario.advance$ADVANCE_stat),
    to = as.character(TL$scenario.advance$legend ))

levels(water.absolute$scenario) <-  plyr::mapvalues(
    levels(water.absolute$scenario),
    as.character(TL$scenario.advance$ADVANCE_dyn),
    to = as.character(TL$scenario.advance$legend ))


# ---- remove water withdrawal for hydropower ----
# (definitional issue with POLES)
water.absolute <- water.absolute %>%
    filter(!(technology == "Hydro" & impact == "Water Withdrawal"))

# Assume that hydro withdrawal equals consumption
water.absolute <- rbind(
    water.absolute,

    water.absolute %>%
        filter(technology == "Hydro", impact == "Water Consumption") %>%
        mutate(impact = "Water Withdrawal") %>%
        factor.data.frame()
)

# add tech variant "NA"
water.absolute <- rbind(
    water.absolute,

    water.absolute %>%
        group_by(scenario, region, impact, technology, unit, period, model,
                 phase) %>%
        summarise(value = mean(value)) %>%
        ungroup() %>%
        mutate(tech.variant = NA)
)

# ---- Biomass water withdrawal ----
MAgPIE <- read.quitte("MAgPIE_Fertilizer_Inputs/lca_gunnar_tc_exo.mif",
                      convert.periods = FALSE) %>%
    tbl_df()

tmp <- MAgPIE %>%
    filter(variable %in% c("Water|Withdrawal|Irrigation",
                           "Primary Energy|Biomass")) %>%
    extract(scenario,
            c("scen.biomass.amount", "scen.tax", "scen.biomass.type"),
            "B([0-9]+)_TAX([0-9]+)_(.*)") %>%
    select(-model, -unit)

biomass.irr <- inner_join(
    tmp %>%
        filter(scen.biomass.amount == 0) %>%
        select(-scen.biomass.amount, base = value),

    tmp %>% filter(scen.biomass.amount != 0),

    by = c("scen.tax", "scen.biomass.type", "region", "variable", "period")
) %>%
    mutate(value = value - base) %>%
    select(-base) %>%
    spread(variable, value) %>%
    mutate(
        withdrawal = `Water|Withdrawal|Irrigation`,
        pe         = `Primary Energy|Biomass`,
        value      = ifelse(pe == 0, 0, withdrawal / pe),
        unit       = "million m3/EJ") %>%
    select(scen.biomass.amount, scen.tax, scen.biomass.type, region, period,
           unit, value)

biomass.irr <- biomass.irr %>%
    inner_join(
        paste(
            "region;   NTNU.region",
            "AFR;      AME",
            "PAS;      AS",
            "CPA;      CN",
            "FSU;      EIT",
            "SAS;      IN",
            "LAM;      LA",
            "PAC;      PAC",
            "RER;      RER",
            "NAM;      US",
            sep = "\n") %>%
            textConnection() %>%
            read.table(header = TRUE, sep = ";", strip.white = TRUE),

        by = "region"
    ) %>%
    select(-region, region = NTNU.region, -unit, withdrawal = value) %>%
    inner_join(
        IAM.SE.production %>%
            filter(grepl("^Bio", technology),
                   scenario %in% as.character(TL$scenario.advance$ADVANCE_stat)
                ) %>%
            factor.data.frame() %>%
            select(-unit, production = value),

        by = c("region", "period")
    ) %>%
    select(model, scenario, region, technology, period, scen.biomass.amount,
           scen.tax, scen.biomass.type, withdrawal, production) %>%
    # EJ_el/a * million m3/EJ_PE * 1e-3 km3/million m3 * 3 EJ_el/EJ_PE = km3/a
    mutate(value = production * withdrawal * 3e-3) %>%
    group_by(model, scenario, region, technology, scen.biomass.type,
             period) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>%
    group_by(model, scenario, region, scen.biomass.type,
             period) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    rename(tech.variant = scen.biomass.type) %>%
    mutate(phase = "Operation",
           impact = "Water Withdrawal",
           technology = "Biomass irrigation",
           unit = "km3/yr")

# add tech variant "NA"
biomass.irr <- rbind(
    biomass.irr,

    biomass.irr %>%
        group_by(model, scenario, region, technology, period, impact, phase,
                 unit) %>%
        summarize(value = mean(value)) %>%
        ungroup() %>%
        mutate(tech.variant = NA)
)

levels(biomass.irr$scenario) <-  plyr::mapvalues(
    levels(biomass.irr$scenario),
    as.character(TL$scenario.advance$ADVANCE_stat),
    to = as.character(TL$scenario.advance$legend))

water.absolute <- rbind(
    water.absolute,
    biomass.irr)

water.absolute$tech.variant <- factor(water.absolute$tech.variant)

# ---- calculate absolute GHG emissions (all.emissions data frame) ----
source("calc_absolute_emissions_IAM.R")

all.emissions %>%
    filter(species != "BECCS",
           scenario %in% as.character(TL$scenario.advance$ADVANCE_stat)) %>%
    group_by(model, scenario, region, technology, tech.variant, period) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(impact = "GHG (excl. CDR)",
           phase = "Operation",
           unit = "Mt CO2e") %>%
    factor.data.frame() -> ghg.absolute

levels(ghg.absolute$scenario) <-  plyr::mapvalues(
    levels(ghg.absolute$scenario),
    as.character(TL$scenario.advance$ADVANCE_stat),
    to = as.character(TL$scenario.advance$legend))

# ---- Air pollutants ----
list_pollutants = c("Sulfur", "NOx", "BC", "OC", "CO")
# list_pollutants_indirect = c("indirect Sulfur", "indirect NOx","indirect BC",
#                              "indirect OC", "indirect CO")

# direct AP emissions
emi.ap.direct <-  IAM.Emi %>%
    filter(scenario %in% as.character(TL$scenario.advance$ADVANCE_stat),
           region %in%  levels(impacts.absolute$region),
           impact %in% list_pollutants) %>%
  mutate(impact = paste("direct", as.character(impact)))

levels(emi.ap.direct$scenario) <-  plyr::mapvalues(levels(emi.ap.direct$scenario),
      as.character(TL$scenario.advance$ADVANCE_stat),
       to = as.character(TL$scenario.advance$legend))

source("calc_IAM_indirect_airpollution.R")

# expand AP impacts to non-NA tech.variants
emi.ap.direct <- inner_join(
    emi.ap.direct,
    emi.ap.indirect %>%
      mutate(impact = sub("indirect ", "direct ", as.character(impact))) %>%
                select(impact, technology, tech.variant) %>%
                distinct(),

            by = c("impact", "technology")
        )

# calculate total air pollutants as sum of direct and indirect
# (emi.ap.absolute contains direct, indirect, and totals)
emi.ap.absolute <- bind_rows(
    emi.ap.direct %>%
        mutate(impact = sub("direct ", "", as.character(impact))),

    emi.ap.indirect %>%
        mutate(impact = sub("indirect ", "", as.character(impact))),

    emi.ap.direct,
    emi.ap.indirect
) %>%
    filter(impact %in% paste0(rep(c('direct ', 'indirect ', ''),
                                  length(list_pollutants)),
                              list_pollutants)) %>%
    group_by(model, scenario, region, technology, tech.variant, phase, impact,
             unit, period) %>%
    summarise(value = sum(value)) %>%
    ungroup()

# calculate air pollution-related midpoints
source("read_recipe.R")

impacts.ap.abs <- inner_join(
    recipe.factors %>%
        separate(impact, c('impact', 'unit.impact'), sep = '#') %>%
        expand(nesting(Substance, unit, impact, unit.impact, value),
               vector = c('direct', 'indirect', '')) %>%
        mutate(Substance = sub('^ ', '', paste(vector, Substance)),
               impact = sub('\\|#', '#',
                            paste0(impact, '|', vector, '#', unit.impact))) %>%
        select(Substance, unit, impact, value),

    emi.ap.absolute,

    c('Substance' = 'impact')
) %>%
    # multiply emissions with characterization factor, convert units from Mt to
    # kg
    mutate(value = value.x * value.y * 1e9) %>%
    select(-value.x, -value.y, -unit.x, -unit.y) %>%
    group_by_(.dots = setdiff(colnames(.), c("Substance", "value"))) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    separate(impact, c("impact", "unit"), sep = "#") %>%
    factor.data.frame()

# rename pre-exsiting ap-related midpoints from NTNU data set in impacts.
mp.renaming.list <- inline.data.frame(
        'old;                                                new',
        'HumanHealth|Particulate matter formation;           HumanHealth_|Particulate matter formation',
        'Particulate matter formation;                       Particulate matter formation_',
        'HumanHealth|Photochemical oxidant formation;        HumanHealth_|Photochemical oxidant formation',
        'HumanHealth|Climate change;                         HumanHealth_|Climate change',
        'HumanHealth|Ozone depletion;                        HumanHealth_|Ozone depletion',
        'Photochemical oxidant formation;                    Photochemical oxidant formation_',
       # 'Ecosystem|Natural land transformation;               Ecosystem_|Natural land transformation',
        'Ecosystem|Climate change;                           Ecosystem_|Climate change',
        'Ecosystem|Agricultural and urban land occupation;   Ecosystem|Agricultural and urban land occupation',
        'Ecosystem|Terrestrial acidification;                Ecosystem_|Terrestrial acidification',
        'Terrestrial acidification;                          Terrestrial acidification_')

levels(impacts.absolute$impact) <- plyr::mapvalues(
    levels(impacts.absolute$impact),
    from = mp.renaming.list$old,
    to = mp.renaming.list$new)

# ---- CCS storage ----
source('calc_CCS_storage.R')

CCS.storage <- CCS.storage %>%
    filter(region %in% levels(impacts.absolute$region))

# --- calculate aggregate ecotoxicity indicator ----
impacts.abs.ecotox <- impacts.absolute %>%
    filter(grepl("Ecosystem\\|.+ ecotoxicity$", impact)) %>%
    group_by_(.dots = setdiff(colnames(impacts.absolute),
                              c("impact", "value"))) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(impact = "Ecotoxicity") %>%
    factor.data.frame()

# ---- add fossil depetion ----
source('calc_fossil_depletion.R')

fossil.depletion <- fossil.depletion %>%
    filter(region %in% levels(impacts.absolute$region)) %>%
    inner_join(
        TL$scenario.advance %>%
            select(scenario = ADVANCE_dyn, legend),
        'scenario'
    ) %>%
    select(-scenario, scenario = legend) %>%
    mutate(impact = paste0('Fossil depletion|', energy.carrier)) %>%
    # sum over energy carriers
    group_by(model, scenario, region, technology, tech.variant, phase, unit,
             impact, period) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    sum_total(impact, name = 'Fossil depletion')

resource.fossil.depletion <- inner_join(
    fossil.depletion %>%
        # do not double-count total fossil depletion
        filter('Fossil depletion' != impact) %>%
        select(model, scenario, region, technology, tech.variant, phase, period,
               unit, fossil.depletion = value) %>%
        character.data.frame(),

    impacts %>%
        filter(impact == 'Resource|Fossil depletion') %>%
        character.data.frame() %>%
        inner_join(
            TL$scenario.advance %>%
                select(ADVANCE_dyn, legend) %>%
                character.data.frame(),
            c('scenario' = 'ADVANCE_dyn')
        ) %>%
        select(-scenario, scenario = legend),

    c('scenario', 'region', 'technology', 'tech.variant', 'phase', 'period',
      'unit' = 'unit.denominator')
) %>%
    # $/kg oil eq * kg oil eq = $
    mutate(value = value * fossil.depletion) %>%
    select(-unit) %>%
    select(model, scenario, region, technology, tech.variant, phase,
           impact, unit = unit.numerator, period, value)

# ---- combine different impacts into one dataframe ----

rm(IAMdata, specific.air.pollutant.emissions, NTNU.impacts.IAM,
   indirect.fossil.depletion, emi.ap.direct, impacts, NTNU.LCA.coefficients.IAM,
   REMIND, all.emissions, total.life.cycle.energy.use, emi.ap.indirect, IAM.Emi,
   fixed.life.cycle.energy.use)
gc(verbose = TRUE)

impacts.absolute.IAM <- bind_rows(
    impacts.absolute %>% filter(value != 0),
    water.absolute %>% filter(value != 0),
    emi.ap.absolute %>% filter(value != 0),
    impacts.ap.abs %>% filter(value != 0),
    # ghg.absolute %>% filter(value != 0),
    impacts.abs.ecotox %>% filter(value != 0),
    CCS.storage %>% filter(value != 0),
    fossil.depletion %>% filter(value != 0),
    resource.fossil.depletion %>% filter(value != 0)
) %>%
    mutate(impact = ifelse(as.character(impact) == "GHG", "GHG (excl. CDR)",
                           as.character(impact)))

# clear some memory
rm(impacts.absolute, water.absolute, emi.ap.absolute, ghg.absolute,
   foss.dep.absolute, impacts.abs.ecotox, CCS.storage,
   indirect.fossil.depletion, indirect.fossil.depletion.construction,
   indirect.fossil.depletion.operation.capacity,
   indirect.fossil.depletion.operation.production,
   specific.air.pollutant.emissions, emi.ap.indirect,emi.ap.direct, impacts,
   fossil.depletion, direct.fossil.depletion, all.emissions, REMIND)
gc(verbose = TRUE)

# ---- calculate global totals ----
impacts.absolute.IAM <- rbind(
  impacts.absolute.IAM %>%
    filter(region != "World"),

  impacts.absolute.IAM %>%
    filter(region != "World") %>%
    group_by(model, scenario, technology, tech.variant, phase, impact, unit,
             period) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(region = "World")
) %>%
  factor.data.frame()

# ---- calculate aggregate endpoint results ----
impacts.endpoint <- impacts.absolute.IAM %>%
    filter(grepl('^(Ecosystem|HumanHealth|Resource)\\|[^\\|]+$', impact)) %>%
    separate( impact, c("endpoint", "midpoint"), sep = "\\|") %>%
    factor.data.frame()

impacts.ep.bytech <- impacts.endpoint %>%
    group_by_(.dots = setdiff(colnames(impacts.endpoint),
                              c("midpoint", "value"))) %>%
    summarise(value = sum(value)) %>%
    ungroup()

impacts.ep.by.mp <- impacts.endpoint %>%
    filter(is.na(tech.variant)) %>%
    group_by_(.dots = setdiff(colnames(impacts.endpoint),
                              c("technology", "value"))) %>%
    summarise(value = sum(value)) %>%
    ungroup()

impacts.absolute.IAM <- rbind(
    impacts.absolute.IAM,
    impacts.ep.bytech %>%
        rename(impact = endpoint)
)

# ---- save absolute impacts ----
save(impacts.absolute.IAM, file = "ADVANCE.Impacts.Absolute.Rdata")
