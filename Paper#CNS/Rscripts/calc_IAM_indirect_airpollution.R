
library(quitte)
library(dplyr)
library(tidyr)
library(zoo)

load("EmiFactorsGAINS.RData")
load("NTNU.LCA.coefficients.IAM.Rdata")
load("IAMdata.Rdata")

# ---- GAINS emission factors ----
# emission factors for cement, steel, non-electricity transport, and
# non-electricity energy use
GAINS.factors <- all_diff %>%
    filter(scenario == "CLE",
           between(year, 2010, 2050),
           sector %in% c("CEMENT", "STEEL", "End_Use_Transport_Coal",
                         "End_Use_Transport_HLF", "End_Use_Transport_NatGas")
    ) %>%
    mutate(variable = ifelse(variable == "SO2", "Sulfur",
                             as.character(variable))) %>%
    select(region, sector, species = variable, period = year, EF_ECLIPSE) %>%
    # map to NTNU activities
    inner_join(
        inline.data.frame(c(
            "quantity;                   energy.carrier;   sector",
            "Cement;                     NA;               CEMENT",
            "Iron;                       NA;               STEEL",
            "Freight transportation;     Solids;           End_Use_Transport_Coal",
            "Freight transportation;     Liquids;          End_Use_Transport_HLF",
            "Freight transportation;     Gases;            End_Use_Transport_NatGas",
            "Energy;                     Solids;           End_Use_Industry_Coal",
            "Energy;                     Liquids;          End_Use_Industry_HLF",
            "Energy;                     Gases;            End_Use_Industry_NatGas")
        ),
        by = "sector"
    ) %>%
    # map to ADVANCE regions
    inner_join(
        inline.data.frame(c(
            "region.GAINS;         region.ADVANCE",
            "Asia-Stan;            AS",
            "Southeast Asia;       PAC",
            "India;                IN",
            "China+;               CN",
            "Central Europe;       EIT",
            "Rest South America;   LA",
            "Western Europe;       RER",
            "Middle East;          AME",
            "USA;                  US")),

        by = c("region" = "region.GAINS")
    ) %>%
    select(-region, region = region.ADVANCE) %>%
    # fake 2050 timestep
    group_by(species, quantity, energy.carrier, region, sector) %>%
    complete(period = seq(2010, 2050, by = 5)) %>%
    group_by(species, quantity, energy.carrier, region, sector) %>%
    mutate(
        EF_ECLIPSE = na.approx(EF_ECLIPSE,
                               yleft = head(EF_ECLIPSE[!is.na(EF_ECLIPSE)], 1),
                               yright = tail(EF_ECLIPSE[!is.na(EF_ECLIPSE)], 1))
    ) %>%
    ungroup() %>%
    select(-sector) %>%
    # kt/PJ * GJ/a * 1e-6 PJ/GJ * 1e-3 Mt/kt = Mt/x
    # kt/Mt * t/x * 1e-6 Mt/t * 1e-3 Mt/kt = Mt/x
    mutate(EF_ECLIPSE = EF_ECLIPSE * 1e-9) %>%
    filter(!is.nan(EF_ECLIPSE))

# ---- IAM emission factors ----
# emission factors for electricity (transport and unspecified energy use)
IAM.factors <- inner_join(
    IAMdata %>%
        filter(region %in% levels(NTNU.LCA.coefficients.IAM$region),
               between(period, 2010, 2050),
               grepl(paste0("Emissions\\|.*\\|Fossil Fuels and Industry\\|",
                            "Energy Supply\\|Electricity"), variable)) %>%
        droplevels() %>%
        extract(variable, "species", "^Emissions\\|([^\\|]*)\\|.*") %>%
        filter(species != "CO2") %>%
        select(-unit) %>%
        # # REMIND CO seems to be too large by factor 1000
        # mutate(value = ifelse((model == "REMIND" & species == "CO"),
        #                       value * 1e-3,
        #                       value)) %>%
        # fill missing data with average across other models
        spread(model, value, drop = FALSE) %>%
        gather(model, value, -scenario, -region, -species, -period) %>%
        group_by(scenario, region, species, period) %>%
        mutate(emissions = ifelse(!is.na(value), value,
                              mean(value, na.rm = TRUE))) %>%
        ungroup() %>%
        select(-value),

    IAMdata %>%
        filter(region != "World",
               between(period, 2010, 2050),
               variable == "Secondary Energy|Electricity") %>%
        select(-variable, -unit, electricity = value),

    by = c("model", "scenario", "region", "period")
) %>%
    # Mt / EJ * GJ/a * 1e-9 EJ/GJ = Mt/x
    mutate(EF_IAM = emissions / electricity * 1e-9,
           match = TRUE) %>%
    select(-emissions, -electricity) %>%
    inner_join(
        inline.data.frame(c(
            "quantity;                 energy.carrier;   match",
            "Freight transportation;   Electricity;      TRUE",
            "Energy;                   Electricity;      TRUE")),

        by = "match"
    ) %>%
    select(-match)

# ---- calculate specific emissions ----
specific.air.pollutant.emissions <- rbind(
    inner_join(
        IAM.factors,
        NTNU.LCA.coefficients.IAM %>%
            select(-model, -unit.numerator),
        by = c("scenario", "region", "quantity", "energy.carrier", "period")
    ) %>%
        # units already match
        mutate(specific.emissions = value * EF_IAM) %>%
        select(-EF_IAM, -value),

    inner_join(
        GAINS.factors,
        NTNU.LCA.coefficients.IAM %>%
            select(-model, -unit.numerator),
        by = c("region", "quantity", "energy.carrier", "period")
    ) %>%
        # units already match
        mutate(specific.emissions = value * EF_ECLIPSE,
               match = TRUE) %>%
        select(-EF_ECLIPSE, -value) %>%
        inner_join(
            data.frame(model = levels(IAMdata$model),
                       match = TRUE),
            by = "match"
        ) %>%
        select(-match)
) %>%
    group_by(model, scenario, region, technology, tech.variant, phase,
             unit.denominator, species, period) %>%
    summarise(specific.emissions = sum(specific.emissions)) %>%
    ungroup() %>%
    calc_addVariable('`Oil|w/o CCS`' = '`Coal|w/o CCS`',
                     '`Oil|w/ CCS`' = '`Coal|w/ CCS`',
                     variable = technology,
                     value = specific.emissions)

# include end-of-life with construction
specific.air.pollutant.emissions <- rbind(
    specific.air.pollutant.emissions %>%
        filter(phase == "Operation"),

    specific.air.pollutant.emissions %>%
        filter(phase != "Operation") %>%
        group_by(model, scenario, region, technology, tech.variant,
                 unit.denominator, species, period) %>%
        summarise(specific.emissions = sum(specific.emissions)) %>%
        ungroup() %>%
        mutate(phase = "Construction")
)

# ---- calculate absolute emissions ----
emi.ap.indirect <- rbind(
    # emissions from construction and end-of-life
    inner_join(
        specific.air.pollutant.emissions %>%
            filter(phase == "Construction") %>%
            select(-unit.denominator),

        IAM.Capacities %>%
            filter(phase == "Capacity Additions") %>%
            select(-phase, -unit),

        by = c("model", "scenario", "region", "technology", "period")
    ) %>%
        # Mt/MW * GW/a * 1e3 MW/GW = Mt/a
        mutate(value = value * specific.emissions * 1e3) %>%
        select(-specific.emissions),

    # emissions from operation by capacity
    inner_join(
        specific.air.pollutant.emissions %>%
            filter(phase == "Operation",
                   unit.denominator == "MW/yr") %>%
            select(-unit.denominator),

        IAM.Capacities %>%
            filter(phase == "Capacity") %>%
            select(-phase, -unit),

        by = c("model", "scenario", "region", "technology", "period")
    ) %>%
        # Mt/(MW/a) * GW * 1e3 MW/GW = Mt/a
        mutate(value = value * specific.emissions * 1e3) %>%
        select(-specific.emissions),

    # emissions from operation by production
    inner_join(
        specific.air.pollutant.emissions %>%
            filter(phase == "Operation",
                   unit.denominator == "kWh") %>%
            select(-unit.denominator),

        IAM.SE.production %>%
            filter(energy.carrier == "Electricity",
                   !is.na(technology)) %>%
            select(-energy.carrier, -unit),

        by = c("model", "scenario", "region", "technology", "period")
    ) %>%
        # Mt/kWh * EJ/a / (3.6e-12 EJ/kWh) = Mt/a
        mutate(value = value * specific.emissions / 3.6e-12) %>%
        select(-specific.emissions)
) %>%
    group_by(model, scenario, region, technology, tech.variant, phase, species,
             period) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(scenario = sub("ADV_WP5(_P240)?_(.*)_dyn", "\\2", scenario)) %>%
    inner_join(
        inline.data.frame(c(
            "species;   unit",
            "BC;        Mt BC/yr",
            "CO;        Mt CO/yr",
            "NOx;       Mt N2O/yr",
            "OC;        Mt OC/yr",
            "Sulfur;    Mt SO2/yr",
            "VOC;       Mt VOC/yr")),
        by = "species"
    ) %>%
    mutate(impact = paste("indirect", species)) %>%
    select(-species)
