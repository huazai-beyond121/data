#!/usr/bin/Rscript

# ******************************************************************************
#                    T i d y   N T N U   L C A   d a t a
# ******************************************************************************
# This script performs certain needed modifications on the NTNU LCA data set.
# 1) Rename scenarios, regions, and technologies to conform with IAM values
# 2) Split variable names into technologies, phases, quantities, and energy
#    carriers
# 3) Join the two data sets on self energy consumption and specific energy
#    input, giving the indirect energy used for Cement, Iron, and Freight
#    provision
# 4) Interpolate missing 5-year time steps
# ******************************************************************************
# The result is a data frame NTNU.LCA.coefficients with the following layout:
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':    248292 obs. of  11 variables:
# $ model           : Factor w/ 1 level "NTNU_LCA":
# $ scenario        : Factor w/ 4 levels "SSP2_3.2W_default",..:
# $ region          : Factor w/ 11 levels "AFR","CHN","EUR",..:
# $ technology      : Factor w/ 11 levels "Biomass|w/ CCS",..:
# $ phase           : Factor w/ 3 levels "Construction",..:
# $ quantity        : Factor w/ 4 levels "Cement","Energy",..:
# $ energy.carrier  : Factor w/ 5 levels "Electricity",..:
# $ unit.numerator  : Factor w/ 3 levels "GJ","t","tkm":
# $ unit.denominator: Factor w/ 3 levels "kWh","MW","MW/yr":
# $ period          : num 2010 2015 2020 2025 2030 2035 ...
# $ value           : num  47.44 1.01 140.6 108.81 95.5 ...
# ******************************************************************************

# #### Load libraries and scripts ##############################################

library(dplyr)
library(tidyr)
library(zoo)

if (!exists("ntnu"))
    stop("define ntnu with path to LCA coefficient csv")

source("read_quitte.R")
source("quitte_interpolate_periods.R")
source("transformation_list.R")
source("factor.data.frame.R")

# Load NTNU data ###############################################################
# If NTNU_LCA.csv is newer than NTNU.Rdata, compile new data set
read.quitte(ntnu, convert.periods = FALSE) %>% distinct() %>% tbl_df() -> NTNU
save(NTNU, file = "NTNU.Rdata")

# ---- re-mix PV to 30:70 rooftop:ground mounted ----
if (different.pv.shares) {
    NTNU <- rbind(
        # all but PV mix
        NTNU %>%
            filter(!(grepl("PV\\|", variable) & period > 2010)),

        # re-calculate PV mix from PV technology variants
        inner_join(
            # PV technology variants w/o NA
            NTNU %>%
                filter(grepl("PV [0-9]", variable) & period > 2010) %>%
                extract(variable, c("prefix", "tech.variant", "postfix"),
                        "^(.*\\|PV) ([0-9])(\\|.*)$"),

            # assumed shares of technologies
            paste("tech.variant;   group;      2010;   2030;   2050",
                  "1;              grounded;   0.369;     5;      7",
                  "3;              grounded;   0.006;     5;     14",
                  "5;              grounded;   0.036;     5;     14",
                  "2;              roof.top;   0.529;     4;      4",
                  "4;              roof.top;   0.009;     4;      6",
                  "6;              roof.top;   0.051;     4;      6",
                  sep = "\n") %>%
                textConnection() %>%
                read.table(header = TRUE, sep = ";", strip.white = TRUE,
                           check.names = FALSE) %>%
                gather(period, share, -tech.variant, -group) %>%
                group_by(period) %>%
                mutate(share = share / sum(share)) %>%
                ungroup() %>%
                mutate(tech.variant = as.character(tech.variant),
                       period = as.integer(period)) %>%
                select(-group),

            by = c("tech.variant", "period")
        ) %>%
            unite(variable, prefix, postfix, sep = "") %>%
            group_by(model, scenario, region, variable, unit, period) %>%
            summarise(value = sum(value * share)) %>%
            ungroup()
    )
}

# #### Interpolate missing periods #############################################
NTNU <- NTNU %>%
    complete(nesting(model, scenario, region, variable, unit),
             period = seq(from = min(unique(NTNU$period)),
                          to   = max(unique(NTNU$period)),
                          by   = 5)) %>%
    mutate(value = na.approx(value,
                             yleft  = head(value[!is.na(value)], 1),
                             yright = tail(value[!is.na(value)], 1)))

# #### Rename regions, scenarios, split units ##################################
NTNU %>%
    # Split units
    separate(unit, c("unit.numerator", "unit.denominator"), sep = "/",
             extra = "merge") %>%
    # rename variables changed by Anders
    mutate(variable = sub('(Residual energy|Energy service) requirements',
                          'Self energy consumption',
                          variable),
           variable = sub('Industry direct energy requirements',
                          'Specific energy input',
                          variable),
           variable = sub('Freight transport',
                          'Freight transportation',
                          variable)) %>%
    tbl_df() -> df_NTNU

# #### Split into self energy consumption and specific energy input ############
# Self energy consumption (and service level) data frame
df_NTNU %>%
    filter(grepl("^Self energy consumption", variable)) %>%
    # Split variable names
    extract(variable, c("technology", "phase", "quantity"),
            "Self energy consumption\\|([^|]*)\\|([^|]*)\\|([^|]*)"
    ) -> NTNU.self.energy.consumption

# # --- convert from gross calorific value to net calorific value
if (gcv2ncv) {
    NTNU.self.energy.consumption <- left_join(
        NTNU.self.energy.consumption,
        data_frame(
            quantity = c("Solids", "Liquids", "Gases"),
            multiplier = c(1.05, 1.06, 1.11)),
        by = "quantity"
    ) %>%
        mutate(value = ifelse(is.na(multiplier), value, value * multiplier)) %>%
        select(-multiplier)
}

# ---- include tech variants ----
NTNU.self.energy.consumption <- inner_join(
    read.table("data/tech.variants.csv", header = TRUE, sep = ";") %>%
        tbl_df(),

    NTNU.self.energy.consumption,

    by = c("technology.tech.variant" = "technology")) %>%
    select(model, scenario, region, technology, tech.variant, phase, quantity,
           unit.numerator, unit.denominator, period, value)

NTNU.self.energy.consumption %>%
    # Rename technologies to match IAM entries
    inner_join(
        TL$technology %>%
            filter(!is.na(REMIND)),
        c("technology" = "NTNU")) %>%
    select(-technology) %>%
    rename(technology = REMIND) %>%
    factor.data.frame() -> NTNU.self.energy.consumption

# Specific energy input data frame #############################################
df_NTNU %>%
    filter(grepl("^Specific energy input", variable)) %>%
    # Split variable names
    extract(variable, c("quantity", "energy.carrier"),
            "Specific energy input\\|([^|]*)\\|([^|]*)") %>%

    # Remap service level names and service level units to conform with self
    # energy consumption data frame
    inner_join(TL$self_specific,
               by = c("quantity" = "specific.energy.input")) %>%
    inner_join(TL$self_specific.units,
               by = c("unit.denominator" = "specific.unit")) %>%
    select(-quantity, -unit.denominator) %>%
    rename(quantity = self.energy.consumption, unit.denominator = self.unit
    ) -> NTNU.specific.energy.input


# ---- Other life cycle effects data frame -------------------------------------
df_NTNU %>%
    filter(!grepl("^(Specific energy input|Self energy consumption)",
                  variable),
           region != "Generic") %>%
    extract(variable, c("impact", "technology", "phase"),
            "(.*)\\|(.*)\\|(.*)") -> NTNU.impacts


# ---- include tech variants ----
NTNU.impacts <- inner_join(
    read.table("data/tech.variants.csv", header = TRUE, sep = ";") %>%
        tbl_df(),

    NTNU.impacts,

    by = c("technology.tech.variant" = "technology")) %>%
    select(model, scenario, region, technology, tech.variant, impact, phase,
           unit.numerator, unit.denominator, period, value)

# ---- impacts from storage and grid expansion ----
NTNU.impacts <- rbind(
    NTNU.impacts,
    read.quitte("data/NTNU_LCA_storage_grid.csv",
                convert.periods = FALSE) %>%
        tbl_df() %>%
        separate(variable, c("impact", "technology", "phase"), "\\|") %>%
        separate(unit, c("unit.numerator", "unit.denominator"), "/") %>%
        # include tech variants
        inner_join(
            read.table("data/tech.variants.csv", header = TRUE, sep = ";"),
            by = c("technology" = "technology.tech.variant")
        ) %>%
        select(model, scenario, region, impact, -technology,
               technology = technology.y, tech.variant, phase, unit.numerator,
               unit.denominator, period, value) %>%
        # duplicate grid to base grid and VRE grid
        mutate(technology = as.character(technology)) %>%
        inner_join(
            paste(
                "technology;   new.technology",
                "Storage;      Storage",
                "Grid;         Base Grid",
                "Grid;         VRE Grid",
                sep = "\n") %>%
                textConnection() %>%
                read.table(header = TRUE, sep = ";", strip.white = TRUE,
                           stringsAsFactors = FALSE),
            by = "technology"
        ) %>%
        select(-technology) %>%
        rename(technology = new.technology) %>%
        # replace Storage NA technology with mean across technologies
        group_by(model, scenario, region, impact, phase, unit.numerator,
                 unit.denominator, period) %>%
        mutate(value = ifelse(technology == "Storage" & is.na(tech.variant),
                              (sum(value) - value) / (n() - 1), value)) %>%
        ungroup() %>%
        # blow up regions ...
        inner_join(
            data.frame(region     = "World",
                       region.new = unique(NTNU.impacts$region)),
            by = "region"
        ) %>%
        select(-region, region = region.new) %>%
        # ... scenarios ...
        inner_join(
            data.frame(scenario     = "Baseline",
                       scenario.new = unique(NTNU.impacts$scenario)),
            by = "scenario"
        ) %>%
        select(-scenario, scenario = scenario.new) %>%
        # ... and periods
        inner_join(
            data.frame(period     = 2010,
                       period.new = unique(NTNU.impacts$period)),
            by = "period"
        ) %>%
        select(-period, period = period.new) %>%
        # x/GWh * 1e-3 GWh/MWh = x/MWh
        mutate(value = value * ifelse(unit.denominator == "GWh", 1e-3, 1),
               unit.denominator = sub("GWh", "MWh", unit.denominator))
)

# Rename technologies to match IAM entries
NTNU.impacts <- inner_join(
    NTNU.impacts,
    TL$technology %>%
        # skip offshore wind
        filter(!is.na(REMIND)),
    by = c("technology" = "NTNU")
) %>%
    select(-technology) %>%
    rename(technology = REMIND) %>%
    factor.data.frame()

##################################

# #### Combine both data frames, yielding indirect energy use by services ######
rbind(
    # Product of service level demand (Cement, Freight, Iron) and specific
    # energy inputs
    inner_join(
        NTNU.self.energy.consumption %>%
            rename(self.energy.consumption = value) %>%
            select(-unit.numerator),
        NTNU.specific.energy.input %>%
            rename(specific.energy.input = value) %>%
            select(-unit.denominator),
        by = c("model", "scenario", "region", "quantity", "period")
    ) %>%
        mutate(value = self.energy.consumption * specific.energy.input) %>%
        select(model, scenario, region, technology, tech.variant, phase,
               quantity, energy.carrier, unit.numerator, unit.denominator,
               period, value),

    # Self energy inputs (in GJ)
    NTNU.self.energy.consumption %>%
        filter(!(quantity %in% TL$self_specific$self.energy.consumption)) %>%
        mutate(energy.carrier = quantity, quantity = "Energy"),

    # Service level inputs (in tkm or t)
    NTNU.self.energy.consumption %>%
        filter(quantity %in% TL$self_specific$self.energy.consumption) %>%
        mutate(energy.carrier = NA)
) -> NTNU.LCA.coefficients

# #### Factorize all character columns #########################################
NTNU.LCA.coefficients %>%
    factor.data.frame() %>%
    tbl_df() -> NTNU.LCA.coefficients

left_join(
    NTNU.LCA.coefficients,

    TL$scenario.advance %>%
        select(-legend),

    by = c("scenario" = "NTNU")
) %>%
    select(-scenario, -ADVANCE_stat) %>%
    rename(scenario = ADVANCE_dyn) -> NTNU.LCA.coefficients.IAM

left_join(
    NTNU.impacts,

    TL$scenario.advance %>%
        select(-legend),

    by = c("scenario" = "NTNU")
) %>%
    select(-scenario, -ADVANCE_stat) %>%
    rename(scenario = ADVANCE_dyn) -> NTNU.impacts.IAM

# ---- other impacts from .mat file ----
source('./tidy_NTNU_impacts_mat.R')

# use TAX5_begr_betr_rf as default case
impacts.coeff <- rbind(
    filter(impacts.coeff, technology %in% c("Biomass|w/ CCS", "Biomass|w/o CCS"), tech.variant == "TAX5_begr_betr_rf")  %>%
        mutate(tech.variant = NA),
    filter(impacts.coeff, !(technology %in% c("Biomass|w/ CCS", "Biomass|w/o CCS") & is.na(tech.variant) ))) %>% factor.data.frame()

NTNU.impacts.IAM <- impacts.coeff %>%
    mutate(model = 'NTNU_LCA') %>%
    rename(value = coeff) %>%
    overwrite(NTNU.impacts.IAM)

# ---- calculate endpoints by midpoints for Grid & Storage ----
NTNU.impacts.IAM <- bind_rows(
    NTNU.impacts.IAM,

    inner_join(
        NTNU.impacts.IAM %>%
            filter(technology %in% c('Base Grid', 'VRE Grid', 'Storage')),

        inline.data.frame(c(
            'endpoint;                                           midpoint;                     new.numerator;   ReCiPe.factor',
            'Ecosystem|Agricultural and urban land occupation;   Land occupation;              species.yr;      9.3e-9',     # Occupation, forest; Hierarchist
            'Ecosystem|Freshwater ecotoxicity;                   Freshwater ecotoxicity;       species.yr;      8.61e-10',
            'Ecosystem|Freshwater eutrophication;                Freshwater eutrophication;    species.yr;      4.44e-8',
            'Ecosystem|Terrestrial ecotoxicity;                  Terrestrial ecotoxicity;      species.yr;      5.80e-9',
            'HumanHealth|Climate change;                         GHG;                          DALY;            1.4e-6',
            'HumanHealth|Human toxicity;                         Human toxicity;               DALY;            7.00e-7',
            'HumanHealth|Ionising radiation;                     Ionising radiation;           DALY;            1.64e-8',
            'Resource|Metal depletion;                           Mineral resource depletion;   $;               7.15e-2',
            NULL
        )),

        c('impact' = 'midpoint')
    ) %>%
        mutate(value = value * ReCiPe.factor) %>%
        select(-impact, impact = endpoint,
               -unit.numerator, unit.numerator = new.numerator,
               -ReCiPe.factor)
)

# ---- replace land use impacts from bioenergy operation by global average ----
NTNU.impacts.IAM <- left_join(
    # get land occupation coefficients for bioenergy operation
    NTNU.impacts.IAM %>%
        filter(grepl('^Bio', technology),
               impact %in% c('Ecosystem|Agricultural and urban land occupation',
                             'Ecosystem|Natural land transformation',
                             'Natural land transformation',
                             'Land occupation'),
               'Operation' == phase) %>%
        # replace <NA> with 'NA' for joining
        mutate(tech.variant = ifelse(is.na(tech.variant), 'NA',
                                     as.character(tech.variant))),

    # get MAgPIE data on Biomass production
    read.quitte('./MAgPIE_Fertilizer_Inputs/lca_gunnar_tc_exo.mif',
                convert.periods = FALSE) %>%
        filter(grepl('B100', scenario),
               variable == 'Primary Energy|Biomass') %>%
        mutate(tech.variant = sub('B100_', '', scenario)) %>%
        select(tech.variant, region, period, PE = value) %>%
        # add default technology variant
        spread(tech.variant, PE) %>%
        mutate(`NA` = `TAX5_begr_betr_rf`) %>%
        gather(tech.variant, PE, -region, -period) %>%
        # replaec MAgPIE with NTNU regions
        inner_join(
            data_frame(MAgPIE = c("AFR", "CPA", "EUR", "SAS", "OAS", "LAM",
                                  "MEA", "PAS", "PAO", "FSU", "NAM"),
                       NTNU   = c("AME", "CN",  "RER", "IN",  "PAC", "LA",
                                  "AME", "AS",  "PAC", "EIT", "US")),
            c('region' = 'MAgPIE')
        ) %>%
        select(-region, region = NTNU) %>%
        group_by(tech.variant, region, period) %>%
        summarise(PE = sum(PE)) %>%
        ungroup(),

    c('tech.variant', 'region', 'period')
) %>%
    # weigh residues and 2010 (no biomass production) equally
    mutate(PE = ifelse('residue' == tech.variant | 2010 == period, 1, PE),
           tech.variant = ifelse('NA' == tech.variant, NA, tech.variant)) %>%
    group_by(model, scenario, technology, tech.variant, impact, unit.numerator,
             unit.denominator, period) %>%
    mutate(value = sum(value * PE) / sum(PE)) %>%
    ungroup() %>%
    select(-PE) %>%
    overwrite(NTNU.impacts.IAM)

# ---- calculate deforestation from "Ecosystem|Natural land transformation" ----
NTNU.impacts.IAM <- rbind(
    NTNU.impacts.IAM,

    NTNU.impacts.IAM %>%
        filter(impact == "Ecosystem|Natural land transformation") %>%
        mutate(impact = "Deforestation",
               unit.numerator = "m2",
               value = value / 1.92e-6)
) %>% factor.data.frame()

# ### For REMIND: rename regions to REMIND regions and map to REMIND scenarios
NTNU.LCA.coefficients <- left_join(
    NTNU.LCA.coefficients,
    TL$region,
    by = c("region" = "NTNU")
) %>%
    select(-region) %>%
    rename(region = REMIND) %>%
    inner_join(
        TL$scenario %>%
            select(-legend),
        by = c("scenario" = "NTNU")
    ) %>%
    select(-scenario) %>%
    select(model, scenario = REMIND, region, technology, tech.variant, phase,
           quantity, energy.carrier, unit.numerator, unit.denominator, period,
           value)

NTNU.impacts %>%
    left_join(TL$region, by = c("region" = "NTNU")) %>%
    select(-region) %>%
    rename(region = REMIND) %>%
    left_join(TL$scenario %>% select(-legend), by = c("scenario" = "NTNU")) %>%
    select(-scenario) %>%
    rename(scenario = REMIND) -> NTNU.impacts

# workaround to correct for unit conversion error
# NTNU.impacts.IAM[NTNU.impacts.IAM$technology %in% c("Biomass|w/ CCS","Biomass|w/o CCS") & NTNU.impacts.IAM$impact == "Ecosystem|Natural land transformation",10 ]  = NTNU.impacts.IAM[NTNU.impacts.IAM$technology %in% c("Biomass|w/ CCS","Biomass|w/o CCS") & NTNU.impacts.IAM$impact == "Ecosystem|Natural land transformation",10 ]* 200

# ---- remix Biomass shares ----
if (differnet.bio.shares) {
    # use a mix of TAX5_begr_betr_rf and residue as default technology,
    # according to their shares in Final Energy
    NTNU.LCA.coefficients <- bind_rows(
        NTNU.LCA.coefficients %>%
            filter(!(grepl('^Bio', technology) & is.na(tech.variant))),

        NTNU.LCA.coefficients %>%
            filter(grepl('^Bio', technology),
                   tech.variant %in% c('TAX5_begr_betr_rf', 'residue')) %>%
            mutate(tech.variant = as.character(tech.variant)) %>%
            inner_join(
                tribble(
                    ~tech.variant,       ~share,
                    # 100 EJ purpose grown + 47 EJ residue biomass
                    'TAX5_begr_betr_rf', 100,
                    'residue',            47) %>%
                    mutate(share = share / sum(share)),

                'tech.variant') %>%
            group_by(model, scenario, region, technology, phase, quantity,
                     energy.carrier, unit.numerator, unit.denominator,
                     period) %>%
            summarise(value = sum(value * share),
                      tech.variant = NA) %>%
            ungroup()
    )

    NTNU.impacts <- bind_rows(
        NTNU.impacts %>%
            filter(!(grepl('^Bio', technology) & is.na(tech.variant))),

        NTNU.impacts %>%
            filter(grepl('^Bio', technology),
                   tech.variant %in% c('TAX5_begr_betr_rf', 'residue')) %>%
            mutate(tech.variant = as.character(tech.variant)) %>%
            inner_join(
                tribble(
                    ~tech.variant,       ~share,
                    # 100 EJ purpose grown + 47 EJ residue biomass
                    'TAX5_begr_betr_rf', 100,
                    'residue',            47) %>%
                    mutate(share = share / sum(share)),

                'tech.variant') %>%
            group_by(model, scenario, region, technology, impact, phase,
                     unit.numerator, unit.denominator, period) %>%
            summarise(value = sum(value * share),
                      tech.variant = NA) %>%
            ungroup()
    )


    NTNU.impacts.IAM <- bind_rows(
        NTNU.impacts.IAM %>%
            filter(!(grepl('^Bio', technology) & is.na(tech.variant))),

        NTNU.impacts.IAM %>%
            filter(grepl('^Bio', technology),
                   tech.variant %in% c('TAX5_begr_betr_rf', 'residue')) %>%
            mutate(tech.variant = as.character(tech.variant)) %>%
            inner_join(
                tribble(
                    ~tech.variant,       ~share,
                    # 100 EJ purpose grown + 47 EJ residue biomass
                    'TAX5_begr_betr_rf', 100,
                    'residue',            47) %>%
                    mutate(share = share / sum(share)),

                'tech.variant') %>%
            group_by(model, scenario, region, technology, impact, phase,
                     unit.numerator, unit.denominator, period) %>%
            summarise(value = sum(value * share),
                      tech.variant = NA) %>%
            ungroup()
    )
}

# ####  save the result ############################################
save(NTNU.LCA.coefficients, NTNU.impacts, file = "NTNU.LCA.coefficients.Rdata")

save(NTNU.LCA.coefficients.IAM, NTNU.impacts.IAM,
     file = "NTNU.LCA.coefficients.IAM.Rdata")

# #### Clean up  ############################################
rm(NTNU, df_NTNU, NTNU.specific.energy.input, NTNU.self.energy.consumption,
   NTNU.LCA.coefficients, NTNU.LCA.coefficients.IAM, NTNU.impacts,
   NTNU.impacts.IAM, tmp)
