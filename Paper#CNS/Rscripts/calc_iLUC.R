
library(dplyr)
library(tidyr)
library(quitte)

# --- load MAgPIE data ----
x <- read.quitte('./MAgPIE_Fertilizer_Inputs/lca_gunnar_tc_exo.mif',
                 convert.periods = FALSE) %>%
    select(-model, -unit) %>%
    filter(period %in% seq(2005, 2100, 5),
           variable %in% c('Emissions|CO2|Land Use',
                           'Emissions|CH4|Land Use',
                           'Emissions|N2O|Land Use',
                           'Primary Energy|Biomass')) %>%
    extract(scenario, c('scen.biomass.amount', 'scen.tax', 'scen.biomass.type'),
            '^B([^_]*)_TAX([^_]*)_(.*)$', convert = TRUE)

# ---- calculate LU CO2 and bio PE deltas ----
iLUC <- bind_rows(
    x %>%
        filter('Emissions|CO2|Land Use' == variable) %>%
        spread(scen.biomass.amount, value) %>%
        mutate(`100`      = `100` - `0`,
               `50`       =  `50` - `0`,
               `100 - 50` = `100` - `50`,
               variable = 'delta.lu.co2') %>%
        select(-`0`) %>%
        gather(scen.biomass.amount, value, matches('^[0-9]')),

    x %>%
        filter('Primary Energy|Biomass' == variable) %>%
        spread(scen.biomass.amount, value) %>%
        mutate(`100`      = `100` - `0`,
               `50`       =  `50` - `0`,
               `100 - 50` = `100` - `50`,
               variable = 'delta.pe.bio') %>%
        select(-`0`) %>%
        gather(scen.biomass.amount, value, matches('^[0-9]'))
)

# ---- cumulate LU CO2 and bio PE deltas ----
iLUC <- bind_rows(
    iLUC,

    iLUC %>%
        group_by(scen.biomass.amount, scen.tax, scen.biomass.type, region,
                 variable) %>%
        arrange(period) %>%
        mutate(value = cumsum(value)) %>%
        ungroup() %>%
        mutate(variable = sub('delta', 'cum', variable))
)

# ---- calculate indirect LUC emissions ----
iLUC <- bind_rows(
    iLUC,

    iLUC %>%
        filter(variable %in% c('delta.lu.co2', 'delta.pe.bio')) %>%
        spread(variable, value) %>%
        mutate(value = delta.lu.co2 / delta.pe.bio,
               variable = 'iLUC') %>%
        select(-delta.lu.co2, -delta.pe.bio),

    iLUC %>%
        filter(variable %in% c('cum.lu.co2', 'cum.pe.bio')) %>%
        spread(variable, value) %>%
        mutate(value = cum.lu.co2 / cum.pe.bio,
               variable = 'cum.iLUC') %>%
        select(-cum.lu.co2, -cum.pe.bio)
)

# ---- add tech variants ----
iLUC <- full_join(
    iLUC,

    tribble(
        ~scen.biomass.amount,  ~scen.tax,   ~scen.biomass.type,   ~tech.variant,
        '100',                  0,          'begr_betr_ir',        1,
        '100',                  0,          'begr_betr_rf',        2,
        '100',                  0,          'betr_rf',             3,
        '100',                 30,          'begr_betr_ir',        4,
        '100',                 30,          'begr_betr_rf',        5,
        '100',                 30,          'betr_rf',             6,
        '100',                  5,          'begr_betr_ir',        7,
        '100',                  5,          'begr_betr_rf',        8,
        '100',                  5,          'betr_rf',             9,
        '50',                   0,          'begr_betr_ir',       10,
        '50',                   0,          'begr_betr_rf',       11,
        '50',                   0,          'betr_rf',            12,
        '50',                  30,          'begr_betr_ir',       13,
        '50',                  30,          'begr_betr_rf',       14,
        '50',                  30,          'betr_rf',            15,
        '50',                   5,          'begr_betr_ir',       16,
        '50',                   5,          'begr_betr_rf',       17,
        '50',                   5,          'betr_rf',            18,
        '100 - 50',             0,          'begr_betr_ir',       19,
        '100 - 50',             0,          'begr_betr_rf',       20,
        '100 - 50',             0,          'betr_rf',            21,
        '100 - 50',            30,          'begr_betr_ir',       22,
        '100 - 50',            30,          'begr_betr_rf',       23,
        '100 - 50',            30,          'betr_rf',            24,
        '100 - 50',             5,          'begr_betr_ir',       25,
        '100 - 50',             5,          'begr_betr_rf',       26,
        '100 - 50',             5,          'betr_rf',            27,
        '100',                 30,          'begr_betr_rf',       NA),

    c('scen.biomass.amount', 'scen.tax', 'scen.biomass.type')
)

save(iLUC, file = 'iLUC.Rdata')

