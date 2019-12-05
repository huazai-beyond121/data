
# ---- TODO: ----
# - compute own Fossil Depletion midpoint

# ---- load stuff ----
library(tidyverse)
library(R.matlab)
library(zoo)
library(quitte)

# ---- read .mat file ----
impacts.coeff <- readMat(mat)

# ---- get midpoint coefficients ----
midpoint.units <- data_frame(
    # # short names
    # a = unlist(impacts.coeff$midpointImpactsInMidpointUnitsInfo)[1:16],
    # b = unlist(impacts.coeff$midpointImpactsInMidpointUnitsInfo)[17:32],
    midpoint = unlist(impacts.coeff$midpointImpactsInMidpointUnitsInfo)[33:48],
    # # all hierarchist
    # d = unlist(impacts.coeff$midpointImpactsInMidpointUnitsInfo)[49:64],
    unit = unlist(impacts.coeff$midpointImpactsInMidpointUnitsInfo)[65:80]
)

midpoint.coeff <- expand.grid(
    scenario   = unlist(impacts.coeff$scenarioName),
    period     = as.numeric(unlist(impacts.coeff$years)),
    region     = unlist(impacts.coeff$regionName),
    # add tech variant 1 to 'Wind, onshore'
    technology = sub('(onshore)$', '\\1 1', unlist(impacts.coeff$techName)),
    midpoint   = midpoint.units$midpoint,
    phase      = unlist(impacts.coeff$phaseName)
) %>%
    tbl_df()

midpoint.coeff <- bind_rows(
    midpoint.coeff %>%
        mutate(unit.denominator = factor('kWh', levels = c('GW', 'kWh')),
               coeff = c(impacts.coeff$d.perkWh)),

    midpoint.coeff %>%
        mutate(unit.denominator = factor('GW', levels = c('GW', 'kWh')),
               coeff = c(impacts.coeff$d.perGW))
) %>%
    filter(0 != coeff) %>%
    # GHG emissions and fossil energy use are provided by IAMs
    filter(!midpoint %in% c('Climate change', 'Fossil depletion')) %>%
    # add midpoint units
    inner_join(midpoint.units %>% rename(unit.numerator = unit), 'midpoint') %>%
    select(scenario, region, technology, phase, midpoint, unit.numerator,
           unit.denominator, period, coeff)

# ---- rename midpoint impacts/units to comply with older data set ----
midpoint.coeff <- midpoint.coeff %>%
    left_join(
        tribble(
            ~mat.impact,                               ~impact,
            'Agricultural and urban land occupation',  'Land occupation'),

        c('midpoint' = 'mat.impact')
    ) %>%
    mutate(midpoint = ifelse(is.na(impact), midpoint, impact)) %>%
    select(-impact) %>%
    left_join(
        tribble(
            ~mat.unit,        ~old.unit,
            'kg 1,4-DB eq',   'kg 1,4-DCB-Eq',
            'kg P eq',        'kg P-Eq',
            'kg U235 eq',     'kg U235-Eq',
            'kg N eq',        'kg N-Eq',
            'kg Fe eq',       'kg Fe-eq'),

        c('unit.numerator' = 'mat.unit')
    ) %>%
    mutate(
        unit.numerator = ifelse(is.na(old.unit), unit.numerator, old.unit)) %>%
    select(-old.unit)

# ---- get endpoint coefficients ----
endpoint.units <- data_frame(
    endpoint = unlist(impacts.coeff$endpointImpactInfo)[1:5],
    # # all hierarchist
    # b = unlist(impacts.coeff$endpointImpactInfo)[6:10],
    unit = sub('spieces', 'species',
                unlist(impacts.coeff$endpointImpactInfo)[11:15])
)

endpoint.coeff <- expand.grid(
    scenario   = unlist(impacts.coeff$scenarioName),
    period     = as.numeric(unlist(impacts.coeff$years)),
    region     = unlist(impacts.coeff$regionName),
    # add tech variant 1 to 'Wind, onshore'
    technology = sub('(onshore)$', '\\1 1', unlist(impacts.coeff$techName)),
    midpoint   = unlist(
        impacts.coeff$midpointContributionsToEndpointImpactsInfo)[33:48],
    endpoint   = endpoint.units$endpoint,
    phase      = unlist(impacts.coeff$phaseName)
) %>%
    tbl_df() %>%
    # rename since Anders used different midpoint names for midpoints and
    # endpoints
    full_join(
        tribble(
            ~midpoints,                                 ~end.midpoints,
            'Climate change',                           'Climate change',
            'Fossil depletion',                         'Fossil depletion',
            'Freshwater ecotoxicity',                   'Freshwater ecotoxicity',
            'Freshwater eutrophication',                'Freshwater eutrophication',
            'Human toxicity',                           'Human toxicity',
            'Ionising radiation',                       'Ionising radiation',
            'Marine ecotoxicity',                       'Marine ecotoxicity',
            'Marine eutrophication',                    'Marine eutrophication',
            'Metal depletion',                          'Metal depletion',
            'Natural land transformation',              'Ecosystem damage from land transformation',
            'Ozone depletion',                          'Human health damage from ozone depletion',
            'Particulate matter formation',             'Particulate matter formation',
            'Photochemical oxidant formation',          'Photochemical oxidant formation',
            'Terrestrial acidification',                'Terrestrial acidification',
            'Terrestrial ecotoxicity',                  'Terrestrial ecotoxicity',
            'Agricultural and urban land occupation',   'Ecosystem damage from land occupation'
        ),

        c('midpoint' = 'end.midpoints')
    ) %>%
    select(-midpoint, midpoint = midpoints)


endpoint.coeff <- bind_rows(
    endpoint.coeff %>%
        mutate(unit.denominator = factor('kWh', levels = c('GW', 'kWh')),
               coeff = c(impacts.coeff$d.end.perkWh)),

    endpoint.coeff %>%
        mutate(unit.denominator = factor('GW', levels = c('GW', 'kWh')),
               coeff = c(impacts.coeff$d.end.perGW))
) %>%
    filter(0 != coeff) %>%
    # GHG emissions and fossil energy use are calculated using ReCiPe factors
    filter(!midpoint %in% c('Climate change', 'Fossil depletion')) %>%
    # add endpoint units
    inner_join(endpoint.units %>% rename(unit.numerator = unit), 'endpoint') %>%
    select(scenario, region, technology, phase, midpoint, endpoint,
           unit.numerator, unit.denominator, period, coeff)

# ---- remove 'without GWP' endpoints ----
endpoint.coeff <- endpoint.coeff %>%
    filter(!grepl('WithoutGWP', endpoint))

# ---- add endpoint coefficients from ReCiPe ----
endpoint.coeff <- bind_rows(
    endpoint.coeff,

    full_join(
        endpoint.coeff %>%
            distinct(scenario, region, technology, phase, period) %>%
            mutate(match = TRUE),

        data.frame(
            endpoint = c('HumanHealth', 'Ecosystem', 'Resource'),
            midpoint = c(rep('Climate change', 2), 'Fossil depletion'),
            unit.numerator = c('DALY', 'species.yr', '$'),
            unit.denominator = c(rep('kg CO2 eq', 2), 'kg oil eq'),
            coeff = c(1.4e-6, 7.93e-9, 0.165),
            match = TRUE),

        'match'
    ) %>%
        select(-match)
)

# ---- combine mid- end endpoint coefficients ----
impacts.coeff <- bind_rows(
    midpoint.coeff %>%
        rename(impact = midpoint),

    endpoint.coeff %>%
        unite(impact, endpoint, midpoint, sep = '|')
) %>%
    # convert per GW to per MW impacts
    inner_join(
        data_frame(
            unit.denominator = factor(c('GW', 'kWh', 'kg CO2 eq', 'kg oil eq')),
            new.denominator  = factor(c('MW', 'kWh', 'kg CO2 eq', 'kg oil eq')),
            factor           = c(       1e-3,     1,           1,          1)),
        'unit.denominator'
    ) %>%
    mutate(coeff = coeff * factor) %>%
    select(-factor, -unit.denominator, unit.denominator = new.denominator)

rm(midpoint.coeff, endpoint.coeff)

# ---- split technologies and technology variants ----
impacts.coeff <- impacts.coeff %>%
    extract(technology, c('technology', 'tech.variant'),
            '(.*[^,]),? ([^ ]*)$')

# ---- rename technologies ----
impacts.coeff <- inner_join(
    impacts.coeff,
    TL$technology %>%
        # skip offshore wind
        filter(!is.na(REMIND)),
    c('technology' = 'NTNU')
) %>%
    select(-technology, technology = REMIND)

# ---- remove unused technology/phase/unit combinations ----
impacts.coeff <- bind_rows(
    inner_join(
        impacts.coeff %>%
            # fix unit for VRE technologies/operation
            mutate(unit.denominator = ifelse(
                (grepl('(Hydro|Wind|Solar)', technology)
                 & 'Operation' == phase
                 & 'MW' == unit.denominator
                ),
                'MW/yr',
                as.character(unit.denominator))),

        NTNU.self.energy.consumption %>%
            filter(!is.na(technology)) %>%
            distinct(technology, phase, unit.denominator),

        c('technology', 'phase', 'unit.denominator')
    ),

    impacts.coeff %>%
        filter(grepl('(Climate change|Fossil depletion)', impact))
)

# ---- calculate <NA> technology variants ----
tech.variant.shares <- tribble(
    ~technology,         ~tech.variant,         ~`2010`,  ~`2030`,    ~`2050`,
    'Coal|w/o CCS',      '1',                  0.72,   0.66,   0.66,
    'Coal|w/o CCS',      '2',                  0,      0,      0,
    'Coal|w/o CCS',      '3',                  0.28,   0.34,   0.34,
    'Coal|w/ CCS',       '1',                  1,      0.11,   0.11,
    'Coal|w/ CCS',       '2',                  0,      0.19,   0.19,
    'Coal|w/ CCS',       '3',                  0,      0.7,    0.7,
    'Gas|w/o CCS',       '1',                  1,      1,      1,
    'Gas|w/ CCS',        '1',                  1,      1,      1,
    # assumptions on default biomass production scheme
    'Biomass|w/o CCS',   'TAX30_begr_betr_rf', 1,      1,      1,
    'Biomass|w/ CCS',    'TAX30_begr_betr_rf', 1,      1,      1,
    'Hydro',             '1',                  0.2,    0.2,    0.2,
    'Hydro',             '2',                  0.8,    0.8,    0.8,
    'Nuclear',           '1',                  0.3,    0.3,    0.3,
    'Nuclear',           '2',                  0.7,    0.7,    0.7,
    'Wind',              '1',                  1,      1,      1,
    'Solar|CSP',         '1',                  0.5,    0.5,    0.5,
    'Solar|CSP',         '2',                  0.5,    0.5,    0.5
) %>%
    gather(period, share, -technology, -tech.variant, convert = TRUE)

if (different.pv.shares) {
    tech.variant.shares <- bind_rows(
        tech.variant.shares,
        tribble(
            ~technology,   ~tech.variant,   ~`2010`,   ~`2030`,   ~`2050`,
            'Solar|PV',   '1',              0.369,     5,          7,
            'Solar|PV',   '3',              0.006,     5,         14,
            'Solar|PV',   '5',              0.036,     5,         14,
            'Solar|PV',   '2',              0.529,     4,          4,
            'Solar|PV',   '4',              0.009,     4,          6,
            'Solar|PV',   '6',              0.051,     4,          6) %>%
            gather(period, share, -technology, -tech.variant,
                   convert = TRUE) %>%
            mutate(tech.variant = as.character(tech.variant)) %>%
            group_by(period) %>%
            mutate(share = share / sum(share)) %>%
            ungroup()
    )
} else {
    tech.variant.shares <- bind_rows(
        tech.variant.shares,
        tribble(
            ~technology,   ~tech.variant,   ~`2010`,   ~`2030`,   ~`2050`,
            'Solar|PV',   '1',              0.369,     0.137,     0.082,
            'Solar|PV',   '2',              0.529,     0.196,     0.118,
            'Solar|PV',   '3',              0.006,     0.137,     0.164,
            'Solar|PV',   '4',              0.009,     0.196,     0.236,
            'Solar|PV',   '5',              0.036,     0.137,     0.164,
            'Solar|PV',   '6',              0.051,     0.196,     0.236) %>%
            gather(period, share, -technology, -tech.variant,
                   convert = TRUE) %>%
            mutate(tech.variant = as.character(tech.variant))
    )
}

impacts.coeff <- bind_rows(
    impacts.coeff,

    inner_join(
        impacts.coeff,
        tech.variant.shares,
        c('technology', 'tech.variant', 'period')
    ) %>%
        group_by(scenario, region, technology, phase, impact, unit.numerator,
                 unit.denominator, period) %>%
        summarise(coeff = sum(coeff * share)) %>%
        ungroup() %>%
        mutate(tech.variant = NA)
)

# ---- rename scenarios ----
impacts.coeff <- inner_join(
    impacts.coeff,

    TL$scenario.advance %>%
        select(NTNU, ADVANCE_dyn) %>%
        mutate(NTNU = sub('_', ' ', NTNU)),

    c('scenario' = 'NTNU')
) %>%
    select(-scenario, scenario = ADVANCE_dyn)

# ---- interpolate missing periods ----
impacts.coeff <- impacts.coeff %>%
    complete(nesting(scenario, region, technology, tech.variant, phase, impact,
                     unit.numerator, unit.denominator),
             period = seq(from = min(impacts.coeff$period),
                          to   = max(impacts.coeff$period),
                          by   = 5),
             fill = list(coeff = NA)) %>%
    mutate(coeff = na.approx(coeff,
                             yleft  = head(coeff[!is.na(coeff)], 1),
                             yright = tail(coeff[!is.na(coeff)], 1))) %>%
    select(scenario, region, technology, tech.variant, phase, impact,
           unit.numerator, unit.denominator, period, coeff) %>%
    arrange(scenario, region, technology, tech.variant, phase, impact,
            unit.numerator, unit.denominator, period) %>%
    factor.data.frame()
