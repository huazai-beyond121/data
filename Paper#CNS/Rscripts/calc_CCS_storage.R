# ---- get CCS numbers ----
CCS.storage <- IAMdata %>%
    filter(grepl('^Carbon Sequestration\\|CCS\\|Fossil\\|', variable),
           value != 0)

# ---- filter technology names ----
CCS.storage <- CCS.storage %>%
    mutate(technology = sub(
        pattern = paste0('^Carbon Sequestration\\|CCS\\|Fossil\\|',
                         '(?:Energy)?(Biomass)?',
                         '\\|Supply\\|Electricity\\|?',
                         '(.*)$'),
        replacement = '\\1\\2|w/ CCS',
        x = as.character(variable),
        perl = TRUE)) %>%
    select(model, scenario, region, technology, period, value)

# ---- expand tech.variants ----
CCS.storage <- inner_join(
    CCS.storage,

    bind_rows(
        inner_join(
            read.csv2('data/tech.variants.csv', row.names = NULL,
                      stringsAsFactors = FALSE) %>%
                select(technology, tech.variant) %>%
                filter(technology %in% paste(c('Bio', 'Coal', 'Gas'), 'w CCS')),

            TL$technology %>%
                character.data.frame(),

            c('technology' = 'NTNU')
        ) %>%
            select(-technology, technology = REMIND),

        data_frame(technology = 'Oil|w/ CCS',
                   tech.variant = c('1', NA))
    ),

    'technology'
)

# ---- rename scenarios ----
CCS.storage <- inner_join(
    CCS.storage,

    TL$scenario.advance %>%
        select(scenario = ADVANCE_dyn, legend) %>%
        character.data.frame(),

    'scenario'
) %>%
    select(-scenario, scenario = legend)

# ---- add phase, impact and unit columns ----
CCS.storage <- CCS.storage %>%
    mutate(phase = 'Operation',
           impact = 'CO2 storage',
           unit = 'Mt CO2') %>%
    select(model, scenario, region, technology, tech.variant, phase, impact,
           unit, period, value) %>%
    arrange(model, scenario, region, technology, tech.variant, phase, impact,
             unit, period)
