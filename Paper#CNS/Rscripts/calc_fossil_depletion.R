
library(tidyverse)
library(quitte)

indirect.fossil.depletion <- inner_join(
    # fossil intensity of
    IAMdata %>%
        filter(grepl('^(Primary|Secondary) Energy\\|', variable)) %>%
        calc_addVariable(
            # fossil electricity: coal and gas, neglecting oil
            'Fossil intensity|Electricity' = paste(
                '( `Primary Energy|Coal|Electricity`',
                '+ `Primary Energy|Gas|Electricity`',
                ')',
                '/ `Secondary Energy|Electricity`'),

            # fossil electricity, coal and gas individually
            'Fossil intensity|Electricity|Coal' = paste(
                '  `Primary Energy|Coal|Electricity`',
                '/ `Secondary Energy|Electricity`'),

            'Fossil intensity|Electricity|Gas' = paste(
                '  `Primary Energy|Gas|Electricity`',
                '/ `Secondary Energy|Electricity`'),

            # others are assumed to be 1
            'Fossil intensity|Solids'  = '1',
            'Fossil intensity|Liquids' = '1',
            'Fossil intensity|Gases'   = '1',
            units = 'EJ/EJ',
            only.new = TRUE) %>%
        extract(variable, c('energy.carrier', 'tmp'),
                '^Fossil intensity\\|([^\\|]*)\\|?(.*)$') %>%
        # mutate(energy.carrier = sub('Fossil intensity\\|', '', variable)) %>%
        group_by(scenario, region, energy.carrier, tmp, period) %>%
        # since the "C" in GCAM does not stand for "Conservation of Energy" ...
        mutate(coeff = ifelse('GCAM' == model, value,
                              (sum(value) - value) / (n() - 1))
        ) %>%
        ungroup() %>%
        select(model, scenario, region, energy.carrier, tmp, period, coeff),

    NTNU.LCA.coefficients.IAM %>%
        filter(!is.na(energy.carrier)) %>%
        group_by(scenario, region, technology, tech.variant, phase,
                 energy.carrier, unit.numerator, unit.denominator, period) %>%
        summarise(value = sum(value)) %>%
        ungroup(),

    c('scenario', 'region', 'energy.carrier', 'period')
) %>%
    # GJ / (4.1868e-2 GJ/ktoe) * 1 = kgoe
    mutate(value = value / 4.1868e-2 * coeff,
           unit.numerator = 'kg oil eq') %>%
    inner_join(
        inline.data.frame(
            'energy.carrier;   tmp;    new.energy.carrier',
            'Solids;           ;       Solids',
            'Liquids;          ;       Liquids',
            'Gases;            ;       Gases',
            'Electricity;      Coal;   Solids',
            'Electricity;      Gas;    Gases'),
        c('energy.carrier', 'tmp')) %>%
    select(model, scenario, region, technology, tech.variant, phase,
           -energy.carrier, energy.carrier = new.energy.carrier, unit.numerator,
           unit.denominator, period, value)

# ---- use gas coefficients for Oil too ----
indirect.fossil.depletion <- rbind(
    indirect.fossil.depletion,

    indirect.fossil.depletion %>%
        filter(grepl('Gas', technology)) %>%
        mutate(technology = sub('Gas', 'Oil', technology))
)

# ---- indirect fossil depletion from Construction and End-of-life ----
indirect.fossil.depletion.construction <- inner_join(
    indirect.fossil.depletion %>%
        filter(phase != 'Operation') %>%
        group_by(model, scenario, region, technology, tech.variant,
                 energy.carrier, unit.numerator, period) %>%
        summarise(value = sum(value)) %>%
        ungroup(),

    IAM.Capacities %>%
        filter(phase == 'Capacity Additions',
               value != 0) %>%
        select(model, scenario, region, technology, period, new.cap = value),

    c('model', 'scenario', 'region', 'technology', 'period')
) %>%
    # kg oil eq/MW * GW * 1000 MW/GW = kg oil eq
    mutate(value = value * new.cap * 1e3,
           phase = 'Construction') %>%
    select(model, scenario, region, technology, tech.variant, phase,
           energy.carrier, unit = unit.numerator, period, value)

# ---- indirect fossil depletion from Operation (by Capacity) ----
indirect.fossil.depletion.operation.capacity <- inner_join(
    indirect.fossil.depletion %>%
        filter(phase == 'Operation',
               unit.denominator == 'MW/yr') %>%
        select(-unit.denominator),

    IAM.Capacities %>%
        filter(phase == 'Capacity',
               value != 0) %>%
        select(model, scenario, region, technology, period, cap = value),

    c('model', 'scenario', 'region', 'technology', 'period')
) %>%
    # kg oil eq/MW * GW * 1000 MW/GW = kg oil eq
    mutate(value = value * cap * 1e3) %>%
    select(model, scenario, region, technology, tech.variant, phase,
           energy.carrier, unit = unit.numerator, period, value)

# ---- indirect fossil depletion from Operation (by Production) ----
indirect.fossil.depletion.operation.production <- inner_join(
    indirect.fossil.depletion %>%
        filter(phase == 'Operation',
               unit.denominator == 'kWh') %>%
        select(-unit.denominator),

    IAM.SE.production %>%
        filter(!is.na(technology),
               value != 0) %>%
        select(-energy.carrier, -unit, production = value),

    c('model', 'scenario', 'region', 'technology', 'period')
) %>%
    # kg oil eq/kWh * EJ/a / (3.6e-12 EJ/kWh) = kg oil eq
    mutate(value = value * production / 3.6e-12) %>%
    select(model, scenario, region, technology, tech.variant, phase,
           energy.carrier, unit = unit.numerator, period, value)

direct.fossil.depletion <- bind_rows(
    # Primary Energy for Coal and Gas
    IAMdata %>%
        filter(
            grepl('^Primary Energy\\|(Coal|Gas)\\|Electricity', variable)) %>%
        mutate(variable = sub('Primary Energy\\|(.*)\\|Electricity(\\|?.*)',
                              '\\1\\2', variable)),

    # Secondary Energy for Oil, since not in reporting
    IAMdata %>%
        filter(grepl('^Secondary Energy\\|Electricity\\|Oil(\\|w/ CCS)?$',
                     variable)) %>%
        mutate(variable = sub('^Secondary Energy\\|Electricity\\|(.*)', '\\1',
                              variable),
               # assume eta of 0.3 for both without and with CCS
               value = value / 0.3)
) %>%
    # EJ / (4.1868e-11 EJ/kgoe) = kgoe
    mutate(value = value / 4.1868e-11,
           unit = 'kg oil eq') %>%
    extract(variable, c('technology', 'CCS'), '(Coal|Gas|Oil)\\|?(.*)') %>%
    mutate(CCS = ifelse('' == CCS, 'Total', CCS)) %>%
    spread(CCS, value, fill = 0) %>%
    mutate(`w/o CCS` = ifelse(`w/ CCS` > Total, Total, Total - `w/ CCS`)) %>%
    select(-Total) %>%
    gather(CCS, value, ends_with('CCS')) %>%
    unite(technology, technology, CCS, sep = '|') %>%
    select(model, scenario, region, technology, unit, period, value)

# ---- workaround for reporting errors in GCAM, POLES ----
direct.fossil.depletion <- inner_join(
    direct.fossil.depletion,

    inner_join(
        direct.fossil.depletion %>%
            filter(model %in% c('GCAM', 'POLES'),
                   scenario == 'ADV_WP5_Base_dyn',
                   region == 'World',
                   grepl('w/o CCS', technology),
                   period == 2005) %>%
            select(model, technology, value),

        direct.fossil.depletion %>%
            filter(model == 'REMIND',
                   scenario == 'ADV_WP5_Base_dyn',
                   region == 'World',
                   grepl('w/o CCS', technology),
                   period == 2005) %>%
            select(technology, base = value),

        'technology'
    ) %>%
        mutate(factor = base / value) %>%
        select(-value, -base) %>%
        inner_join(
            data_frame(
                technology = rep(paste0(c('Coal', 'Gas', 'Oil'), '|w/o CCS'),
                                 each = 2),
                tech = paste0(rep(c('Coal', 'Gas', 'Oil'), each = 2),
                              c('|w/o CCS', '|w/ CCS'))),
            'technology'
        ) %>%
        select(-technology, technology = tech) %>%
        complete(technology, model = levels(direct.fossil.depletion$model),
                 fill = list(factor = 1)),

    c('model', 'technology')
) %>%
    mutate(value = value * factor) %>%
    select(-factor)

# ---- blow up tech.variants ----
direct.fossil.depletion <- direct.fossil.depletion %>%
    inner_join(
        bind_rows(
            NTNU.LCA.coefficients.IAM %>%
                distinct(technology, tech.variant),

            expand.grid(technology = c('Oil|w/o CCS', 'Oil|w/ CCS'),
                        tech.variant = c('1', NA))
        ),

        'technology'
    ) %>%
    mutate(phase = 'Operation',
           energy.carrier = ifelse(grepl('Coal', technology),
                                   'Solids', 'Gases'))

fossil.depletion <- bind_rows(
    bind_rows(
        indirect.fossil.depletion.construction,
        indirect.fossil.depletion.operation.capacity,
        indirect.fossil.depletion.operation.production
    ) %>%
        sum_total(region, name = 'World'),
    direct.fossil.depletion
)

fossil.depletion.plot.data <- bind_rows(
    full_join(
        bind_rows(
            indirect.fossil.depletion.construction,
            indirect.fossil.depletion.operation.capacity,
            indirect.fossil.depletion.operation.production
        ) %>%
            sum_total(region, name = 'World'),

        IAMdata %>%
            filter(grepl('^Primary Energy\\|(Coal|Gas)\\|Electricity$',
                         variable)) %>%
            spread(variable, value) %>%
            mutate(Solids = `Primary Energy|Coal|Electricity`
                          / ( `Primary Energy|Coal|Electricity`
                            + `Primary Energy|Gas|Electricity`
                            ),
                   Gases  = 1 - Solids) %>%
            select(model, scenario, region, period, Solids, Gases) %>%
            gather(Fossil, factor, Solids, Gases) %>%
            mutate(energy.carrier = 'Electricity'),

        c('model', 'scenario', 'region', 'period', 'energy.carrier')
    ) %>%
        mutate(value = ifelse('Electricity' != energy.carrier, value,
                              value * factor),
               energy.carrier = ifelse('Electricity' != energy.carrier,
                                       energy.carrier, Fossil)) %>%
        group_by(model, scenario, region, technology, tech.variant, phase,
                 energy.carrier, unit, period) %>%
        summarise(value = sum(value)) %>%
        ungroup(),

    direct.fossil.depletion
) %>%
    filter(!is.na(technology), 0 != value) %>%
    group_by(model, scenario, region, technology, tech.variant, phase,
             energy.carrier, unit, period) %>%
    summarise(value = sum(value)) %>%
    ungroup()

ggplot() +
    geom_col(data = fossil.depletion.plot.data %>%
                 filter('World' == region, is.na(tech.variant)) %>%
                 inner_join(
                     inline.data.frame(
                         'scenario;                    period;   x.label',
                         'ADV_WP5_Base_dyn;            2010;     2010',
                         'ADV_WP5_Base_dyn;            2050;     Base',
                         'ADV_WP5_P240_FullTech_dyn;   2050;     FullTech',
                         'ADV_WP5_P240_LimVRE_dyn;     2050;     Conv',
                         'ADV_WP5_P240_NoCCSNucPO;     2050;     NewRE'),
                     c('scenario', 'period')) %>%
                 group_by(x.label, technology, energy.carrier) %>%
                 summarise(value = sum(value)) %>%
                 ungroup(),
             mapping = aes(x = x.label, y = value * 4.1868e-5,
                           fill = technology)) +
    facet_wrap(~energy.carrier, nrow = 1)
