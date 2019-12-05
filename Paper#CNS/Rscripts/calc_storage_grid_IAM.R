
library(dplyr)
library(tidyr)
library(ggplot2)

source("read_quitte.R")

load("NTNU.LCA.coefficients.IAM.Rdata")
# load("IAMdata.Rdata")

source("transformation_list.R")

# ---- read storage parameters ----
if ('original' == storage_parameters) {
    STScost <- inner_join(
        read.table("data/storage_grid/STScost.csv", header = TRUE, sep = ";"),

        paste(
            "NTNU.region;   region",
            "AME; AFR",
            "AS;  China",
            "CN;  China",
            "EIT; EU",
            "IN;  India",
            "LA;  Brazil",
            "PAC; Japan",
            "RER; EU",
            "US;  USA",
            sep = "\n") %>%
            textConnection() %>%
            read.table(header = TRUE, sep = ";", strip.white = TRUE),

        by = "region"
    ) %>%
        select(region = NTNU.region, parameter, value) %>%
        tbl_df()
} else if ('updated' == storage_parameters) {
    STScost <- quitte::inline.data.frame(
        'region;   p00;   p01;     p02;      p03;       p10;      p11;       p12;       p20;       p21;       p30',
        'AME;      0;     0e+00;   0.9302;   -0.2270;   0.0098;    0.3393;    0.0825;    0.0034;   -0.1854;    0.0085',
        'AS;       0;     0e+00;   1.0933;   -0.4336;   0.0000;    0.7249;   -0.0660;    0.0665;   -0.1774;   -0.0341',
        'CN;       0;     0e+00;   1.0933;   -0.4336;   0.0000;    0.7249;   -0.0660;    0.0665;   -0.1774;   -0.0341',
        'EIT;      0;     1e-04;   1.1569;   -0.5933;   0.0000;   -0.0081;    0.7310;    0.0384;    0.1626;   -0.0181',
        'IN;       0;     0e+00;   1.0076;   -0.0511;   0.0164;    0.4927;   -0.2087;   -0.0107;   -0.0851;    0.0023',
        'LA;       0;     0e+00;   0.7425;    0.1426;   0.0000;    0.5985;    0.4030;    0.0261;   -0.2831;    0.0588',
        'PAC;      0;     0e+00;   1.2253;   -0.6283;   0.0000;    0.8531;   -0.3281;    0.0369;    0.0000;   -0.0143',
        'RER;      0;     1e-04;   1.1569;   -0.5933;   0.0000;   -0.0081;    0.7310;    0.0384;    0.1626;   -0.0181',
        'US;       0;     0e+00;   0.8188;   -0.2330;   0.0000;    0.2354;    0.5058;    0.0360;   -0.1811;   -0.0119') %>%
        gather(parameter, value, -region)
}

# ---- calculate storage capacity needs ----
IAM.storage <- inner_join(
    IAM.SE.production %>%
        filter(region %in% c("AME", "AS", "CN", "EIT", "IN", "LA", "PAC", "RER",
                             "US"),
               energy.carrier == "Electricity",
               technology %in% c("Wind", "Solar|PV", "Solar|CSP", NA),
               between(period, 2010, 2050)) %>%
        mutate(technology = ifelse(is.na(technology), "Total",
                                   as.character(technology))) %>%
        spread(technology, value) %>%
        group_by(model, scenario, region, period) %>%
        summarise(
            wind.share  = Wind / Total,
            # solar.share = (`Solar|CSP` + `Solar|PV`) / Total,
            pv.share    = `Solar|PV` / Total,
            Total       = Total) %>%
        ungroup(),

    STScost %>%
        spread(parameter, value),

    by = "region"
) %>%
    mutate(
        coefficient
        = p00
        + p10 * wind.share
        + p01 * pv.share
        + p20 * wind.share^2
        + p11 * wind.share   * pv.share
        + p02                * pv.share^2
        + p30 * wind.share^3
        + p21 * wind.share^2 * pv.share
        + p12 * wind.share   * pv.share
        + p03                * pv.share^3,

        # limit to positive values
        coefficient = ifelse(coefficient > 0, coefficient, 0),

        # split to wind/solar by 30/70
        wind = (3/10 * wind.share) / (3/10 * wind.share + 7/10 * pv.share) * coefficient,
        pv   = (7/10 * pv.share)   / (3/10 * wind.share + 7/10 * pv.share) * coefficient,

        # EJ/a / (3.6e-6 EJ/GWh) / (8760 h/a) = GW
        wind = wind * 1.4 * Total / 3.1536e-2,
        pv   = pv   * 1.4 * Total / 3.1536e-2,

        # Robert calulated for 8 hours of short term storage
        # GW * 8h = GWh
        wind = wind * 8,
        pv   = pv   * 8) %>%
    select(model, scenario, region, period, Wind = wind, `Solar|PV` = pv) %>%
    gather(technology, value, Wind, `Solar|PV`) %>%
    mutate(unit = "GWh")

IAM.storage.capacity <- rbind(
    IAM.storage %>%
        mutate(phase = "Cap"),

    IAM.storage %>%
        group_by(model, scenario, region, technology) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value)) / 5,
               value = ifelse(is.na(value) | value < 0, 0, value),
               unit  = "GWh/a",
               phase = "New Cap") %>%
        ungroup()
)

IAM.storage.capacity <- rbind(
    IAM.storage.capacity %>%
        filter(region != "World"),

    IAM.storage.capacity %>%
        group_by(model, scenario, technology, phase, unit, period) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(region = "World")
) %>%
    select(model, scenario, region, technology, phase, unit, period, value) %>%
    factor.data.frame()

IAM.storage.work <- IAM.storage %>%
    # GWh * 200 cycles/a * 3.6e-6 EJ/GWh = EJ/a
    mutate(value = value * 200 * 3.6e-6,
           unit  = "EJ/a")

IAM.storage.work <- rbind(
    IAM.storage.work %>%
        filter(region != "World"),

    IAM.storage.work %>%
        filter(region != "World") %>%
        group_by(model, scenario, technology, unit, period) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(region = "World")
) %>%
    select(model, scenario, region, technology, unit, period, value) %>%
    factor.data.frame()

save(IAM.storage.capacity, IAM.storage.work, file = "IAM.storage.Rdata")

bind_rows(
    IAM.storage.capacity %>%
        unite(technology, technology, phase, sep = '|'),

    IAM.storage.work
) %>%
    write.csv2(paste0('storage_data_', storage_parameters, '.csv'))



# ---- calculate transmission capacity needs -----------------------------------
tmp <- IAM.SE.production %>%
    filter(energy.carrier == "Electricity",
           technology %in% c(NA, "Wind", "Solar|PV", "Solar|CSP"),
           between(period, 2010, 2050)) %>%
    select(model, scenario, region, period, technology, value)

# new grid calculation
IAM.grid <- tmp %>%
    mutate(technology = ifelse(is.na(technology), "Total",
                               as.character(technology))) %>%
    spread(technology, value) %>%
    mutate(Base = Total * 132,
           VRE  = pmax(0,
                            ( Wind * 36
                            + (`Solar|CSP` + `Solar|PV`) * 20
    # factor 1.5 to scale global values for large regions
    # introduce region-specific factor later on
                            ) - 1.1 * Total) * 0.76 * 1.5,
           unit = "TWkm") %>%
    select(-`Solar|CSP`, -`Solar|PV`, -Wind, -Total) %>%
    gather(technology, value, Base, VRE) %>%
    select(model, scenario, region, technology, unit, period, value) %>%
    factor.data.frame()

IAM.grid.capacity <- rbind(
    IAM.grid %>%
        group_by(model, scenario, region, technology) %>%
        arrange(period) %>%
        mutate(value = (value - lag(value)) / 5,
               unit  = "TWkm/a",
               value = ifelse(is.na(value) | value < 0, 0, value)) %>%
        ungroup() %>%
        mutate(phase = "New Cap"),

    IAM.grid %>%
        mutate(phase = "Cap")
)

IAM.grid.capacity <- rbind(
    IAM.grid.capacity %>%
        filter(region != "World"),

    IAM.grid.capacity %>%
        filter(region != "World") %>%
        group_by(model, scenario, technology, phase, unit, period) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(region = "World")
)

save(IAM.grid.capacity, file = "IAM.grid.Rdata")

# ---- plots ----
tech.colours <- c("Wind" = "#337fff", "Solar|CSP" = "#ff8000",
                  "Solar|PV" = "#ffcc80")

plot.theme <- theme_bw() +
    theme(legend.key = element_blank(),
          legend.title = element_blank())

.scenarios <- c("ADV_WP5_Base", "ADV_WP5_P240_FullTech", "ADV_WP5_P240_LimVRE",
                "ADV_WP5_P240_NoCCSNucPO", "ADV_WP5_P240_LimVRENucPO")

pdf("plots/storage_grid_capacities.pdf",
    paper = "a4r", width = 297 / 25.4, height = 21 / 2.54)

for (.region in c("World", "AME", "AS", "CN", "EIT", "IN", "LA", "PAC",
                  "RER", "US")) {
    .scenario <- "ADV_WP5_P240_FullTech"

    p <- IAM.storage.capacity %>%
        filter(scenario == .scenario, region == .region) %>%
        mutate(label = paste0(phase, "acity [", unit, "]")) %>%
        ggplot(aes(x = period, y = value, fill = technology)) +
        geom_bar(stat = "identity") +
        facet_grid(label ~ model, scales = "free_y", drop = FALSE) +
        scale_fill_manual(values = tech.colours) +
        xlab("") +
        ylab("") +
        ggtitle(paste0("Storage (", .region, " - ", .scenario, ")")) +
        plot.theme
    plot(p)

    p <- IAM.storage.capacity %>%
        filter(scenario %in% .scenarios, region == .region, period == 2050) %>%
        mutate(label = paste0(phase, "acity [", unit, "]"),
               scenario = sub("ADV_WP5_(P240_)?(.*)", "\\2", scenario)) %>%
        ggplot(aes(x = scenario, y = value, fill = technology)) +
        geom_bar(stat = "identity") +
        facet_grid(label ~ model, scales = "free_y", drop = FALSE) +
        scale_fill_manual(values = tech.colours) +
        xlab("") +
        ylab("") +
        ggtitle(paste0("Storage (", .region, " - 2050)")) +
        plot.theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    plot(p)

    p <- IAM.grid.capacity %>%
        filter(scenario == .scenario, region == .region) %>%
        mutate(label = paste0(phase, "acity [", unit, "]"),
               scenario = sub("ADV_WP5_(P240_)?(.*)", "\\2", scenario)) %>%
        ggplot(aes(x = period, y = value, fill = technology)) +
        geom_bar(stat = "identity") +
        facet_grid(phase ~ model, scales = "free_y", drop = FALSE) +
        # scale_fill_manual(values = tech.colours) +
        xlab("") +
        ylab("") +
        ggtitle(paste0("Grid (", .region, " - ", .scenario, ")")) +
        plot.theme
    plot(p)

    p <- IAM.grid.capacity %>%
        filter(scenario %in% .scenarios, region == .region, period == 2050) %>%
        mutate(label = paste0(phase, "acity [", unit, "]"),
               scenario = sub("ADV_WP5_(P240_)?(.*)", "\\2", scenario)) %>%
        ggplot(aes(x = scenario, y = value, fill = technology)) +
        geom_bar(stat = "identity") +
        facet_grid(label ~ model, scales = "free_y", drop = FALSE) +
        # scale_fill_manual(values = tech.colours) +
        xlab("") +
        ylab("") +
        ggtitle(paste0("Grid (", .region, " - 2050)")) +
        plot.theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
    plot(p)
}

dev.off()

colours_grid <- TL$technology.names.LCA %>%
    filter(grepl('(Base|VRE) Grid', legend)) %>%
    select(-IAMC) %>%
    df.2.named.vector()

p <- ggplot() +
    geom_boxplot(
        data = IAM.grid.capacity %>%
            filter(scenario %in% .scenarios, 'World' == region) %>%
            inner_join(
                inline.data.frame(
                    'scenario;                  period;   xpos',
                    'ADV_WP5_Base;              2010;     0.5',
                    'ADV_WP5_Base;              2050;     2',
                    'ADV_WP5_P240_FullTech;     2050;     3',
                    'ADV_WP5_P240_LimVRE;       2050;     4',
                    'ADV_WP5_P240_NoCCSNucPO;   2050;     5'),
                c('scenario', 'period')) %>%
            filter(0 != value) %>%
            inner_join(
                inline.data.frame(
                    'phase;     unit;     y.facet',
                    'Cap;       TWkm;     Capacity [PWkm]',
                    'New Cap;   TWkm/a;   Capacity Additions [PWkm/a]'),
                c('phase', 'unit')) %>%
            mutate(technology = paste(technology, 'Grid')),
        mapping = aes(x = xpos, y = value / 1000, fill = technology,
                      group = interaction(xpos, technology)),
        width = 0.85,
        coef = Inf) +
    scale_fill_manual(values = colours_grid, name = 'Grid Component') +
    scale_x_continuous(
        breaks = c(0.5, 2, 3, 4, 5),
        minor_breaks = NULL,
        labels = c('2010'     = '2010',
                   Base       = 'Base\n2050',
                   FullTech   = 'FullTech\n2050',
                   LimVRE     = 'Conv\n2050',
                   NoCCSNucPO = 'NewRE\n2050'),
        name = NULL) +
    facet_wrap(~y.facet, nrow = 1, scales = 'free_y') +
    labs(x = NULL, y = NULL,
         title = 'World Power Grid Capacity and Additions') +
    theme_bw(base_size = 9) +
    theme(axis.text.x  = element_text(size = 12),
          axis.text.y  = element_text(size = 12),
          legend.title = element_blank())

ggsave(filename = './plots/Cap_Add_Grid.png', plot = p, width = 20, height = 15,
       units = 'cm')

p <- ggplot() +
    geom_boxplot(
        data = IAM.storage.capacity %>%
            filter(scenario %in% .scenarios, 'World' == region) %>%
            inner_join(
                inline.data.frame(
                    'scenario;                  period;   xpos',
                    'ADV_WP5_Base;              2010;     0.5',
                    'ADV_WP5_Base;              2050;     2',
                    'ADV_WP5_P240_FullTech;     2050;     3',
                    'ADV_WP5_P240_LimVRE;       2050;     4',
                    'ADV_WP5_P240_NoCCSNucPO;   2050;     5'),
                c('scenario', 'period')) %>%
            filter(0 != value) %>%
            inner_join(
                inline.data.frame(
                    'phase;     unit;     y.facet',
                    'Cap;       GWh;     Capacity [TWh]',
                    'New Cap;   GWh/a;   Capacity Additions [TWh/a]'),
                c('phase', 'unit')) %>%
            group_by(model, xpos, y.facet) %>%
            summarise(value = sum(value) / 1000) %>%
            ungroup(),
        mapping = aes(x = xpos, y = value, group = xpos),
        fill = TL$technology.names.LCA %>%
            filter('Storage' == legend) %>%
            getElement('colour'),
        width = 0.85,
        coef = Inf) +
    scale_x_continuous(
        breaks = c(0.5, 2, 3, 4, 5),
        minor_breaks = NULL,
        labels = c('2010'     = '2010',
                   Base       = 'Base\n2050',
                   FullTech   = 'FullTech\n2050',
                   LimVRE     = 'Conv\n2050',
                   NoCCSNucPO = 'NewRE\n2050'),
        name = NULL) +
    facet_wrap(~y.facet, nrow = 1, scales = 'free_y') +
    labs(x = NULL, y = NULL, title = paste('World Wind and Solar Power',
                                           'Storage Capacity and Additions')) +
    theme_bw(base_size = 9) +
    theme(axis.text.x  = element_text(size = 12),
          axis.text.y  = element_text(size = 12),
          legend.title = element_blank())

ggsave(filename = './plots/Cap_Add_Storage.png', plot = p, width = 20,
       height = 15, units = 'cm')
