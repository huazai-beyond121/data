
library(quitte)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

load("ADVANCE.Impacts.Absolute.Rdata")
load("NTNU.LCA.coefficients.IAM.Rdata")
load("IAMdata.Rdata")

source("transformation_list.R")

# ---- calculate 2010 global averages ----
impacts.baseline.2010 <- rbind(
    # impacts from operation: global impacts / global electricity production
    inner_join(
        impacts.absolute.IAM %>%
            filter(scenario == "Base",
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   phase == "Operation",
                   period == 2010,
                   is.na(tech.variant)) %>%
            group_by(model, impact, unit) %>%
            summarise(value = sum(value)) %>%
            ungroup(),

        IAM.SE.production %>%
            filter(scenario == "ADV_WP5_Base_dyn",
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   energy.carrier == "Electricity",
                   !is.na(technology),
                   period == 2010) %>%
            group_by(model) %>%
            summarise(production = sum(value)) %>%
            ungroup(),

        by = "model"
    ) %>%
        # x/a / EJ/a = x/EJ
        mutate(value = value / production) %>%
        group_by(impact, unit) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        ungroup(),

    # impacts from construction: global impacts / global lifetime elec. prod.
    inner_join(
        impacts.absolute.IAM %>%
            filter(scenario == "Base",
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   phase == "Construction",
                   period == 2010,
                   is.na(tech.variant)) %>%
            group_by(model, impact, unit) %>%
            summarise(value = sum(value)) %>%
            ungroup(),

        inner_join(
            IAM.Capacities %>%
                filter(scenario == "ADV_WP5_Base_dyn",
                       region %in% levels(NTNU.LCA.coefficients.IAM$region),
                       phase == "Capacity Additions",
                       period == 2010,
                       value > 0) %>%
                select(model, region, technology, new.cap = value),

            IAM.specific.lifetime.electricity.production %>%
                filter(scenario == "ADV_WP5_Base_dyn",
                       region %in% levels(NTNU.LCA.coefficients.IAM$region),
                       period == 2010) %>%
                select(model, region, technology, slep = value),

            by = c("model", "region", "technology")
        ) %>%
            # GW/a * EJ/GW = EJ/a
            group_by(model) %>%
            summarise(lep = sum(new.cap * slep)) %>%
            ungroup(),

        by = "model"
    ) %>%
        # x/a / EJ/a = x/EJ
        group_by(impact, unit) %>%
        summarise(value = mean(value / lep)) %>%
        ungroup()
) %>%
    group_by(impact, unit) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(unit = sub("^([^/]*).*", "\\1/EJ", as.character(unit)))

# ---- calculate 2050 FullTech averages ----
impacts.fulltech.2050 <- rbind(
    # impacts from operation: global impacts / global electricity production
    inner_join(
        impacts.absolute.IAM %>%
            filter(scenario == "FullTech",
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   phase == "Operation",
                   period == 2050,
                   is.na(tech.variant)) %>%
            group_by(model, impact, unit) %>%
            summarise(value = sum(value)) %>%
            ungroup(),

        IAM.SE.production %>%
            filter(scenario == "ADV_WP5_P240_FullTech" ,
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   energy.carrier == "Electricity",
                   !is.na(technology),
                   period == 2050) %>%
            group_by(model) %>%
            summarise(production = sum(value)) %>%
            ungroup(),

        by = "model"
    ) %>%
        # x/a / EJ/a = x/EJ
        mutate(value = value / production) %>%
        group_by(impact, unit) %>%
        summarise(value = mean(value, na.rm = TRUE)) %>%
        ungroup(),

    # impacts from construction: global impacts / global lifetime elec. prod.
    inner_join(
        impacts.absolute.IAM %>%
            filter(scenario == "FullTech",
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   phase == "Construction",
                   period == 2050,
                   is.na(tech.variant)) %>%
            group_by(model, impact, unit) %>%
            summarise(value = sum(value)) %>%
            ungroup(),

        inner_join(
            IAM.Capacities %>%
                filter(scenario == "ADV_WP5_P240_FullTech",
                       region %in% levels(NTNU.LCA.coefficients.IAM$region),
                       phase == "Capacity Additions",
                       period == 2050,
                       value > 0) %>%
                select(model, region, technology, new.cap = value),

            IAM.specific.lifetime.electricity.production %>%
                filter(scenario =="ADV_WP5_P240_FullTech",
                       region %in% levels(NTNU.LCA.coefficients.IAM$region),
                       period == 2050) %>%
                select(model, region, technology, slep = value),

            by = c("model", "region", "technology")
        ) %>%
            # GW/a * EJ/GW = EJ/a
            group_by(model) %>%
            summarise(lep = sum(new.cap * slep)) %>%
            ungroup(),

        by = "model"
    ) %>%
        # x/a / EJ/a = x/EJ
        group_by(impact, unit) %>%
        summarise(value = mean(value / lep)) %>%
        ungroup()
) %>%
    group_by(impact, unit) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(unit = sub("^([^/]*).*", "\\1/EJ", as.character(unit)))

# ---- calculate specific impacts ----
operation.impacts <- inner_join(
    impacts.absolute.IAM %>%
        filter(region %in% levels(NTNU.LCA.coefficients.IAM$region),
               phase == "Operation"),

    IAM.SE.production %>%
        filter(grepl("_dyn", scenario),
               region %in% levels(NTNU.LCA.coefficients.IAM$region),
               energy.carrier == "Electricity",
               !is.na(technology),
               value > 0) %>%
        mutate(scenario = sub("^ADV_WP5(_P240)?_(.*)_dyn", "\\2", scenario)) %>%
        select(model, scenario, region, technology, period, production = value),

    by = c("model", "scenario", "region", "technology", "period")
) %>%
    # x/a / EJ/a = x/EJ
    mutate(value = value / production,
           unit = sub("^([^/]*).*", "\\1/EJ", as.character(unit))) %>%
    select(-production)

construction.impacts <- inner_join(
    impacts.absolute.IAM %>%
        filter(region %in% levels(NTNU.LCA.coefficients.IAM$region),
               phase == "Construction"),

    inner_join(
        IAM.Capacities %>%
            filter(grepl("_dyn", scenario),
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   phase == "Capacity Additions",
                   value > 0) %>%
            mutate(scenario = sub("^ADV_WP5(_P240)_(.*)_dyn", "\\2",
                                  scenario)) %>%
            select(model, scenario, region, technology, period,
                   new.cap = value),

        IAM.specific.lifetime.electricity.production %>%
            filter(grepl("_dyn", scenario),
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   value > 0) %>%
            mutate(scenario = sub("^ADV_WP5(_P240)_(.*)_dyn", "\\2",
                                  scenario)) %>%
            select(model, scenario, region, technology, period, slep = value),

        by = c("model", "scenario", "region", "technology", "period")
        ) %>%
        # GW/a * EJ/GW = EJ/a
        mutate(lep = new.cap * slep) %>%
        select(-new.cap, -slep),

    by = c("model", "scenario", "region", "technology", "period")
) %>%
    # x/a / EJ/a = x/EJ
    mutate(value = value / lep,
           unit = sub("^([^/]*).*", "\\1/EJ", as.character(unit))) %>%
    select(-lep)



# ---- plot global average ----

impacts.specific <-
    rbind(
        operation.impacts,
        construction.impacts) %>%
    filter(!is.na(tech.variant)) %>%
    group_by(model, scenario, region, technology, tech.variant, phase, impact,
             unit, period) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    inner_join(
        IAM.Capacities %>%
            filter(grepl("_dyn", scenario),
                   region %in% levels(NTNU.LCA.coefficients.IAM$region),
                   phase == "Capacity Additions",
                   value > 0) %>%
            mutate(scenario = sub("^ADV_WP5(_P240)_(.*)_dyn", "\\2",
                                  scenario)) %>%
            select(model, scenario, region, technology, period,
                   new.cap = value),

        by = c("model", "scenario", "region", "technology", "period")
    ) %>%
    group_by(model, scenario, technology, tech.variant, impact, unit,
             period) %>%
    summarise(value = sum(value * new.cap) / sum(new.cap)) %>%
    ungroup() %>%
    mutate(region = "World") %>%
    group_by(scenario, region, technology, impact, unit, period) %>%
    summarize(q0   = min(value, na.rm = TRUE),
              q10  = quantile(value, 0.10, na.rm = TRUE),
              q25  = quantile(value, 0.25, na.rm = TRUE),
              q50  = median(value, na.rm = TRUE),
              q75  = quantile(value, 0.75, na.rm = TRUE),
              q90  = quantile(value, 0.90, na.rm = TRUE),
              q100 = max(value, na.rm = TRUE),
              mean = mean(value,na.rm = TRUE)) %>%
    ungroup() %>%
    gather(quantile, value, -scenario, -region, -technology, -impact, -unit,
           -period)  %>%
    factor.data.frame()



# add variable for normalization

x <-     inner_join(impacts.specific,
            #  impacts.baseline.2010,
            impacts.fulltech.2050,
                    by = c("impact", "unit"))


impacts <- c("GHG (excl. CDR)",
  "Sulfur",
  "NOx",
  "Human toxicity",
  "Freshwater ecotoxicity",
  "Freshwater eutrophication",
  "Marine eutrophication",
  "Water Withdrawal",
  "Ionising radiation",
  "Land occupation",
  "Metal depletion",
  "Fossil depletion"

)

x.pl <- x %>%
    filter(impact %in% impacts) %>%
    mutate(impact = factor(impact, levels = impacts, ordered = T))


# use square root to ensure that impact is proportional to the area
x.pl <- x.pl %>%
    mutate(value = sqrt(value.x / value.y)) %>%
    select(-value.x, -value.y) %>%
    spread(quantile, value) %>%
    inner_join(
        TL$technology.names.LCA %>%
            select(-colour),
        by = c("technology" = "IAMC")) %>%
    select(-technology, technology = legend) %>%
    factor.data.frame() %>%
    order.levels(technology = c(
        "Coal w/o CCS", "Coal w/ CCS", "Nuclear", "Hydropower",
        "Gas w/o CCS",  "Gas w/ CCS",  "PV",      "CSP",
        "Bio w/o CCS",  "Bio w/ CCS",  "Wind")) %>%
    filter(scenario == "FullTech", period == 2050) %>% factor.data.frame()

    data.max = max(x.pl$mean)


p <- ggplot(data = x.pl, aes(x = impact)) +
    geom_bar(mapping = aes(y = mean, fill = impact),
             stat = "identity") +
    scale_fill_manual(values = c("#111111","#8B4513", "#FF8C00", "#e31a1c","#fb9a99", "#b2df8a", "#cab2d6",
                                 "#a6cee3","#FFFF00", "#1f78b4", "#33a02c",
                                 "#555555",  "#fdbf6f", "#FFFF00","#33a02c",
                                  "#6a3d9a")) +
    geom_hline(data = data.frame(level = sqrt(c(1, 2, 4, 8)),
                                 label = c("100%", "200%", "400%", "800%")),
               mapping = aes(yintercept = level, linetype = label)) +
    geom_text(aes(label = technology), y= 3.5, x=nlevels(x.pl$technology)/2 + 1) +

    scale_linetype_manual(values = c("solid", "dashed", "dotdash", "dotted")) +
    # geom_pointrange(mapping = aes(ymin = q0, y = q50, ymax = q100)) +
    facet_wrap(~ technology) +
    coord_polar(theta = "x") +
    xlab("") +
    ylab("") +
#    ylim(0,1.01*data.max) +
#    ggtitle("specific impacts relative to 2010 power system") +
    theme_void() +
    theme(strip.text.x = element_blank(),panel.margin = unit(-0.8, units = "cm") )
    # theme(strip.text = element_text(size = 12),
    #       panel.margin = unit(0, units = "cm"))


ggsave("plots/specific_relative_impacts.png", p,
       width = 25, height = 25, units = "cm", dpi = 600 )




source("df.2.named.vector.R")
tech.colours <- df.2.named.vector(TL$technology.names %>%
                                      select(legend, colour))

convert.units <- paste(
    "impact;                   label;                       unit;   new.unit;   factor",
    "Particulate matter;       PM-10 formation;           kg PM10 eq/EJ;      g PM10 eq/MWh;      3.6e-6",
    "Oxidant formation;        Oxidant formation;         kg NMVOC-eq/EJ;     g NMVOC-eq/MWh;      3.6e-6",
    "NOx;                      NOx;                       Mt N2O/EJ;          g N2O/MWh;          3.6e3",
    "Sulfur;                   Sulfur;                    Mt SO2/EJ;          g SO2/MWh;          3.6e3",
    "Human toxicity;           Human toxicity;            kg 1,4-DCB-Eq/EJ;   g 1,4-DCB-eq/MWh;   3.6e-6",
    "Land occupation;          Land occupation;           m2a/EJ;             m2a/MWh;            3.6e-9",
    "Natural land transformation; Natural land transf.;  m2/EJ;        m2/MWh;            3.6e-9",
    "Ionising radiation;       Ionising radiation;        kg U235-Eq/EJ;      kg U235-eq/MWh;      3.6e-9",
    "Ecotoxicity;              Ecotoxicity;               species.yr/EJ;      species.yr/MWh;     3.6e-9",
    "Marine eutrophication;    Marine eutrophication;     kg N-Eq/EJ;         g N-eq/MWh;         3.6e-6",
    "Terrestrial acidification; Terrestrial acid.;        kg SO2eq/EJ;        g SO2eq/MWh;      3.6e-6",
    "Metal depletion; Mineral res. depletion;  kg Fe-eq/EJ;        kg Fe-eq/MWh;        3.6e-9",
    "Aluminium;                Aluminium;                 t Al/EJ;            kg Al/MWh;           3.6e-6",
    "Cement;                   Cement;                    t cement/EJ;        kg Cement/MWh;       3.6e-6",
    "Copper;                   Copper;                    t Cu/EJ;            kg Cu/MWh;           3.6e-6",
    "Iron;                     Iron;                      t Fe/EJ;            kg Fe/MWh;           3.6e-6",
    "Fossil depletion;         Fossil depletion;          kg oil eq/EJ;       kg oil eq/MWh;       3.6e-9",
    "CO2 storage;              CO2 storage;               Mt CO2/EJ;          tCO2/MWh;       3.6e-3",
    "Water Consumption;        Water Consumption;         km3/EJ;             m3/MWh;            3.6",
    "Water Withdrawal;         Water Withdrawal;          km3/EJ;             m3/MWh;            3.6",
    sep = "\n") %>%
    textConnection() %>%
    read.table(header = TRUE, sep = ";", quote = "\"", strip.white = TRUE,
               stringsAsFactors = FALSE) %>%
    mutate(impact.unit = paste0(label,"\n[", new.unit, "]")) %>% factor.data.frame()


plot.data <- impacts.specific %>%
    filter(scenario == "FullTech",
           region == "World",
           period == 2050) %>%
    inner_join(TL$technology.names %>%
                   select(-colour),
               by = c("technology" = "REMIND")
    ) %>%
    select(-technology) %>%
    rename(technology = legend) %>%
    inner_join(convert.units, by = c("impact", "unit")) %>%
    mutate(value = value * factor) %>%
    select(-unit, -factor) %>%
    rename(unit = new.unit) %>%
    spread(quantile, value) %>%
    order.levels(technology = names(tech.colours), impact.unit = convert.units$impact.unit) %>%
    arrange(technology, impact.unit) %>%
    factor.data.frame()

reference.data <- inner_join(
    impacts.baseline.2010 %>%
        rename(`2010 average` = value),

    impacts.fulltech.2050 %>%
        rename(`2050 average` = value),

    by = c("impact", "unit")
) %>%
    filter(unit != "kg CO2-Eq/EJ") %>%
    gather(reference, value, -impact, -unit) %>%
    inner_join(convert.units, by = c("impact", "unit")) %>%
    mutate(value = value * factor) %>%
    select(-unit, -factor) %>%
    rename(unit = new.unit)

plot.data %>% select_(.dots =c("scenario", "region", "impact", "period", "technology", "unit",
                            "mean", "q25", "q75", "q10", "q50", "q90") ) %>%
    openxlsx::write.xlsx( "plots/specific_impacts_box.xlsx")




for (imp in levels(plot.data$impact)) {

    y.label <- plot.data %>%
        select(impact, unit) %>%
        filter(impact == imp) %>%
        getElement("unit") %>%
        as.character() %>%
        first()

    p <- ggplot() +
        geom_boxplot(data = plot.data %>%
                 filter(impact == imp),
                 mapping = aes(x = technology,
                               ymin   = q10,
                               lower  = q25,
                               middle = q50,
                               upper  = q75,
                               ymax   = q90,
                               fill = technology
                               ),
                 alpha = 0.8,
                 stat = "identity") +
        scale_fill_manual(values = tech.colours) +
        geom_hline(
            data = reference.data %>%
                filter(impact == imp),
            mapping = aes(yintercept = value, linetype = reference,
                          colour = reference)) +
        scale_linetype_manual(values = c("dashed", "dotted")) +
        xlab("") +
        ylab(y.label) +
        scale_y_log10() +
#        ggtitle(imp) +
        theme_bw(base_size = 7) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_text(size=7),
              axis.ticks.x = element_blank(),
              legend.key =  element_blank(),
              legend.title = element_blank())

    plot(p)

    fname = paste0("./plots/specific_impacts_box_", sub("\\|","",sub(" ", "_", imp)), ".png")

    ggsave(fname,
           p,
           width = 11, height = 8, units = "cm", dpi = 300)

}



## most significant impacts


plot.data.select = filter(plot.data, impact %in%
                              c( "Particulate matter",
                                 "Human toxicity",
                                 "Land occupation",
                                 "Natural land transformation",
                                 "Fossil depletion",
                                 "Metal depletion")) %>%
    factor.data.frame()

p <- ggplot() +
    geom_boxplot(data = plot.data.select ,
                 mapping = aes(x = technology,
                               ymin   = q10,
                               lower  = q25,
                               middle = q50,
                               upper  = q75,
                               ymax   = q90,
                               fill = technology,
                               color = technology
                 ),
                 alpha = 0.7,
                 stat = "identity") +
    scale_fill_manual(values = tech.colours) +
    scale_color_manual(values = tech.colours) +
    xlab("") +
    #    ylab(y.label) +
    scale_y_continuous(trans = log10_trans(),
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
    #        ggtitle(imp) +
    facet_wrap(~impact.unit, nrow = 1, scales = "free") +
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size=7),
          axis.ticks.x = element_blank(),
          legend.key =  element_blank(),
          #          legend.key.height =   unit(2, "cm"),
          legend.title = element_blank(),
          strip.background = element_blank(),
          legend.position = "bottom")

plot(p)


ggsave("./plots/specific_impacts_sel.png",
       p,
       width = 18, height = 9, units = "cm", dpi = 300)


ggsave("./plots/specific_impacts_sel.pdf",
       p,
       width = 18, height = 9, units = "cm", dpi = 300)


plot.data.select = filter(plot.data, impact %in%
                              c( "Particulate matter",
                                 "Human toxicity",
                                 "Land occupation",
                                 "Natural land transformation",
                                 "Fossil depletion",
                                 "Metal depletion")) %>%
    factor.data.frame()

# c("Aluminium",
#   "Cement", "CO2 storage", "Copper", "Ecotoxicity", "Fossil depletion",
#   "Human toxicity", "Ionising radiation", "Iron", "Land occupation",
#   "Marine eutrophication", "Mineral resource depletion", "Natural land transformation",
#   "NOx", "Oxidant formation", "Particulate matter", "Sulfur", "Terrestrial acidification",
#   "Water Consumption", "Water Withdrawal")

## only selected impacts

p <- ggplot() +
    geom_boxplot(data = plot.data ,
                 mapping = aes(x = technology,
                               ymin   = q10,
                               lower  = q25,
                               middle = q50,
                               upper  = q75,
                               ymax   = q90,
                               fill = technology,
                               color = technology
                 ),
                 alpha = 0.7,
                 stat = "identity") +
    scale_fill_manual(values = tech.colours) +
    scale_color_manual(values = tech.colours) +
    # geom_hline(
    #     data = reference.data %>%
    #         filter(impact == imp),
    #     mapping = aes(yintercept = value, linetype = reference,
    #                   colour = reference)) +
    # scale_linetype_manual(values = c("dashed", "dotted")) +
    xlab("") +
    #    ylab(y.label) +
    scale_y_continuous(trans = log10_trans(),
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
    #        ggtitle(imp) +
    facet_wrap(~impact.unit, ncol = 5, scales = "free") +
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size=7),
          axis.ticks.x = element_blank(),
          legend.key =  element_blank(),
          #          legend.key.height =   unit(2, "cm"),
          legend.title = element_blank(),
          strip.background = element_blank(),
          legend.position = "bottom")

plot(p)


ggsave("./plots/specific_impacts_all.png",
       p,
       width = 18, height = 24, units = "cm", dpi = 300)



