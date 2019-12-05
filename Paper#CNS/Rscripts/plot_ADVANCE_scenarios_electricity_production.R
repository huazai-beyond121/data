
library(grid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quitte)


.scens = c("ADV_WP5_Base", "ADV_WP5_P240_FullTech", "ADV_WP5_P240_LimVRE",
  "ADV_WP5_P240_NoCCSNucPO")


# read snapshot
ADVANCE <- read.quitte(mifall, sep = ",", quote = "\"",
                       convert.periods = FALSE) %>%
    filter( !(model %in% c( "WITCH 2014")), scenario %in% c("ADV_WP5_Base", "ADV_WP5_Base_dyn", "ADV_WP5_P240_FullTech",
                                                   "ADV_WP5_P240_FullTech_dyn", "ADV_WP5_P240_LimVRE", "ADV_WP5_P240_LimVRE_dyn",
                                                    "ADV_WP5_P240_NoCCSNucPO",
                                                   "ADV_WP5_P240_NoCCSNucPO_dyn") ) %>%
    tbl_df()


# plot electricity production
plot.tmp <- ADVANCE %>%
    filter(scenario %in% .scens,
           region == "World",
           grepl("^Secondary Energy\\|Electricity\\|", variable),
           between(period, 2005, 2050)) %>%
    extract(variable, "technology",
            "Secondary Energy\\|Electricity\\|(.*)") %>%
    filter(technology %in% getElement(TL$technology.names.LCA, "IAMC")) %>%
    factor.data.frame() %>%
    order.levels(technology = as.character(getElement(TL$technology.names.LCA,
                                                      "IAMC"))) %>%
    arrange(model, scenario, region, period, technology)

levels(plot.tmp$scenario) <- c("Base", "FullTech", "Conv", "NewRE")
levels(plot.tmp$model) <- c("GCAM", "IMAGE", "MESSAGE", "POLES", "REMIND")
# techcolours <- c("#101010", "#b2b2b2", "#804020", "#999959", "#e5e5b2", "#ff33ff",
#                  "#191999", "#005900", "#33ff00",  "#900000", "#337fff",
#                  "#ff8000", "#ffcc00")
#
techcolours <- TL$technology.names.LCA %>% filter(IAMC %in% levels(plot.tmp$technology)) %>%
    getElement("colour") %>% as.character()


p <- ggplot(plot.tmp, aes(x = period, y = value)) +
    geom_area(aes(fill = technology), stat = "identity") +
    #     geom_point(
    #         data = ADVANCE %>%
    #             filter(grepl("dyn", scenario),
    #                    region == "World",
    #                    variable == "Secondary Energy|Electricity",
    #                    between(period, 2005, 2050)) %>%
    #             mutate(scenario = sub(".*_([^_]+)_dyn", "\\1", scenario)) %>%
    #             factor.data.frame()) +
    scale_fill_manual(values = techcolours) +
    scale_x_continuous(breaks = c(2010, 2030, 2050))+
    facet_grid( model ~ scenario) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 9),
          axis.text.y  = element_text(size = 9),
          legend.title = element_blank()) +
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          plot.margin = unit(c(0, 0, 0, 0), "mm"),
          strip.background = element_blank()) +
    xlab("") +
    ylab("EJ/a")

ggsave("plots/ElectricityMix_ADVANCE.png", width = 18, height = 18, unit = "cm")
ggsave("plots/ElectricityMix_ADVANCE.pdf", width = 18, height = 18, unit = "cm")




# plot capacity additions
plot.tmp <- ADVANCE %>%
    filter(scenario %in% TL$scenario.advance$ADVANCE_stat,
           region == "World",
           grepl("^Capacity Additions\\|Electricity\\|", variable),
           between(period, 2005, 2050)) %>%
    extract(variable, "technology",
            "Capacity Additions\\|Electricity\\|(.*)") %>%
    filter(technology %in% getElement(TL$technology.names.LCA, "IAMC")) %>%
    mutate(scenario = sub(".*_([^_]+)_dyn", "\\1", scenario)) %>%
    factor.data.frame() %>%
    order.levels(technology = as.character(getElement(TL$technology.names.LCA,
                                                      "IAMC"))) %>%
    arrange(model, scenario, region, period, technology)

techcolours <- TL$technology.names.LCA %>% filter(IAMC %in% levels(plot.tmp$technology)) %>%
    getElement("colour") %>% as.character()


levels(plot.tmp$scenario) <- c("Base", "FullTech", "Conv", "RE")

    p <- ggplot(plot.tmp, aes(x = period, y = value)) +
    geom_bar(aes(fill = technology), stat = "identity") +
    scale_fill_manual(values = techcolours) +
    facet_grid(scenario ~ model, scales = "free") +
    theme_bw() +
        theme(legend.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "mm")) +
    xlab("") +
    ylab("GW/a")

print(p)
ggsave("plots/CapAdditions_ADVANCE.png", width = 29.7, height = 21, unit = "cm")

# plot capacities
plot.tmp <- ADVANCE %>%
    filter(scenario %in% TL$scenario.advance$ADVANCE_stat,
           region == "World",
           grepl("^Capacity\\|Electricity\\|", variable),
           between(period, 2005, 2050)) %>%
    extract(variable, "technology",
            "Capacity\\|Electricity\\|(.*)") %>%
    filter(technology %in% getElement(TL$technology.names.LCA, "IAMC")) %>%
    mutate(scenario = sub(".*_([^_]+)_dyn", "\\1", scenario)) %>%
    factor.data.frame() %>%
    order.levels(technology = as.character(getElement(TL$technology.names.LCA,
                                                      "IAMC"))) %>%
    arrange(model, scenario, region, period, technology)

techcolours <- TL$technology.names.LCA %>% filter(IAMC %in% levels(plot.tmp$technology)) %>%
    getElement("colour") %>% as.character()


levels(plot.tmp$scenario) <- c("Base", "FullTech", "Conv", "RE")

p <- ggplot(plot.tmp, aes(x = period, y = value)) +
    geom_bar(aes(fill = technology), stat = "identity") +
    scale_fill_manual(values = techcolours) +
    facet_grid(scenario ~ model, scales = "free") +
    theme_bw() +
    theme(legend.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "mm")) +
    xlab("") +
    ylab("GW/a")

print(p)
ggsave("plots/Capacity_ADVANCE.png", width = 29.7, height = 21, unit = "cm")


