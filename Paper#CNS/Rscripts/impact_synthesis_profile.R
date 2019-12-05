

impact.names = matrix(c(
    "Climate change", "              #111111", "Clim. change ",
    "Particulate matter", "          #111111", "PM-10",
    "Human toxicity", "              #e31a1c", "Human tox.",
    "Ionising radiation", "          #FFFF00", "Ionizing\nradiation",
    "Oxidant formation", "           #FF8C00", "Oxidant\nformation",
    "Ozone depletion", "             #6a3d9a", "O3 depl.",
    "Natural land transformation", " #8B4513", "Nat. land\ntransformation",
    "Agricultural and urban land occupation", " #33a02c", "Land\noccupation",
    "Land occupation", " #33a02c", "Land\noccupation",
    "Terrestrial acidification", "   #eeCC00", "Terr. acid.",
    "Terrestrial ecotoxicity", "     #b2df8a", "Terr. ecotox.",
    "Freshwater ecotoxicity", "      #fb9a99", "Freshwater\necotox.",
    "Marine ecotoxicity", "          #cab2d6", "Marine\necotox.",
    "Metal depletion", "  #dd7000", "Mineral\nresource depletion",
    "Freshwater eutrophication", "   #e31a1c", "Freshwater\neutrophication",
    "Marine eutrophication", "   #e31a1c", "Marine\neutrophication.",
    "Fossil depletion", "            #555555", "Fossil depletion",
    "Water Withdrawal", "             #dd7000", "Water\nwithdrawal"),
    nrow = 3, dimnames = list(c("impact","color", "label")))

impact.names <- impact.names %>% t() %>% as.data.frame()



scens =   c('Base', 'LimVRE', 'NoCCSNucPO') # c('Base', 'FullTech', 'LimVRE', 'NoCCSNucPO')
newscens = c('Base',   'Conv', 'NewRE')                   # c('Base', 'FullTech', 'LimVRE', 'NoCCSNucPO') #
# ---- arrange plot data ----
imp.order <- c(
    "Particulate matter",
    "Oxidant formation",
    "Human toxicity",
    "Ionising radiation",
    "Natural land transformation",
    "Land occupation",
    "Ecotoxicity",
    "Marine eutrophication",
    "Freshwater eutrophication",
    "Terrestrial acidification",
    # "total Sulfur",
    # "total NOx",
    "Water Withdrawal",
    "Metal depletion",
    "Fossil depletion")

imp.order = rev(imp.order)


d_plot <- rbind(
    df.imp.fullrange.2010.2050.2100 %>%
        filter(quantile != "mean"),

    df.imp.2010.2050.2100.agg
) %>%
    filter(scenario %in% paste(scens, '2050'),
           impact %in% imp.order) %>%
    mutate(scenario = as.character(scenario)) %>%
    inner_join(
        data_frame(scenario = paste(scens, '2050'),
                   new.scen = newscens),
        'scenario') %>%
    select(-scenario, scenario = new.scen) %>%
    factor.data.frame()


d_plot_radar <- inner_join(
    d_plot,

    d_plot %>%
        filter(scenario == 'Base', quantile == 'mean') %>%
        select(-scenario, -quantile, base = value),

    c('impact', 'impact.unit', 'unit')
) %>%
    mutate(value = value / base) %>%
    select(-base) %>%
    filter(quantile %in% c('q25', 'mean', 'q75')) %>%
    select(scenario, impact, quantile, value) %>%
    order.levels(impact = imp.order) %>%
    spread(quantile, value)


ggplot() +
    # coord_polar(start = -pi/length(imp.order)) +
    geom_rect(xmin = 2, xmax = 4, ymin=1, ymax =2, fill = 'green')+
    # expand_limits(x = c(0, 5), y = c(0, 3))
    geom_rect(
        data = d_plot_radar %>%
            filter('Base' != scenario) %>%
            inner_join(data_frame(
                impact = imp.order,
                x      = 1:length(imp.order)),
                'impact') %>%
            mutate( x = x ),
        mapping = aes(xmin = x - 0.5, xmax = x+0.5,  ymin = q25, ymax = q75, fill = scenario, group = scenario),
        width = 1,
        alpha = 0.3,
        show.legend = F) +
    #    geom_ribbon( ymin = 1, ymax =2 ) +
    scale_fill_manual(values = c('Base' = 'black',
                                 'FullTech' = 'green',
                                 'Conv' = 'red',
                                 'NewRE'   = 'blue')) +

    geom_errorbar(
        data = d_plot_radar %>%
            select(scenario, impact, mean) %>%
            inner_join(data_frame(
                impact     = imp.order,
                x      = 1:length(imp.order)),
                'impact') %>%
            mutate( x = x),
        mapping = aes(x = x, ymin = mean, ymax = mean, colour = scenario, group = scenario),
        size = 1,
        alpha = 0.8, width = 1) +
    scale_colour_manual(values = c('Base' = 'black',
                                   'FullTech' = '#009900',
                                   'Conv' = '#990000',
                                   'NewRE'   = '#000099'),
                        name = "") +
    scale_x_continuous(breaks = 1:length(imp.order),
                      expand = c(0,0),

                       labels = plyr::mapvalues(imp.order,
                                                from = as.character( impact.names$impact),
                                                to = as.character(impact.names$label))) +
    scale_y_log10(breaks = c(0.02, 0.05, 0.1,0.2, 0.5, 1, 2, 5, 10),
                       labels = paste0(c(0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10) * 100, '%'),
                        limits = (c(0.02,10)),
                       oob=scales::rescale_none) +
    theme_bw(base_size = 8) +
    theme(# axis.text.y = element_blank(),
           axis.ticks.x = element_blank(),
          axis.title = element_blank(),
 #         axis.text.x = element_text(vjust = 0.5, hjust = 1, angle = 90),   # element_text(vjust = 0, angle = (seq(length(imp.order),1, by = -1))*360/length(imp.order)),
          # panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
         legend.position = "bottom",
#          panel.border = element_blank(),
          #          panel.background = element_rect(fill = "#FFEBAD"),
          panel.background = element_blank()) +
    coord_flip()

ggsave('./plots/impacts_profile.png', last_plot(),
       width = 8, height = 13, units = 'cm', dpi = 600, scale = 1.)
ggsave('./plots/impacts_profile.pdf', last_plot(),
       width = 8, height = 13, units = 'cm', dpi = 600, scale = 1.)
