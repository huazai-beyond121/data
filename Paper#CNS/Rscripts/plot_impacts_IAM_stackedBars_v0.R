# ---- plot base year and 2050 impact by technology results -------------------
switch.plot.multimodel = TRUE
switch.plot.reference.values = FALSE

list.plot.reference.values <- list('Aluminium'             = TRUE,
                                   'Copper'                = TRUE,
                                   'Iron'                  = TRUE,
                                   'Coal depletion'        = TRUE,
                                   'Oil depletion'         = TRUE,
                                   'Natural Gas depletion' = TRUE)

plots  = list()

df.imp.2010.2050.2100 %>%
    filter(is.na(model), is.na(tech.variant))  %>%
    select(-model, -tech.variant) %>%
    openxlsx::write.xlsx( "plots/impacts_IAM_stackedBars.xlsx")

df.imp.fullrange.2010.2050.2100 %>%
    spread(key = quantile, value = value) %>%
    openxlsx::write.xlsx( "plots/impacts_IAM_stackedBars_totals.xlsx")

bar_stats <- df.imp.2010.2050.2100 %>%
    distinct(scenario) %>%
    separate(scenario, c('name', 'period'), sep = ' ', remove = FALSE,
             fill = 'left') %>%
    arrange(period, name) %>%
    mutate(xpos = 1:n(),
           xpos = ifelse('2010' == period, 0.5,
                         ifelse('2100' == period, xpos + 0.5,
                                xpos)),
           alpha = ifelse('2010' == period, 0.1, 1),
           width = ifelse('2010' == period, 0.55, 0.8)) %>%
    select(-name, -period)


for (imp in levels(df.imp.2010.2050.2100$impact)) {

    plot.tech <- df.imp.2010.2050.2100 %>%
        filter(impact == imp,
               value >= 0,
               is.na(model),
               is.na(tech.variant)) %>%
        inner_join(bar_stats, 'scenario') %>%
        select(-model, -tech.variant) %>%
        factor.data.frame() %>%
        order.levels(technology = rev(levels(.$technology))) %>%
        arrange(technology)

    ymax <- plot.tech %>%
        select(-alpha, -width) %>%
        group_by(scenario) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        getElement("value") %>%
        max()

    techcolours <- TL$technology.names.LCA %>%
        filter(legend %in% levels(plot.tech$technology)) %>%
        getElement("colour") %>%
        as.character() %>%
        rev()

    plot.range <- df.imp.fullrange.2010.2050.2100 %>%
        filter(impact == imp) %>%
        inner_join(bar_stats, 'scenario') %>%
        factor.data.frame()

    ymax.t <- plot.tech  %>%
        select(-alpha) %>%
        group_by(scenario) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        getElement("value") %>%
        max()

    if (imp == "GHG (excl. CDR)")
    {
        ymax <- ymax * 1.2
    } else {
        if (max(plot.range$value, na.rm = TRUE) > 1.5 * ymax.t) {
            ymax <- filter(plot.range, quantile == "q75") %>%
                getElement("value") %>%
                max(na.rm = TRUE) * 1.2
        } else {
            ymax <- max(plot.range$value, na.rm = TRUE) * 1.1
        }
    }

    imp.unit = levels(plot.tech$impact.unit)

    p <- ggplot() +
        geom_col(data = plot.tech,
                 mapping = aes(x = xpos, y = value, fill = technology,
                               width = width)) +
        scale_x_continuous(
            breaks = bar_stats$xpos,
            minor_breaks = NULL,
            labels = c('2010' = '2010',
                       Base = 'Base\n2050',
                       FullTech = 'FullTech\n2050',
                       LimVRE    = 'Conv\n2050',
                       NoCCSNucPO = 'NewRE\n2050',
                       Base = 'Base\n2100',
                       FullTech = 'FullTech\n2100',
                       LimVRE    = 'Conv\n2100',
                       NoCCSNucPO = 'NewRE\n2100'), name = NULL) +
        ggtitle(levels(plot.tech$impact.unit)) +
        theme_bw(base_size = 9) +
        xlab("") +
        ylab("") +
        scale_fill_manual(values = techcolours, name = "Technology")

    # plot reference values
    if (all(imp %in% names(list.plot.reference.values),
            list.plot.reference.values[[imp]],
            imp %in% levels(ref.values$impact))) {

        nudge_label <- plot.tech %>%
            group_by(scenario) %>%
            summarise(value = 0.01 *sum(value)) %>%
            getElement('value') %>%
            max()

        p <- p +
            geom_hline(data = ref.values %>%
                           filter(impact == imp),
                       mapping = aes(yintercept = reference),
                       linetype = 'dotdash', size = .7, colour = 'black') +
            geom_text(data = ref.values %>%
                          filter(impact == imp),
                      mapping = aes(x = 5.44, y = reference, label = label),
                      colour = "black", size = 5, hjust = 1, vjust = 0,
                      nudge_y = nudge_label)
    }



    plot(p)

    p <- p +
        geom_boxplot(data = plot.range %>%
                         spread(quantile, value),
                     mapping = aes(x = xpos,
                                   width = 0.16,
                                   group = scenario,
                                   ymin   =   q0,
                                   lower  =  q25,
                                   middle =  q50,
                                   upper  =  q75,
                                   ymax   = q90),
                     stat  = "identity",
                     alpha = 0.5)
    plot(p)

    # standalone figures
    p <- p +
        theme(axis.text.x = element_text(size = 12),
              axis.text.y  = element_text(size = 12),
              legend.title = element_blank())

    # with additional ranges
    p_ranges <- p +
        geom_boxplot(
            data = df.imp.techrange.2010.2050.2100 %>%
                filter(impact == imp,
                       value >= 0,
                       is.na(model)) %>%
                group_by(scenario, impact, impact.unit, unit, quantile) %>%
                summarise(value = sum(value)) %>%
                ungroup() %>%
                inner_join(bar_stats, 'scenario') %>%
                spread(quantile, value),
            mapping = aes(x = xpos - 0.2,
                          width = 0.08,
                          group  = scenario,
                          ymin   =   q0,
                          lower  =  q25,
                          middle =  q50,
                          upper  =  q75,
                          ymax   = q90),
            stat  = "identity", alpha = 0.5, color = 'gray50', fill = 'orchid') +
        geom_boxplot(
            data = df.imp.modelrange.2010.2050.2100 %>%
                filter(impact == imp,
                       value >= 0) %>%
                group_by(scenario, impact, impact.unit, unit, quantile) %>%
                summarise(value = sum(value)) %>%
                ungroup() %>%
                inner_join(bar_stats, 'scenario') %>%
                spread(quantile, value),
            mapping = aes(x = xpos + 0.20,
                          width = 0.08,
                          group  = scenario,
                          ymin   =   q0,
                          lower  =  q25,
                          middle =  q50,
                          upper  =  q75,
                          ymax   = q90),
            stat  = "identity", alpha = 0.5, color = 'gray50',  fill = 'orange')

    ggsave(filename = paste0('./plots/ImpactsDet_', gsub('(\\|| )', '', imp),
                             '_ranges.png'),
           plot = p_ranges +
               coord_cartesian(xlim = c(0.25, bar_stats %>%
                                            filter(grepl('2050', scenario)) %>%
                                            getElement('xpos') %>%
                                            max() + 0.4)),
           width = 20, height = 15, unit = 'cm')

    if (any(grepl('2100$', unique(plot.tech$scenario))) & imp == "CO2 storage") {
        ggsave(filename = paste0('./plots/ImpactsDet_',
                                 gsub('(\\|| )', '', imp), '_2100.png'),
               plot = p, width = 20, height = 15, unit = 'cm')

        ggsave(filename = paste0('./plots/ImpactsDet_',
                                 gsub('(\\|| )', '', imp), '_ranges_2100.png'),
               plot = p_ranges, width = 20, height = 15, unit = 'cm')
    }



    # save raw version for gridded plots
    plots[[imp]] <- p_ranges +
        coord_cartesian(xlim = c(0.25, bar_stats %>%
                                     filter(grepl('2050', scenario)) %>%
                                     getElement('xpos') %>%
                                     max() + 0.25)) +
        theme(text = element_text(size = 8),
              legend.position = "none",
              legend.key.height = unit(0.3, units = "cm"))



    }



# # ---- plot base year and 2050 endpoint by midpoint results ---------------------------------
plots.ep  = list()

bar_stats <- df.imp.2010.2050.2100 %>%
    filter(!grepl('2100$', scenario)) %>%
    distinct(scenario) %>%
    separate(scenario, c('name', 'period'), sep = ' ', remove = FALSE,
             fill = 'left') %>%
    arrange(period, name) %>%
    mutate(xpos = 1:n(),
           xpos = ifelse('2010' == period, 0.5,
                         ifelse('2100' == period, xpos + 0.5,
                                xpos)),
           alpha = ifelse('2010' == period, 0.1, 1),
           width = ifelse('2010' == period, 0.55, 0.8)) %>%
    select(-name, -period)

df.imp.2010.2050.2100.mp2ep %>%
    filter(is.na(model)) %>%
    select(-model) %>%
    openxlsx::write.xlsx( "plots/impacts_IAM_stackedBars_mp2ep.xlsx")

df.imp.fullrange.2010.2050.2100.mp2ep %>%
    spread(key = quantile, value = value) %>%
    openxlsx::write.xlsx( "plots/impacts_IAM_stackedBars_mp2ep_totals.xlsx")

for (imp in levels(df.imp.2010.2050.2100.mp2ep$endpoint)) {

    plot.ep <- df.imp.2010.2050.2100.mp2ep %>%
        filter(endpoint == imp,
               value >= 0,
               !grepl('2100$', scenario),
               is.na(model)) %>%
        inner_join(bar_stats, 'scenario') %>%
        factor.data.frame()

    # determine order of impacts
    imp.ep <- TL$impact.names.LCA %>%
        filter(impact %in% levels(plot.ep$midpoint)) %>%
        getElement("impact") %>%
        as.character() %>%
        rev()

    imp.colours <- TL$impact.names.LCA %>%
        filter(impact %in% imp.ep) %>%
        getElement("colour") %>%
        as.character()  %>%
        rev()

    # order impacts
    plot.ep <- plot.ep %>%
        order.levels(midpoint = imp.ep) %>%
        arrange(midpoint)


    ymax <- plot.ep %>%
        select(-alpha, -width) %>%
        group_by(scenario) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        getElement("value") %>%
        max()

    plot.range <- df.imp.fullrange.2010.2050.2100.mp2ep %>%
        filter(endpoint == imp)

    ymax.t <- plot.ep  %>%
        select(-alpha) %>%
        group_by(scenario) %>%
        summarize(value = sum(value)) %>%
        ungroup() %>%
        getElement("value") %>%
        max()

    p <- ggplot() +
        geom_col(data = plot.ep,
                 mapping = aes(x = xpos, y = value, fill = midpoint,
                               width = width)) +
        scale_x_continuous(
            breaks = bar_stats$xpos,
            minor_breaks = NULL,
            labels = c('2010'     = '2010',
                       Base       = 'Base\n2050',
                       FullTech   = 'FullTech\n2050',
                       LimVRE     = 'Conv\n2050',
                       NoCCSNucPO = 'NewRE\n2050',
                       NULL), name = NULL) +
        ggtitle(levels(plot.ep$endpoint.unit)) +
        theme_bw() +
        xlab("") +
        ylab("") +
        scale_fill_manual(values = imp.colours,
                          name = "",
                          labels = plyr::mapvalues(
                              imp.ep,
                              from = as.character( impact.names$impact),
                              to = as.character(impact.names$label),
                              warn_missing = FALSE))
    plot(p)

    p <- p +
        geom_boxplot(data = plot.range %>%
                         filter(!grepl('2100$', scenario)) %>%
                         spread(quantile, value) %>%
                         inner_join(bar_stats, 'scenario'),
                     mapping = aes(x      = xpos,
                                   width  =  0.2,
                                   group  = scenario,
                                   ymin   =   q0,
                                   lower  =  q25,
                                   middle =  q50,
                                   upper  =  q75,
                                   ymax   = q100),
                     stat  = "identity", alpha = 0.5)
    plot(p)

    # save raw version for gridded plots
    plots.ep[[imp]] =  p + theme(text = element_text(size = 8),
                                 legend.position = "right" ) +
        coord_cartesian(xlim = c(0.25, bar_stats %>%
                                     filter(grepl('2050', scenario)) %>%
                                     getElement('xpos') %>%
                                     max() + 0.25))

    # standalone figures
    p <- p +
        theme_bw(base_size = 7) +
        theme(legend.title = element_blank()) +
        coord_cartesian(xlim = c(0.25, bar_stats %>%
                                     filter(grepl('2050', scenario)) %>%
                                     getElement('xpos') %>%
                                     max() + 0.25))

    fname = paste0("plots/",  "EndpointByMidpdoint_", imp)
    fname = gsub("\\|","", fname)
    fname = gsub(" ","", fname)
    ggsave(filename = paste0(fname, ".png"),
           width=8, height=4.5, unit ="cm")

    ggsave(filename = paste0(fname, ".pdf"),
           width=8, height=4.5, unit ="cm")
}


# multi-impact panels

pmulti<- gridExtra::grid.arrange(
    plots[["Particulate matter"]],
    plots[["Oxidant formation"]],
    plots[[ "Human toxicity"]],
    plots[["Ionising radiation" ]],
    ncol= 2)

ggsave("plots/HumanHealth.pdf",pmulti, width = 15, height =15, unit = "cm")


ggsave("plots/HH_legend.pdf",plots[["Ionising radiation"]] + theme(legend.title = element_blank(),
                                                                   legend.position = "right",
                                                                   panel.grid = element_line(size = 0.3),
                                                                   legend.key.height = unit(0.5, units = "cm"),
                                                                   legend.text= element_text(size = 9)))

pmulti <- gridExtra::grid.arrange(
    plots[["Land occupation" ]],
    plots[["Natural land transformation" ]],
    plots[["Water Withdrawal"]],
    plots[["Terrestrial acidification"]],
    plots[["Ecotoxicity"]],
    plots[["Ecotoxicity"]],
    plots[["Marine eutrophication"]],
    plots[["Freshwater eutrophication"]],
    nrow= 3)

ggsave("plots/EcosystemDamage.pdf",pmulti, width = 21, height =21, unit = "cm")


ggsave("plots/ED_legend.pdf",plots[["Water Withdrawal"]] +  theme(legend.title = element_blank(),
                                                                  legend.position = "right",
                                                                  panel.grid = element_line(size = 0.3),
                                                                  legend.key.height = unit(0.5, units = "cm"),
                                                                  legend.text= element_text(size = 9)))


pmulti <- gridExtra::grid.arrange(
    plots[["Fossil depletion" ]],
    plots[["Mineral resource depletion"]],
    plots[["CO2 storage"]],
    ncol= 2)

ggsave("plots/Ressources.pdf",pmulti, width = 15, height =15, unit = "cm")
#
#
#
#
# png("plots/pollution_wEP.png", width = 20.5, height = 30, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Sulfur"]],
#                         plots[["NOx"]],
#                         plots[[ "Human toxicity"]],
#                         plots[["Freshwater ecotoxicity"]],
#                         plots[["Marine eutrophication"]],
#                         plots[["Freshwater eutrophication"]], ncol= 2)
# dev.off()
#
# png("plots/bulkres.png", width = 20, height = 20, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Aluminium"]], plots[["Cement"]], plots[[ "Copper"]], plots[["Iron"]], ncol= 2)
# dev.off()
#
# png("plots/pollution.png", width = 20.5, height = 30, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Sulfur"]],
#                         plots[["NOx"]],
#                         plots[[ "Human toxicity"]],
#                         plots[["Freshwater ecotoxicity"]],
#                         plots[["Marine eutrophication"]],
#                         plots[["Freshwater eutrophication"]], ncol= 2)
# dev.off()
#
# png("plots/HumanHealth2.png", width = 20.5, height =20, unit = "cm", res = 600)
# gridExtra::grid.arrange(
#                         plots[["Particulate matter"]],
#                         plots[["Oxidant formation"]],
#                         plots[[ "Human toxicity"]],
#                         plots[["Ionising radiation" ]],
#                        ncol= 2)
# dev.off()
#
# png("plots/HumanHealth3.png", width = 15, height =14, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Particulate matter"]],
#                         plots[["Oxidant formation"]],
#                         plots[[ "Human toxicity"]],
#                         plots[[ "Ionising radiation"]],
#                         ncol= 2)
# dev.off()
#
# png("plots/NaturalEnv.png", width = 20.5, height =30, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Land occupation" ]],
#                         plots[["Terrestrial acidification"]],
#                         plots[["Water Withdrawal" ]],
#                         plots[["Ecotoxicity"]],
#                         plots[["Marine eutrophication"]],
#                         plots[["Freshwater eutrophication"]],
#                      ncol= 2)
# dev.off()
#
# png("plots/EcosystmsEnv.png", width = 15, height = 16, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Land occupation" ]],
#                         plots[["Natural land transformation" ]],
#                         plots[["Terrestrial acidification"]],
#                         plots[["Ecotoxicity"]],
#                         plots[["Marine eutrophication"]],
#                         plots[["Freshwater eutrophication"]],
#                         ncol= 2)
# dev.off()
#
# png("plots/EcosystmsEnv2.png", width = 15, height = 20, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Land occupation" ]],
#                         plots[["Natural land transformation" ]],
#                         plots[["Terrestrial acidification"]],
#                         plots[["Ecotoxicity"]],
#                         plots[["Marine eutrophication"]],
#                         plots[["Freshwater eutrophication"]],
#                         plots[["Water Withdrawal"]],
#                         ncol= 2)
# dev.off()
#
# png("plots/EcosystmsEnv3.png", width = 15, height = 20, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Land occupation" ]],
#                         plots[["Natural land transformation" ]],
#                         plots[["Terrestrial acidification"]],
#                         plots[["Ecotoxicity"]],
#                         plots[["Marine eutrophication"]],
#                         plots[["Freshwater eutrophication"]],
#                         plots[["Water Withdrawal"]],
#                         plots[["Water Consumption"]],
#                         ncol= 2)
# dev.off()
#
# png("plots/Resources2.png", width = 15, height =15, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Fossil depletion" ]],
#                         plots[["Metal depletion"]],
#                         plots[["Resource"]],
#                         plots.ep[["Resource"]],
#                         plots[["CO2 storage"]],
#                         plots[["Water Withdrawal" ]],
#                         ncol= 2)
# dev.off()
#
# png("plots/Resources3.png", width = 15, height =14, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Fossil depletion" ]],
#                         plots[["Metal depletion"]],
#                         plots[["CO2 storage"]],
#                         plots[["Water Withdrawal" ]],
#                         ncol= 2)
# dev.off()
#
# png("plots/Resources4.png", width = 15, height =14, unit = "cm", res = 600)
# gridExtra::grid.arrange(plots[["Fossil depletion" ]],
#                         plots[["Metal depletion"]],
#                         plots[["CO2 storage"]],
#                         ncol= 2)
# dev.off()

# # ---- plot all models and time steps  ----------------------------------
if (switch.plot.multimodel) {
    for (imp in levels(df.imp.all$impact)) {
        plot.tmp <- df.imp.all %>%
            # filter for negative values due to wrong POLES reporting of capacity additions
            filter(impact == imp,
                   scenario %in% c("Base", "FullTech", "LimVRE", "NoCCSNucPO"),
                   between(period, 2005, 2050),
                   !is.na(model),
                   is.na(tech.variant)) %>%
            droplevels()

        plot.tmp <- plot.tmp %>%
            order.levels(technology = rev(levels(plot.tmp$technology))) %>%
            arrange(technology)

        techcolours <- TL$technology.names.LCA %>%
            filter(legend %in% levels(plot.tmp$technology)) %>%
            getElement("colour") %>%
            as.character() %>%
            rev()

        p <- ggplot() +
            ggtitle(levels(plot.tmp$impact.unit)) +
            geom_bar( aes(x = period, y = value, fill = technology), plot.tmp, stat = "identity") +
            facet_grid(model ~ scenario) +
            theme_bw() +
            xlab("") +
            ylab("") +
            scale_fill_manual(values = techcolours)

        plot(p)

        fname = paste0("plots/", "ImpactsDetail2_byModelScenario_", imp, ".png")
        fname = gsub("\\|","", fname)
        fname = gsub(" ","", fname)
        ggsave(filename = fname,
               width=25, height=25, unit ="cm")
    }

    imp = "CO2 storage"
    plot.tmp <- df.imp.all %>%
        # filter for negative values due to wrong POLES reporting of capacity additions
        filter(impact == imp,
               scenario %in% c("FullTech", "LimVRE"),
               between(period, 2005, 2100),
               model %in% c("IMAGE", "MESSAGE", "REMIND"),
               !is.na(model),
               is.na(tech.variant)) %>%
        droplevels()

    plot.tmp <- plot.tmp %>%
        order.levels(technology = rev(levels(plot.tmp$technology))) %>%
        arrange(technology)

    techcolours <- TL$technology.names.LCA %>%
        filter(legend %in% levels(plot.tmp$technology)) %>%
        getElement("colour") %>%
        as.character() %>%
        rev()

    p <- ggplot() +
        ggtitle(levels(plot.tmp$impact.unit)) +
        geom_area( aes(x = period, y = value, fill = technology), plot.tmp) +
        facet_grid(model ~ scenario) +
        theme_bw() +
        xlab("") +
        ylab("") +
        scale_fill_manual(values = techcolours)

    plot(p)

    fname = paste0("plots/", "ImpactsDetail2_byModelScenario_", imp, "_2100.png")
    fname = gsub("\\|","", fname)
    fname = gsub(" ","", fname)
    ggsave(filename = fname,
           width=15, height=15, unit ="cm")

}

# ---- compare direct, indirect and total NOx/Sulfur ----
for (i in c("Sulfur", "NOx")) {
    p <- df.imp.2010.2050.2100 %>%
        filter(grepl(i, impact),
               !grepl('2100$', scenario),
               is.na(tech.variant),
               is.na(model)) %>%
        order.levels(technology = rev(TL$technology.names.LCA$legend)) %>%
        ggplot() +
        geom_bar(mapping = aes(x = impact, y = value, fill = technology),
                 stat = "identity") +
        facet_wrap(~ scenario, nrow = 1) +
        scale_fill_manual(values = TL$technology.names.LCA %>%
                              select(legend, colour) %>%
                              df.2.named.vector(),
                          name = NULL) +
        xlab("") +
        ylab("") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 12, angle = 20, hjust = 1),
              axis.text.y  = element_text(size = 12),
              legend.title = element_blank())

    ggsave(paste0("plots/Compare_direct_indirect_total_", i, ".png"), p,
           width=25, height=25, unit ="cm")

    ggsave(paste0("plots/Compare_direct_indirect_total_", i, "_ranges.png"),
           p + geom_boxplot(data = df.imp.fullrange.2010.2050.2100 %>%
                                filter(grepl(i, impact),
                                       !grepl('2100$', scenario)) %>%
                                spread(quantile, value),
                            mapping = aes(x = impact,
                                          width = 0.2,
                                          ymin   =   q0,
                                          lower  =  q25,
                                          middle =  q50,
                                          upper  =  q75,
                                          ymax   = q100),
                            stat  = "identity",
                            alpha = 0.5),
           width=25, height=25, unit ="cm")
}

# ---- compare land use across bio technology variants ----
scenario.mask <- tribble(
    ~scenario,      ~new.scenario,
    'Base',         'Base',
    'FullTech',     'FullTech',
    'LimVRE',       'Conv',
    'NoCCSNucPO',   'NewRE')

for (i in c('Ecosystem|Natural land transformation',
            'Land occupation', 'Natural land transformation')) {

    data.plot <- df.imp.all %>%
        filter(is.na(model),
               2050 == period,
               i == impact,
               grepl('^Bio', technology)) %>%
        inner_join(scenario.mask, 'scenario') %>%
        select(-scenario, scenario = new.scenario) %>%
        mutate(scenario = ifelse(2010 == period, period,
                                 paste(scenario, period))) %>%
        select(scenario, technology, tech.variant, value)

    data.plot <- data.plot %>%
        mutate(tech.variant = ifelse(is.na(tech.variant), 'default mix',
                                     as.character(tech.variant)),
               tech.variant = factor(tech.variant),
               scenario = factor(scenario)) %>%
        order.levels(
            scenario = paste(scenario.mask$new.scenario, '2050'),
            technology = levels(TL$technology.names.LCA$legend),
            tech.variant = c(
                'default mix',
                'TAX30_betr_rf', 'TAX30_begr_betr_rf', 'TAX30_begr_betr_ir',
                'TAX5_betr_rf', 'TAX5_begr_betr_rf', 'TAX5_begr_betr_ir',
                'TAX0_betr_rf',  'TAX0_begr_betr_rf',  'TAX0_begr_betr_ir',
                'residue'))

    nudge.marker <- data.plot %>%
        group_by(scenario, tech.variant) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        getElement('value') %>%
        max() * 0.015

    positions.plot <- data.plot %>%
        distinct(scenario, tech.variant) %>%
        arrange(scenario, tech.variant) %>%
        group_by(scenario) %>%
        ungroup() %>%
        mutate(x = (length(unique(data.plot$tech.variant)) + 2)
               * as.integer(scenario)
               + as.integer(tech.variant))

    data.plot <- data.plot %>%
        inner_join(positions.plot, c('scenario', 'tech.variant'))

    x.labels <- positions.plot %>%
        filter(6 == as.integer(tech.variant)) %>%
        select(scenario, x) %>%
        mutate(scenario = sub(' ', '\n', as.character(scenario)))

    p <- ggplot() +
        geom_col(
            data = data.plot,
            mapping = aes(x = x, y = value, fill = technology)) +
        scale_fill_manual(values = TL$technology.names.LCA %>%
                              select(legend, colour) %>%
                              df.2.named.vector(),
                          name = NULL) +
        geom_point(
            data = positions.plot,
            mapping = aes(x = x, y = 0, shape = tech.variant),
            size = 2,
            position = position_nudge(y = -nudge.marker)) +
        scale_shape_manual(
            values = c(
                'default mix'        = 'square cross',
                'TAX30_betr_rf'      = 'circle',
                'TAX30_begr_betr_rf' = 'square',
                'TAX30_begr_betr_ir' = 'diamond',
                'TAX5_betr_rf'       = 'circle open',
                'TAX5_begr_betr_rf'  = 'square open',
                'TAX5_begr_betr_ir'  = 'diamond open',
                'TAX0_betr_rf'       = 'circle plus',
                'TAX0_begr_betr_rf'  = 'square plus',
                'TAX0_begr_betr_ir'  = 'diamond plus',
                'residue'            = 'cross',
                NULL),
            name = 'Technology Variant') +
        scale_x_continuous(breaks = setNames(x.labels$x, x.labels$scenario)) +
        labs(x = NULL, y = NULL, title = df.imp.all %>%
                 filter(i == impact) %>%
                 distinct(impact.unit) %>%
                 getElement('impact.unit')) +
        theme_bw() +
        theme(axis.text.x = element_text(size = 12), # , angle = 20, hjust = 1),
              axis.text.y  = element_text(size = 12))

    ggsave(gsub('(\\|| )', '', paste0('plots/spread_', i, '_bio.png')), p,
           width = 25, height = 25, unit = 'cm')
}
