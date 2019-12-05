# #### Include libraries and scripts ###########################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(quitte)


# ## load data
load("ADVANCE.Impacts.Absolute.Rdata")

source("transformation_list.R")

# .scenario <-  c("Base", "FullTech", "LimVRE", "NoCCSNucPO", "LimVRENucPO")
.scenario <-  c("Base", "FullTech", "LimVRE", "NoCCSNucPO")

.model <- c("GCAM", "IMAGE", "MESSAGE", "POLES", "REMIND")

impacts_all_df <- inline.data.frame(
    'name;                         label;               category;  Endpoint;      all',
    'Sulfur;                       SO2;                 none;      inventory;     FALSE',
    'NOx;                          NOx;                 none;      inventory;     FALSE',
    'BC;                           BC;                  env;       inventory;     FALSE',
    'CO;                           CO;                  env;       inventory;     FALSE',
    'OC;                           OC;                  env;       inventory;     FALSE',
    'VOC;                          VOC;                 env;       inventory;     FALSE',
    'Particulate matter;           PM;                  env;       HumanHealth;     TRUE',
    'Oxidant formation;            O3 Formation;        env;       HumanHealth;   TRUE',
    'Terrestrial acidification;    Acidification;       env;       Ecosystem;     TRUE',
    'Ionising radiation;           Radiation;           env;       HumanHealth;   TRUE',
    'Human toxicity;               Hum. Toxicity;       env;       HumanHealth;   TRUE',
    'Freshwater ecotoxicity;       Fr. Ecotoxicity;     env;       Ecosystem;     TRUE',
    'Terrestrial ecotoxicity;      Terr. Ecotoxicity;   env;       Ecosystem;     TRUE',
    'Marine ecotoxicity;           Mar. Ecotoxicity;    env;       Ecosystem;     TRUE',
    'Ecotoxicity;                  Ecotoxicity;         env;       Ecosystem;     TRUE',
    'Freshwater eutrophication;    P eutroph.;          env;       Ecosystem;     TRUE',
    'Marine eutrophication;        N eutroph.;          env;       Ecosystem;     TRUE',
    'Water Withdrawal;             Water;               env;       inventory;     TRUE',
    'Land occupation;              Land;                env;       Ecosystem;     TRUE',
    'Mineral resource depletion;   Minerals;            mat;       Resource;      TRUE',
    'Fossil depletion;             Fossils;             mat;       Resource;     TRUE',
    'CO2 storage;                  CCS;                 mat;       inventory;     TRUE',
    'Iron;                         Fe;                  mat;       inventory;     FALSE',
    'Copper;                       Cu;                  mat;       inventory;     FALSE',
    'Aluminium;                    Al;                  mat;       inventory;     FALSE',
    'Cement;                       Cement;              mat;       inventory;     FALSE')

impacts_all <- list(names  = impacts_all_df$name,
                    labels = impacts_all_df$label)

# split into environmental and resource impact categories

imp.cat <- list(
    env = impacts_all_df %>% filter(category == 'env') %>% getElement('name'),
    mat = impacts_all_df %>% filter(category == 'mat') %>% getElement('name'),
    all = impacts_all_df %>% filter(all) %>% getElement('name'))

# reference values from RECIPE-1.11, metals USGS
# read from
ref.values <- paste(
    "impact;                       reference;  label",
    "Freshwater ecotoxicity;       2.62E+01;       World 2000 total",
    "Terrestrial ecotoxicity;      3.61E+01;       World 2000 total",
    "Marine ecotoxicity;           1.50E+01;       World 2000 total",
    "Ecotoxicity;                       545;   10% World 2000 total",
    "Freshwater eutrophication;    1.76E+00;       World 2000 total",
    "Terrestrial acidification;    2.32E+01;   10% World 2000 total",
    "Marine eutrophication;        4.46E+00;   10% World 2000 total",
    "Human toxicity;               1.98E+03;       World 2000 total",
    "Ionising radiation;           8.01E+03;       World 2000 total",
    "Land occupation;              1.88E+02;    5% World 2000 total",
    "Mineral resource depletion;   2.71E+02;   10% World 2000 total",
    "Fossil depletion;             7.84E+02;   10% World 2000 total",
    # "Aluminium;                    8.55E+00;   10% World 2010 total",  #                                        4.50E+00;   10% World 2011 total",
    # "Copper;                       1.91E+00;   10% World 2010 total",
    # "Iron;                         1.44E+02;   10% World 2010 total",
    "Aluminium;                      7.5;      0.025% 2017 Bauxite Reserves",
    "Copper;                         3.95;     0.5% 2017 Copper Reserves",
    "Iron;                         166;        0.2% 2017 Iron Reserves",
    'Coal depletion;               122.345;    0.5% 2017 Coal Reserves',
    'Oil depletion;                 10.015;    0.1% 2017 Oil Reserves',
    'Natural Gas depletion;         69.67;     1% 2017 Natural Gas Reserves',
    "NOx;                          1.25E+01;   10% World 2010 total",
    "SO2;                          1.07E+01;   10% World 2010 total",
    "Sulfur;                       1.07E+01;   10% World 2010 total",
    "total NOx;                    1.25E+01;   10% World 2010 total",
    "total Sulfur;                 1.07E+01;   10% World 2010 total",
    "Particulate matter;           8.55E+00;   10% World 2000 total",
    "Oxidant formation;            3.45E+01;   10% World 2000 total",
    "Cement;                       3.40E+01;    1% World 2011 total",
    "Water Withdrawal;             3.00E+02;   10% World 2011 total",
    sep = "\n") %>%
   textConnection() %>%
    read.table(header = TRUE, sep = ";", strip.white = TRUE)


# rescale impact units  ---------------------------------------------------
rescaled.impacts.absolute <- rbind(
    impacts.absolute.IAM %>% filter(impact == "Aluminium") %>%
        mutate(value = value * 1e-6, unit = "Mt Al"),
    impacts.absolute.IAM %>% filter(impact == "Copper") %>%
        mutate(value = value * 1e-6, unit = "Mt Cu"),
    impacts.absolute.IAM %>% filter(impact == "Iron") %>%
        mutate(value = value * 1e-6, unit = "Mt Fe"),
    impacts.absolute.IAM %>% filter(impact == "Cement") %>%
        mutate(value = value * 1e-6, unit = "Mt Cement"),
    impacts.absolute.IAM %>% filter(impact == "Mineral resource depletion") %>%
        mutate(value = value * 1e-9, unit = "Mt Fe-eq"),
    impacts.absolute.IAM %>% filter(impact == "Fossil depletion") %>%
        mutate(value = value * 4.1868e-5, unit = "EJ"),
    impacts.absolute.IAM %>% filter(impact == "Fossil depletion|Solids") %>%
        mutate(value = value * 4.1868e-5, unit = "EJ",
               impact = 'Coal depletion'),
    impacts.absolute.IAM %>% filter(impact == "Fossil depletion|Liquids") %>%
        mutate(value = value * 4.1868e-5, unit = "EJ",
               impact = 'Oil depletion'),
    impacts.absolute.IAM %>% filter(impact == "Fossil depletion|Gases") %>%
        mutate(value = value * 4.1868e-5, unit = "EJ",
               impact = 'Natural Gas depletion'),
    impacts.absolute.IAM %>% filter(impact == "Particulate matter") %>%
        mutate(value = value * 1e-9, unit = "Mt PM10-eq"),
    impacts.absolute.IAM %>% filter(impact == "Particulate matter|direct") %>%
        mutate(value = value * 1e-9, unit = "Mt PM-10"),
    impacts.absolute.IAM %>% filter(impact == "Particulate matter|indirect") %>%
        mutate(value = value * 1e-9, unit = "Mt PM-10"),
    impacts.absolute.IAM %>% filter(impact == "Oxidant formation") %>%
        mutate(value = value * 1e-9, unit = "Mt NMVOC-eq"),
    impacts.absolute.IAM %>% filter(impact == "Oxidant formation|direct") %>%
        mutate(value = value * 1e-9, unit = "Mt NMVOC-eq"),
    impacts.absolute.IAM %>% filter(impact == "Oxidant formation|indirect") %>%
        mutate(value = value * 1e-9, unit = "Mt NMVOC-eq"),
    impacts.absolute.IAM %>% filter(impact == "Land occupation") %>%
        mutate(value = value * 1e-10, unit = "Mha"),
    impacts.absolute.IAM %>% filter(impact == "Natural land transformation") %>%
        mutate(value = value * 1e-10, unit = "Mha"),
    impacts.absolute.IAM %>% filter(impact == "Deforestation") %>%
        mutate(value = value * 1e-10, unit = "Mha"),
    impacts.absolute.IAM %>% filter(impact == "Freshwater eutrophication") %>%
        mutate(value = value * 1e-9, unit = "Mt P-eq"),
    impacts.absolute.IAM %>% filter(impact == "Marine eutrophication") %>%
        mutate(value = value * 1e-9, unit = "Mt N-eq"),
    impacts.absolute.IAM %>% filter(impact == "Freshwater ecotoxicity") %>%
        mutate(value = value * 1e-9, unit = "Mt 1,4-DCB-eq"),
    impacts.absolute.IAM %>% filter(impact == "Terrestrial ecotoxicity") %>%
      mutate(value = value * 1e-9, unit = "Mt 1,4-DCB-eq"),
    impacts.absolute.IAM %>% filter(impact == "Marine ecotoxicity") %>%
      mutate(value = value * 1e-9, unit = "Mt 1,4-DCB-eq"),
    impacts.absolute.IAM %>% filter(impact == "Human toxicity") %>%
        mutate(value = value * 1e-9, unit = "Mt 1,4-DCB-eq"),
    impacts.absolute.IAM %>% filter(impact == "Marine eutrophication") %>%
        mutate(value = value * 1e-9, unit = "Mt N-eq"),
    impacts.absolute.IAM %>% filter(impact == "Terrestrial acidification") %>%
        mutate(value = value * 1e-9, unit = "Mt SO2-eq"),
    impacts.absolute.IAM %>% filter(impact == "Ionising radiation" ) %>%
        mutate(value = value * 1e-9, unit = "Mt U235-eq"),
    impacts.absolute.IAM %>% filter(impact == "Water Withdrawal") %>%
        mutate(value = value * 1, unit = "km3"),
    impacts.absolute.IAM %>% filter(grepl("^Ecosystem", impact ))  %>%
        mutate(value = value, unit = "species.yr"),
    impacts.absolute.IAM %>% filter(grepl("^HumanHealth", impact )) %>%
        mutate(value = value * 1e-6, unit = "mio DALY"),

    impacts.absolute.IAM %>% filter(grepl("^Resource", impact )) %>%
        mutate(value = value * 1e-9, unit = "bn US$")
) %>%
    factor.data.frame()

rescaled.impacts.absolute <- rbind(
    impacts.absolute.IAM %>%
        filter(!(impact %in% levels(rescaled.impacts.absolute$impact))),

    rescaled.impacts.absolute
) %>%
    mutate(value = value * ifelse(as.character(unit) == "kg CO2-Eq", 1e-9, 1),
           unit = ifelse(as.character(unit) == "kg CO2-Eq", "Mt CO2e",
                         as.character(unit))) %>%
    factor.data.frame()

tech.mapping <- TL$technology.names.LCA %>% filter(legend != "Wind farm")

df.imp <- rescaled.impacts.absolute %>%
    filter(scenario %in% .scenario,
           model %in% .model,
           ( impact %in% c(impact_order,
                           'Oxidant formation|direct',
                           'Oxidant formation|indirect',
                           'Particulate matter|direct',
                           'Particulate matter|indirect',
                           'Fossil depletion|Solids',
                           'Fossil depletion|Liquids',
                           'Fossil depletion|Gases')
           | grepl("^(HumanHealth|Ecosystem|Resource)_?\\|", impact)
           ),
           region == "World") %>%
    order.levels(scenario = .scenario) %>%
    # sum over phase and regions
    group_by(model, scenario, technology, tech.variant, impact, unit,
             period) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    factor.data.frame()


# rename technologies, add impact.unit
df.imp <- df.imp %>%
  character.data.frame(technology, impact, unit) %>%
  inner_join(
    tech.mapping %>%
      select(-colour) %>%
      character.data.frame(),
    by = c("technology" = "IAMC")
  ) %>%
  select(-technology, technology = legend) %>%
  mutate(technology = factor(technology)) %>%
  order.levels(scenario =  .scenario,
               technology = as.character(TL$technology.names.LCA$legend)) %>%
  inner_join(
    rescaled.impacts.absolute %>%
      select(impact, unit) %>%
      character.data.frame() %>%
      distinct() %>%
      mutate(
        impact.unit = ifelse(
          impact == "Land occupation",
          paste0(impact, " [", gsub(" ", "", unit), "]"),
          paste0(impact, " [", gsub(" ", "", unit), "/yr]"))
      ),
    by = c("impact", "unit")
  ) %>%
  factor.data.frame() %>%
  arrange(scenario, impact.unit, technology)

# calculate multi-model means and add to impacts dataframe ----------------
# find number of models with valid results
df.imp.n_models <- df.imp %>%
    distinct(model, impact, technology) %>%
    group_by(model, impact) %>%
    # count number of technologies a model is reporting a certain impact for
    summarise(n_technologies = n()) %>%
    ungroup() %>%
    # fill 0 for models reporting no technologies at all
    complete(crossing(model, impact), fill = list(n_technologies = 0)) %>%
    group_by(impact) %>%
    # subtract number of suspicious models from total number of models
    # models reporting for only one technology are suspicious, too
    # (currently MESSAGE/Water Withdrawal)
    summarise(
        n_models = length(levels(df.imp$model)) - sum(1 >= n_technologies)) %>%
    ungroup()

df.imp.all <- df.imp %>%
    filter(!is.na(value)) %>%
    group_by(scenario, impact, technology, tech.variant, impact.unit, unit,
             period) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    inner_join(df.imp.n_models, 'impact') %>%
    mutate(value = value / n_models,
           model = NA) %>%
    select(-n_models) %>%
    rbind(df.imp) %>%
    factor.data.frame()

# add fake "only bio' impacts
df.imp.all <- bind_rows(
    df.imp.all,

    df.imp.all %>%
        filter(impact %in% c(
            'Ecosystem|Natural land transformation',
            NULL),
            grepl('^Bio', technology)) %>%
        mutate(impact = paste('only bio', impact))
)

# calculate uncertainty ranges --------------------------------------------
# calculate quantiles per model and tech.variant
df.imp.modelrange <- df.imp.all %>%
    filter(is.na(tech.variant)) %>%
    group_by(scenario, impact, technology, impact.unit, unit, period) %>%
    summarize(q0   = min(value, na.rm = TRUE),
              q10  = quantile(value, 0.10, na.rm = TRUE),
              q25  = quantile(value, 0.25, na.rm = TRUE),
              q50  = median(value, na.rm = TRUE),
              q75  = quantile(value, 0.75, na.rm = TRUE),
              q90 = quantile(value, 0.9, na.rm = TRUE),
              q100 = quantile(value, 1.0, na.rm = TRUE),
              mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    gather(key = quantile, value = value, q0, q10, q25, q50, q75, q90, q100, mean )

df.imp.techrange <- df.imp.all %>%
    filter(!is.na(tech.variant)) %>%
    group_by(model, scenario, impact, technology, impact.unit, unit, period) %>%
    summarize(q0   = min(value, na.rm = TRUE),
              q10  = quantile(value, 0.10, na.rm = TRUE),
              q25  = quantile(value, 0.25, na.rm = TRUE),
              q50  = median(value, na.rm = TRUE),
              q75  = quantile(value, 0.75, na.rm = TRUE),
              q90 = quantile(value, 0.9, na.rm = TRUE),
              q100 = quantile(value, 1., na.rm = TRUE),
              mean = mean(value,na.rm = TRUE)) %>%
    ungroup() %>%
    gather(key = quantile, value = value, q0, q10, q25, q50, q75, q90, q100, mean )

df.imp.fullrange <- df.imp.all %>%
    group_by(scenario, impact, technology, impact.unit, unit, period) %>%
    summarize(q0   = min(value, na.rm = TRUE),
              q10  = quantile(value, 0.10, na.rm = TRUE),
              q25  = quantile(value, 0.25, na.rm = TRUE),
              q50  = median(value, na.rm = TRUE),
              q75  = quantile(value, 0.75, na.rm = TRUE),
              q90 = quantile(value, 0.9, na.rm = TRUE),
              q100 = quantile(value, 1.0, na.rm = TRUE),
              mean = mean(value,na.rm = TRUE)) %>%
    ungroup() %>%
    gather(key = quantile, value = value, q0, q10, q25, q50, q75, q90, q100, mean )

# extract 2010, 2050 and 2100 numbers ---------------------------------------
df.imp.2010.2050.2100 <- rbind(
        df.imp.all %>%
            filter(scenario == .scenario[1] & period == 2010) %>%
            mutate(scenario = "2010") %>%
            select(-period),
        df.imp.all %>%
            filter(period %in%  c(2050, 2100)) %>%
            unite(scenario, scenario, period, sep = ' '))  %>%
    factor.data.frame()

df.imp.2010.2050.2100.agg <- df.imp.2010.2050.2100 %>%
    filter(is.na(tech.variant), is.na(model)) %>%
    group_by(scenario, impact,  impact.unit, unit) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    mutate(quantile = "mean")

df.imp.fullrange.2010.2050.2100.tech <- rbind(
    df.imp.fullrange %>%
        filter(scenario == .scenario[1] & period == 2010) %>%
        mutate(scenario = "2010") %>%
        select(-period),
    df.imp.fullrange %>%
        filter(period %in% c(2050, 2100)) %>%
        unite(scenario, scenario, period, sep = ' '))  %>%
    factor.data.frame()

df.imp.fullrange.2010.2050.2100 <- df.imp.fullrange.2010.2050.2100.tech %>%
    group_by(scenario, impact, impact.unit, unit, quantile) %>%
    summarize(value = sum(value)) %>%
    ungroup() %>%
    factor.data.frame()

df.imp.techrange.2010.2050.2100 <- rbind(
    df.imp.techrange %>%
        filter(scenario == .scenario[1] & period == 2010) %>%
        mutate(scenario = '2010') %>%
        select(-period),

    df.imp.techrange %>%
        filter(period %in% c(2050, 2100)) %>%
        unite(scenario, scenario, period, sep = ' ')
) %>%
    factor.data.frame()

df.imp.modelrange.2010.2050.2100 <- rbind(
    df.imp.modelrange %>%
        filter(scenario == .scenario[1] & period == 2010) %>%
        mutate(scenario = '2010') %>%
        select(-period),

    df.imp.modelrange %>%
        filter(period %in% c(2050, 2100)) %>%
        unite(scenario, scenario, period, sep = ' ')
) %>%
    factor.data.frame()

# calculate endpoint by midpoint results ----------------------------------
df.imp.2010.2050.2100.mp2ep <- df.imp.2010.2050.2100 %>%
    filter(grepl("^(HumanHealth|Ecosystem|Resource)\\|", impact),
		is.na(tech.variant)) %>%
    select(-tech.variant, -impact.unit) %>%
    separate(impact, c("endpoint", "midpoint"), sep = "\\|", extra = 'drop') %>%
    group_by_(.dots = setdiff(colnames(.), c("technology", "value"))) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(endpoint.unit = paste0(endpoint, " [", unit, "]")) %>%
    factor.data.frame()

df.imp.fullrange.2010.2050.2100.mp2ep <- df.imp.fullrange.2010.2050.2100 %>%
    filter(grepl("^(HumanHealth|Ecosystem|Resource)\\|", impact)) %>%
    select( -impact.unit) %>%
    separate(impact, c("endpoint", "midpoint"), sep = "\\|", extra = 'drop') %>%
    group_by_(.dots = setdiff(colnames(.), c("midpoint", "value"))) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    factor.data.frame()
