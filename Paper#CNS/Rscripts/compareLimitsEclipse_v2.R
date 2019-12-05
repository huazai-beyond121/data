#----------------------------
# INITIALISE
#----------------------------
# Load libraries
library(openxlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quitte)


#rm_sectors = c("CUSM", "CHEM" )
rm_sectors = c("" )
# rm_unwanted = c("Sum", "Total" , "Unattributed")
rm_unwanted = c( "Unattributed")

#----------------------------
# READ IN DATA
#----------------------------
# ECLIPSE
# Activities
act_eclipse <- read.xlsx("data/ACTIVITIES_EMF30_aggregated_Ev5a_Nov2015.xlsx", sheet="Air pollutants")
act_eclipse <- act_eclipse %>%
  gather(year, value, -Region, -Sector, -Unit) %>%
  mutate(year=as.numeric(paste(year))) %>%
  rename(region=Region, sector=Sector, unit=Unit) %>%
  mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>%
  filter(!sector %in% c(rm_sectors, rm_unwanted)) %>%
  select(-unit) %>%
  as.data.frame() %>%
    factor.data.frame()

species <- c("SO2", "NOx", "VOC", "BC", "OC", "CO")

cle <- do.call("bind_rows",
               lapply(species,
                      function(s) {
                        out <- read.xlsx("data/EMISSIONS_EMF30_aggregated_Ev5a_CLE_Nov2015.xlsx", sheet=s) %>%
                          gather(year, value, -Region, -Sector) %>%
                          mutate(year=as.numeric(paste(year))) %>%
                          rename(region=Region, sector=Sector) %>%
                          mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>%
                          mutate(variable = s) %>%
                          filter(!sector %in% c(rm_sectors, rm_unwanted))

                        return(out)
                      })) %>%
  mutate(scenario = "CLE")

mfr <- do.call("bind_rows",
               lapply(species,
                      function(s) {
                        out <- read.xlsx("data/EMISSIONS_EMF30_aggregated_Ev5a_MFR_Nov2015.xlsx", sheet=s) %>%
                          gather(year, value, -Region, -Sector) %>%
                          mutate(year=as.numeric(paste(year))) %>%
                          rename(region=Region, sector=Sector) %>%
                          mutate(region = gsub("^\\s+|\\s+$", "", gsub("[0-9]", "", region))) %>%
                          mutate(variable = s) %>%
                          filter(!sector %in% c(rm_sectors, rm_unwanted), region != "Global")
                        return(out)
                      })) %>%
  mutate(scenario = "MFR")

# MFR only has 2030 and 2050, so add CLE values for other years
emi_eclipse <- bind_rows(cle,
               bind_rows(cle %>% filter(year %in% c(2000,2005,2010,2020)) %>% mutate(scenario = "MFR"), mfr)) %>%
  select(region,year,sector,variable,scenario,value)  %>%
    factor.data.frame()

# LIMITS
act_limits <- read.xlsx("data/LIMITS_sector_act_emi.xlsx", sheet="activities") %>%
  rename(region=TIMER_REGION, sector=TIMER, unit=UNIT, year=IDYEARS, value=ACT) %>%
  select(-unit)  %>%
    factor.data.frame()

emi_limits <- read.xlsx("data/LIMITS_sector_act_emi.xlsx", sheet="emissions") %>%
  rename(region=TIMER_REGION, sector=TIMER, unit=UNIT, variable=POLL, year=IDYEARS) %>%
  gather(scenario, value, -region, -sector, -unit, -variable, -year) %>%
  select(region,year,sector,variable,scenario,value)
emi_limits$variable[which(emi_limits$variable == "NOX")] <- "NOx"


emi_limits <-   factor.data.frame(emi_limits)


#----------------------------
# PROCESS DATA
#----------------------------
#-- Activities -------------
act_diff = inner_join(
  act_limits %>% rename(act_LIMITS = value),
  act_eclipse %>% rename(act_ECLIPSE = value),
  by=c("region", "sector", "year")) %>%
  mutate(act_ADiff=act_ECLIPSE-act_LIMITS) %>%
  mutate(act_RDiff=abs(act_ECLIPSE-act_LIMITS)/act_LIMITS)

act_sumSector = act_diff %>%
  filter(sector != "Total") %>%
  select(-act_ADiff,-act_RDiff) %>%
  rename(LIMITS=act_LIMITS) %>%
  rename(ECLIPSE=act_ECLIPSE) %>%
  gather(source, value, -region,-sector,-year) %>%
  group_by(region,year,source) %>%
  summarize(value=sum(value)) %>%
  ungroup()

act_sumRegion = act_diff %>%
    filter(region != "Global") %>%

select(-act_ADiff,-act_RDiff) %>%
    rename(LIMITS=act_LIMITS) %>%
    rename(ECLIPSE=act_ECLIPSE) %>%
    gather(source, value, -region,-sector,-year) %>%
    group_by(sector,year,source) %>%
    summarize(value=sum(value)) %>%
    ungroup()



#-- Emissions --------------
emi_diff = inner_join(
  emi_limits %>% rename(emi_LIMITS = value),
  emi_eclipse %>% rename(emi_ECLIPSE = value),
  by=c("region","sector","variable","scenario","year")) %>%
  mutate(emi_ADiff=emi_ECLIPSE-emi_LIMITS) %>%
  mutate(emi_RDiff=abs(emi_ECLIPSE-emi_LIMITS)/emi_LIMITS)

emi_sumSector = emi_diff %>%
    filter(sector != "Total") %>%
    select(-emi_ADiff,-emi_RDiff) %>%
  rename(LIMITS=emi_LIMITS) %>%
  rename(ECLIPSE=emi_ECLIPSE) %>%
  gather(source, value, -region,-sector,-year,-variable,-scenario) %>%
  group_by(region,year,variable,scenario,source) %>%
  summarize(value=sum(value)) %>%
  ungroup()

emi_sumRegion = emi_diff %>%
    filter(region != "Global") %>%
    select(-emi_ADiff,-emi_RDiff) %>%
  rename(LIMITS=emi_LIMITS) %>%
  rename(ECLIPSE=emi_ECLIPSE) %>%
  gather(source, value, -region,-sector,-year,-variable,-scenario) %>%
  group_by(sector,year,variable,scenario,source) %>%
  summarize(value=sum(value)) %>%
  ungroup()

emi_sumSectorRegion = emi_diff %>%
    filter(sector != "Total",region != "Global") %>%
    select(-emi_ADiff,-emi_RDiff) %>%
  rename(LIMITS=emi_LIMITS) %>%
  rename(ECLIPSE=emi_ECLIPSE) %>%
  gather(source, value, -region,-sector,-year,-variable,-scenario) %>%
  group_by(year,variable,scenario,source) %>%
  summarize(value=sum(value)) %>%
  ungroup()

#-- Shares of sectors in global emissions  -------------------------------

sector.share =  inner_join(emi_sumRegion, emi_sumSectorRegion, by = c("year", "variable", "scenario", "source")) %>%
    mutate (share = value.x / value.y ) %>%
    select(-value.x, -value.y) %>% mutate(string = sprintf("%.1f %%", 100*share))

#-- All differences -------------------------------
all_diff = inner_join(act_diff, emi_diff, by=c("region","year","sector")) %>%
  mutate(EF_LIMITS  = emi_LIMITS/act_LIMITS) %>%
  mutate(EF_ECLIPSE = emi_ECLIPSE/act_ECLIPSE) %>%
  mutate(EF_ADiff=EF_ECLIPSE-EF_LIMITS) %>%
  mutate(EF_RDiff=abs(EF_ECLIPSE-EF_LIMITS)/EF_LIMITS) %>%
  select(region,sector,variable,scenario,year,act_LIMITS,act_ECLIPSE,act_ADiff,act_RDiff,emi_LIMITS,emi_ECLIPSE,emi_ADiff,emi_RDiff,EF_LIMITS,EF_ECLIPSE,EF_ADiff,EF_RDiff)

save(all_diff, file = 'EmiFactorsGAINS.RData')
