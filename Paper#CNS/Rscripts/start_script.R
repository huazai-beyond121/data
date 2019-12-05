
packrat::restore()

force.tidy.NTNU.data    <- TRUE
force.tidy.IAM.data     <- TRUE

gcv2ncv              <- TRUE         # convert gross to net calorific value
different.pv.shares  <- TRUE         # assume different PV technology variant
                                     #   shares
differnet.bio.shares <- FALSE        # use 2/3 TAX5_begr_betr_rf and 1/3
                                     #   residues for default Bioenergy
                                     #   technology
storage_parameters   <- 'original'   # 'original' or 'updated'

mifall <- "data/advance_compare_20170130-162243.csv"

ntnu   <- "data/NTNU_LCA_2018-12-10.csv"
mat    <- './data/endpointImpactsForPIK.mat'

source("transformation_list.R")
source("order.levels.R")
source("df.2.named.vector.R")

# ---- Process or load NTNU data -----------------------------------------------
# output: NTNU.LCA.coefficients (energy demand, energy service demands),
#         NTNU.impacts (other LCA impacts
if (any(force.tidy.NTNU.data,
        !file.exists("NTNU.LCA.coefficients.Rdata"),
        (file.info("NTNU.LCA.coefficients.Rdata")$mtime <
         max(file.info(ntnu)$mtime, file.info(mat)$mtime)))) {
    source("tidy_NTNU_data.R")
} else {
    load("NTNU.LCA.coefficients.Rdata")
}


# ---- Process or load IAM data ------------------------------------------------
# IAM multimodel analysis of LCA impacts
if (any(force.tidy.IAM.data,
        !file.exists("IAMdata.Rdata"),
        file.info("IAMdata.Rdata")$mtime <
        file.info(mifall)$mtime)) {
    source("tidy_IAM_data.R")
} else {
    load("IAMdata.Rdata")
}

if (!file.exists('iLUC.Rdata')) {
    source('calc_iLUC.R')
}

if (!file.exists('EmiFactorsGAINS.RData')) {
    source('compareLimitsEclipse_v2.R')
}

# calculate absolute impacts, integrate into common data.frame
# "impacts.absolute.IAM"
source("calc_absolute_impacts_IAM.R")

# clear memory and invoke garbage collection
rm(list = ls())
gc()

# calculate means as well as ranges of absolute impacts
source("calc_impacts_IAM_ranges.R")

# plot stacked bars per impact
source("plot_impacts_IAM_stackedBars.R")

# plot per kWh impacts per impact category
source("plot_relative_specific_impacts.R")

source("impact_synthesis_profile.R")

# write data file for supplementary data set
source("WriteOutputs.R")
