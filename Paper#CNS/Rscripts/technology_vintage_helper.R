# ==============================================================================
# technology_vintage_helper.R                            Michaja Pehl 2015-11-11
# ------------------------------------------------------------------------------
# Function to generate a data frame with vintage information for IAM scenario 
# data.  "time.lived" tells you that capacty build in "base.year" is still 
# standing in "period".
# ------------------------------------------------------------------------------
# Inputs: SE.production - data frame with columns "model" and "period" for the 
#                         model specific time horizon
#         lifetimes     - data frame with column "technologies" and one ore 
#                         more columns with model names containing technology 
#                         lifetimes (wide format) 
#         base.year     - time horizon for which to calculate vintage data. 
#                         defaults to seq(2010, 2050, by = 5) 
# ------------------------------------------------------------------------------
# Output: data frame 
#   $ model     : Factor 
#   $ period    : num  
#   $ technology: Factor 
#   $ lifetime  : int  
#   $ base.year : num  
#   $ time.lived: num
# ------------------------------------------------------------------------------

technology_vintage_helper <- function(SE.production, 
                                      lifetimes, 
                                      base.year = seq(2010, 2050, by = 5)) {
    require(dplyr) 
    require(tidyr) 
    
    inner_join( 
        inner_join( 
            expand.grid(model  = levels(SE.production$model), 
                        period = unique(SE.production$period)), 
            
            lifetimes %>% 
                gather(model, lifetime, -technology), 
            
            by = "model" 
        ), 
        
        expand.grid(period    = unique(SE.production$period), 
                    base.year = base.year) %>% 
            mutate(time.lived = period - base.year) %>% 
            filter(time.lived >= 0), 
        
        by = "period" 
    ) %>% 
        filter(time.lived <= lifetime) %>% 
        tbl_df() 
} 
