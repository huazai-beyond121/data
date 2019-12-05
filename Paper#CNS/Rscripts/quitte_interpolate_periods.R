
quitte.interpolate.periods <- function(df, periods, return.POSIXct = TRUE) {

    if (!is.data.frame(df))
        stop("Need data frame")

    if (!min(c("period", "value") %in% colnames(df)))
        stop("Data frame needs 'period' and 'value' columns")

    require(dplyr,     quietly = TRUE)
    require(tidyr,     quietly = TRUE)
    require(reshape2,  quietly = TRUE)
    require(lubridate, quietly = TRUE)

    # ASCII 'Group Seperator' is used to separate column items
    SUBSEP <- "\035"

    # The 'period' column might hold a numeric vector, a numeric factor, or
    # POSIXct values
    have.POSIXct <- any("POSIXct" == class(df$period))

    # Internally, the function uses integers
    if (is.factor(df$period)) {
        periods.have <- sort(as.numeric(levels(df$period)))
    } else if (have.POSIXct) {
        periods.have <- sort(as.numeric(unique(df$period)))
    } else {
        periods.have <- sort(unique(df$period))
    }

    # Sort periods
    if (any("POSIXct" == class(periods))) {
        # Use intergers instead of POSIXct internally
        periods <- sort(as.numeric(periods))
    } else {
        periods <- sort(periods)
    }

    # Drop all wanted periods that would need to be _extrapolated_
    periods.drop <- c(periods[periods < min(periods.have)],
                      periods[periods > max(periods.have)])

    if (length(periods.drop)) {
        warning(paste("Dropping", length(periods.drop), "outside periods"))
        periods <- setdiff(periods, periods.drop)
    }

    # These periods will have to be computed
    periods.interpolate <- setdiff(periods, periods.have)

    # Convert 'period' column to integer
    if (is.factor(df$period)) {
        df$period <- as.numeric(as.vector(df$period))
    } else {
        df$period <- as.numeric(df$period)
    }

    # Store indices and names of columns to preserve
    col.indices <- which(!colnames(df) %in% c("period", "value"))
    col.names   <- paste(colnames(df)[col.indices], sep = SUBSEP)

    # Create a matrix with periods (for which there is data) on the second
    # dimension and all other columns combined in the first dimension
    a <- df %>%
        unite(col.names, col.indices, sep = SUBSEP) %>%
        acast(formula = ... ~ period)

    # Create a second matrix with the missing periods
    b <- array(data     = NA,
               dim      = c(dim(a)[1],  length(periods.interpolate)),
               dimnames = c(dimnames(a)[1], list(periods.interpolate)))

    # Calculate missing values as weighted averages from surrounding values
    for (p in 1:length(periods.interpolate)) {

        p.val    <- periods.interpolate[p]
        before   <- max(which(periods.have < p.val))
        after    <- min(which(periods.have > p.val))
        p.before <- periods.have[before]
        p.after  <- periods.have[after]

        b[,p] <- ( a[,before] * (p.after - p.val)
                 + a[,after]  * (p.val   - p.before)
        )                     / (p.after - p.before)
    }

    # Expand the matrix with interpolated values to a data frame and restore the
    # preserved column names
    b %>%
        melt() %>%
        separate(Var1, col.names, sep = SUBSEP) %>%
        rename(period = Var2) %>%
        rbind(df) %>%
        tbl_df() -> df

    # Return periods as POSIXct values, if wanted
    if (return.POSIXct) {
        # If this were POSIXct values in the first place, convert them back
        if (have.POSIXct) {
            data.frame(key   = c(periods.have, periods.interpolate),
                       value = as.POSIXct(c(periods.have, periods.interpolate),
                                          origin = "1970-01-01 00:00.00 UTC",
                                          tz = "GMT")
            ) -> periods.lookup
        # If they weren't, do it differently
        } else {
            data.frame(key   = c(periods.have, periods.interpolate),
                       value = ymd_h(paste0(c(periods.have,
                                              periods.interpolate),
                                            "-07-02 12 GMT"))
            ) -> periods.lookup
        }

        df$period <- periods.lookup[match(df$period, periods_lookup$key),"value"]
    }

    return(df)
}
