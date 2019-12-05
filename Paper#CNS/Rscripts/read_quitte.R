read.quitte <- function(file, sep = ";", quote = "",
                        na.strings = c("UNDF", "NA", "N/A", "n_a"),
                        convert.periods = TRUE) {

    require(reshape2, quietly = TRUE)

    if (convert.periods)
        source("ISOyear.R")

    # Check the header for correct names, periods all in one block and no
    # additional columns after the periods
    header <- read.table(file, header = TRUE, sep = sep, quote = quote,
                         na.strings = na.strings, nrows = 1,
                         check.names = FALSE, strip.white = TRUE)

    colnames(header) <- tolower(colnames(header))
    period_columns   <- grep("^[0-9]{4}$", colnames(header))

    default_columns <- c("model", "scenario", "region", "variable", "unit")

    if (  min(header[1:5] == default_columns) != 1
          & tail(period_columns, n = 1) != length(header)
          & min(period_columns == min(period_columns):max(period_columns)) != 1)
        stop(paste("wrong header in file", file))

    periods <- colnames(header)[period_columns]

    # Check if the last column of df is of class logical, i.e. if it was a
    # propper .mif file with the pointless trailing semi-colon.
    useless_last_column <- tail(colnames(header), 1) == ""

    colClasses <- c(rep("factor", period_columns[1]-1),
                    rep("numeric", length(period_columns)))
    if (useless_last_column)
        colClasses <- c(colClasses, "NULL")

    df <- read.table(file, header = TRUE, sep = sep, quote = quote,
                     na.strings = na.strings, colClasses = colClasses,
                     check.names = FALSE, strip.white = TRUE)

    colnames(df) <- tolower(colnames(df))

    df <- melt(df, measure.vars = periods, variable.name = "period",
               na.rm = TRUE)

    if (convert.periods) {
        ISOyear <- make.ISOyear(2005, 2150, 5)
        df$period <- ISOyear(df$period)
    } else {
        df$period <- as.numeric(as.character(df$period))
    }

    return(df)
}
