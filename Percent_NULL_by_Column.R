#################################################################
#################################################################
###                                                           ###
### Function to find % of NA rows in each column of a dataset ###
###                                                           ###
#################################################################
#################################################################

# **IMPORTANT**
# MAKE SURE YOU DO NOT HAVE ANY FACTOR VARIABLES IN YOUR DATASET

# Either define "import strings as factors", or remove levels by:
# as.numeric(levels(variable))[variable]

# Load package for creating clean data.table to viz results
library(data.table)

# Function (only argument is the dataframe you want to use this with)
get.NA.percent.by.col <- function(my.df) {
  NA.percent.by.col <- my.df
  NA.percent.by.col[] <- lapply(NA.percent.by.col, as.character)
  NA.percent.by.col[NA.percent.by.col=="NULL"] <- NA  # In case data was brought in from SQL Server, etc.
  NA.percent.by.col <- as.data.frame(colMeans(is.na(NA.percent.by.col)))
  setDT(NA.percent.by.col, keep.rownames = TRUE)[]
  colnames(NA.percent.by.col)[1] <- "Column.Name"
  colnames(NA.percent.by.col)[2] <- "Percentage.Missing"
  NA.percent.by.col$Percentage.Complete <- 1 - NA.percent.by.col$Percentage.Missing
  return(NA.percent.by.col)
}

# End
