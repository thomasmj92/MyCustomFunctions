# Here's a function to stratify a sample of data, then split that stratified
# sample into separate "training" and "testing" sets, held within one list object.

# Note:  ensure that the inputs for the 'target.var' and 'id.var' arguments are
# wrapped in quotation marks

# This function depends on two packages:  'dplyr' and 'purrr' (both part of the
# 'tidyverse' package group)

StratifySplitTrainTest <- function(data, 
                                   target.var, 
                                   id.var, 
                                   pct.in.training) {
  
  # Identify the column position of the column that holds your 'target.var' data
  target.column.pos <- which(colnames(data) == target.var)
  
  # Determine the size of the target class that has the fewest number of 
  # observations
  num.rows.smallest.class <- min(table(data[, target.column.pos]))
  
  # Use the 'num.rows.smallest.class' value to stratify the data;
  # this will randomly sample 'num.rows.smallest.class' rows from each class
  strat.data <- data %>% 
    dplyr::group_by(eval(parse(text = target.var))) %>% 
    dplyr::sample_n(num.rows.smallest.class) %>% 
    dplyr::ungroup()

  # Drop the junk column resulting from the eval/parse in the chunk above
  strat.data <- strat.data[, -(ncol(strat.data))]
  
  # Create a training dataset by sampling a % ('pct.in.training') of observations
  # for each group within the stratified sample
  train.data <- strat.data %>% 
    dplyr::group_by(eval(parse(text = target.var))) %>% 
    dplyr::sample_n(as.integer(num.rows.smallest.class * pct.in.training)) %>% 
    dplyr::ungroup()
  
  # Drop the junk column resulting from the eval/parse in the chunk above
  train.data <- train.data[, -(ncol(train.data))]
  
  # Hold the id's of the observations that were used for the training dataset
  train.data.ids <- eval(parse(text = paste0("train.data", "$", id.var)))
  
  # Create a small function that is the opposite of '%in%'
  `%not_in%` <- purrr::negate(`%in%`)
  
  # Create a testing dataset that holds the observations from the stratified sample
  # that were NOT used in the training dataset
  test.data <- strat.data %>% 
    dplyr::filter(eval(parse(text = id.var)) %not_in% train.data.ids)
  
  # Create a list object to hold both the training and testing sets
  train.test.ls <- list(train = train.data, 
                        test = test.data)
  
  return(train.test.ls)
  
}






