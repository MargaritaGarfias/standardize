standardize <-
function(path) { 
  ## Read files
  path.indicators <- paste(path, "indicators.csv", sep="/")
  col.indindicators <- ncol(read.csv(path.indicators))-1
  indicators <- read.csv(path.indicators, colClasses=c("character", rep("numeric", col.indindicators)), row.names = 1)

  path.normalization <- paste(path, "normalization.csv", sep="/")
  col.normalization <- ncol(read.csv(path.normalization))-1
  normalization <- read.csv(path.normalization, colClasses=c("character", rep("numeric", col.normalization)), row.names = 1)

  path.weighting <- paste(path, "weighting.csv", sep="/")
  col.weighting <- ncol(read.csv(path.weighting))-1
  weighting <- read.csv(path.weighting, colClasses=c("character", rep("numeric", col.weighting)), row.names = 1)
  
  path.aggregation <- paste(path, "aggregation.csv", sep="/")
  col.aggregation <- ncol(read.csv(path.aggregation))-2
  aggregation <- read.csv(path.aggregation, colClasses=c("character", "factor", rep("numeric", col.aggregation)), row.names = 1)

  
  ## Needed functions
  normalize.linear   <- function(indicator, min.value, max.value){
  # normalize
  answer = 
    100 * 
    (indicator - min.value) /
    (max.value - min.value)
  # cut data
  if(answer < 0) {answer=0}
  if(answer > 100) {answer=100}
  return(answer)
  }

  normalize.midpoint <- function(indicator, min.value, max.value, midpoint){
  if(indicator >= midpoint) {
    answer = normalize.linear(indicator = indicator, 
                              min.value = max.value, 
                              max.value = midpoint)
  }
  else {
    answer = normalize.linear(indicator = indicator, 
                              min.value = min.value, 
                              max.value = midpoint)      
  }
  # cut data
  if(answer < 0) {answer=0}
  if(answer > 100) {answer=100}
  return(answer)
  }

  normalize <- function (indicator, min.value, max.value, method = 1, midpoint, values) {
  # error handling
  if(method != 1 & method != 2 & method != 3 & method != 4){
    print('Error: please select a valid method. Options are 1 for linear, 2 midpoint, 3 for percentile or 4 for inverted percentile.')
  }
  
  # linear
  if(method == 1){
    return(normalize.linear(indicator = indicator, 
                            min.value = min.value, 
                            max.value = max.value))
  }
  
  # midpoint
  if(method == 2){
    return(normalize.midpoint(indicator = indicator, 
                              min.value = min.value, 
                              max.value = max.value, 
                              midpoint  = midpoint))
  }
  
  # percentile
  if(method == 3){
    percentile = ecdf(values)
    return(100*percentile(indicator))
  }
  
  # inverted percentile
  if(method == 4){
    percentile = ecdf(values)
    return(-(100-100*percentile(indicator)))
  }
  
  }
  
  
  ## NORMALIZE the indicators table
  # First duplicate the indicators table
  indicators.normalized <- indicators
  
  # Afterwards, overwrite the duplicated table with the
  # normalized values
  for(my.indicator in colnames(normalization)){
    indicators.normalized[,my.indicator] <-
      sapply(X = indicators[,my.indicator],
             FUN = normalize, 
             min.value = normalization["min.value", my.indicator], 
             max.value = normalization["max.value", my.indicator], 
             method    = normalization["method",    my.indicator], 
             midpoint  = normalization["midpoint",  my.indicator], 
             values    = indicators[, my.indicator])
  }
  
  ## WEIGHT the normalized indicators table
    # First, duplicate the indicators.normalized table;
  # but keep only the row.names
  indicators.weighted <- indicators[,0]
  for(new.column in row.names(weighting)){
    indicators.weighted[,new.column] = 0
  }
  
  # Subsequently, write over the empty column the weighted values
  # As you will notice, you can chose which weight to use: w1 or w2
  # In this case, we will use w1
  
  for(my.weighting in colnames(indicators.weighted)){
    for(my.unit in rownames(indicators.weighted)){
      indicators.weighted[my.unit, my.weighting] <-
        weighted.mean(x = indicators.normalized[my.unit,],
                      w = weighting[my.weighting,]  )
      indicators.weighted[my.unit, my.weighting][is.na(indicators.weighted[my.unit, my.weighting])] <- 0
    }
  }

  ## AGGREGATE the weighted indicators' table
  # First, create the table joining the groups of the aggregation table and the weights of 
  # the weighting table. Add an additional empty column where the final results will be
  # saved.
  indicators.aggregated <- expand.grid(levels(aggregation$group),
                                       colnames(aggregation)[2:ncol(aggregation)],
                                       colnames(indicators.weighted))
  colnames(indicators.aggregated) <- c("group", "aggregation", "weighting")
  indicators.aggregated$value <- 0

  # Overwrite the previous table by adding the results: each row by each column. 
  # As you will notice, you get a the table with all the possible combinations of 
  # weighting and aggregations. 
   for(r in 1:nrow(indicators.aggregated)){
    tem = subset(cbind(indicators.weighted, aggregation),
                 group == indicators.aggregated$group[r])
    tem = tem[, c(as.character(indicators.aggregated$aggregation[r]),
                  as.character(indicators.aggregated$weighting[r]))]
    tem = weighted.mean(x = tem[,2], w = tem[,1])
    indicators.aggregated[r,"value"] <- tem
     }


  # Reorder the data.frame by group and aggregation variable and remove the row.names
  indicators.aggregated <- indicators.aggregated[order(indicators.aggregated$group,indicators.aggregated$aggregation),]
  row.names(indicators.aggregated) <- NULL 
  
  ## FINALLY, print final data frame with all the values normalized, weighted and aggregated.
  return(indicators.aggregated)
}
