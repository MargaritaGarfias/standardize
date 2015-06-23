\name{normalization}
\alias{normalization}
\docType{data}
\title{
Normalization data frame. 
}
\description{
Data frame with values for normalizing each indicator. This data frame is a simulation to illustrate how a normalization data frame should be written in order for the function to work. 
}
\usage{data("normalization")}
\format{
  A data frame with 4 observations on the following 4 variables.
  \describe{
    \item{\code{EN.POP.DNST}}{A numeric vector. Population density (people per sq. km of land area).}
    \item{\code{SH.H2O.SAFE.UR.ZS}}{A numeric vector. Improved water source, urban (\% of urban population with access).}
    \item{\code{SP.URB.GROW}}{A numeric vector. Urban population growth (annual \%).}
    \item{\code{SP.URB.TOTL.IN.ZS}}{A numeric vector. Urban population (\% of total).}
  }
}
\details{
For the function to read the normalization data frame properly, it must be compiled in the following manner: 

1. The first column must be named "row.names" and contain four rows named: "min.value", "max.value", "method", and "midpoint", in that order. 

2. The following columns must have the same name as the indicators, the same number of columns as the indicators data frame and must be in the same order. So if one of your indicators (in the indicators data frame) is called "indicator1", then you must have a column with the same name in the normalizing data frame.

3. For filling the data frame, it must be done as follows: 
    
    - min.value: the minimum value accepdata frame for the variables.
    
    - max.value: the maximum value accepdata frame for the variables.
    
    - method: a method for normalization must be chosen. The methods available are: 
      
        1: linear, if the min.value and the max.value are fixed; 
        
        2: midpoint, if the midpoint between the min.value and the max.value is the ideal, and the closest from the 
           middle the value falls, the better it is; 
        
        3: percentile, if there is no maximum or minimum range, the function will take the min.value and the max.value from the given values (in the indicators data frame) and normalize them with that information; and 
        
        4: inverse percentile, for when a smaller value is a preferred condition and if there is no maximum or minimum range, the function will take the min.value and the max.value from the given values (in the indicators data frame) and normalize them with that information.
           
The name of the file should be "normalization.csv".
}
\source{
The data was simulated taking into consideration four indicators of the Urban Development data from the
World Bank World Development Indicators. Link: http://data.worldbank.org/topic/urban-development}
\references{
World Bank Group. (2015). World Development Indicators: Urban Development. Available at: http://data.worldbank.org/topic/urban-development
}
\examples{
normalization <- data(normalization)
}
\keyword{datasets}
\keyword{normalization}
