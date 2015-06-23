\name{weighting}
\alias{weighting}
\docType{data}
\title{
Weighting data frame
}
\description{
Data frame with values for weighting each normalized indicator. This data frame is a simulation to illustrate how a weighting data frame should be written in order for the function to work. 
}
\usage{data("weighting")}
\format{
  A data frame with 3 observations on the following 4 variables.
  \describe{
    \item{\code{EN.POP.DNST}}{A numeric vector. Population density (people per sq. km of land area).}
    \item{\code{SH.H2O.SAFE.UR.ZS}}{A numeric vector. Improved water source, urban (\% of urban population with access).}
    \item{\code{SP.URB.GROW}}{A numeric vector. Urban population growth (annual \%).}
    \item{\code{SP.URB.TOTL.IN.ZS}}{A numeric vector. Urban population (\% of total).}
  }
}
\details{
For the function to read the weighting data frame properly, it must be compiled in the following manner: 

1. The first column must be named "row.names" and contain the names of each set of weighting values. It is preferred that the name of each set of weighting values is formed by the letter "w" (for weighting) followed by a number; e.g.: weighting set #1 = "w1", and so on depending on the number of sets of weighting values there are.

2. The following columns must have the same name, the same number of columns and must be in the same order as the indicators data frame . So if one of the indicators (in the indicators data frame) is called "indicator1", then there must be a column with the same name in the weighting data frame, and it must contain a numeric value for each variable in each weighting set. There can be as many sets of weighting values as needed.
   
The name of the file should be "weighting.csv".
}
\source{
The data was simulated taking into consideration four indicators of the Urban Development data from the
World Bank World Development Indicators.\link{http://data.worldbank.org/topic/urban-development}}
\references{
World Bank Group. (2015). World Development Indicators: Urban Development. Available at: \link{http://data.worldbank.org/topic/urban-development}
}
\examples{
data(weighting)
}
\keyword{datasets}
\keyword{weighting}
