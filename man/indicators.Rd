\name{indicators}
\alias{indicators}
\docType{data}
\title{
Indicators data frame
}
\description{
Data frame with four Urban Development indicators and their value for different countries (units of the study area). The information for this data frame was taken from the Urban Development data from the World Bank World Development Indicators.
}
\usage{data("indicators")}
\format{
  A data frame with 198 observations on the following 4 variables.
  \describe{
    \item{\code{EN.POP.DNST}}{A numeric vector. Population density (people per sq. km of land area).}
    \item{\code{SH.H2O.SAFE.UR.ZS}}{A numeric vector. Improved water source, urban (\% of urban population with access).}
    \item{\code{SP.URB.GROW}}{A numeric vector. Urban population growth (annual \%).}
    \item{\code{SP.URB.TOTL.IN.ZS}}{A numeric vector. Urban population (\% of total).}
  }
}
\details{
For the function to read the indicators data frame properly, it must be compiled in the following manner: 

1. The first column must be named "row.names" and contain the name or code of each unit of the study area. It can be a name, initials or numbers. 

2. The following columns must have the name or initials of the indicators; and should be preferably filled with a numeric value for each unit of the study area. If there are missing values, the function will ignore them.
   
The name of the file should be "indicators.csv".
}
\source{
The four indicators contained in this data frame were taken from the Urban Development data from the
World Bank World Development Indicators. \link{http://data.worldbank.org/topic/urban-development}}

\references{
World Bank Group. (2015). World Development Indicators: Urban Development. Available at: \link{http://data.worldbank.org/topic/urban-development}
}
\examples{
indicators <- data(indicators)
}
\keyword{datasets}
\keyword{indicators}
