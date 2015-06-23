\name{aggregation}
\alias{aggregation}
\docType{data}
\title{
Aggregation data frame. 
}
\description{
Data frame with the values for aggregating the indicators into groups and weighting them according to different values. This data frame is a simulation to illustrate how an aggregation data frame should be written in order for the function to work. 
}
\usage{data("aggregation")}
\format{
  A data frame with 198 observations on the following 3 variables.
  \describe{
    \item{\code{group}}{A factor with levels \code{East Asia & Pacific} \code{Europe & Central Asia} \code{Latin America & Caribbean} \code{Middle East & North Africa} \code{North America} \code{South Asia} \code{Sub-Saharan Africa}. 
    The countries are aggregated by their global region, according to the World Bank. }
    \item{\code{W1}}{A numeric vector. Weighting of the aggregated groups by SP.URB.TOTL: Urban population.}
    \item{\code{W2}}{A numeric vector. Weighting of the aggregated groups by EN.POP.DNST: Population density (people per sq. km of land area). }
  }
}
\details{
For the function to read the aggregation data frame properly, it must be compiled in the following manner: 

1. The first column must be named "row.names" and it should contain the name, initials, number or code of each unit of the study area. 

2. The second column must be named "group" and it should contain the name, initials, number or code of each group to which each unit of the the study area belongs to. 

3. The following columns must contain the names of each set of the final weighting values. It is preferable that the name of each column is formed by the letter "W" (for Weighting) followed by a number; e.g.: Weighting column #1 = "W1", and so on depending on the number of columns of weighting values there are. You can have as many columns of weighting values as you need.

For the purpose of this example, the aggregation information taken into consideration was the global regions to which each country of the sample belongs to. The final weighting was carried out taking into consideration either the urban population or the population density. 

The name of the file should be "aggregation.csv".
}
\source{
The data was simulated taking into consideration four indicators of the Urban Development data from the
World Bank World Development Indicators. Link: http://data.worldbank.org/topic/urban-development}
\references{
World Bank Group. (2015). World Development Indicators: Urban Development. Available at: http://data.worldbank.org/topic/urban-development
}
\examples{
aggregation <- data(aggregation)
}
\keyword{datasets}
\keyword{aggregation}
