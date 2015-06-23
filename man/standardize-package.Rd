\name{standardize-package}
\alias{standardize-package}
\alias{standardize}
\docType{package}
\title{
\packageTitle{standardize}
}
\description{
\packageDescription{standardize}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{standardize}
\packageIndices{standardize}
This is a function that helps to normalize, weight and aggregate variables of different units. The variables are normalized by assigning a "score" from 0 to 100 according to a range of "normal" values for the variable. This ranges can be linear, have a midpoint, be percentiles or inverse percentiles (a.k.a. the less the better). Once the variables have been scored and normalized to the same "units", each variable is weighted according to its importance, which is defined by the user. Finally, all the variables can be aggregated in groups according to any desired variable(s).
}
\author{
\packageAuthor{standardize}

Maintainer: \packageMaintainer{standardize}
}
\keyword{package}
\keyword{standardize}
\keyword{indicators}
\keyword{weighting}
\keyword{normalization}
\keyword{aggregation}

\seealso{
\code{\link[weighted.mean]{weighted.mean}}
}
\examples{

## In order to use this function properly, you must have a folder with four csv's named as follows: 
## indicators.csv
## normalization.csv
## weighting.csv
## aggregation.csv

## For the function to read the INDICATORS data frame properly, it must be compiled in the following manner: 
## 1. The first column must be named "row.names" and contain the name or code of each unit of the study area. 
##    It can be a name, initials or numbers. 
## 2. The following columns must have the name or initials of the indicators; and should be preferably filled 
##    with a numeric value for each unit of the study area. If there are missing values, the function will ignore them.
## 3. The name of the file should be "indicators.csv".

## For the function to read the NORMALIZATION data frame properly, it must be compiled in the following manner: 
## 1. The first column must be named "row.names" and contain four rows named: "min.value", "max.value", "method",
##    and "midpoint", in that order. 
## 2. The following columns must have the same name as the indicators, the same number of columns as 
##    the indicators data frame and must be in the same order. So if one of your indicators (in the indicators data frame) 
##    is called "indicator1", then you must have a column with the same name in the normalizing data frame.
## 3. For filling the data frame, it must be done as follows: 
##     - min.value: the minimum value accepdata frame for the variables.
##     - max.value: the maximum value accepdata frame for the variables.
##     - method: a method for normalization must be chosen. The methods available are: 
##         1: linear, if the min.value and the max.value are fixed; 
##         2: midpoint, if the midpoint between the min.value and the max.value is the ideal, and the closest from the 
##            middle the value falls, the better it is; 
##         3: percentile, if there is no maximum or minimum range, the function will take the min.value and the max.value
##            from the given values (in the indicators data frame\) and normalize them with that information; and 
##         4: inverse percentile, for when a smaller value is a preferred condition and if there is no maximum or minimum
##            range, the function will take the min.value and the max.value from the given values (in the indicators data frame) 
##            and normalize them with that information.
## 4. The name of the file should be "normalization.csv".

## For the function to read the WEIGHTING data frame properly, it must be compiled in the following manner: 
## 1. The first column must be named "row.names" and contain the names of each set of weighting values. 
##    It is preferable that the name of each set of weighting values is formed by the letter "w" (for 
##    weighting\) followed by a number; e.g.: weighting set #1 = "w1", and so on depending on the number 
##    of sets of weighting values there are.
## 2. The following columns must have the same name as the indicators, the same number of columns as 
##    the indicators data frame and must be in the same order. So if one of your indicators (in the indicators data frame) 
##    is called "indicator1", then you must have a column with the same name in the weighting data frame, and it 
##    must contain a numeric value for each variable in each weighting set. You can have as many sets of 
##    weighting values as you need.
## 3. The name of the file should be "weighting.csv".

## For the function to read the AGGREGATION data frame properly, it must be compiled in the following manner: 
## 1. The first column must be named "row.names" and it should contain the name, initials, number 
##    or code of each unit of the study area. 
## 2. The second column must be named "group" and it should contain the name, initials, number 
##    or code of each group to which each unit of the the study area belongs to. 
## 3. The following columns must contain the names of each set of the final weighting values. It is 
##    preferable that the name of each column is formed by the letter "W" (for 
##    Weighting) followed by a number; e.g.: Weighting column #1 = "W1", and so on depending on the number 
##    of columns of weighting values there are. You can have as many columns of weighting values as needed.
## 4. The name of the file should be "aggregation.csv".


## For using the function, you must set your working directory to where the folder is located and in the argument "path",
## type ONLY the name of the folder. Alternatively, you can type "C:/.../NameOfFolder".


## Have ready the four csv's needed in the same folder:
## indicators.csv
## normalization.csv
## weighting.csv
## aggregation.csv

## Set the working directory to where the folder is located
## setwd("C:/...")

## Write the name of the folder in "quotations".
## standardize <- standardize(path = "FolderName")

## The aggregated data frame will be printed in the environment.
}
