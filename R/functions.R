#' Provides basic information related to Statistical Analysis
#'
#' @param values A list of values.
#' @return Returns the number of NA values in the \code{values}, the mean of the \code{values}, the range of the \code{values}, the quartile values of the \code{values}, the variance of the \code{values}, and the standard deviation of the \code{values}.
#' @examples
#' sparrow(c(2,3,4,5,7,8,9,10,13,15,17,21,22,23))
#' @export
sparrow <- function(values) {
  sprwNaIf <- if(sum(is.na(values)) > 0) {
    sprwNA <- sum(is.na(values))
  } else {
    sprwNA <- 0
  }
  sprwMeanIf <- if(sprwNA > 0) {
    sprwMeanNA <- mean(vector, na.rm = TRUE)
    vector[is.na(values)] <- sprwMeanNA
    vector <- sort(values)
    sprwMean <- mean(values)
  } else {
    sprwMean <- mean(values)
  }
  sprwRange <- max(values) - min(values)
  sprwQuart <- summary(values)
  sprwVar <- stats::var(values)
  sprwSD <- stats::sd(values)
  sprwCat <- if(sprwNA > 0) {
    cat("# of NAs =", sprwNA, "\nMean =", sprwMean, "\nRange =", sprwRange, "\nQuartiles =\n   Min:", sprwQuart[1], "\n   1st:", sprwQuart[2], "\n   Median:", sprwQuart[3], "\n   3rd:", sprwQuart[5], "\n   Max:", sprwQuart[6], "\nVariance =", sprwVar, "\nStandard Dev. =", sprwSD, "\n\n*** To remove NA values, NA replaced in vector with mean excluding NAs.\n")
  } else {
    cat("# of NAs =", sprwNA, "\nMean =", sprwMean, "\nRange =", sprwRange, "\nQuartiles =\n   Min:", sprwQuart[1], "\n   1st:", sprwQuart[2], "\n   Median:", sprwQuart[3], "\n   3rd:", sprwQuart[5], "\n   Max:", sprwQuart[6], "\nVariance =", sprwVar, "\nStandard Dev. =", sprwSD, "\n")
  }
}

#' Provides an interval estimate for a population mean with a sample mean.
#'
#' @param sampleMean A number representing the mean of a sample.
#' @param size A number representing the number of values in a sample.
#' @param sdev A number representing the standard deviation of a sample.
#' @param interval A number representing the confidence interval estimate of a sample.
#' @return Returns the degrees of freedom based on the \code{size}, the critical value based on the degrees of freedom and the \code{interval}, the standard error of the mean based on the \code{sdev} and the \code{size}, and the confidence interval based on the \code{sampleMean}, the critical values, and the standard error of the mean.
#' @examples
#' sparrowhawk(200, 50, 20, .95)
#' @export
sparrowhawk <- function(sampleMean, size, sdev, interval) {
  hawkDF <- size - 1
  hawkCV <- round(stats::qt((interval + (1 - interval) / 2), hawkDF), 3)
  hawkSEM <- sdev / sqrt(size)
  hawkIntMinus <- sampleMean - (hawkCV * hawkSEM)
  hawkIntPlus <- sampleMean + (hawkCV * hawkSEM)
  cat("Degrees of Freedom =", hawkDF, "\nCritical Value =", hawkCV, "\nStandard Error of the Mean =", hawkSEM, "\nConfidence Interval =", hawkIntMinus, "-", hawkIntPlus, "\n\nWith", (interval*100), "% confidence, the population mean is between", hawkIntMinus, "and", hawkIntPlus, ".\n")
}

#' Determines if the null hypothesis is rejected or if it is failed to be rejected.
#'
#' @param popMean A number representing the mean of a population.
#' @param sampleMean A number representing the mean of a sample.
#' @param sdev A number representing the standard deviation of a sample.
#' @param slvl A number representing the level of significance.
#' @param size A number representing the number of values in a sample.
#' @return Returns the null and alternative hypotheses using the \code{popMean}, the confidence interval using the \code{slvl}, the critical value using the \code{size} and the confidence interval, the Z-Score using the \code{sampleMean}, the \code{popMean}, the \code{sdev}, and the \code{size}, and the P-Value using the Z-Score.
#' @examples
#' sparrowhype(200, 203, 25, .05, 140)
#' sparrowhype(50, 49, 2, .01, 100)
#' @export
sparrowhype <- function(popMean, sampleMean, sdev, slvl, size){
  hypeInt <- 1 - slvl
  hypeCV <- round(stats::qt((hypeInt + (1 - hypeInt) / 2), size - 1), 3)
  hypeZScore <- (sampleMean - popMean) / (sdev / sqrt(size))
  hypePValue <- round(2 * stats::pnorm(-abs(hypeZScore)), 6)
  hypeCat1 <- if(hypeZScore > 0 & hypeZScore > hypeCV) {
    cat("Null: Population Mean ==", popMean, "\nAlternative: Population Mean !=", popMean, "\nConfidence Interval =", hypeInt, "\nCritical Value =", hypeCV, "\nZ-Score =", hypeZScore, "\nP-Value =", hypePValue, "\n\nThe Z-Score", hypeZScore, "is outside the range of", -abs(hypeCV), "and", abs(hypeCV), "and we should reject the null hypothesis.")
  } else if (hypeZScore < 0 & hypeZScore < hypeCV) {
    cat("Null: Population Mean ==", popMean, "\nAlternative: Population Mean !=", popMean, "\nConfidence Interval =", hypeInt, "\nCritical Value =", hypeCV, "\nZ-Score =", hypeZScore, "\nP-Value =", hypePValue, "\n\nThe Z-Score", hypeZScore, "is outside the range of", -abs(hypeCV), "and", abs(hypeCV), "and we should reject the null hypothesis.")
  } else {
    cat("Null: Population Mean ==", popMean, "\nAlternative: Population Mean !=", popMean, "\nConfidence Interval =", hypeInt, "\nCritical Value =", hypeCV, "\nZ-Score =", hypeZScore, "\nP-Value =", hypePValue, "\n\nThe Z-Score", hypeZScore, "is inside the range of", -abs(hypeCV), "and", abs(hypeCV), "and we would fail to reject the null hypothesis.")
  }
}

#' Annual Temperature Data for London from 1800 - 2012
#'
#' A dataset that consists of the annual average temperature for
#' London, England starting in 1800 and ending in 2012.
#'
#' @format A data frame with 213 rows and 2 variables:
#' \describe{
#'   \item{year}{year, in date format}
#'   \item{avgTemp}{average temperature of London, in Celsius}
#' }
#' @source \url{https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data}
"londonYear"
