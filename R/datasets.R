#' @title Datasets
#' @name datasets
#' @aliases AA SolarFlares
#' @description
#' Collection of datasets in this package.
#' @docType data
#' @importFrom utils data
#' @keywords datasets
#' 
NULL


#' @rdname datasets
#' @name AA
#' @description
#' The Australian athletes dataset (\code{AA}) were collected in a study of how 
#' data on various characteristics of the blood varied with sport body size and 
#' sex of the athlete. Identical to the \code{ais} dataset in the \code{DAAG} package.
#' 
#' @format A data frame with 202 observations on the following 13 variables.
#' \describe{ \item{rcc}{red blood cell count, in \eqn{10^{12} l^{-1}}{}}
#' \item{wcc}{while blood cell count, in \eqn{10^{12}}{} per liter}
#' \item{hc}{hematocrit, percent} \item{hg}{hemaglobin concentration, in g per
#' decaliter} \item{ferr}{plasma ferritins, ng \eqn{dl^{-1}}{}} \item{bmi}{Body
#' mass index, kg \eqn{cm^{-2} 10^2}{}} \item{ssf}{sum of skin folds}
#' \item{pcBfat}{percent Body fat} \item{lbm}{lean body mass, kg}
#' \item{ht}{height, cm} \item{wt}{weight, kg} \item{sex}{a factor with levels
#' \code{f} \code{m}} \item{sport}{a factor with levels \code{B_Ball}
#' \code{Field} \code{Gym} \code{Netball} \code{Row} \code{Swim} \code{T_400m}
#' \code{T_Sprnt} \code{Tennis} \code{W_Polo}} }
#' @references Telford, R.D. and Cunningham, R.B. 1991.  Sex, sport and
#' body-size dependency of hematology in highly trained athletes.  Medicine and
#' Science in Sports and Exercise 23: 788-794.
#' @source 
#' \code{AA} was the basis for the analyses that are reported in 
#' Telford and Cunningham (1991).
#' @keywords datasets
#' 
NULL

#' @rdname datasets
#' @name SolarFlares
#' @description
#' The \code{SolarFlares} data represents the peak gamma-ray intensity of solar 
#' flares recorded from Feb, 1980 - Dec, 1989. It was analyzed for power-law
#' properties in Clauset et al. (2009) and comes originally from Dennis et al.
#' (1991).  Thanks to the authors for giving permission to include the dataset in 
#' this package.
#' 
#' @format A numeric vector with \eqn{12,773} observations.
#' @references 
#' Dennis, B. R.; Orwig, L. E.; Kennard, G. S.; Labow, G. J.;
#' Schwartz, R. A.; Shaver, A. R.; Tolbert, A. K. (1991). ``The Complete Hard X
#' Ray Burst Spectrometer Event List, 1980-1989.'' NASA Technical Memorandum
#' 4332.
#' 
#' Clauset, A., C. R. Shalizi, and M. E. J. Newman (2009). ``Power-law
#' distributions in empirical data''. SIAM Review 51, 661-703 (2009). See also
#' \url{http://tuvalu.santafe.edu/~aaronc/powerlaws/}.
#' 
#' @source 
#' Resources on the \code{SolarFlares} dataset can be found at:
#' \url{http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm}
#' 
#' \url{http://adsabs.harvard.edu/abs/1991chxb.book.....D}
#' 
#' See also References.
#' 
NULL

