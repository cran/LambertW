\name{SolarFlares}
\alias{SolarFlares}
\docType{data}
\title{Solar Flares data set}
\description{
This dataset represents the peak gamma-ray intensity of solar flares recorded from Feb, 1980 - Dec, 1989. It was analyzed for power-law properties in Clauset et al. (2009) and comes originally from Dennis et al. (1991). 

Thanks to both authors and co-authors it could be made available in this package.
}
\usage{data(SolarFlares)}
\format{
A numeric vector of with \eqn{T = 12,773} observations.
}
\source{
http://tuvalu.santafe.edu/~aaronc/powerlaws/data.htm

http://adsabs.harvard.edu/abs/1991chxb.book.....D

See also References.
}
\references{
Dennis, B. R.; Orwig, L. E.; Kennard, G. S.; Labow, G. J.; Schwartz, R. A.; Shaver, A. R.; Tolbert, A. K. (1991). ``The Complete Hard X Ray Burst Spectrometer Event List, 1980-1989.'' NASA Technical Memorandum 4332. 
See also http://adsabs.harvard.edu/abs/1991chxb.book.....D

Clauset, A., C.~R. Shalizi, and M.~E.~J. Newman (2009). ``Power-law distributions in empirical data''. SIAM Review 51, 661-703 (2009).
See also http://tuvalu.santafe.edu/~aaronc/powerlaws/
}
\examples{
data(SolarFlares)

plot(density(SolarFlares[SolarFlares < 350]), main = "Original Data (truncated to < 350)")

plot(SolarFlares, pch = ".", cex = 2)
plot(SolarFlares[SolarFlares < 350], pch = ".", cex = 2)

fit = MLE_LambertW(SolarFlares, type = "h", distname = "normal")
summary(fit)

xx = get.input(fit)
plot(xx, pch = ".", cex = 2, main = "Heavy-tails removed")
plot(density(xx), main = "Heavy-tails removed") 

}
\keyword{datasets}


