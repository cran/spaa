\name{niche.overlap.boot}
\alias{niche.overlap.boot}
\title{
Boostrap of niche overlap
}
\description{
Boostrap of niche overlap between species.
}
\usage{
niche.overlap.boot(mat, method = c("levins", "schoener", "petraitis", "pianka", "czech", "morisita"), times = 999, quant = c(0.025, 0.975))
}
\arguments{
  \item{mat}{
A community data matrix with each column for each species, and each row for each plot.
}
  \item{method}{
Index of niche overlap.
}
  \item{times}{
Number of bootstrap to be implemented
}
  \item{quant}{
Quantile of bootstrap results, the confidence intervals.
}
}
\details{
To be added.
}
\value{

a data frame with each row for each pair of species , the columns are \code{"Observed","Boot mean","Boot std","Boot CI1", "Boot CI2", "times"}

}
\references{

Zhang Jin-tun,(2004 ) Quantitative Ecology, Science Press, Beijing

Nicholas J. Gotelli. 2000. Null model analysis of species co-occurrence patterns. Ecology 81:2606-2621. http://esapubs.org/archive/ecol/E081/022/EcoSim%20Help/Niche/NicheOverlap.htm

}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\seealso{
\code{\link{niche.overlap.boot.pair}}
}
\examples{

data(datasample)
niche.overlap.boot(datasample[,1:4], method = "levins")

}

\keyword{ niche}
\keyword{ bootstrap }
\keyword{ overlap }

