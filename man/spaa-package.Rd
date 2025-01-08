\name{spaa-package}
\alias{spaa-package}
\alias{spaa}
\docType{package}
\title{
SPecies Association Analysis
}
\description{
Miscellaneous functions for analysis of species association and niche overlap.
}
\details{
\tabular{ll}{
Package: \tab spaa\cr
Type: \tab Package\cr
Version: \tab 0.2.5\cr
Date: \tab 2024-12-23\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
}
\author{
Author: Jinlong Zhang \email{jinlongzhang01@gmail.com}

Maintainer: Jinlong Zhang \email{jinlongzhang01@gmail.com}
}

\keyword{ species }
\keyword{ association }

\examples{

data(testdata)
testdata

data(splist)
splist

### data tranformation
(spmatrix <- data2mat(testdata))
#Species association
sp.assoc(spmatrix)

# Species association between each pair of species
(result <- sp.pair(spmatrix))

#### Niche width and niche overlap
data(datasample)
niche.overlap.boot(datasample[,1:3], method = "levins")
niche.overlap(datasample, method = "levins")
niche.width(datasample[,1:3], method = "shannon")

##example turnover()
plotlab1 <- XYname(4,6)
xxx <- 1:240
dim(xxx) <- c(24, 10)
rownames(xxx) <- plotlab1

### Distance between each pair of plots
ddd <- dist(xxx)

### label matrix
labmat1 <- lab.mat(plotlab1)
yyy <- turnover(labmat1, ddd)

## geodist() example
## Paris
L1 = deg2dec(-2,20,14)
phi1 = deg2dec(48, 50, 11)
## Washington DC
L2 = deg2dec(77,03,56)
phi2 = deg2dec(38,55,17)

##High precision Great Circle distance
geodist(L1, phi1, L2, phi2)

}
