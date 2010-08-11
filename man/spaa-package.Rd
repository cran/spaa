\name{spaa-package}
\alias{spaa-package}
\alias{spaa}
\docType{package}
\title{
Species Association Analysis and Niche Overlap
}
\description{
Package for species association analysis and niche overlap
}
\details{
\tabular{ll}{
Package: \tab spaa\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2010-08-11\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
This package will be used in examining species association.

\code{\link{add.col}} adds information from one dataframe to another.

\code{\link{data2mat}} converts field data record to species matrix.

\code{\link{freq.calc}} returns the relative frequency for each species.

\code{\link{plotlowertri}} plots the semimatrix using setted types of points.

\code{\link{plotnetwork}} plots the simple correlation network.

\code{\link{sp.assoc}} gives the total species association results.

\code{\link{sp.pair}} calculates species association between each pair of species.

\code{\link{sub.sp.matrix}} obtains the  subset of a standard species matrix, according to species relative frequency.

\code{\link{niche.overlap}} calculates species niche overlap.

\code{\link{niche.overlap.boot}} bootstrap the spcies niche overlap

\code{\link{niche.width}} calculates species niche width.

}
\author{
Author: Jinlong Zhang \email{jinlongzhang01@gmail.com}

        Qiong Ding \email{dingqiong@ibcas.ac.cn}
		
		Jihong Huang \email{xjhjh@ibcas.ac.cn}
		
Maintainer: Jinlong Zhang \email{jinlongzhang01@gmail.com}
}
\references{

Zhang Qiaoying, Peng Shaolin, Zhang Sumei, Zhang Yunchun, Hou Yuping. (2008). Association of dominant species in Guia hill Municipal Park of Macao. Ecology and Environment. 17:1541-1547 

Guo Zhongling, Ma Yuandan, Zheng Jiping, Liu Wande, Jin Zefeng. (2004). Biodiversity of tree species, their populations'spatial distribution pattern and interspecific association in mixed deciduous broadleaved forest in Changbai Mountains Chinese Journal of Applied Ecology. 15:2013-2018 

Shi Zuomin, Liu Shirong, Cheng Ruimei, Jiang Youxu. (2001). Interspecific association of plant populations in deciduous broad leaved forest in Baotianman. Scientia Silvae Sinicae. 37:30-35 

Zhou XY, Wang BS, Li MG, Zan QJ. (2000). An analysis of interspecific associations in secondary  succession forest communities in Heishiding Nature Reserve, Guangdong Province. Acta Phytoecologica Sinica. 24:332-339

JIAN Minfei, LIU qijing, ZHU du, YOU hai. (2009). Inter-specific correlations among dorminant populations of tree layer species in evergreen broad-leaved forest in Jiulianshan Mountain of subtropical China. Chinese Journal of Plant Ecology. 33:672-680 

Zhang Jin-tun,(2004 ) Quantitative Ecology, Science Press, Beijing

Nicholas J. Gotelli. 2000. Null model analysis of species co-occurrence patterns. Ecology 81:2606-2621. http://esapubs.org/archive/ecol/E081/022/EcoSim%20Help/Niche/NicheOverlap.htm


}

\keyword{ species }
\keyword{ association }

\examples{
data(testdata)
testdata
data(splist)
splist

## adding information
## add genera from dataframe B to dataframe A.
add.col(inputA = testdata, inputB = splist, add = "genera",
according = "species")
## add family from dataframe B to dataframe A.
add.col(inputA = testdata, inputB = splist, add = "family", 
according = "species")


### data tranformation
(spmatrix <- data2mat(testdata))
#Species association
sp.assoc(spmatrix)


# Species association between each pair of species
(result <- sp.pair(spmatrix))

# simple network with positive lines in red and negative lines 
# in blue
plotnetwork(result$Pearson)
title("Pearson Correlation Network")

# The lower matrix plot illustrating Pearson's correlation 
# between each pair of species Note the triangle didn't appeared
# in the plots, but have been added to the legend. This is due 
# to the distribution of data. Be carefull in seletion of intervals.

plotlowertri(result$Pearson, int = 0.5, cex=3, interval = 4, 
pchlist = c(19, 17, 15, 1, 5), size = TRUE)
title("Pearson Correlation Lower Matrix Plot")

## plot lower matrix
## Using BCI data for lower matrix plot
library(vegan)
data(BCI)
## select the top 30 species according to relative frequeny.
sub <- sub.sp.matrix(BCI, common = 30)
## Set the digits to 1
plotlowertri(cor(sub), size = TRUE, cex = 3, digits = 1)

#### Niche width and niche overlap
data(datasample)
niche.overlap.boot(datasample[,1:3], method = "levins")
niche.overlap(datasample, method = "levins")
niche.width(datasample[,1:3], method = "shannon")

}
