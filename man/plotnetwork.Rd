\name{plotnetwork}
\alias{plotnetwork}
\title{
Correlation network plot
}
\description{
This function could be used to plot correlation network, with less than 15 sites (recommended). The points lie in a circle with lines connected. Blue lines indicate negative values and the red ones the positive ones.
}
\usage{
plotnetwork(datainput, interval = 8, xlim = c(-2.5,5), 
ylim=c(-3.2,3.2), lty = c(1,2,3,4,4,3,2,1,5), value = "r", 
legendx = 3, legendy = 0, right = 1.2, intcept = 0.22, 
left = 0.35, linelength = 0.3, cex = 3, lwd = 1.5, 
show.legend = TRUE, digits = 2, dit = 1.2, 
number.label = TRUE, text.label = TRUE, 
linecol = c("red", "black"), ...)
}
\arguments{
  \item{datainput}{
The correlation matrix, ex. Pearson's correlation matrix
}
  \item{interval}{
Number of intervals of the values, indicating how to partition the range of input data 
}
  \item{xlim}{
The x range of the plot. The users need not to change it.
}
  \item{ylim}{
The y range of the plot. The users need not to change it.
}
  \item{lty}{
Line styles used in connection lines for each interval. Must have number of elements + 1.
}
  \item{value}{
Value of the matrix, should be a character, will used in legend.
}
  \item{legendx}{
Legend position x.
}
  \item{legendy}{
Legend position y.
}
  \item{right}{
Legend position adjustment parameter.
}
  \item{intcept}{
Legend position adjustment. can be used to specify the row space between each line for the legend.
}
  \item{left}{
Legend position adjustment parameter.
}
  \item{linelength}{
Line length in the legend. 
}
  \item{cex}{
Point size for each circle.
}
  \item{lwd}{
Line width for each circle.
}
  \item{\dots}{
Other arguments to be passed from. 
}
  \item{show.legend}{
Whether the legend should be drawn.
}
  \item{digits}{
Number of digits displayed in legend.
}
  \item{dit}{
Distance of text labels from each corner.
}
  \item{number.label}{
Whether the number label should be drawn.
}
  \item{text.label}{
Whether the text label should be drawn.
}
  \item{linecol}{
Colors of the lines speciefied. The positive correlation will be drawn in the first color specified.
}

}
\details{
This function could be used to plot connections for less than 20 sites 
( above 20 are not recommend since there would be too many connections, 
and difficult to obtain usefull information).

The lines will be in red or blue, according to whether the value is 
positive or negative. Users may specify the line style and legends 
position depends on their private interests.
}
\value{
Correlation network plots.
}
\references{
None
}
\author{
Jinlong Zhang \email{jinlongzhang01@gmail.com}
}
\examples{
data(testdata)
spmatrix <- data2mat(testdata)
result <- sp.pair(spmatrix)
plotnetwork(result$Pearson)

plotnetwork(result$Pearson, linecol = c("orange", "blue"),
number.label = FALSE)

title("Pearson Correlation Network")
}
\keyword{ network }
\keyword{ plot }
