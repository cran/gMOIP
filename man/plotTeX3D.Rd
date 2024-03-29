% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plotTeX3D}
\alias{plotTeX3D}
\title{Plot TeX at a position.}
\usage{
plotTeX3D(
  x,
  y,
  z,
  tex,
  cex = graphics::par("cex"),
  fixedSize = FALSE,
  size = 480,
  ...
)
}
\arguments{
\item{x}{Coordinate.}

\item{y}{Coordinate.}

\item{z}{Coordinate.}

\item{tex}{TeX string.}

\item{cex}{Expansion factor (you properly have to fine tune it).}

\item{fixedSize}{Fix the size of the object (no scaling when zoom).}

\item{size}{Size of the generated png.}

\item{...}{Arguments passed on to \code{\link[rgl:sprites]{rgl::sprites3d()}} and \code{\link[=texToPng]{texToPng()}}.}
}
\value{
The shape ID of the displayed object is returned.
}
\description{
Plot TeX at a position.
}
\examples{
\dontrun{
tex0 <- "$\\\\mathbb{R}_{\\\\geqq}$"
tex1 <- "\\\\LaTeX"
tex2 <- "This is a title"
ini3D(argsPlot3d = list(xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 2)))
plotTeX3D(0.75,0.75,0.75, tex0)
plotTeX3D(0.5,0.5,0.5, tex0, cex = 2)
plotTeX3D(1,1,1, tex2)
finalize3D()
ini3D(new = TRUE, argsPlot3d = list(xlim = c(0, 200), ylim = c(0, 200), zlim = c(0, 200)))
plotTeX3D(75,75,75, tex0)
plotTeX3D(50,50,50, tex1)
plotTeX3D(100,100,100, tex2)
finalize3D()
}
}
