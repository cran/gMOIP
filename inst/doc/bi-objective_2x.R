## ----include = FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(
  collapse = TRUE,
  #cache = TRUE, autodep = TRUE, 
  comment = "#>",
  fig.show = "asis", 
  warning=FALSE, message=FALSE, include = TRUE, 
  out.width = "99%", fig.width = 8, fig.align = "center", fig.asp = 0.62
)

if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available("1.14")) {
   warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14 (or higher). These were not found. Older versions will not work.")
   knitr::knit_exit()
}

## ----setup--------------------------------------------------------------------
library(gMOIP)

## ----2DPlotFunction-----------------------------------------------------------
plotBiObj2D <- function(A, b, obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = "numb",
   addTriangles = TRUE,
   addHull = TRUE)
{
   p1 <- plotPolytope(A, b, type = type, crit = crit, faces = faces, plotFaces = plotFaces,
                      plotFeasible = plotFeasible, plotOptimum = plotOptimum, labels = labels) + 
      ggplot2::ggtitle("Solution space")
   p2 <- plotCriterion2D(A, b, obj, type = type, crit = crit, addTriangles = addTriangles,
                         addHull = addHull, plotFeasible = plotFeasible, labels = labels) +
      ggplot2::ggtitle("Criterion space")
   gridExtra::grid.arrange(p1, p2, nrow = 1) 
}

## ----2DConstraints------------------------------------------------------------
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)

## ----2DLP---------------------------------------------------------------------
obj <- matrix(
   c(7, -10, # first criterion
     -10, -10), # second criterion
   nrow = 2)
plotBiObj2D(A, b, obj, addTriangles = FALSE)

## ----2DILPMax-----------------------------------------------------------------
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)))
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)))
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)))
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)))

## ----2DILPMin-----------------------------------------------------------------
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)), crit = "min")
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)), crit = "min")
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)), crit = "min")
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)), crit = "min")

## ----2DMILPMax----------------------------------------------------------------
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj2D(A, b, obj, type = c("i", "c"))
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = c("i", "c"))
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj2D(A, b, obj, type = c("i", "c"))
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = c("i", "c"))

## ----2DMILPMin----------------------------------------------------------------
## 
obj <- matrix(c(7, -10, -10, -10), nrow = 2)
plotBiObj2D(A, b, obj, type = c("c", "i"), crit = "min")
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = c("c", "i"), crit = "min")
obj <- matrix(c(-7, -1, -5, 5), nrow = 2)
plotBiObj2D(A, b, obj, type = c("c", "i"), crit = "min")
obj <- matrix(c(-1, -1, 2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = c("c", "i"), crit = "min")

## ----eval=FALSE---------------------------------------------------------------
# library(tikzDevice)
# tikz(file = "plot_polytope.tex", standAlone=F, width = 7, height = 6)
# plotPolytope(
#    A,
#    b,
#    obj,
#    type = rep("i", ncol(A)),
#    crit = "max",
#    faces = rep("c", ncol(A)),
#    plotFaces = TRUE,
#    plotFeasible = TRUE,
#    plotOptimum = TRUE,
#    labels = "n",
#    latex = TRUE
# )
# dev.off()

