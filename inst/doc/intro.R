## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(rgl)
library(ggsci)
library(magrittr)
rgl::setupKnitr()
options(rgl.useNULL=TRUE)
rgl::par3d("family" = "serif")
opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning=FALSE, message=FALSE, include = TRUE, 
  out.width = "99%", fig.width = 8, fig.align = "center", fig.asp = 0.62
)

if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available("1.14")) {
   warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14 (or higher). These were not found. Older versions will not work.")
   knitr::knit_exit()
}

## ---- cache=FALSE, include=FALSE----------------------------------------------
library(gMOIP)

## -----------------------------------------------------------------------------
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)  # coefficients c

## -----------------------------------------------------------------------------
plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)

## -----------------------------------------------------------------------------
plotPolytope(
   A,
   b,
   obj,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)

## -----------------------------------------------------------------------------
plotPolytope(
   A,
   b,
   obj,
   type = c("i", "c"),
   crit = "max",
   faces = c("c", "c"),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
)

## -----------------------------------------------------------------------------
A <- matrix( c(
   3, 2, 5,
   2, 1, 1,
   1, 1, 3,
   5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- c(20, 10, 15)

## ---- webgl=TRUE--------------------------------------------------------------
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)   
loadView(v = view)  # set view angle
plotPolytope(A,
             b,
             obj,
             plotOptimum = TRUE,
             labels = "n")

## ---- webgl=TRUE--------------------------------------------------------------
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)   
loadView(v = view)  # set view angle
plotPolytope(A,
             b,
             obj, 
             type = c("i", "i", "i"), 
             plotOptimum = TRUE, 
             labels = "n")

## ---- webgl=TRUE, cache=FALSE-------------------------------------------------
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)   
loadView(v = view)  # set view angle
plotPolytope(A,
             b,
             obj, 
             type = c("i", "i", "c"), 
             faces = c("c", "c", "c"), 
             plotOptimum = TRUE, 
             # plotFaces = FALSE,
             labels = "n")

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
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = rep("i", ncol(A)))

## ----2DMILPMin----------------------------------------------------------------
obj <- matrix(c(3, -1, -2, 2), nrow = 2)
plotBiObj2D(A, b, obj, type = c("c", "i"), crit = "min")

## ----3DPlotFunction-----------------------------------------------------------
plotSol <- function(A, b, type = rep("c", ncol(A)),
                        faces = rep("c", ncol(A)),
                        plotFaces = TRUE, labels = "numb")
{
   loadView(v = view, close = F, zoom = 0.75)
   plotPolytope(A, b, type = type, faces = faces, labels = labels, plotFaces = plotFaces, 
                argsTitle3d = list(main = "Solution space"))
}

plotCrit <- function(A, b, obj, crit = "min", type = rep("c", ncol(A)), addTriangles = TRUE, 
                     labels = "numb") 
{
    plotCriterion2D(A, b, obj, type = type, crit = crit, addTriangles = addTriangles, 
                   labels = labels) + 
      ggplot2::ggtitle("Criterion space")
}

## ----Ex1Model-----------------------------------------------------------------
Ab <- matrix( c(
   1, 1, 2, 5,
   2, -1, 0, 3,
   -1, 2, 1, 3,
   0, -3, 5, 2
), nc = 4, byrow = TRUE)
A <- Ab[,1:3]
b <- Ab[,4]
obj <- matrix(c(1, -6, 3, -4, 1, 6), nrow = 2)

## ----Ex1View------------------------------------------------------------------
view <- matrix( c(-0.452365815639496, -0.446501553058624, 0.77201122045517, 0, 0.886364221572876,
                  -0.320795893669128, 0.333835482597351, 0, 0.0986008867621422, 0.835299551486969,
                  0.540881276130676, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)

## ----Ex1LPSol, webgl=TRUE, cache=FALSE----------------------------------------
plotSol(A, b)

## ----Ex1LPCrit----------------------------------------------------------------
plotCrit(A, b, obj, addTriangles = FALSE) 

## ----Ex1ILPSol, webgl=TRUE, cache=FALSE---------------------------------------
plotSol(A, b, type = c("i","i","i"))

## ----Ex1ILPCrit---------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","i"))

## ---- include=F---------------------------------------------------------------
rm(list = ls(all.names = TRUE))

