## ----include = FALSE----------------------------------------------------------
library(knitr)
library(rgl)
rgl::setupKnitr()
options(rgl.useNULL=TRUE)
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

## ----3DPlotFunction-----------------------------------------------------------
plotSol <- function(A, b, type = rep("c", ncol(A)),
                        faces = rep("c", ncol(A)),
                        plotFaces = TRUE, labels = "numb")
{
   #loadView(v = view, close = F, zoom = 0.75)
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

## ----Ex1LPSol, webgl = TRUE---------------------------------------------------
plotSol(A, b)

## ----Ex1LPCrit----------------------------------------------------------------
plotCrit(A, b, obj, addTriangles = FALSE) 

## ----Ex1ILPSol, webgl = TRUE--------------------------------------------------
plotSol(A, b, type = c("i","i","i"))

## ----Ex1ILPCrit---------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","i"))

## ----Ex1MILPSol1, webgl = TRUE------------------------------------------------
plotSol(A, b, type = c("c","i","i"))

## ----Ex1MILPCrit1-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","i"))

## ----Ex1MILPSol2, webgl = TRUE------------------------------------------------
plotSol(A, b, type = c("i","c","i"))

## ----Ex1MILPCrit2-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","i"))

## ----Ex1MILPSol3, webgl = TRUE------------------------------------------------
plotSol(A, b, type = c("i","i","c"))

## ----Ex1MILPCrit3-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","c"))

## ----Ex1NILP4Sol, webgl = TRUE------------------------------------------------
plotSol(A, b, type = c("i","c","c"), plotFaces = F)

## ----Ex1MILP4Crit-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","c"))

## ----Ex1MILPSol5, webgl = TRUE------------------------------------------------
plotSol(A, b, type = c("c","i","c"), plotFaces = F)

## ----Ex1MILPCrit5-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","c"))

## ----Ex1MILPSol6, webgl=TRUE--------------------------------------------------
plotSol(A, b, type = c("c","c","i"), plotFaces = F)

## ----Ex1MILPCrit6-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","c","i"))

## ----include=F----------------------------------------------------------------
rm(list = ls(all.names = TRUE))

