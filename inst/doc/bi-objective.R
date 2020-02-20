## ---- include = FALSE---------------------------------------------------------
library(knitr)
library(rgl)
opts_chunk$set(
  collapse = TRUE,
  #cache = TRUE, autodep = TRUE, 
  comment = "#>",
  fig.path = "bi-obj-", fig.show = "asis", 
  warning=FALSE, message=FALSE, include = TRUE, 
  out.width = "99%", fig.width = 8, fig.align = "center", fig.asp = 0.62
)

knit_hooks$set(webgl = hook_webgl, rgl = hook_rgl)

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

## ----Ex1LPSol, rgl=TRUE-------------------------------------------------------
plotSol(A, b)

## ----Ex1LPCrit----------------------------------------------------------------
plotCrit(A, b, obj, addTriangles = FALSE) 

## ----Ex1ILPSol, rgl=TRUE------------------------------------------------------
plotSol(A, b, type = c("i","i","i"))

## ----Ex1ILPCrit---------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","i"))

## ----Ex1MILPSol1, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","i"))

## ----Ex1MILPCrit1-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","i"))

## ----Ex1MILPSol2, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","i"))

## ----Ex1MILPCrit2-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","i"))

## ----Ex1MILPSol3, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","i","c"))

## ----Ex1MILPCrit3-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","c"))

## ----Ex1NILP4Sol, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","c"), plotFaces = F)

## ----Ex1MILP4Crit-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","c"))

## ----Ex1MILPSol5, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","c"), plotFaces = F)

## ----Ex1MILPCrit5-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","c"))

## ----Ex1MILPSol6, webgl=TRUE--------------------------------------------------
plotSol(A, b, type = c("c","c","i"), plotFaces = F)

## ----Ex1MILPCrit6-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","c","i"))

## ----Ex2Model-----------------------------------------------------------------
A <- matrix( c(
   -1, 1, 0,
   1, 4, 0,
   2, 1, 0,
   3, -4, 0,
   0, 0, 4
), nc = 3, byrow = TRUE)
b <- c(5, 45, 27, 24, 10)
obj <- matrix(c(1, -6, 3, -4, 1, 6), nrow = 2)

## ----Ex2View------------------------------------------------------------------
view <- matrix( c(0.976349174976349, -0.202332556247711, 0.0761845782399178, 0, 0.0903248339891434,
                  0.701892614364624, 0.706531345844269, 0, -0.196427255868912, -0.682940244674683,
                  0.703568696975708, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)

## ----Ex2LPSol, rgl=TRUE-------------------------------------------------------
plotSol(A, b)

## ----Ex2LPCrit----------------------------------------------------------------
plotCrit(A, b, obj, addTriangles = FALSE) 

## ----Ex2ILPSol, rgl=TRUE------------------------------------------------------
plotSol(A, b, type = c("i","i","i"))

## ----Ex2ILPCrit---------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","i"))

## ----Ex2MILPSol1, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","i"))

## ----Ex2MILPCrit1-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","i"))

## ----Ex2MILPSol2, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","i"), plotFaces = FALSE)

## ----Ex2MILPCrit2-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","i"))

## ----Ex2MILPSol3, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","i","c"))

## ----Ex2MILPCrit3-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","c"))

## ----Ex2NILP4Sol, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","c"), plotFaces = F)

## ----Ex2MILP4Crit-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","c"))

## ----Ex2MILPSol5, webgl=TRUE--------------------------------------------------
plotSol(A, b, type = c("c","i","c"), plotFaces = F)

## ----Ex2MILPCrit5-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","c"))

## ----Ex2MILPSol6, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","c","i"))

## ----Ex2MILPCrit6-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","c","i"))

## ----Ex3Model-----------------------------------------------------------------
A <- matrix( c(
   1, 1, 1,
   3, 0, 1
), nc = 3, byrow = TRUE)
b <- c(10, 24)
obj <- matrix(c(1, -6, 3, -4, 1, 6), nrow = 2)

## ----Ex3View------------------------------------------------------------------
view <- matrix( c(-0.812462985515594, -0.029454167932272, 0.582268416881561, 0, 0.579295456409454,
                  -0.153386667370796, 0.800555109977722, 0, 0.0657325685024261, 0.987727105617523,
                  0.14168381690979, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)

## ----Ex3LPSol, rgl=TRUE-------------------------------------------------------
plotSol(A, b)

## ----Ex3LPCrit----------------------------------------------------------------
plotCrit(A, b, obj, addTriangles = FALSE) 

## ----Ex3ILPSol, rgl=TRUE------------------------------------------------------
plotSol(A, b, type = c("i","i","i"))

## ----Ex3ILPCrit---------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","i"))

## ----Ex3MILPSol1, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","i"))

## ----Ex3MILPCrit1-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","i"))

## ----Ex3MILPSol2, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","i"), plotFaces = FALSE)

## ----Ex3MILPCrit2-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","i"))

## ----Ex3MILPSol3--------------------------------------------------------------
plotSol(A, b, type = c("i","i","c"))

## ----Ex3MILPCrit3-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","c"))

## ----Ex3NILP4Sol, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","c"), plotFaces = F)

## ----Ex3MILP4Crit-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","c"))

## ----Ex3MILPSol5, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","c"), plotFaces = F)

## ----Ex3MILPCrit5-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","c"))

## ----Ex3MILPSol6, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","c","i"), plotFaces = F)

## ----Ex3MILPCrit6-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","c","i"))

## ----Ex4Model-----------------------------------------------------------------
A <- matrix( c(
3, 2, 5,
2, 1, 1,
1, 1, 3,
5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- matrix(c(1, -6, 3, -4, 1, -1), nrow = 2)

## ----Ex4View------------------------------------------------------------------
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)

## ----Ex4LPSol, rgl=TRUE-------------------------------------------------------
plotSol(A, b)

## ----Ex4LPCrit----------------------------------------------------------------
plotCrit(A, b, obj, addTriangles = FALSE) 

## ----Ex4ILPSol, rgl=TRUE------------------------------------------------------
plotSol(A, b, type = c("i","i","i"), labels="n")

## ----Ex4ILPCrit---------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","i"), labels="n")

## ----Ex4MILPSol1, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","i"), labels="n")

## ----Ex4MILPCrit1-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","i"), labels="n")

## ----Ex4MILPSol2, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","i"), plotFaces = FALSE, labels="n")

## ----Ex4MILPCrit2-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","i"), labels="n")

## ----Ex4MILPSol3--------------------------------------------------------------
plotSol(A, b, type = c("i","i","c"), labels="n")

## ----Ex4MILPCrit3-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","i","c"), labels="n")

## ----Ex4NILP4Sol, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("i","c","c"), labels="n")

## ----Ex4MILP4Crit-------------------------------------------------------------
plotCrit(A, b, obj, type = c("i","c","c"), labels="n")

## ----Ex4MILPSol5, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","i","c"), plotFaces = F, labels="n")

## ----Ex4MILPCrit5-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","i","c"), labels="n")

## ----Ex4MILPSol6, rgl=TRUE----------------------------------------------------
plotSol(A, b, type = c("c","c","i"), labels="n")

## ----Ex4MILPCrit6-------------------------------------------------------------
plotCrit(A, b, obj, type = c("c","c","i"), labels="n")

