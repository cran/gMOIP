## ---- include = FALSE---------------------------------------------------------
library(knitr)
library(rgl)
rgl::setupKnitr()
options(rgl.useNULL=TRUE)
opts_chunk$set(
  collapse = TRUE,
  webgl = TRUE,
  #comment = "#>",
  warning=FALSE, message=FALSE, include = TRUE, 
  out.width = "99%", fig.width = 8, fig.align = "center", fig.asp = 0.62
)

## ----setup--------------------------------------------------------------------
library(gMOIP)

## ----ex1Model-----------------------------------------------------------------
A <- matrix( c(
   3, 2, 5,
   2, 1, 1,
   1, 1, 3,
   5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- c(20, 10, 15)

## ----ex1View------------------------------------------------------------------
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)

## ----ex1LP, webgl = TRUE------------------------------------------------------
loadView(v = view, close = F, zoom = 0.75)
plotPolytope(A, b, plotOptimum = TRUE, obj = obj)

## ----ex1ILP, webgl = TRUE-----------------------------------------------------
loadView(v = view)
mfrow3d(nr = 1, nc = 2, sharedMouse = TRUE)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","i"), plotOptimum = TRUE, obj = obj, 
             argsTitle3d = list(main = "With LP faces"), argsPlot3d = list(box = F, axes = T) )
plotPolytope(A, b, faces = c("i","i","i"), type = c("i","i","i"), plotFeasible = FALSE, obj = obj,
             argsTitle3d = list(main = "ILP faces") )

## ----ex1MILP_1----------------------------------------------------------------
loadView(v = view, close = T, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","c","i"), plotOptimum = TRUE, obj = obj)

## ----ex1MILP_2----------------------------------------------------------------
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("c","i","i"), plotOptimum = TRUE, obj = obj)

## ----ex1MILP_3----------------------------------------------------------------
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, faces = c("c","c","c"), type = c("i","i","c"), plotOptimum = TRUE, obj = obj)

## ----ex1MILP_4----------------------------------------------------------------
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("i","c","c"), plotOptimum = TRUE, obj = obj, plotFaces = FALSE)

## ----ex1MILP_5----------------------------------------------------------------
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","i","c"), plotOptimum = TRUE, obj = obj, plotFaces = FALSE)

## ----ex1MILP_6----------------------------------------------------------------
loadView(v = view, zoom = 0.75)
plotPolytope(A, b, type = c("c","c","i"), plotOptimum = TRUE, obj = obj, plotFaces = FALSE)

## ---- include=F---------------------------------------------------------------
rm(list = ls(all.names = TRUE))

