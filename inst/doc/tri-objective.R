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

## ----cache=FALSE--------------------------------------------------------------
library(gMOIP)

## ----webgl=TRUE, cache=FALSE--------------------------------------------------
view <- matrix( c(0.333316594362259, 0.938472270965576, -0.0903875231742859, 0, 0.83994072675705, -0.339126199483871, -0.423665106296539, 0, -0.428250730037689, 0.0652943551540375, -0.901297807693481, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)
set.seed(1234)
pts <- genNDSet(3, 20, argsSphere = list(below = FALSE), dubND = FALSE)
pts <- classifyNDSet(pts[,1:3])
head(pts)
ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
   ylim = c(min(pts[,2])-2,max(pts[,2])+2),
   zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1, color = "cornflowerblue"))
plotHull3D(pts[,1:3], addRays = TRUE, argsPolygon3d = list(alpha = 0.25, color = "red"), useRGLBBox = TRUE)
finalize3D(argsAxes3d = list(edges = "bbox"))

## ----webgl=TRUE, cache=FALSE--------------------------------------------------
loadView(v = view)
pts <- genNDSet(3, 10, argsSphere = list(below = TRUE), dubND = FALSE)
pts <- classifyNDSet(pts[,1:3])
ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
   ylim = c(min(pts[,2])-2,max(pts[,2])+2),
   zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1, color = "cornflowerblue"))
plotHull3D(pts[,1:3], addRays = TRUE, argsPolygon3d = list(alpha = 0.25, color = "red"), useRGLBBox = TRUE)
finalize3D(argsAxes3d = list(edges = "bbox"))

## -----------------------------------------------------------------------------
pts <- matrix(c(0,0,1, 0,1,0, 1,0,0, 0.5,0.2,0.5, 0.25,0.5,0.25), ncol = 3, byrow = TRUE)
open3d()
ini3D(argsPlot3d = list(xlim = c(min(pts[,1])-2,max(pts[,1])+2),
  ylim = c(min(pts[,2])-2,max(pts[,2])+2),
  zlim = c(min(pts[,3])-2,max(pts[,3])+2)))
plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5), useRGLBBox = TRUE)
pts <- classifyNDSet(pts[,1:3])
pts
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
plotCones3D(pts[,1:3], rectangle = TRUE, argsPolygon3d = list(alpha = 1))
finalize3D()
rglwidget(reuse = F)

## -----------------------------------------------------------------------------
pts <- genNDSet(3,20, dubND = FALSE)[,1:3]
open3d()
ini3D(argsPlot3d = list(xlim = c(0,max(pts$z1)+2),
  ylim = c(0,max(pts$z2)+2),
  zlim = c(0,max(pts$z3)+2)))
plotHull3D(pts, addRays = TRUE, argsPolygon3d = list(alpha = 0.5))
pts <- classifyNDSet(pts[,1:3])
pts
plotPoints3D(pts[pts$se,1:3], argsPlot3d = list(col = "red"))
plotPoints3D(pts[!pts$sne,1:3], argsPlot3d = list(col = "black"))
plotPoints3D(pts[!pts$us,1:3], argsPlot3d = list(col = "blue"))
finalize3D()
rglwidget(reuse = F)

## ----include=F----------------------------------------------------------------
rm(list = ls(all.names = TRUE))

