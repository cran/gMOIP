## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning=FALSE, message=FALSE, include = TRUE, 
  out.width = "99%", fig.width = 8, fig.align = "center", fig.asp = 0.62
)

if (!requireNamespace("rmarkdown", quietly = TRUE) || !rmarkdown::pandoc_available("1.14")) {
   warning(call. = FALSE, "These vignettes assume rmarkdown and pandoc version 1.14 (or higher). These were not found. Older versions will not work.")
   knitr::knit_exit()
}

## ----setup--------------------------------------------------------------------
library(gMOIP)

## ----model--------------------------------------------------------------------
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)  # coefficients c

## ----LP1----------------------------------------------------------------------
# The polytope with the corner points
p1 <- plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = NULL
) + ggplot2::ggtitle("Feasible region only")

p2 <- plotPolytope(
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
) + ggplot2::ggtitle("Solution LP max")

p3 <- plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   crit = "min",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "n"
) + ggplot2::ggtitle("Solution LP min")

p4 <- plotPolytope(
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
) + ggplot2::xlab("x") + ggplot2::ylab("y") + ggplot2::ggtitle("Solution (max) with other axis labels")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

## ----LP2----------------------------------------------------------------------
A <- matrix(c(-3, 2, 2, 4, 9, 10, 1, -2), ncol = 2, byrow = TRUE)
b <- c(3, 27, 90, 2)
obj <- c(7.75, 10)
plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   nonneg = rep(FALSE, ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = NULL
)

## ----unbounded1---------------------------------------------------------------
A <- matrix(c(-3,2), ncol = 2, byrow = TRUE)
b <- c(3)
obj <- c(7.75, 10)
# Wrong plot
plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = NULL
)

## ----unbounded2---------------------------------------------------------------
A <- rbind(A, c(1,0), c(0,1))
b <- c(b, 10, 10)
plotPolytope(
   A,
   b,
   obj,
   type = rep("c", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = NULL
)

## ----ILP----------------------------------------------------------------------
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)

p1 <- plotPolytope(
   A,
   b,
   obj,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = "n"
) + ggplot2::ggtitle("Relaxed region with LP faces")

p2 <- plotPolytope(
   A,
   b,
   obj,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("i", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = FALSE,
   labels = "n"
) + ggplot2::ggtitle("Relaxed region with IP faces")

p3 <- plotPolytope(
   A,
   b,
   obj,
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("c", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "n"
) + ggplot2::ggtitle("Optimal solution (max)")

p4 <- plotPolytope(
   A,
   b,
   obj = c(-3, 3),
   type = rep("i", ncol(A)),
   crit = "max",
   faces = rep("i", ncol(A)),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "n"
) + ggplot2::ggtitle("Other objective (min)")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

## ----MILP---------------------------------------------------------------------
A <- matrix(c(-3,2,2,4,9,10), ncol = 2, byrow = TRUE)
b <- c(3,27,90)
obj <- c(7.75, 10)

p1 <- plotPolytope(
   A,
   b,
   obj,
   type = c("c", "i"),
   crit = "max",
   faces = c("c", "c"),
   plotFaces =  TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "n"
) + ggplot2::ggtitle("Second coordinate integer (LP faces)")

p2 <- plotPolytope(
   A,
   b,
   obj,
   type = c("c", "i"),
   crit = "max",
   faces = c("c", "i"),
   plotFaces =  TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
) + ggplot2::ggtitle("Second coordinate integer (MILP faces)")

p3 <- plotPolytope(
   A,
   b,
   obj,
   type = c("i", "c"),
   crit = "max",
   faces = c("c", "c"),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "n"
) + ggplot2::ggtitle("First coordinate integer (LP faces)")

p4 <- plotPolytope(
   A,
   b,
   obj,
   type = c("i", "c"),
   crit = "max",
   faces = c("i", "c"),
   plotFaces = TRUE,
   plotFeasible = TRUE,
   plotOptimum = TRUE,
   labels = "coord"
) + ggplot2::ggtitle("First coordinate integer (MILP faces)")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

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

