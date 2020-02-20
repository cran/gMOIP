library(testthat)
library(gMOIP)
library(rgl)

skip_on_cran()

A <- matrix( c(
   3, 2, 5,
   2, 1, 1,
   1, 1, 3,
   5, 2, 4
), nc = 3, byrow = TRUE)
b <- c(55, 26, 30, 57)
obj <- c(20, 10, 15)
view <- matrix( c(-0.412063330411911, -0.228006735444069, 0.882166087627411, 0, 0.910147845745087,
                  -0.0574885793030262, 0.410274744033813, 0, -0.042830865830183, 0.97196090221405,
                  0.231208890676498, 0, 0, 0, 0, 1), nc = 4)
loadView(v = view)  # set view angle
plotPolytope(A, b, plotOptimum = TRUE, obj = obj, labels = "n")
rgl::writeASY(title = "tests/testTex/fig2", outtype = "asy")
rgl::writeASY(title = "tests/testTex/fig1", outtype = "pdflatex")

## Now complile main LaTeX files


# open3d()
# plot3d(data.frame(x = c(-1,1,3), y = c(-1,1,3), z = c(-1,1,3)))
# planes3d(a = 1, b = 1,c = 1, d = -3, col = "grey100", alpha = 0.5)
# rgl::writeASY(title = "tests/testTex/main", outtype = "latex")

# cleanUp <- function() {
#    aux = c(
#       'log', 'idx', 'aux', 'bcf', 'blg', 'bbl', 'fls', 'out', 'lof', 'lot', 'toc',
#       'nav', 'snm', 'vrb', 'ilg', 'ind', 'xwm', 'brf', 'run.xml'
#    )
#    auxFiles = paste0("*.", aux)
#    fls <- list.files("./tests/testTeX/")
#    grep(auxFiles,fls)
#
#
# }