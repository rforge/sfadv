print.summary.sfaendog <- function(x, digits = max(3L, getOption("digits") - 3L), 
    signif.stars = getOption("show.signif.stars"), ...) {
    cat("\n#############\n")
    cat(" Name of the data frame used:", x[["Step4"]]$Data, "\n")
    cat(" Number of Observations:", x[["Step4"]]$n, "\n")
    cat("#############\n")
    cat("\n#############\n")
    cat("# Summary of 'Step1' results (OLS on the endogenous input)\n")
    cat("#############\n")
    print(x[["Step1"]], digits = digits, signif.stars = signif.stars, print.gap = 2)
    cat(" ---------\n")
    cat("| Fisher test on the strength of the external instrumental variables\n")
    cat(" ---------")
    cat("\n \u00A4 List of external instrumental variables:\n", paste0(x[["FishTest"]][[1L]], collapse = ", "), "\n")
    row.names(x[["FishTest"]][[2L]]) <- ""
    cat("\n \u00A4 Test statistic (H0: nullity of the parameters related to the external instrumental variables):\n")
    print(x[["FishTest"]][[2L]], digits = digits, signif.stars = signif.stars, print.gap = 3)
    cat("\n")

    if (!is.null(x[["Step2"]])) {
        cat("\n#############\n")
        cat("# Summary of 'Step2' results (NLS)\n")
        cat("#############\n")
        cat("\n-- non consistent and non efficient estimator --\n")
        cat("\n\u00A4 Non-linear regression model\n")
        cat("\u00A4 Algorithm:", x[["Step2"]]$algo,"\n\n")
        print(x[["Step2"]], digits = digits, signif.stars = signif.stars, print.gap = 2)
        }

    if (!is.null(x[["Step3"]])) {
        cat("\n#############\n")
        cat("# Summary of 'Step3' results (GMM)\n")
        cat("#############\n")
        cat("\n-- consistent but non efficient estimator --\n")
        cat("\n\u00A4 Endogenous input variable:", x[["Step4"]]$x.endo, "\n")
        cat("  \u2018*", x[["Step4"]]$x.endo, "*\u2019 in the result table is the predicted value of \u2018", 
            x[["Step4"]]$x.endo, "\u2019, obtained in Step1\n", sep="")
        cat("\n\nParameters:\n")
        printCoefmat(x[["Step3"]]$coefficients, digits = digits, signif.stars = signif.stars, 
             print.gap = 2)
        cat("\n---------\n")
        cat(x[["Step3"]]$stest$ntest, "\n")
        print(x[["Step3"]]$stest$test, digits = digits, print.gap = 2)
        if (!is.null(x[["Step3"]]$initTheta)) {
            cat("Initial values of the coefficients\n")
            print(x[["Step3"]]$initTheta, digits = digits, print.gap = 2)
        }
        if (!is.null(x[["Step3"]]$algoInfo)) {
            cat("---------\n")
            cat("Information related to the numerical optimization\n")
        }
        if (!is.null(x[["Step3"]]$algoInfo$convergence)) {
            cat("Convergence code =", x[["Step3"]]$algoInfo$convergence, "\n")
        }
        if (!is.null(x[["Step3"]]$algoInfo$counts)) {
            cat("Function eval. =", x[["Step3"]]$algoInfo$counts[1L], "\n")
            cat("Gradian eval. =", x[["Step3"]]$algoInfo$counts[2L], "\n")
        }
        cat("---------\n")
        if (!is.null(x[["Step3"]]$algoInfo$message)) {
            cat("Message:", x[["Step3"]]$algoInfo$message, "\n")
        }
            cat("\n")
        }

    cat("\n#############\n")
    cat("# Summary of 'Step4' results (GMM)\n")
    cat("#############\n")
    cat("\n-- consistent and efficient estimator --\n")
    cat("\n\u00A4 Optimisation algorithm:", x[["Step4"]]$opt, "\n")
    cat("\u00A4 Kernel:", x[["Step4"]]$kernel, "\n")
    if (x[["Step4"]]$met == "cue") {
        cat("         (", x[["Step4"]]$cue$message, ")\n\n", sep="")
    }
    cat("\n\u00A4 Endogenous input variable:", x[["Step4"]]$x.endo, "\n")
    cat("  \u2018*", x[["Step4"]]$x.endo, "*\u2019 in the result table is the predicted value of \u2018", 
        x[["Step4"]]$x.endo, "\u2019, obtained in Step1\n", sep="")
    cat("\n\nParameters:\n")
    printCoefmat(x[["Step4"]]$coefficients, digits = digits, signif.stars = signif.stars, print.gap = 2)
    cat("\n---------\n")
    cat(x[["Step4"]]$stest$ntest, "\n")
    print(x[["Step4"]]$stest$test, digits = digits, print.gap = 2)
    if (!is.null(x[["Step4"]]$initTheta)) {
        cat("Initial values of the coefficients\n")
        print(x[["Step4"]]$initTheta, digits = digits, print.gap = 2)
    }
    if (!is.null(x[["Step4"]]$algoInfo)) {
        cat("---------\n")
        cat("Information related to the numerical optimization\n")
    }
    if (!is.null(x[["Step4"]]$algoInfo$convergence)) {
        cat("Convergence code =", x[["Step4"]]$algoInfo$convergence, "\n")
    }
    if (!is.null(x[["Step4"]]$algoInfo$counts)) {
        cat("Function eval. =", x[["Step4"]]$algoInfo$counts[1L], "\n")
        cat("Gradian eval. =", x[["Step4"]]$algoInfo$counts[2L], "\n")
    }
    cat("---------\n")
    if (!is.null(x[["Step4"]]$algoInfo$message)) {
        cat("Message:", x[["Step4"]]$algoInfo$message, "\n")
    }
    invisible(x)
}
