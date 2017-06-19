summary.sfaendog <- function(object, all = FALSE, ...) {
    if (!is.logical(all)) {
        stop("Argument 'all' must be a single logical value")
    }
    Step1 <- summary(object[["Step1"]])
    Step2 <- summary(object[["Step2"]])
    Step2$formula <- formula(object[["Step2"]]$otherArgs$Formula)
    Step2$algo <- object[["Step2"]]$algo
    dimnames(Step2$coefficients)[[1L]] <- object[["Step2"]]$otherArgs$CoefNames
    dimnames(Step2$parameters)[[1L]] <- object[["Step2"]]$otherArgs$CoefNames
    Step3 <- summary(object[["Step3"]])
    Step4 <- summary(object[["Step4"]])
    Step4$x.endo <- object[["Step4"]]$x.endo
    Step4$nobs <- object[["Step4"]]$n
    Step4$opt <- object[["Step4"]]$allArg$method
    Step4$Data <- object[["Step4"]]$Data
    if (all) {
        ans <- list(Step1 = Step1, 
                    FishTest = list(object[["FishTest"]]$Instr, object[["FishTest"]]$Test[2,5:6]), 
                    Step2 = Step2, 
                    Step3 = Step3, 
                    Step4 = Step4)
    } else {
        ans <- list(Step1 = Step1, 
                    FishTest = list(object[["FishTest"]]$Instr, object[["FishTest"]]$Test[2,5:6]),
                    Step4 = Step4)
    }
    class(ans) <- "summary.sfaendog"
    ans
}
