sfaendog <- function(y, x.exo, x.endo, c.var, ineff, inst, data, nls.algo = c("GN", 
    "LM"), gmm.kernel = c("Bartlett", "Quadratic Spectral", "Truncated", "Parzen", 
    "Tukey-Hanning"), gmm.optim = c("BFGS", "Nelder-Mead", "CG", "SANN"), maxiter = 100) {
    if (!is.character(y) || length(y) != 1) {
        stop("Argument 'y' must be a character vector of length one")
    }
    if (!is.character(x.exo)) {
        stop("Argument 'x.exo' must be a character vector of names")
    }
    if (!is.character(x.endo) || length(x.endo) != 1) {
        stop("Argument 'x.endo' must be a character vector of length one")
    }
    if (!is.character(c.var)) {
        stop("Argument 'c.var' must be a character vector of names")
    }
    if (!is.character(ineff)) {
        stop("Argument 'ineff' must be a character vector of names")
    }
    if (!is.character(inst)) {
        stop("Argument 'inst' must be a character vector of names")
    }
    varnames <- c(y, x.exo, x.endo, c.var, ineff, inst)
    if (!any(grepl("[:||*||)||(]", varnames))) {
        validNames(varnames, names(data))
    }
    data.name <- substitute(data)
    nls.algo <- match.arg(nls.algo)
    if(nls.algo == "GN") nls.algo <- "default"
    kernel <- match.arg(gmm.kernel)
    method <- match.arg(gmm.optim)
    
    x.endo.logged <- paste0("log(", x.endo, ")")
    x.exo.logged <- paste0("log(", x.exo, ")")
    y.logged <- paste0("log(", y, ")")
    
    formula_1 <- as.formula(paste(x.endo.logged, paste0(c(x.exo.logged, ineff, c.var, 
        inst), collapse = "+"), sep = "~"))
    formula_2ai <- as.formula(paste(y.logged, paste0(c(x.exo.logged, x.endo.logged, 
        c.var), collapse = "+"), sep = "~"))
    formula_2aii <- reformulate(termlabels = ineff, response = "u")
    
    # Step 1
    Step1 <- lm(formula = formula_1, data = data)
    Step1$call <- match.call(lm, call("lm", formula_1))
    data$pred1a <- predict(Step1)
    
    # Step 2
    Step2ai <- lm(formula = formula_2ai, data = data)
    Step2ai$call <- match.call(lm, call("lm", formula_2ai))
    startval_2ai <- coef(Step2ai)
    pred_2ai <- predict(Step2ai)
    data$u <- pred_2ai - log(data[, y])
    Step2aii <- lm(formula = formula_2aii, data = data)
    Step2aii$call <- match.call(lm, call("lm", formula_2aii))
    startval_2aii <- coef(Step2aii)
    
    .Var.Prod <- gsub("[:]", ".", names(startval_2ai)[-1L])
    .Prod <- paste0("InterceptX+", paste0("B", 1L:length(.Var.Prod), "*", .Var.Prod, 
        collapse = "+"))
    terms.Prod <- c("InterceptX", paste0("B", 1L:length(.Var.Prod)))
    .Var.Exp <- gsub("[:]", ".", names(startval_2aii)[-1L])
    .Exp <- paste0("InterceptZ+", paste0("Z", 1L:length(.Var.Exp), "*", .Var.Exp, 
        collapse = "+"))
    terms.Exp <- c("InterceptZ", paste0("Z", 1L:length(.Var.Exp)))
    terms2b <- c(terms.Prod, terms.Exp)
    formula_2b <- as.formula(paste(y.logged, "~ ", paste(.Prod, paste0("exp(", .Exp, 
        ")"), sep = "-")))
    startval2b <- list()
    for (i in 1L:length(terms2b)) {
        startval2b[terms2b[i]] <- c(startval_2ai, startval_2aii)[i]
    }
    nd1 <- setdiff(names(as.data.frame(model.matrix(Step2ai))[-1L]), names(data))
    data <- merge(data, model.matrix(Step2ai)[, nd1], by = "row.names", all = TRUE)
    data <- data[, names(data) != "Row.names"]
    nd2 <- setdiff(names(as.data.frame(model.matrix(Step2aii))[-1L]), names(data))
    data <- merge(data, model.matrix(Step2aii)[, nd2], by = "row.names", all = TRUE)
    data <- data[, names(data) != "Row.names"]
    names(data) <- gsub("[:]", ".", names(data))
    Step2b <- if (nls.algo != "LM") {
        nls(formula = formula_2b, data = data, start = startval2b, control = list(maxiter = maxiter), algorithm = nls.algo)
    } else {
        nlsLM(formula = formula_2b, data = data, start = startval2b, control = list(maxiter = maxiter))
    }
    Step2b$otherArgs <- list(Formula = paste(y.logged, "~ ", paste(paste0("InterceptX+", 
        paste0(.Var.Prod, collapse = "+")), paste0("exp(", paste0("InterceptZ+", 
        paste0(.Var.Exp, collapse = "+")), ")"), sep = "-")), CoefNames = c("InterceptX", 
        .Var.Prod, "InterceptZ", .Var.Exp))
    coef2b <- coef(Step2b)
    if(nls.algo=="default") Step2b$algo <- "Gauss-Newton" 
    if(nls.algo == "LM") Step2b$algo <- "Levenberg-Marquardt"
    
    ## Step 3
    data <- within(data, {
        instr0 <- exp(eval(parse(text = paste(coef2b[terms.Exp][1L], paste(paste0(coef2b[terms.Exp][-1L], 
            "*", .Var.Exp), collapse = "+"), sep = "+"))))
    })
    for (i in 1L:length(.Var.Exp)) {
        data[, paste0("instr", i)] <- data[.Var.Exp[i]] * data$instr0
    }
    ## Defining 'g' function (where E[g]=0)
    g <- function(bheta, x) {
        V <- eval(parse(text = paste(paste(paste0("log(x[,'", y, "']) - ", "(bheta[1]"), 
            paste0(c(paste0("bheta[", 2L:(length(c(x.exo, x.endo)) + 1), "]")), "*", 
                "log(x[,'", c(x.exo, x.endo), "'])", collapse = "+"), paste0(c(paste0("bheta[", 
                (length(c(x.exo, x.endo)) + 2):length(terms.Prod), "]")), "*", "x[,'", 
                setdiff(.Var.Prod, c(x.exo.logged, x.endo.logged)), "']", collapse = "+"), 
            sep = "+"), paste(paste0("exp(bheta[", length(terms.Prod) + 1, "]"), 
            paste0(paste0(c(paste0("bheta[", (length(c(terms.Prod)) + 2):length(terms2b), 
                "]")), "*", "x[,'", .Var.Exp, "']", collapse = "+"), "))"), sep = "+"), 
            sep = " - ")))
        m <- list()
        n <- as.list(c(1, paste0("log(x[,'", x.exo, "'])"), "x[,'pred1a']", paste0("x[,'", 
            setdiff(.Var.Prod, c(x.exo.logged, x.endo.logged)), "']"), paste0("x[,'instr", 
            0:length(.Var.Exp), "']")))
        for (i in 1L:length(n)) {
            m[[i]] <- eval(parse(text = paste0("m", i, " <- V * ", n[[i]])))
        }
        f <- do.call(cbind, m)
        return(f)
    }
    Step3 <- gmm(g = g, x = data, t0 = coef2b, kernel = kernel, method = method, itermax = maxiter)
    coef3 <- coef(Step3)
    names(Step3$coefficients) <- c("InterceptX", .Var.Prod, "InterceptZ", .Var.Exp)
    names(Step3$coefficients)[names(Step3$coefficients) == x.endo.logged] <- paste0("log(*", x.endo, "*)")
    
    # Step 4
    for (i in 0:length(.Var.Exp)) {
        data[, paste0("instr", i)] <- NULL
    }
    data <- within(data, {
        instr0 <- exp(eval(parse(text = paste(coef2b[terms.Exp][1L], paste(paste0(coef2b[terms.Exp][-1L], 
            "*", .Var.Exp), collapse = "+"), sep = "+"))))
    })
    for (i in 1L:length(.Var.Exp)) {
        data[, paste0("instr", i)] <- data[.Var.Exp[i]] * data$instr0
    }
    Step4 <- gmm(g = g, x = data, t0 = coef3, kernel = kernel, method = method, itermax = maxiter)
    names(Step4$coefficients) <- c("InterceptX", .Var.Prod, "InterceptZ", .Var.Exp)
    names(Step4$coefficients)[names(Step4$coefficients) == x.endo.logged] <- paste0("log(*", x.endo, "*)")
    Step4$x.endo <- x.endo
    Step4$Data <- data.name
    
    ## Fisher test on instruments
    FishTest <- list(Instr = inst, Test = anova(Step1, lm(as.formula(paste(x.endo.logged, 
        paste0(c(x.exo.logged, ineff, c.var), collapse = "+"), sep = "~")), data = data)))
    dimnames(FishTest$Test)[[2]]=c("Res.Df","RSS","Df","Sum of Sq","F value","Pr(>F)")
    ans <- list(Step1 = Step1, FishTest = FishTest, Step2 = Step2b, Step3 = Step3, Step4 = Step4)
    class(ans) <- "sfaendog"
    ans
}
