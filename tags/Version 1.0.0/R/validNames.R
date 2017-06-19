validNames <- function(names_to_test, all_names) {
    inAll <- names_to_test %in% all_names
    if (!all(inAll)) {
        stop("Variable names(s) '", paste(names_to_test[!inAll], collapse = "', '"), 
            "' not found in the data.")
    }
}
