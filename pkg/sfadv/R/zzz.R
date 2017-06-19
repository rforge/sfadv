.onAttach <- function(lib, pkg) {
    packageStartupMessage(paste0("\n* Please cite the 'sfadv' package as:\n", 
    "  Desjeux Y. and Latruffe L. (2017). sfadv: Advanced Methods for Stochastic Frontier Analyses. R package version 1.0.0. \n", 
    "  URL: https://CRAN.R-project.org/package=sfadv.\n\n", 
    "See also: citation(\"sfadv\")\n\n",
    "* For any questions, suggestions, or comments on the 'sfadv' package, please make use of 'tracker' facilities at:\n", 
    "  https://r-forge.r-project.org/projects/sfadv/"), domain = NULL, appendLF = TRUE)
}