\name{summary}
\alias{summary.sfaendog}
\alias{print.summary.sfaendog}
\title{Summary method for object of class \dQuote{sfaendog}}
\description{It creates and displays the results of an estimation returned by \code{\link{sfaendog}}.}

\usage{
\method{summary}{sfaendog}(object, all = FALSE, \dots)

\method{print}{summary.sfaendog}(x, digits = max(3, getOption("digits") - 3), 
     signif.stars = getOption("show.signif.stars"), \dots) 
}

\arguments{
  \item{object}{An object of class \verb{'sfaendog'} returned by the function 
  \code{\link{sfaendog}}.}
  \item{x}{An object of class \verb{'summary.sfaendog'} returned by the function 
  \code{\link[=summary.sfaendog]{summary}}.}
  \item{all}{Logical. If \code{FALSE} (default), only \sQuote{Step1} results, Fisher test 
  results for the strength of external instrumental variables, and \sQuote{Step4} results 
  are returned. \cr
  If \code{all = TRUE}, summary of all \sQuote{Steps} in \code{\link{sfaendog}} are returned.}
  \item{signif.stars}{Logical. If \code{TRUE}, significance stars are printed.}
  \item{digits}{The minimum number of significant digits to be printed in values.} 
  \item{\dots}{Currently not used.} 
}

\details{
\code{print.summary.sfaendog} tries to be smart about formatting the coefficients, 
standard errors, etc. and additionally gives significance stars if 
\code{signif.stars} is \code{TRUE}.
}

\value{
A list of class \verb{'summary.sfaendog'} is returned, containing the number of 
observations, the parameter estimates, standard errors, t-values and p-values. 

  More precisely:
  \item{ Step1}{The list of summary statistics of \sQuote{Step1} OLS estimation of 
  the endogenous input.}
  \item{ FishTest}{A list containing the names of the variables used as external instrumental 
  variables in the estimation, and the results (\emph{i.e.} \code{F-value} and \code{Pr(>F)}) 
  of the Fisher test on the strength of these external instrumental variables.}
  \item{ Step2}{The list of summary statistics of \sQuote{Step2} NLS estimation of the 
  stochastic frontier production model.}
  \item{ Step3}{The list of summary statistics of \sQuote{Step3} GMM estimation of the 
  stochastic frontier production model. The name of the endogenous input (wrapped with 
  \sQuote{\code{*}}) is also returned.}
  \item{ Step4}{The list of summary statistics of \sQuote{Step4} GMM estimation of the 
  stochastic frontier production model. In addition, it also returns the name of the 
  endogenous input (wrapped with \sQuote{\code{*}} in the result table) indicating 
  that the predicted value from \code{Step1} is used.}
  
\code{Step2} and \code{Step3} are returned only if \code{all = TRUE}.
  
In \code{Step2} to \code{Step4} results, \code{InterceptX} is the constant term in 
the production part of the stochastic frontier production model, while \code{InterceptZ} is 
the constant term in the inefficiency effect part.

Information on convergence is also provided with \code{Step3} and \code{Step4} results, where 
\code{Convergence} \code{code = 0} indicates successful completion (see \code{\link{optim}}).}  

\author{Yann Desjeux, Laure Latruffe}

\seealso{\code{\link{sfaendog}}, for the model fitting function.}

\keyword{methods}
