\name{sfaendog}
\alias{sfaendog}
\title{Stochastic frontier analysis with technical inefficiency effects \cr
and endogeneity of one input}

\description{It implements a Method of Moments (MM) estimation of stochastic production 
frontier with explanatory variables influencing technical inefficiency (\emph{i.e.} the 
technical inefficiency effects) and accounting for one single endogenous input.} 

\usage{
sfaendog(y, x.exo, x.endo, c.var, ineff, inst, data, nls.algo = c("GN", "LM"), 
      gmm.kernel = c("Bartlett", "Quadratic Spectral", "Truncated", "Parzen", 
      "Tukey-Hanning"), gmm.optim = c("BFGS", "Nelder-Mead", "CG", "SANN"), 
      maxiter = 100)
}

\arguments{
  \item{y}{A character string specifying the name of the dependent variable of the 
  production function (output). Values of the \code{y} variable will be internally 
  coerced to their \emph{logged} values.}
  \item{x.exo}{A vector of strings containing the names of the exogenous inputs 
  of the production function. Values of the \code{x.exo} variables will be internally 
  coerced to their \emph{logged} values.}
  \item{x.endo}{A character string specifying the name of the input of the 
  production function considered as endogenous. Values of the the \code{x.endo} 
  variable will be internally coerced to their \emph{logged} values.}
  \item{c.var}{A vector of strings containing the names of non-input variables 
  influencing the output (production environment conditions, time trend, etc.).}
  \item{ineff}{A vector of strings containing the names of the variables influencing
  technical inefficiency.}
  \item{inst}{A vector of strings containing the names of the external instrumental
  variables.}
  \item{data}{A data frame containing the variables called in \code{y}, \code{x.exo},
  \code{x.endo}, \code{c.var}, \code{ineff} and \code{inst}.}
  \item{nls.algo}{A character string specifying the algorithm used in the Non-linear Least 
  Squares (NLS) estimation (see \code{Estimation 'recipe'}). The default algorithm is the Gauss-Newton 
  algorithm (\code{"GN"}). The other possible value is \code{"LM"} for the Levenberg-Marquardt 
  algorithm. \cr
  Levenberg-Marquardt being a more robust variation of Gauss-Newton algorithm, 
  specifiying \code{nls.algo = "LM"} may help in case of non convergence.}
  \item{gmm.kernel}{A character string specifying the type of kernel used to compute 
  the covariance matrix of the vector of sample moment conditions in the Generalised Method of 
  Moments (GMM) estimations (see \code{Estimation 'recipe'}). The default value is \code{"Bartlett"} 
  kernel. Other possible options are \code{"Quadratic Spectral"}, \code{"Truncated"}, 
  \code{"Parzen"}, or \code{"Tukey-Hanning"} (see \code{\link[gmm]{gmm}} for more details).}
  \item{gmm.optim}{A character string specifying the optimisation method to be applied
  in the GMM estimations (see \code{Estimation 'recipe'}). The default value (\code{"BFGS"}) 
  is the quasi-Newton method. Other possibles methods are \code{"Nelder-Mead"}, \code{"CG"}, 
  \code{"SANN"} (see \code{\link{optim}} for more details).}
  \item{maxiter}{A positive integer specifying the maximum number of iterations 
  allowed for the NLS and GMM estimations. The \code{default} value is \code{100}.}
}

\details{
The function \code{sfaendog()} implements the 4-step \sQuote{recipe} detailed 
in Latruffe \emph{et al}. (2017, p.788).
\cr \cr
The use of basic formula operators generally involved in model formulae, such as 
\code{":"}, \code{"*"}, or \code{"^"}, are allowed in \code{x.exo}, \code{x.endo}, 
\code{c.var}, \code{ineff} and \code{inst}. \cr
As in function \code{\link[=formula]{formula}}, the function \code{\link[=AsIs]{I()}} 
can also be used to inhibit the interpretation of operators such as \code{"+"}, 
\code{"-"}, \code{"*"} and \code{"^"} as formula operators, so that they are used as 
arithmetical operators.
\cr

\subsection{- Stochastic production frontier model with a single endogenous input}{

\code{sfaendog()} assumes a Cobb-Douglas functional form for the production frontier.

In this case, the stochastic frontier production model is written as: 
\deqn{
\ln\textrm{y}=\boldsymbol{\alpha'}_0\ln\textbf{x}-\eta\exp(\boldsymbol{\theta'}_0
\textbf{z})+\textrm{v}}{ln(y) = \alpha'_0 ln(x) - \eta exp(\theta'_0z) + v}
where \eqn{\textrm{ln}}{ln} is the log; \eqn{\textrm{y}}{y} is the observed output; 
\eqn{\boldsymbol{\alpha}_0}{\alpha_0} and \eqn{\boldsymbol{\theta}_0}{\theta_0} are 
vectors of parameters to be estimated; \eqn{\textbf{x}}{x} is a vector containing 
the inputs as well as a constant term one; \eqn{\textrm{v}}{v} is a random term 
which accounts for the effects of unobserved heterogeneity across observations and 
for stochastic events affecting the production process; 
\eqn{\eta\exp(\boldsymbol{\theta'}_0\textbf{z})}{\eta exp(\theta'_0z)} is a non-negative 
term accounting for the presence of technical inefficiency; \eqn{\textbf{z}}{z} 
is a vector of variables influencing technical inefficiency (the inefficiency effects), 
including a constant term one; and \eqn{\eta}{\eta} is a positive random term with mean one. 

If all inputs are exogenous, the above equation can be estimated by NLS. 
\cr
\cr
\code{sfaendog()} accounts for the endogeneity of one input, with a MM estimator based 
on Chamberlain's (1987) \sQuote{efficient instruments}.
In the case of endogeneity of one input, the stochastic frontier production model 
defined above can  be rewritten as: 
\deqn{
\ln\textrm{y}=\boldsymbol{\alpha}_{\textup{x},0}\ln\mathbf{x}_x+\alpha_{e,0}\ln{x_e}-
\eta\exp(\boldsymbol{\theta'}_0\textbf{z})+\textrm{v}}{ln(y) = \alpha_(x,0) 
ln(x)_x + \alpha_(e,0) ln(x_e) - \eta exp(\theta'_0z) + v}
where \eqn{\mathbf{x}_x}{x_x} is the vector containing the exogenous inputs and the 
constant term one; \eqn{x_e} is the endogenous input; and the subscript \eqn{0} 
denotes the \sQuote{true} parameters value. The vector of exogenous variables is 
denoted by \eqn{\textbf{w}=(\ln\textbf{x}_x,\textbf{q},\textbf{z})}{w = (ln(x)_x,q,z)} 
where \eqn{\textbf{q}}{q} is the vector of external instrumental variables. 

Assuming that \eqn{E[\textrm{v}|\textbf{w}]=0}{E[v|w]=0}, and that \eqn{\eta}{\eta} and 
\eqn{(\textrm{v},\textbf{w})}{(v,w)} are independent, the stochastic frontier 
production model can be rewritten as: 
\deqn{\ln\textrm{y}=\boldsymbol{\alpha}_{\textup{x},0}\ln\mathbf{x}_x+\alpha_{e,0}\ln{x_e}-
\exp(\boldsymbol{\theta'}_0\textbf{z})+e\;\;\textrm{with}\;\;E[e|\textbf{w}]=0}{ln(y) 
= \alpha_(x,0) ln(x)_x + \alpha_(e,0) ln(x_e) - exp(\theta'_0z) + e   With   E[e|w]=0}
where the error term \eqn{e} is defined as \eqn{e\equiv 
e(\boldsymbol{\delta}_0)=\ln\textrm{y}-\boldsymbol{\alpha}_{\textup{x},0}\ln\textbf{x}_x 
-\alpha_{e,0}\ln{x_e}+\exp(\boldsymbol{\theta'}_0\textbf{z})}{e ≡ e(\delta_0) = 
ln(y) - \alpha_(x,0) ln(x)_x - \alpha_(e,0) ln(x_e) + exp(\theta'_0z)}
\cr
}

\subsection{- Estimation 'recipe'}{

The estimation 'recipe' detailed in Latruffe \emph{et al}. (2017, p.788) and implemented 
through \code{sfaendog()} consists in the following four steps: \cr

\itemize{
\item{\strong{Step1:} Ordinary Least Squares (OLS) estimation of \eqn{\ln{x_e}}{ln(x_e)} 
  on a set of external instrumental variables (\eqn{\textbf{q}}{q}) and all exogenous 
  variables included in the stochastic frontier production model. This generates the 
  predicted values of \eqn{\ln{x_e}}{ln(x_e)} to be used as instruments in \code{Step3}. \cr
  The strength of the external instrumental variables (\eqn{\textbf{q}}{q}) is measured by 
  testing, with a Fisher test, the nullity of the parameters related to these external 
  instrumental variables. \cr
  This \code{Step1} corresponds to \emph{Step 1} of the estimation 'recipe' presented in 
  Latruffe \emph{et al}. (2017, p.788).}
\cr
\item{\strong{Step2:} NLS estimation of the stochastic frontier production model, 
  to compute a \bold{non consistent and non efficient} estimator to be used in \code{Step3}. \cr
  \code{Step2} consists in three sub-steps (not detailed as such in 
  Latruffe \emph{et al}.'s (2017, p.788) recipe):}
\itemize{
\item{\bold{\emph{Step2a.i}:} OLS estimation of a production model with the output 
    as the dependent variable, and the explanatory variables being the inputs and the 
    non-input variables influencing the output. \cr
    This provides predicted residuals to be used in \code{Step2a.ii}, and parameters to be 
    used as starting values for the variables in the production part of the stochastic frontier 
    production model in \code{Step2b}.}
\item{\bold{\emph{Step2a.ii}:} OLS estimation of the predicted residuals of \code{Step2a.i} 
    on the variables influencing technical inefficiency. \cr
    This provides parameters to be used as starting values for the variables in the inefficiency 
    effect part of the stochastic frontier production model in \code{Step2b}.}
\item{\bold{\emph{Step2b}:} NLS estimation of the stochastic frontier production model, 
    using starting values obtained from \code{Step2a.i} and \code{Step2a.ii}. \cr
    This sub-step corresponds to \emph{Step 2} of the estimation 'recipe' presented in 
    Latruffe \emph{et al}. (2017, p.788).}
\cr
}
\item{\strong{Step3:} Estimation of the stochastic frontier production model, using the 
  predicted values of the endogenous input obtained from \code{Step1} and using the parameters 
  obtained from \code{Step2b} as starting values. The estimation is done with GMM. \cr
  \code{Step3} computes a \bold{consistent but non efficient} estimator to be used in \code{Step4}. \cr
  This \code{Step3} corresponds to \emph{Step 3} of the estimation 'recipe' presented in 
  Latruffe \emph{et al}. (2017, p.788).}
\cr
\item{\strong{Step4:} Estimation of the stochastic frontier production model, using the 
  predicted values of the endogenous input obtained from \code{Step1} the parameters 
  obtained from \code{Step3} as starting values. This estimation is done with GMM. \cr
  \code{Step4} computes a \strong{consistent and efficient} estimator, and returns 
  the final results of the estimation of the stochastic frontier production model. \cr
  This \code{Step4} corresponds to \emph{Step 4} of the estimation 'recipe' presented in 
  Latruffe \emph{et al}. (2017, p.788).}\cr}
  }
Please note that, the applicability of default options in \code{nls.algo}, \code{gmm.kernel}, and \code{gmm.optim} 
is highly data-dependent and the user may have to play around with different options.
}

\value{
\code{sfaendog} returns a list of class \verb{'sfaendog'}.

The object of class \verb{'sfaendog'} is a list containing at least the following 
components: 
  \item{Step1}{Results of \code{Step1} (see \code{Estimation 'recipe'}), inheriting from 
  class \verb{'lm'} for which details on components are given in \code{\link[stats]{lm}}.}

  \item{FishTest}{The list of external instrumental variables (\code{inst}) used in 
  the specification, as well as Fisher test results showing the strength of these 
  external instrumental variables.}
  
  \item{Step2}{Results of \code{Step2b} (see \code{Estimation 'recipe'}), inheriting from 
  class \verb{'nls'} for which details on components are given in \code{\link[stats]{nls}}.}
  
  \item{Step3}{Results of \code{Step3} (see \code{Estimation 'recipe'}), inheriting from 
  class \verb{'gmm'} for which details on components are given in \code{\link[gmm]{gmm}}.}
  
  \item{Step4}{Results of \code{Step4} (see \code{Estimation 'recipe'}), inheriting from 
  class \verb{'gmm'} for which details on components are given in \code{\link[gmm]{gmm}}.}

The function \code{\link[=summary.sfaendog]{summary}} is used to obtain and print 
a summary of the results.
}

\references{
Chamberlain G. (1987). Asymptotic Efficiency in Estimation with Conditional Moment
Restrictions. \emph{Journal of Econometrics}, \bold{34}(3), 305--334. 
\url{https://doi.org/10.1016/0304-4076(87)90015-7}

Latruffe L., Bravo-Ureta B.E., Carpentier A., Desjeux Y., and Moreira V.H. (2017). 
Subsidies and Technical Efficiency in Agriculture: Evidence from European Dairy Farms. 
\emph{American Journal of Agricultural Economics}, \bold{99}(3), 783--799. 
\url{https://doi.org/10.1093/ajae/aaw077}
}

\author{Yann Desjeux, Laure Latruffe}

\seealso{\code{\link[=summary.sfaendog]{summary}} for creating and printing summary results.}

\examples{
\dontrun{
  y <- "farm_output"
  x.exo <- c("agri_land", "tot_lab", "tot_asset")
  x.endo <-  "costs"
  c.var <- c("LFA", "T", "I(T^2)")
  ineff <- c("hired_lab", "rented_land", "debt_asset", "subs", "region", "region:T")
  inst <- c("milkprice", "I(milkprice^2)", "price_ind", "milkprice:region")
  
  RES <- sfaendog(y, x.exo, x.endo, c.var, ineff, inst, data=Farms)
  
  summary(RES)}
}

\keyword{models}
