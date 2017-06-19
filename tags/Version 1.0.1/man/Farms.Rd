\name{Farms}
\alias{Farms}
\docType{data}
\title{Data set of farm accountancy data}
\description{The \code{Farms} data frame contains simulated data for 2,500 dairy farms.}
\usage{Farms}
\format{
This data frame contains the following 14 variables:\cr

\tabular{ll}{
  \code{farm_output} \tab Farm total output, in constant Euros.\cr
  \code{agri_land} \tab Farm agricultural area, in hectares.\cr
  \code{tot_lab} \tab Total labour used on the farm, in hours.\cr
  \code{tot_asset} \tab Total assets of the farm (excluding land), in constant Euros.\cr 
  \code{LFA} \tab Dummy variable indicating whether the farm is located within a\cr
  \tab Less Favoured Area (\code{1}) or not (\code{0}).\cr
  \code{hired_lab} \tab Ratio of the farm hired labour to total labour (\code{tot_lab}).\cr
  \code{rented_land} \tab Ratio of the farm rented land to agricultural area (\code{agri_land}).\cr
  \code{debt_asset} \tab Ratio of the farm total debts to total assets.\cr
  \code{costs} \tab Costs of variable inputs used on the farm, in constant Euros.\cr
  \code{subs} \tab Amount of the farm production subsidies received per hectare of agricultural area\cr
  \tab (\code{agri_land}), in constant Euros.\cr
  \code{region} \tab Factor variable representing the administrative region of the farm.\cr
  \code{milkprice} \tab Average farm milk price per ton of milk, in constant Euros.\cr
  \code{price_ind} \tab National yearly price index of variable inputs.\cr
  \code{T} \tab Time trend.
  }
}

\source{Simulated farmers' accountancy data}

\examples{
head(Farms)
str(Farms)
summary(Farms)
  
lm.output <- lm(farm_output ~ agri_land + tot_lab + tot_asset + costs, data = Farms)
summary(lm.output)
}

\keyword{datasets}
