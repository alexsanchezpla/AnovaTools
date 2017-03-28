\name{AnovaTools-package}
\alias{AnovaTools-package}
\alias{AnovaTools}
\docType{package}
\title{
\enc{Anàlisi}{Analisi} de la \enc{Variància}{Variancia} amb models de dos i tres factors aleatoris o mixtos
}
\description{
Aquest paquet conté algunes funcions que recalculen els elements de la taula ANOVA en el cas d'un model de dos o tres factors quan algun d'ells és aleatori. Hi ha una funció pel model amb dos factors creuats i unes altres per models jerarquitzats de dos i tres factors.
}
\details{
\tabular{ll}{
Package: \tab AnovaTools\cr
Type: \tab Package\cr
Version: \tab 0.7.2\cr
Date: \tab 2013-11-07\cr
License: \tab GPL (>= 2)\cr
}
}
\author{
Jordi Ocaña <jocana@ub.edu>

Maintainer: Francesc Carmona <fcarmona@ub.edu>
}
\references{
Carles M. Cuadras (2000) \emph{Problemas de Probabilidades y Estadística}. Apartat 11.4 del volum \bold{2}. EUB, Barcelona.
}
\keyword{ package }
\seealso{
\code{\link[nlme]{lme}}, \code{\link{anova2cross}}, \code{\link{anova2nest}}, \code{\link{anova3nest}}
}
\examples{
# anova2cross(aovObj, random = NULL, restricted = TRUE)
# anova2nest(aovObj, random = NULL)
# anova3nest(aovObj, random = NULL)
}
