\name{anova2cross}
\alias{anova2cross}
\title{
\enc{Anàlisi}{Analisi} de la \enc{Variància}{Variancia} amb models de dos factors creuats aleatoris o mixtos
}
\description{
Aquesta funció recalcula els elements de la taula ANOVA en el cas d'un model de dos factors quan algun d'ells és aleatori.
}
\usage{
anova2cross(aovObj, random = NULL, restricted = TRUE)
}
\arguments{
  \item{aovObj}{
Un objecte de la classe \code{aov} o \code{lm}.
}
  \item{random}{
Un vector amb el nom del factor o factors aleatoris.
}
  \item{restricted}{
Lògic: El model ha de ser restringit?
}
}
\details{
Per tal de contrastar correctament la significació del factor aleatori i de la interacció, una vegada tenim un objecte \code{aov} o \code{lm} del model amb factors fixos, aquesta funció recalcula els estadístics \emph{F} i els \emph{p}-valors corresponents de la taula ANOVA amb les mitjanes de les sumes de quadrats apropiades quan un factor és aleatori.
}
\value{
Un objecte de la classe \code{c("anova", "data.frame")}.
}
\references{
Carles M. Cuadras (2000) \emph{Problemas de Probabilidades y Estadística}. Apartat 11.4 del volum \bold{2}. EUB, Barcelona.
}
\author{
Jordi Ocaña \email{jocana@ub.edu}
}
\note{
Les estimacions dels paràmetres associats a factors aleatoris que proporciona el \code{summary} no tenen sentit.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Del llibre de Cuadras pàg. 296 exercici 11.2

temps <- c(548,519,637,619,776,818,641,678,701,
           846,858,855,517,493,618,876,741,849,
           602,719,731,628,595,687)
home <- gl(8, 3)
tractament <- gl(3, 1, length=24, labels=LETTERS[1:3])

fix.aov <- aov(temps ~ tractament + home)

(ac <- anova2cross(fix.aov, random="home"))

# Components de la variància
mm <- matrix(c(1,1,ac["tractament","Df"]+1,0), ncol=2)
solve(mm,ac[-1,"Mean Sq"])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }

