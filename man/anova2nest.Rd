\name{anova2nest}
\alias{anova2nest}
\title{
\enc{Anàlisi}{Analisi} de la \enc{Variància}{Variancia} amb models de dos factors jerarquitzats aleatoris o mixtos
}
\description{
Aquesta funció recalcula els elements de la taula ANOVA en el cas d'un model de dos factors quan algun d'ells és aleatori.
}
\usage{
anova2nest(aovObj, random = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aovObj}{
Un objecte de la classe \code{aov} o \code{lm}.
}
  \item{random}{
Un vector amb el nom del factor o factors aleatoris.
}
}
\details{
Per tal de contrastar correctament la significació del factor aleatori, una vegada tenim un objecte \code{aov} o \code{lm} del model amb factors fixos, aquesta funció recalcula els estadístics \emph{F} i els \emph{p}-valors corresponents de la taula ANOVA amb les mitjanes de les sumes de quadrats apropiades quan un factor és aleatori.
}
\value{
Un objecte de la classe \code{c("anova", "data.frame")}.
}
\references{
%% ~put references to the literature/web site here ~
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
qualitat <- c(6,6.3,6.1,6.7,5,5.3,5.7,5.5,6,6.1,6.7,6.2,4.9,5.5,5.2,5.1)
maquina <- gl(4,4)
operari <- gl(2,8,labels=c("A","B"))

fix.aov <- aov(qualitat ~ operari + maquina \%in\% operari)
anova(fix.aov)

anova2nest(fix.aov, random = "maquina")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
