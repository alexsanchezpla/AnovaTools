\name{anova3nest}
\alias{anova3nest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\enc{Anàlisi}{Analisi} de la \enc{Variància}{Variancia} amb models de tres factors jerarquitzats aleatoris o mixtos
}
\description{
Aquesta funció recalcula els elements de la taula ANOVA en el cas d'un model de tres factors quan algun d'ells és aleatori.
}
\usage{
anova3nest(aovObj, random = NULL)
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
# Exemple dels "bessons"
CAnaerobia <- c(20.6, 20.62, 20.3, 20.7, 22.1, 22.4, 22.3, 23, 21.4, 23.4, 
              24, 23.2, 24, 23.1, 23.5, 22, 21.6, 22.62, 20.5, 21.7, 22.2, 
              24.4, 23.3, 25, 24.4, 22.4, 21, 21.2, 20, 21.1, 22.1, 22)
zigosi <- gl(2,16,labels=c("mono","di"))
parella <- gl(4,4,length=32)
individu <- gl(2,2,length=32)
# La forma més explícita d'indicar el model lineal en el qual
# "parella" és jeràrquicament dins "zigosi" i "individu" és dins les 
# combinacions de "zigosi" i "parella" és:
bessons.aov <- aov(CAnaerobia ~ zigosi + parella \%in\% zigosi 
                      + individu \%in\% (parella \%in\% zigosi))

# Forma equivalent, més concisa
# bessons.aov <- aov(CAnaerobia ~ zigosi/parella/individu)

anova(bessons.aov)
# La taula ANOVA anterior no és correcta.

# La taula ANOVA correcta donat que el primer factor és fix i els dos darrers
# factors són aleatoris és:
anova3nest(bessons.aov, random = c("parella", "individu"))

# Equivalent a:
anova3nest(bessons.aov, random = c("individu", "parella"))
# l'ordre en el qual enumerem els factors aleatoris no importa

# També equivalent a:
anova3nest(bessons.aov, random = "parella")
# tots els jeràrquicament inferiors d'un aleatori els considera aleatoris
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
