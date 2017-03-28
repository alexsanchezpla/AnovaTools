temps <- c(548,520,637,619,776,818,641,678,701,846,858,855,517,493,618,876,741,849,602,719,731,628,595,687)
home <- gl(8, 3)
tractament <- gl(3, 1, length=24, labels=LETTERS[1:3])

fix.aov <- aov(temps ~ tractament + home)

(ac <- anova2cross(fix.aov, random="home"))

# Components de la variància
mm <- matrix(c(1,1,ac["tractament","Df"]+1,0), ncol=2)
solve(mm,ac[-1,"Mean Sq"])

anova2cross <- function (aovObj, random = NULL, restricted = TRUE) 
{
  nrandom <- length(random)
  aovTab <- anova(aovObj)
  if (nrandom == 0) 
    return(aovTab)
  yName <- as.character(aovObj$call$formula[[2]])
  modelNames <- names(aovObj$model)
  if (!all(match(random, modelNames[-match(yName, modelNames)], 
                 nomatch = 0))) 
    stop("Algun dels factors aleatoris indicats no \303\251s un factor o no forma part del model")
  interacEff <- grep(":", rownames(aovTab))
  if (any(interacEff)) {
    msInterac <- aovTab[interacEff, 3]
    dfInterac <- aovTab[interacEff, 1]
    switch(nrandom, {
      if (restricted) {
        fixedName <- modelNames[-match(c(yName, random), 
                                       modelNames)]
        iFixedName <- pmatch(fixedName, rownames(aovTab)[1:2])
        fixedF <- aovTab[iFixedName, "Mean Sq"]/msInterac
        aovTab[iFixedName, "F value"] <- fixedF
        aovTab[iFixedName, "Pr(>F)"] <- pf(fixedF, aovTab[iFixedName, 
                                                          1], dfInterac, lower.tail = FALSE)
      } else {
        correctF <- aovTab[1:2, "Mean Sq"]/msInterac
        aovTab[1:2, "F value"] <- correctF
        aovTab[1:2, "Pr(>F)"] <- pf(correctF, aovTab[1:2, 
                                                     1], dfInterac, lower.tail = FALSE)
      }
    }, {
      correctF <- aovTab[1:2, "Mean Sq"]/msInterac
      aovTab[1:2, "F value"] <- correctF
      aovTab[1:2, "Pr(>F)"] <- pf(correctF, aovTab[1:2, 
                                                   1], dfInterac, lower.tail = FALSE)
    })
  }
  return(aovTab)
}