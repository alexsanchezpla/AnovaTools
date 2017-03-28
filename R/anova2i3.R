anova2cross <- function(aovObj, random = NULL, restricted = TRUE) {
  nrandom <- length(random)
  aovTab <- anova(aovObj)
  if (nrandom == 0) return(aovTab)
  yName <- as.character(aovObj$call$formula[[2]])
  modelNames <- names(aovObj$model)
  if (!all(match(random, modelNames[-match(yName, modelNames)], nomatch = 0))) 
    stop("Algun dels factors aleatoris indicats no es un factor o no forma part del model") 
  interacEff <- grep(":", rownames(aovTab))
  if (any(interacEff)) {
      # Interaction term present
      msInterac <- aovTab[interacEff,3]
      dfInterac <- aovTab[interacEff,1]
      switch( nrandom,
        {
          # mixed model
          if (restricted) {
            fixedName <- modelNames[-match(c(yName, random), modelNames)]
            iFixedName <- pmatch(fixedName, rownames(aovTab)[1:2])
            fixedF <- aovTab[iFixedName, "Mean Sq"] / msInterac
            aovTab[iFixedName,"F value"] <- fixedF
            aovTab[iFixedName,"Pr(>F)"] <- pf(fixedF, aovTab[iFixedName,1], dfInterac, lower.tail = FALSE)
          } else {
            correctF <- aovTab[1:2, "Mean Sq"] / msInterac
            aovTab[1:2,"F value"] <- correctF
            aovTab[1:2,"Pr(>F)"] <- pf(correctF, aovTab[1:2,1], dfInterac, lower.tail = FALSE)
          }
        },
        {
          # 2 random factors
          correctF <- aovTab[1:2, "Mean Sq"] / msInterac
          aovTab[1:2,"F value"] <- correctF
          aovTab[1:2,"Pr(>F)"] <- pf(correctF, aovTab[1:2,1], dfInterac, lower.tail = FALSE)
        }
      )
  }
  return(aovTab)
}

anova2nest <- function(aovObj, random = NULL) {
  nrandom <- length(random)
  aovTab <- anova(aovObj)
  if (nrandom == 0) return(aovTab)
  yName <- as.character(aovObj$call$formula[[2]])
  modelNames <- names(aovObj$model)
  if (!all(match(random, modelNames[-match(yName, modelNames)], nomatch = 0))) 
    stop("Algun dels factors aleatoris indicats no es un factor o no forma part del model") 
  nestEff <- grep(":", rownames(aovTab))
  if (length(nestEff) != 1)
    stop("'anova2nest' espera rebre un model jerarquic de 2 factors")
  msNest <- aovTab[nestEff,3]
  dfNest <- aovTab[nestEff,1]
  correctF <- aovTab[1, "Mean Sq"] / msNest
  aovTab[1,"F value"] <- correctF
  aovTab[1,"Pr(>F)"] <- pf(correctF, aovTab[1,1], dfNest, lower.tail = FALSE)
  return(aovTab)
}

anova3nest <- function(aovObj, random = NULL) {
  waterfall <- function()
  {
    correctF <- aovTab[1,"Mean Sq"] / aovTab[2,"Mean Sq"]
    aovTab[1,"F value"] <- correctF
    aovTab[1,"Pr(>F)"] <- pf(correctF, aovTab[1,1], aovTab[2,1], lower.tail = FALSE)
    correctF <- aovTab[2, "Mean Sq"] / aovTab[3, "Mean Sq"]
    aovTab[2,"F value"] <- correctF
    aovTab[2,"Pr(>F)"] <- pf(correctF, aovTab[2,1], aovTab[3,1], lower.tail = FALSE)
    return(aovTab)
  }
  nrandom <- length(random)
  aovTab <- anova(aovObj)
  if (nrandom == 0) return(aovTab)
  yName <- as.character(aovObj$call$formula[[2]])
  modelNames <- names(aovObj$model)
  factorNames <- modelNames[-match(yName, modelNames)]
  if (!all(match(random, factorNames, nomatch = 0))) 
    stop("Algun dels factors aleatoris indicats no es un factor o no forma part del model") 
  if (length(grep(":", rownames(aovTab))) != 2)
    stop("'anova3nest' espera rebre un model jerarquic de 3 factors")
  firstRandom <- min(match(random, factorNames))
  switch( firstRandom,
    return(waterfall()),
    return(waterfall()),
    {
      msNest <- aovTab[firstRandom,3]
      dfNest <- aovTab[firstRandom,1]
      correctF <- aovTab[1, "Mean Sq"] / msNest
      aovTab[1,"F value"] <- correctF
      aovTab[1,"Pr(>F)"] <- pf(correctF, aovTab[1,1], dfNest, lower.tail = FALSE)
      correctF <- aovTab[2, "Mean Sq"] / msNest
      aovTab[2,"F value"] <- correctF
      aovTab[2,"Pr(>F)"] <- pf(correctF, aovTab[2,1], dfNest, lower.tail = FALSE)
      return(aovTab)
    }
  )
}

