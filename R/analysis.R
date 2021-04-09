

#' Checking homogeneity of regression slopes
#'
#' Returns information that can be used
#' to determine if the homogeneity of regression slopes, an assumption for
#' ANCOVA, is met.
#'
#'To test homogeneity of regression slopes, interaction terms need to be added
#'and the p-value should be assessed. If the p-value is significant for any
#'interaction term, then this means the assumption of homogeneity of regression
#'slopes is not met.
#'
#' @param inputted.data A dataframe
#' @param dependent.variable A string that specifies the column name of the column to use as the dependent variable. Column must be numeric.
#' @param independent.variable A string that specifies the column name of the column to use as the independent variable. Column can be numeric or factor. If it's a factor, then it can only have two levels.
#' @param covariates A vector of strings that specifies the columns names of the columns to be used as covariates. Columns can be numeric or factor.  If it's a factor, then it can only have two levels.
#'
#' @return A matrix with two rows. The first row specify what the values are in the second row. The second row:
#' The first element is the formula used to evaluate p-value of interaction terms. The remaining elements are
#' the p-values for each interaction term.
#'
#' @export
#'
#' @examples
#'
#' dependent.col <- c(10.1, 11.3, 12.1, 13.7, 14.2, 1.6, 2.3, 3.2, 4.1, 5.3)
#' independent.col <- as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
#' covariate.one.col <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
#' covariate.two.col <- as.factor(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
#'
#' inputted.data <- data.frame(dependent.col, independent.col, covariate.one.col,
#'                             covariate.two.col)
#'
#' results <- HomogeneityOfRegressionSlopes(inputted.data, "dependent.col",
#'                                          "independent.col",
#'                                          c("covariate.one.col", "covariate.two.col"))
#'
#' results
#'
HomogeneityOfRegressionSlopes <- function(inputted.data, dependent.variable,
                                          independent.variable, covariates){

  #Testing conditions
  # inputted.data <- working.data
  # dependent.variable <- "Left_Lateral_Ventricle"
  # independent.variable <- "HIV"
  # covariates <- c("AGE", "GENDER", "RACE")

  right.side.of.formula <- independent.variable
  for(i in 1:length(covariates)){

    right.side.of.formula <- paste(right.side.of.formula,
                                   " + ", covariates[[i]], " + ",
                                   covariates[[i]], "*", independent.variable, sep = "")

  }


  formula.to.use.character <- paste(dependent.variable, "~", right.side.of.formula,sep = "")
  formula.to.use <- stats::as.formula(formula.to.use.character)

  #lm.res <- lm(formula.to.use, data=inputted.data)
  #summary.res <- summary(lm.res) #use summary.res[4] to get coefficients

  #aov.res <- stats::aov(formula.to.use, data=inputted.data)
  #Anova(aov.res)

  #This gives the same results as the aov.res. So formula are equivalent.
  #aov.resV2 <-stats::aov(Left_Lateral_Ventricle ~ AGE*HIV + GENDER*HIV, data=inputted.data)
  #Anova(aov.resV2)

  #lm() gives the same result if type = 3 is used forstats::aov()
  #It doesn't matter which type of ANOVA is used because
  #I am only interested in the interaction terms and
  #the p-value of the interaction terms are the same for all three types of ANOVA.
  aov.resV3 <- stats::aov(formula.to.use, data=inputted.data)
  Anova.res <- car::Anova(aov.resV3, type = 3)
  Anova.res.dataframe <- as.data.frame(Anova.res)

  coefficient.names <- row.names(Anova.res.dataframe)
  p.values <- Anova.res.dataframe$`Pr(>F)`

  #Format output
  row1 <- c("Interaction terms Formula (test for homogeneity of slopes)")
  row2 <- c(formula.to.use.character)
  for(i in 1:length(covariates)){

    #index specifies the index of the interaction terms

    index <- length(covariates) + 2 + i
    column.name <- paste("P-value", coefficient.names[[index]])

    row1 <- c(row1, column.name)

    row2 <- c(row2, p.values[[index]])

  }

  interaction.effects.results <- rbind(row1, row2)

  return(interaction.effects.results)

}




#' Checking homogeneity of variance
#'
#' Returns information that can be used
#' to determine if the homogeneity of variance, an assumption for
#' ANCOVA, is met.
#'
#'To test homogeneity of regression slopes, the Levene test is used.
#'If the p-value is significant, then this means the assumption of homogeneity
#'of variance is not met.
#'
#'
#' @param inputted.data A dataframe
#' @param dependent.variable A string that specifies the column name of the column to use as the dependent variable. Column must be numeric.
#' @param independent.variable A string that specifies the column name of the column to use as the independent variable. Column can be numeric or factor. If it's a factor, then it can only have two levels.
#'
#' @return A matrix with two rows. The first row specify what the values are in the second row. The second row:
#' The first element is the formula used to evaluate Levene test. The next element is the p-value from the Levene test.
#'
#' @export
#'
#' @examples
#'
#' dependent.col <- c(10.1, 11.3, 12.1, 13.7, 14.2, 1.6, 2.3, 3.2, 4.1, 5.3)
#' independent.col <- as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
#' covariate.one.col <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
#' covariate.two.col <- as.factor(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
#'
#' inputted.data <- data.frame(dependent.col, independent.col, covariate.one.col,
#'                             covariate.two.col)
#'
#' results <- HomogeneityOfVariance(inputted.data, "dependent.col",
#'                                          "independent.col")
#'
#' results
#'
HomogeneityOfVariance <- function(inputted.data, dependent.variable,
                                  independent.variable){

  #Testing conditions
  #inputted.data <- working.data
  #dependent.variable <- "Left_Lateral_Ventricle"
  #independent.variable <- "HIV"

  formula.to.use.character <- paste(dependent.variable, "~", independent.variable,sep = "")
  formula.to.use <- stats::as.formula(formula.to.use.character)

  levene.res <- car::leveneTest(formula.to.use, inputted.data)

  row1 <- c("Levene Formula (test for homogeneity of variance)", "Levene P-value")
  row2 <- c(formula.to.use.character, levene.res[[3]][1])

  levene.results.organized <- rbind(row1, row2)

  return(levene.results.organized)

}

#' Checks assumptions for ANOVA/ANCOVA
#'
#' Returns information that can be used to determine if assumptions for ANCOVA
#' are met.
#'
#' Homogeneity of slopes and homogeneity of variance are both checked. If the
#' p-value is significant for any of the interaction terms or Levene's test, then
#' this means the assumptions are not met.
#'
#'
#' @param inputted.data A dataframe
#' @param dependent.variable A string that specifies the column name of the column to use as the dependent variable. Column must be numeric.
#' @param independent.variable A string that specifies the column name of the column to use as the independent variable. Column can be numeric or factor. If it's a factor, then it can only have two levels.
#' @param covariates A vector of strings that specifies the columns names of the columns to be used as covariates. Columns can be numeric or factor.  If it's a factor, then it can only have two levels.
#'
#' @return A matrix with two rows. The first row specify what the values are in the second row. The second row:
#' The first element is the formula used to evaluate p-value of interaction terms. The next elements are
#' the p-values for each interaction term. Following the p-value for interaction terms is the formula used to
#' evaluate Levene test. The next element is the p-value from the Levene test.
#'
#' @export
#'
#' @examples
#'
#' dependent.col <- c(10.1, 11.3, 12.1, 13.7, 14.2, 1.6, 2.3, 3.2, 4.1, 5.3)
#' independent.col <- as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
#' covariate.one.col <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
#' covariate.two.col <- as.factor(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
#'
#' inputted.data <- data.frame(dependent.col, independent.col, covariate.one.col,
#'                             covariate.two.col)
#'
#' results <- CheckAllAssumptionsANCOVA(inputted.data, "dependent.col",
#'                                          "independent.col",
#'                                          c("covariate.one.col", "covariate.two.col"))
#'
#' results
#'
#'
CheckAllAssumptionsANCOVA <- function(inputted.data, dependent.variable,
                                      independent.variable, covariates){

  #Also check for outliers
  #Also check for normality of dependent variable

  #Testing conditions
  # inputted.data <- working.data
  # dependent.variable <- "Left_Lateral_Ventricle"
  # independent.variable <- "HIV"
  # covariates <- c("AGE", "GENDER")

  regression.slopes.res <- HomogeneityOfRegressionSlopes(inputted.data, dependent.variable,
                                                         independent.variable, covariates)

  variance.res <- HomogeneityOfVariance(inputted.data, dependent.variable,
                                        independent.variable)

  combined.res <- cbind(regression.slopes.res, variance.res)

  return(combined.res)

}




#' ANCOVA computation
#'
#' ANCOVA computation is performed multiple ways with p-value of the variables
#' captured. The covariate corrected data is also captured.
#'
#' ANCOVA computation is performed 3 ways:
#' 1. Using independent variable and all covariates.
#' 2. Using independent variable and only covariates with p-value <0.05 as determined by method 1.
#' 3. Using independent variable and/or covariates only if they are selected by AIC.
#'
#' @param inputted.data A dataframe
#' @param dependent.variable A string that specifies the column name of the column to use as the dependent variable. Column must be numeric.
#' @param independent.variable A string that specifies the column name of the column to use as the independent variable. Column can be numeric or factor. If it's a factor, then it can only have two levels.
#' @param covariates A vector of strings that specifies the columns names of the columns to be used as covariates. Columns can be numeric or factor.  If it's a factor, then it can only have two levels.
#'
#' @return A list with two objects.
#'
#' The first object is a matrix with two rows. The first row specifies what the values are in the second row. The second row:
#' The first element is the formula used to evaluate ANCOVA with all covariates. The elements that are between
#' this formula and the next formula are the p-values for each variable. The element that comes next is the
#' formula that only includes significant covariates along with the independent variable. The elements that
#' are between this formula and the next formula are the p-values for each variable after doing ANCOVA with only variables
#' with coefficient that have p-value <0.05. The element that comes next
#' is the formula that only includes significant covariates (determined by AIC), and independent variable is only
#' included if it's determined to be significant by AIC. The following elements are the p-values for the variables
#' after doing ANCOVA with only the variables determined to be significant by AIC.
#'
#' The second object is a vector containing the dependent variable values corrected for covariates determined
#' to be significant by AIC. Example:
#' For a sample with a specified gender and age, if you want to get the
#' predicted value from the observed value after adjusting for gender and age,
#' then thus this formula:
#' PredictedVal =  ObservedVal - GlobalMean - (GenderCoefficient * gender) - (AgeCoefficient * age)
#'
#'
#' @export
#'
#' @examples
#'
#' dependent.col <- c(10.1, 11.3, 12.1, 13.7, 14.2, 1.6, 2.3, 3.2, 4.1, 5.3)
#' independent.col <- as.factor(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))
#' covariate.one.col <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
#' covariate.two.col <- as.factor(c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0))
#'
#' inputted.data <- data.frame(dependent.col, independent.col, covariate.one.col,
#'                             covariate.two.col)
#'
#' results <- ANCOVAWithFormattedOutput(inputted.data, "dependent.col",
#'                                          "independent.col",
#'                                          c("covariate.one.col", "covariate.two.col"))
#'
#' table <- results[[1]]
#'
#' ancova.corrected.values <- results[[2]]
#'
#'
ANCOVAWithFormattedOutput <- function(inputted.data, dependent.variable,
                                      independent.variable, covariates){


  #This will contain the ANCOVA adjusted values of the data.
  #This means the value will be corrected for covariates.
  running.pred.data <- NULL

  #------------------------------------------------------------------------------
  # Do ANCOVA with all covariates included.
  #------------------------------------------------------------------------------

  right.side.of.formula <- independent.variable
  for(i in 1:length(covariates)){

    right.side.of.formula <- paste(right.side.of.formula,
                                   " + ", covariates[[i]], sep = "")

  }


  formula.to.use.character <- paste(dependent.variable, "~", right.side.of.formula,sep = "")
  formula.to.use <- stats::as.formula(formula.to.use.character)

  aov.resV3 <- stats::aov(formula.to.use, data=inputted.data)
  Anova.res <- car::Anova(aov.resV3, type = 3)
  Anova.res.dataframe <- as.data.frame(Anova.res)

  coefficient.names <- row.names(Anova.res.dataframe)
  p.values <- Anova.res.dataframe$`Pr(>F)`

  covariates.that.are.significant <- NULL

  #Format output
  row1 <- c("Computation Formula")
  row2 <- c(formula.to.use.character)
  for(i in 1:length(coefficient.names)){

    column.name <- paste("P-value", coefficient.names[[i]])

    row1 <- c(row1, column.name)

    row2 <- c(row2, p.values[[i]])

    #If i is the index of a covariate and the covariate is significant, then
    #make a note of this.
    if(i>1 & i<length(coefficient.names) & p.values[[i]] < 0.05){
      covariates.that.are.significant <- c(covariates.that.are.significant, i)
    }

  }


  #------------------------------------------------------------------------------
  # Keep only covariates that are significant and redo ANCOVA.
  #------------------------------------------------------------------------------

  #Added independent variable, HIV status, before we need at least one variable.
  #Else stats::aov() will give an error.
  right.side.of.formula.only.sig.covariates <- independent.variable
  if(!is.null(covariates.that.are.significant)){
    for(i in 1:length(covariates.that.are.significant)){

      #Make sure independent variable is not included twice
      if( !(coefficient.names[[covariates.that.are.significant[i]]] == independent.variable)){

        right.side.of.formula.only.sig.covariates <- paste(right.side.of.formula.only.sig.covariates,
                                                           " + ", coefficient.names[[covariates.that.are.significant[i]]], sep = "")

      }

    }
  }



  formula.to.use.character.only.sig.covariates <- paste(dependent.variable, "~", right.side.of.formula.only.sig.covariates,sep = "")
  formula.to.use.only.sig.covariates <- stats::as.formula(formula.to.use.character.only.sig.covariates)


  aov.resV3.only.sig.covariates <- stats::aov(formula.to.use.only.sig.covariates, data=inputted.data)
  Anova.res.only.sig.covariates <- car::Anova(aov.resV3.only.sig.covariates, type = 3)
  Anova.res.dataframe.only.sig.covariates <- as.data.frame(Anova.res.only.sig.covariates)

  coefficient.names.only.sig.covariates <- row.names(Anova.res.dataframe.only.sig.covariates)
  p.values.only.sig.covariates <- Anova.res.dataframe.only.sig.covariates$`Pr(>F)`
  p.values.only.sig.covariates.name.added <- p.values.only.sig.covariates
  names(p.values.only.sig.covariates.name.added) <- coefficient.names.only.sig.covariates


  row1 <- c(row1, "Computation Formula With Only Significant Covariates")
  row2 <- c(row2, formula.to.use.character.only.sig.covariates)

  #Skip the first and last elements of coefficient.names because the first
  #is intercept and the last is residual. Go through all coefficients because
  #in the table I want to output all the coefficients and say N/A for the
  #ones that are indicated as not significant.
  for(i in 2:(length(coefficient.names)-1)){

    column.name <- paste("P-value", coefficient.names[[i]])

    row1 <- c(row1, column.name)

    if(coefficient.names[[i]] %in% coefficient.names.only.sig.covariates){

      row2 <- c(row2, as.numeric(p.values.only.sig.covariates.name.added[coefficient.names[[i]]]))

    }else{

      row2 <- c(row2, "N/A")

    }

  }

  ## The effect() function does not help. It just plots the mean for whatever
  ## coefficient you specify. For example, if the model is DEPENDENT ~ INDEPENDENT + COVARIATE
  ## and you do effect("INDEPENDENT", data), then it just plots the averages for each level
  ## in INDEPENDENT. It does not plot the covariate adjusted value.
  #effect.res <- effect("HIV", aov.resV3.only.sig.covariates.AIC)
  #plot(effect.res)

  #------------------------------------------------------------------------------
  # Do AIC step optimization to select for covariates instead of using p-value
  # method from above.
  #------------------------------------------------------------------------------

  #Include the independent variable for the above steps to see
  #if the independent variable might be significant if we only include
  #significant covariates.

  #For AIC, only included the independent variable if it's selected.

  c1.step <- try(stats::step(aov.resV3,trace=0))
  #AIC.coefficients.to.keep <- names(c1.step$coefficients)[-c(1)]

  c1.keep <- summary(c1.step)
  ccc <- c1.keep[[1]]
  ccc <- dimnames(ccc)[[1]]
  AIC.coefficients.to.keep <- ccc[-c(length(ccc))]

  #Trim the trailing white spaces from coefficient names
  AIC.coefficients.to.keep <- trimws(AIC.coefficients.to.keep)

  if(length(AIC.coefficients.to.keep) > 0){

    right.side.of.AIC.formula <- paste(AIC.coefficients.to.keep,collapse="+")
    formula.to.use.character.only.sig.covariates.AIC <- paste(dependent.variable, "~", right.side.of.AIC.formula,sep = "")
    formula.to.use.only.sig.covariates.AIC <- stats::as.formula(formula.to.use.character.only.sig.covariates.AIC)

    formula.to.use.character.only.sig.covariates.AIC <- paste(format(formula.to.use.only.sig.covariates.AIC))

    aov.resV3.only.sig.covariates.AIC <- stats::aov(formula.to.use.only.sig.covariates.AIC, data=inputted.data)
    Anova.res.only.sig.covariates.AIC <- car::Anova(aov.resV3.only.sig.covariates.AIC, type = 3)
    Anova.res.dataframe.only.sig.covariates.AIC <- as.data.frame(Anova.res.only.sig.covariates.AIC)

    coefficient.names.only.sig.covariates.AIC <- row.names(Anova.res.dataframe.only.sig.covariates.AIC)
    p.values.only.sig.covariates.AIC <- Anova.res.dataframe.only.sig.covariates.AIC$`Pr(>F)`
    p.values.only.sig.covariates.AIC.name.added <- p.values.only.sig.covariates.AIC
    names(p.values.only.sig.covariates.AIC.name.added) <- coefficient.names.only.sig.covariates.AIC


    row1 <- c(row1, "Computation Formula With Only Significant Covariates AIC")
    row2 <- c(row2, formula.to.use.character.only.sig.covariates.AIC)

    #Need to get the index for the coefficients to keep according to AIC.
    #covariates.that.are.significant.AIC <- match(AIC.coefficients.to.keep, coefficient.names)

    #Skip the first and last elements of coefficient.names because the first
    #is intercept and the last is residual.
    for(i in 2:(length(coefficient.names)-1)){

      column.name <- paste("P-value", coefficient.names[[i]])

      row1 <- c(row1, column.name)

      if(coefficient.names[[i]] %in% coefficient.names.only.sig.covariates.AIC){

        row2 <- c(row2, as.numeric(p.values.only.sig.covariates.AIC.name.added[coefficient.names[[i]]]))

      }else{

        row2 <- c(row2, "N/A")

      }

    }

    perm.temp <- inputted.data[, c(dependent.variable,
                                   AIC.coefficients.to.keep)]

    #------------------------------------------------------------------------------
    # Get covariate adjusted values
    #------------------------------------------------------------------------------

    # For a sample with a specified gender and age, if you want to get the
    # predicted value from the observed value after adjusting for gender and age,
    # then thus this formula:
    # PredictedVal =  ObservedVal - GlobalMean - (GenderCoefficient * gender) - (AgeCoefficient * age)


    if( (length(AIC.coefficients.to.keep) == 1) & (AIC.coefficients.to.keep[[1]] == independent.variable)){

      #if only the independent variable is kept, then no correction of the data
      #values should be made because the data does not need to be corrected for
      #covariates

      running.pred.data <- rep(NA,dim(inputted.data)[1])

     } else{

      #If only covariates are kept, then use the aov model to correct the data
      #values

      #If independent variable and covariate(s) are kept, then remake anova
      #model with only the covariates and use the anova model to correct the
      #values of the data for the covariates so that the effect of the
      #independent variable can be compared with the covariates corrected for

      #Remove the independent variable from the list of coefficients.
      AIC.coefficients.to.keep.removed.independent <- AIC.coefficients.to.keep[!(AIC.coefficients.to.keep %in% independent.variable)]


      right.side.of.AIC.formula <- paste(AIC.coefficients.to.keep.removed.independent,collapse="+")
      formula.to.use.character.only.sig.covariates.AIC <- paste(dependent.variable, "~", right.side.of.AIC.formula,sep = "")
      formula.to.use.only.sig.covariates.AIC <- stats::as.formula(formula.to.use.character.only.sig.covariates.AIC)

      formula.to.use.character.only.sig.covariates.AIC <- paste(format(formula.to.use.only.sig.covariates.AIC))

      aov.resV3.only.sig.covariates.AIC <- stats::aov(formula.to.use.only.sig.covariates.AIC, data=inputted.data)

      #------------Use the coefficients to correct values of the data.---------#

      #Has value of coefficients, but the name of variables is not exact. For example "RACE" can be assigned as "RACE2"
      #Might be because RACE is a factor and "2" was one of the levels.
      aov.coefficients <- aov.resV3.only.sig.covariates.AIC$coefficients
      aov.coefficients.no.intercept <- aov.coefficients[-c(1)]

      #Name of coefficients
      coefficient.names.no.intercept <- names(aov.resV3.only.sig.covariates.AIC$model)[-c(1)]

      #Combine correct name to coefficients
      aov.coefficients.no.intercept.name.assigned <- aov.coefficients.no.intercept
      names(aov.coefficients.no.intercept.name.assigned) <- coefficient.names.no.intercept


      #Get adjusted values
      #PredictedVal =  ObservedVal - GlobalMean - (GenderCoefficient * gender) - (AgeCoefficient * age)

      ObservedVal <- inputted.data[,dependent.variable]

      GlobalMean <- mean(inputted.data[,dependent.variable])

      PredictedVal <- ObservedVal - GlobalMean

      #Subtract each coefficient to adjust
      for(i in 1:length(aov.coefficients.no.intercept.name.assigned)){

        name.of.coefficient <- names(aov.coefficients.no.intercept.name.assigned)[i]

        coefficient.value <- as.numeric(aov.coefficients.no.intercept.name.assigned[name.of.coefficient])

        #Need to do this in case the data is a factor. Factor has to be binary
        #with 0 and 1 coding.
        value.from.data <- as.numeric(as.character(inputted.data[,name.of.coefficient]))

        PredictedVal <- PredictedVal - (coefficient.value * value.from.data)

      }

      running.pred.data <- PredictedVal

    }


    ##------------------End of code to get covariate adjusted values.


  }else{

    #If no coefficients are significant, then put N/A for all.
    #N/A for the formula
    row1 <- c(row1, "Computation Formula With Only Significant Covariates AIC")
    row2 <- c(row2, "N/A")

    for(i in 2:(length(coefficient.names)-1)){

      column.name <- paste("P-value", coefficient.names[[i]])
      row1 <- c(row1, column.name)
      row2 <- c(row2, "N/A")

    }

    running.pred.data <- rep(NA,dim(inputted.data)[1])

  }

  #------------------------------------------------------------------------------
  # Combine rows and output results
  #------------------------------------------------------------------------------

  names(running.pred.data) <- dependent.variable

  computation.results <- rbind(row1, row2)

  output <- list(computation.results, running.pred.data)

  return(output)

}


#' Checks ANCOVA assumptions and runs ANCOVA
#'
#' The homogeneity of slopes and variance assumptions are checked
#' Normality of residual distribution is not checked. ANCOVA is then
#' performed with all covariates and repeated with only covariates that
#' are significant.
#'
#'
#' Homogeneity of slopes and homogeneity of variance are both checked. If the
#' p-value is significant for any of the interaction terms or Levene's test, then
#' this means the assumptions are not met.
#'
#' ANCOVA computation is performed 3 ways:
#' 1. Using independent variable and all covariates.
#' 2. Using independent variable and only covariates with p-value <0.05 as determined by method 1.
#' 3. Using independent variable and/or covariates only if they are selected by AIC.
#'
#'
#'
#' @param inputted.data A dataframe
#' @param dependent.variable A string that specifies the column name of the column to use as the dependent variable. Column must be numeric.
#' @param independent.variable A string that specifies the column name of the column to use as the independent variable. Column can be numeric or factor. If it's a factor, then it can only have two levels.
#' @param covariates A vector of strings that specifies the columns names of the columns to be used as covariates. Columns can be numeric or factor.  If it's a factor, then it can only have two levels.
#'
#'
#' @return A matrix with two rows. The first row specifies what the values are in the second row. The second row:
#'
#' The first element is the formula used to evaluate p-value of interaction terms. The next elements are
#' the p-values for each interaction term. Following the p-value for interaction terms is the formula used to
#' evaluate Levene test. The next element is the p-value from the Levene test.
#'
#' The next element is the formula used to evaluate ANCOVA with all covariates. The elements that are between
#' this formula and the next formula are the p-values for each variable. The element that comes next is the
#' formula that only includes significant covariates along with the independent variable. The elements that
#' are between this formula and the next formula are the p-values for each variable after doing ANCOVA with only variables
#' with coefficient that have p-value <0.05. The element that comes next
#' is the formula that only includes significant covariates (determined by AIC), and independent variable is only
#' included if it's determined to be significant by AIC. The following elements are the p-values for the variables
#' after doing ANCOVA with only the variables determined to be significant by AIC.
#'
#'
#'
#' @export
#'
#' @examples
ANCOVACheckedAssumptionsAndResults <- function(inputted.data, dependent.variable,
                                               independent.variable, covariates){

  assumption.res <- CheckAllAssumptionsANCOVA(inputted.data, dependent.variable,
                                              independent.variable, covariates)

  computation.res <- ANCOVAWithFormattedOutput(inputted.data, dependent.variable,
                                               independent.variable, covariates)[[1]]


  combined.res <- cbind(assumption.res, computation.res)

  return(combined.res)


}
