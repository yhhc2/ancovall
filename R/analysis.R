

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

  #aov.res <- aov(formula.to.use, data=inputted.data)
  #Anova(aov.res)

  #This gives the same results as the aov.res. So formula are equivalent.
  #aov.resV2 <- aov(Left_Lateral_Ventricle ~ AGE*HIV + GENDER*HIV, data=inputted.data)
  #Anova(aov.resV2)

  #lm() gives the same result if type = 3 is used for aov()
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
#' p-value is significant for the interaction terms of Levene's test, then
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

