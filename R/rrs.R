#' A Robust Regression Simulation
#'
#' @param formula An R formula
#' @param fixed A vector of strings for variables that do not need to vary (default = NA)
#' @param fe A vector of strings for variables that are considered fixed effects (default = NA)
#' @param data The dataframe to be used
#' @param k Number of variables in each regression that vary (default = 4)
#'
#' @return A list of two dataframes, one with the coefficients and one with t-values of the variables
#'
#' @importFrom stats lm coef model.frame model.matrix
#' @importFrom utils combn
#' @importFrom lfe felm
#'
#' @export
#'
#' @examples
#' result <- rrs(mpg ~., data = mtcars, k = 4)
rrs <- function(formula, fixed = NA, fe = NA, data, k = 4){

  X <- model.matrix(formula, data = data)
  y <- as.matrix(model.frame(formula, data = data)[1])

  X <- X[ ,2:ncol(X)] # remove constant

  if ( is.na(fixed[1]) ){
    if (is.na(fe[1]) ){
      X_var <- X
      f <- 0
      K <- ncol(X_var)
      names_X <- colnames(X_var)
    } else {
      d_fe <- data[fe]
      var_index <- !colnames(X) %in% fe
      X_var <- X[ , var_index]
      f <- 0
      K <- ncol(X_var)
      names_X <- colnames(X_var)
    }
  } else {
    if (is.na(fe[1]) ){
      d_fixed <- data[fixed]
      fixed_index <- colnames(d_fixed)
      var_index <- !colnames(X) %in% fixed
      X_var <- X[ , var_index]
      f <- length(fixed_index)
      K <- ncol(X_var)
      names_X <- c(fixed_index, colnames(X_var))
    } else {
      d_fe <- data[fe]
      d_fixed <- data[fixed]
      fixed_index <- colnames(d_fixed)
      var_index <- (!colnames(X) %in% fixed) & (!colnames(X) %in% fe)
      X_var <- X[ , var_index]
      f <- length(fixed_index)
      K <- ncol(X_var)
      names_X <- c(fixed_index, colnames(X_var))
    }
  }

  models <- combn(K, k)

  nr_poss    <- ncol(models)
  if (is.na(fe[1]) ){
    coeff_mat  <- matrix(NA, nrow = nr_poss, ncol = f + K + 1 )
    t_mat      <- matrix(NA, nrow = nr_poss, ncol = f + K + 1 )
  } else {
    coeff_mat  <- matrix(NA, nrow = nr_poss, ncol = f + K  )
    t_mat      <- matrix(NA, nrow = nr_poss, ncol = f + K  )
  }

  if ( is.na(fixed[1]) ){
    if (is.na(fe[1]) ){
      for(i in 1:ncol(models)){
        m <- lm(y ~ X_var[ ,models[ ,i]])
        coeff_mat[i , c(1 , (models[ ,i] + 1) ) ]  <- m$coefficients
        t_mat[i , c(1 , (models[ ,i] + 1 ) ) ]  <- m$coefficients/coef(summary(m))[,2]
      }
    } else {
      for(i in 1:ncol(models)){
        formula_fe <- formula(paste("y~0+", paste(colnames(X_var[ ,models[ ,i]] ), collapse = "+") , "|",
                                    paste(c(paste(colnames(d_fe))), collapse = " + " )) )
        m <- felm(formula_fe, data = as.data.frame(data) )
        coeff_mat[i , c((models[ ,i] ) ) ]  <- m$coefficients
        t_mat[i , c((models[ ,i] ) ) ]  <- m$coefficients/coef(summary(m))[,2]
      }
  }

  } else {
    if (is.na(fe[1]) ){
      for(i in 1:ncol(models)){
        m <- lm(y ~ as.matrix(d_fixed[, fixed_index]) + X_var[ ,models[ ,i]])
        coeff_mat[i , c(1: (1+f) , (models[ ,i] + 1 + f) ) ]  <- m$coefficients
        t_mat[i , c(1: (1+f) , (models[ ,i] + 1 + f) ) ]  <- m$coefficients/coef(summary(m))[,2]
      }
    } else {
        for(i in 1:ncol(models)){
          formula_fe <- formula(paste("y~0+", paste(colnames(d_fixed), collapse = "+"), "+"  ,
                                      paste(colnames(X_var[ ,models[ ,i]] ), collapse = "+"), "|",
                                      paste(c(paste(colnames(d_fe))), collapse = " + " )) )
          m <- felm(formula_fe, data = as.data.frame(data) )
          coeff_mat[i , c(1:f , (models[ ,i] + f) ) ]  <- m$coefficients
          t_mat[i , c(1:f , (models[ ,i] + f) ) ] <- m$coefficients/coef(summary(m))[,2]
        }
    }
  }

  if (is.na(fe[1]) ){
    colnames(coeff_mat) <- c("constant", names_X)
    colnames(t_mat) <- c("constant", names_X)
  } else {
    colnames(coeff_mat) <- names_X
    colnames(t_mat) <- names_X
  }

  return(list(coeff_mat, t_mat))

}
