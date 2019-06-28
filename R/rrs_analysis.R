#' Analyse the robust regression results by providing a table with descriptives
#'
#' @param coeff A dataframe of coefficients
#' @param t_values A dataframe of t-values
#'
#' @return A dataframe with the descriptives
#' @import dplyr
#' @importFrom stats lm coef model.frame model.matrix quantile sd
#' @importFrom utils combn
#' @export
#'
#' @examples
#' result <- rrs(mpg ~., data = mtcars, k = 4)
#' coeff <- result[1]
#' t_values <- result[2]
#' analyse_meta(coeff, t_values)
analyse_meta <- function(coeff, t_values) {
  n = nrow(coeff)
  coeff <- as.data.frame(coeff)
  t_values <- as.data.frame(t_values)
  Positive <- as.data.frame(coeff > 0)
  Significant <- as.data.frame(abs(t_values) >=  1.985)
  PosSignificant <- as.data.frame(t_values >=  1.985)
  NegSignificant <- as.data.frame(t_values <= -1.985)
  LeftSideBoundsRange <- as.data.frame(coeff - 2*coeff/t_values)
  RightSideBoundsRange <- as.data.frame(coeff + 2*coeff/t_values)

  Datamean <- summarise_all(coeff, funs(mean(.,na.rm=TRUE)))
  Datasd <- summarise_all(coeff, funs(sd(.,na.rm=TRUE)))
  PercentPositive <- summarise_all(Positive, funs(mean(.,na.rm=TRUE)))
  PercentSignificant <- summarise_all(Significant, funs(mean(.,na.rm=TRUE)))
  PercentPosSignificant <- summarise_all(PosSignificant, funs(mean(.,na.rm=TRUE)))
  PercentNegSignificant <- summarise_all(NegSignificant, funs(mean(.,na.rm=TRUE)))
  strong_extreme_bound_test <- (PercentNegSignificant==1) | (PercentPosSignificant == 1)
  weak_extreme_bound_test <- (PercentNegSignificant>=0.95) | (PercentPosSignificant >= 0.95)
  left_int <- summarise_all(coeff, funs(quantile(.,0.0025,na.rm=TRUE)))
  right_int <- summarise_all(coeff, funs(quantile(.,0.975,na.rm=TRUE)))
  left_bound <- summarise_all(LeftSideBoundsRange, funs(min(.,na.rm=TRUE)))
  right_bound <- summarise_all(RightSideBoundsRange, funs(max(.,na.rm=TRUE)))

  Results <- rbind(Datamean, Datasd, left_int, right_int, PercentSignificant, PercentNegSignificant, PercentPosSignificant, PercentPositive, strong_extreme_bound_test, weak_extreme_bound_test)
  row.names(Results) <- c("Mean", "Standard Deviation", "Conf. int. left", "Conf. int. right","Percent Significant", "Percent Neg. Significant", "Percent Pos. Significant", "Percent Positive", "Strong extreme bounds", "weak extreme bound test")
  Results <- as.data.frame(t(Results))
  Results <- Results[order(-PercentPositive),]
  return(Results)
}
