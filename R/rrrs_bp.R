#' Creates a barplot of the rrs results
#'
#' @param analysis a dataframe resulting from the \code{\link{rrs_analysis}} function
#' @param rm_constant If there is a constant in the `analysis` dataframe, remove this by setting this at `TRUE`, default is `FALSE`
#'
#' @return a ggplot object
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @importFrom stats reorder
#' @export
#'
#' @examples
#' result <- rrs(mpg ~., data = mtcars, k = 4)
#' coeff <- result[1]
#' t_values <- result[2]
#' analysis <- rrs_analysis(coeff, t_values)
#' rrs_bp(analysis)
rrs_bp <- function(analysis, rm_constant = FALSE) {

  if (rm_constant == TRUE) {
    analysis <- analysis[!rownames(analysis) %in% "constant", ]
  }

  var_names <- rownames(analysis)
  analysis <- data.frame(as.data.frame(var_names), analysis)

  data_plot <- analysis %>%
    select(var_names, `Percent.Neg..Significant`, `Percent.Pos..Significant`) %>%
    rename(Variable = var_names,
           Neg = `Percent.Neg..Significant`,
           Pos = `Percent.Pos..Significant`) %>%
    mutate(Neg = -1 * Neg,
           average = Pos + Neg) %>%
    gather(variable, value, Pos:Neg)

  n_var <- nrow(data_plot)/2

  p <- ggplot(data_plot, aes(x = reorder(Variable, -average), y = value, fill = variable)) +
    geom_bar(stat="identity") +
    coord_flip() +
    geom_hline(yintercept=0.95, linetype="dashed") +
    geom_hline(yintercept=-0.95, linetype="dashed") +
    annotate("text", label = "-0.95",  y = -0.95, x = 1.5, hjust = -0.4) +
    annotate("text", label = "0.95", y = 0.95, x = (n_var- 0.5), hjust = 1.4) +
    theme_bw() +
    labs(title = "Sign and significancy of determinants",
         subtitle = "In sample probabilities (negative denotes sign of coefficient)"
         , x = " ", y = " " ) +
    scale_fill_brewer(name = "",labels=c("Negative", "Positive"), type = "div", palette ="Accent", direction = -1)

  return(p)

}
