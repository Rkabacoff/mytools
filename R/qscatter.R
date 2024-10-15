#'@title Quick scatter plot
#'@description Scatter plot with line of best fit and correlation.
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns a ggplot2 graph
#'@export
#'@import ggplot2
#'@examples
#'qscatter(mtcars, wt, hp)

qscatter <- function(data, x, y){
  xname <- as.character(substitute(x))
  yname <- as.character(substitute(y))
  gtitle <- paste("Relationship between",
                  xname, "and", yname)

  r <- cor.test(data[[xname]], data[[yname]])
  stitle <- paste("r =", round(r$estimate, 3),
                  ", p <", format.pval(r$p.value, 3))

  ggplot(data = data,
         mapping = aes(x = {{x}}, y = {{y}})) +
    geom_point() +
    geom_smooth(method = "lm",
                formula = y ~ x,
                se = FALSE,
                color = "cornflowerblue") +
    theme_minimal() +
    labs(title = gtitle,
         subtitle = stitle)
}


