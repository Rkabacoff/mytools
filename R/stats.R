#'@title Summary statistics
#'@description Calculate simple statistics by group level
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param ... one or more categorical variables
#'@returns a tibble with n, mean, and standard deviation
#'@import dplyr
#'@examples
#'stats(mtcars, mpg, am)
#'stats(mtcars, mpg, am, vs)


stats <- function(data, x, ...){
  data %>%
    group_by(...) %>%
    summarize(n = n(),
              mean = mean({{x}}),
              sd = sd({{x}}))
}
