#' Generate random values based on provided parameters
#'
#' This function samples n integer values between a floor and a ceiling.
#' The probability of the values are calculated based on a
#' truncated normal distribution of a known mean and standard deviation.
#'
#' @param n Number of samples
#' @param mean Population mean
#' @param sd Population SD
#' @param floor Minimum value
#' @param ceiling Maximum value
#'
#' @return A vector of n random values.
#' @export
#' @examples
#' \dontrun{
#' draw_values(n = 10000, mean = 5, sd = 2, floor = 1, ceiling = 9)
#' }
draw_values <- function(n,
                        mean,
                        sd,
                        floor = 1,
                        ceiling = 9
){

  base::sample.int(
    n = ceiling,
    size = n,
    replace = TRUE,
    # Use truncated normal distribution
    prob = msm::dtnorm(x = floor:ceiling,
                       mean = mean,
                       sd = sd,
                       lower = floor,
                       upper = ceiling)
  )
}
