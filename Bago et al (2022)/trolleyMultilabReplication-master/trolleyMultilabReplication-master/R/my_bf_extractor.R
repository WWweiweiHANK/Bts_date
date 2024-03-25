#' Function to exract BF values
#'
#' This is the description
#'
#' @param bf_obj the output object of a BayesFactor function
#'
#' @return A tibble containing the Bayes factor information.
#' @export
my_bf_extractor <- function(bf_obj) {
  BayesFactor::extractBF(bf_obj) %>%
    tibble::rownames_to_column(var = "scale") %>%
    dplyr::mutate(scale = stringr::str_extract(scale, "(?<=\\s)(.*)(?=\\s)"))
}
