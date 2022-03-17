#' Title One-way tabulation for many variables
#'
#' @description
#' One way tabulation for more than one vriaables (based on STATA's \code{tab1} command).
#'
#' @param dat
#' @param ... variables to tab
#' @param max_cat max number of categories to tabulate
#'
#' @return list with tabyl output for each variable
#' @export
#'
#' @examples
#' tab1(mtcars)
#' tab1(mtcars, gear, vs, am)
tab1 <- function(dat, ..., max_cat=30) {

  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop("Janitor package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # library(magrittr)
  vars <- dplyr::quos(...)
  if(length(vars)==0){
  res_list <- dat %>%
    purrr::map(function(x)
      if(length(unique(x))<max_cat) {
        janitor::tabyl(x) %>% janitor::adorn_totals("row") %>%
          dplyr::mutate_at(dplyr::vars(3:length(.)), list(~paste0(format(round(.*100,2), nsmall = 2), "%")))
      }
      else {
        paste0("Variable has more than ", max_cat, " categories. Change `max_cat` to tab variable")
      }
    )
  }else{
  res_list <- dat %>% dplyr::select(!!!vars) %>%
    purrr::map(function(x)
      if(length(unique(x))<max_cat) {
        janitor::tabyl(x) %>% janitor::adorn_totals("row") %>%
          dplyr::mutate_at(dplyr::vars(3:length(.)), list(~paste0(format(round(.*100,2), nsmall = 2), "%")))
      }
      else {
        paste0("Variable has more than ", max_cat, " categories. Change `max_cat` to tab variable")
        }
      )


  }
  res_list
}
