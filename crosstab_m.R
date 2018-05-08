#' Title Crosttab_m many cross-tabulation with pearson chi2
#'
#' @description
#' crosstab for many variables over one goruping variable, adding chi2 test and creating (based on \code{janitor::crostab} command).
#'
#' @param data
#' @param Group_var
#' @param ...
#' @param percent
#' @param max_width_of_table Maximum columns in the table, if there are more the final table would be transposed.
#'
#' @return
#' @export
#'
#' @examples
#' crosstabs_m(mtcars, Group_var = am, vs, gear)
#'
crosstabs_m <- function(data, Group_var, ..., percent = "col", max_width_of_table=10) {
  if(! percent %in% c("row", "col", "all")){stop("'percent' must be one of 'row', 'col', or 'all'")}
  # browser()
  library(tidyverse)
  library(janitor)

  df <- data
  vars <- eval(substitute(alist(...)))
  group_var <- deparse(substitute(Group_var))


  # vars <- dplyr::quos(...)
  # group_var <- rlang::enquo(group_var)
  if(length(vars)!=0){
    df <- df %>% dplyr::select(!!group_var, !!!vars)
  }else{
    df <- df %>% dplyr::select(!!group_var, dplyr::everything())
    names_df <- paste(names(df)[2:length(names(df))], collapse = ",")
    names_df <- as.list(str_split(names_df, pattern = ",")[[1]])
    vars <- purrr::map(names_df, as.symbol)
  }


  # vars <- c("cyl", "am", "gear", "carb")
  final <- data.frame(var=character())
  final <- vector("list", length = length(vars))
  for(v in vars){
    i <- deparse(substitute(v))
    tmp <- crosstab(df[[i]], df[[group_var]], show_na = F) %>%
      adorn_crosstab(show_n = T, digits = 1,
                     denom = percent, show_totals = T)
    chi_res <- chisq.test(table(df[[i]], df[[group_var]]))
    chi_table  <-  tmp[,FALSE] ## copy table length to add chi to the left of crosstab
    chi_table$Pearson_chi2 <- ""
    chi_table[1,1] <- paste0(format(round(chi_res$statistic ,1), nsmall = 3),
                             ", p=",
                             format(round(chi_res$p.value   ,3), nsmall = 3))

    names(tmp)[1] <- "var_cat"
    var_name  <-  tmp[,FALSE]
    var_name$var <- ""
    var_name[1,1] <- paste0(i, "-v- ",group_var, "->" )
    tmp <- bind_cols(var_name,tmp, chi_table)
    # tmp <- add_row(tmp, var=i, .before = 0)
    tmp <- add_row(tmp, var="----")
    # tmp <- bind_rows(y, tmp, )
    # final <- bind_rows(final, tmp)
    final[[i]] <- tmp
    rm(chi_res, chi_table, tmp, var_name)
  }
  final <- bind_rows(final)
  if(length(final)>max_width_of_table){
    final <- data.frame(t(final))
    final <- rownames_to_column(final, var = "X0")
    # return(final_t)
  }
  final[is.na(final)] <- ""
  return(final)

}
