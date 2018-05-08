# library(tidyverse)
# library(purrrlyr)

#' Title Summary statistic for continuas variables by group variable
#' For 2 categories usese \code{stats::t.test}. For more than 2 groups uses
#' \code{stats::oneway.test}. Equality of variances is tested prior to the differnce
#' in mean using Levene's Test for Homogeneity of Variance, and results are forwarded
#' to \code{var.equal} argument in both functions (cutoff point at p=0.05)
#'
#' @param dat data frame
#' @param group_var variable to group by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
desc_oneway <- function(dat, group_var, ...){
  if(missing(group_var)) stop("group_var is required")

  # browser()
  df <- dat
vars <- dplyr::quos(...)
group_var <- rlang::enquo(group_var)
if(length(vars)!=0){
  df <- dat %>% dplyr::select(!!group_var, !!!vars)
}else{
  vars_to_select <- rlang::syms(names(dplyr::select_if(df, is.numeric)))
  df <- dat %>% dplyr::select(!!group_var, !!!vars_to_select)
  message("no variables specified, selecting all numeric variables in data-frame")
}

group_var_char <- rlang::quo_text(group_var)

mean_by_group <- df %>%
        dplyr::group_by(!!group_var) %>%
        dplyr::summarise_if(is.numeric, dplyr::funs(
                paste0(format(round(mean(., na.rm=T) ,2), nsmall = 2), " (",
                       format(round(sd  (., na.rm=T) ,1), nsmall = 1), ")"))) %>%
        dplyr::ungroup() %>% purrrlyr::dmap(as.character)

mean_total <- df %>%
        dplyr::summarise_if(is.numeric, dplyr::funs(
                paste0(format(round(mean(., na.rm=T) ,2), nsmall = 2), " (",
                       format(round(sd  (., na.rm=T) ,1), nsmall = 1), ")"))) %>%
        dplyr::ungroup() %>% purrrlyr::dmap(as.character)

mean_all <- dplyr::bind_rows(mean_by_group, mean_total)
mean_all <- purrrlyr::dmap(mean_all, as.character)
mean_all[nrow(mean_all), 1] <- "Total"
# rm(mean_by_group, mean_total)

mean_table <- data.frame(t(mean_all)) %>%
tibble::rownames_to_column(., var = "Variable")
names(mean_table) <- c("var", dplyr::pull(mean_all, 1))
mean_table <- mean_table[2:nrow(mean_table), ]

final_f_res <- data.frame(var=character(), t.F=character(), method = character())
for(i in names(df)[2:length(names(df))]){
  # browser()
  tryCatch({
    assume_eq_var <-  ifelse(suppressWarnings(car::leveneTest(df[[i]], df[[group_var_char]] ))$`Pr(>F)`[1] > 0.04999, T, F)

    if(length(unique(df[[group_var_char]]))==2) {

      f_res <- suppressMessages(
        broom::tidy(t.test(formula(paste0(i, "~", group_var_char)),
                           data = df, var.equal = assume_eq_var)) %>%
          tibble::add_column(var=i, .before = 1 ) %>%
          dplyr::mutate(t.F = paste0("t(",
                                     format(round(parameter,1), nsmall = 1),
                                     ")=",
                                     format(round(statistic, 1), nsmall = 1),
                                     " , p=",
                                     format(round(p.value, 3), nsmall = 3))) %>%
          dplyr::select(var, t.F, method))
      final_f_res <- suppressWarnings(dplyr::bind_rows(final_f_res, f_res))
    }
    if(length(unique(df[[group_var_char]]))>2) {

    f_res <- suppressMessages(broom::tidy(oneway.test(formula(paste0(i, "~", group_var_char)),
                                                      data = df, var.equal = assume_eq_var)) %>%
                                tibble::add_column(var=i, .before = 1 ) %>%
                                dplyr::mutate(t.F = paste0("F(",
                                                           format(round(num.df, 0), nsmall = 0),
                                                           ", ",
                                                           format(round(denom.df, 1), nsmall = 1),
                                                           ")=",
                                                           format(round(statistic, 1), nsmall = 1),
                                                           " , p=",
                                                           format(round(p.value, 3), nsmall = 3))) %>%
                                dplyr::select(var, t.F, method))

    final_f_res <- suppressWarnings(dplyr::bind_rows(final_f_res, f_res))
    }

  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


mean_table <- data.frame(t(mean_all)) %>%
  tibble::rownames_to_column(., var = "Variable")
names(mean_table) <- c("var", dplyr::pull(mean_all, 1))
mean_table <- mean_table[2:nrow(mean_table), ]


final_mean_table <- suppressWarnings(dplyr::full_join(mean_table, final_f_res, by="var"))
names(final_mean_table)[1] <- paste0("var","_over_>", group_var_char)

final_mean_table <- tibble::as_tibble(final_mean_table)
final_mean_table

}
# http://daniellakens.blogspot.co.il/2015/01/always-use-welchs-t-test-instead-of.html
#
# http://faculty.educ.ubc.ca/zumbo/papers/Scales_of_measurement_Zumbo_Zimmerman.pdf
#
# https://academic.oup.com/beheco/article/17/4/688/215960
#
# https://stats.stackexchange.com/questions/313471/always-use-welch-t-test-unequal-variances-t-test-instead-of-student-t-or-mann
