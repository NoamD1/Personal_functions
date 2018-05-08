#' Clean lm object results using broom
#'
#' @param lm_object object from lm model
#' @param model_name defult for model title is dependent var, add string to change
#' @param b_beta Display b or beta (default) coefficients
#' @param drop_var name of variable(s) to drop from table (as string). Multiple variables should be combined as strings using \code{c()}.
#' @param keep_var name of variable(s) to keep in table (as string). Multiple variables should be combined as strings using \code{c()}.
#'
#'
#' @return data frame with term name and estimate (p value as asterisks), all values are charachters.
#'         adding R2 and n.
#' @export
#'
#' @examples
#' m <- lm(mtcars)
#' clean_lm_res(m)
#' clean_lm_res(m, model_name = "model 1", b_beta = "b")
clean_lm_res <- function(lm_object, model_name="", b_beta = "beta", drop_var="", keep_var="") {
  if(as.character(lm_object$call)[1]!="lm") stop(paste(as.character(substitute(lm_object)),
                                                        " is from ",
                                                        as.character(lm_object$call)[1],
                                                       lm_object$family$family,
                                                        "model, not lm. function supports only lm models",
                                                       sep = " "))
  if(b_beta=="beta"){
    m <- lm.beta::lm.beta(lm_object)  ##add beta coefficients
    tidy_lm_table <- broom::tidy(m)
    tidy_lm_table <- dplyr::select(tidy_lm_table, -estimate)
    names(tidy_lm_table) <- c("term", "estimate", "std.error", "statistic", "p.value")

  }
  if(b_beta=="b"){
    m <- lm_object
    tidy_lm_table <- broom::tidy(m)
  }
  if(b_beta=="b_beta"){
    m <- lm_object
    tidy_lm_table <- full_join(sjstats::std_beta(m) %>%
                                 dplyr::add_row(term = "(Intercept)", .before = 1) %>%
                                 dplyr::select(term, std.estimate),
                               broom::tidy(m), by="term")
  }

  if(b_beta!="b" && b_beta!="beta" && b_beta!="b_beta") stop("Please select estimatie to display: b,  beta, b_beta")



  names(tidy_lm_table)[1] <- c("Variable")
  tidy_lm_table$Variable <- stringr:: str_replace(tidy_lm_table$Variable, ":", " X ")  ## interaction
  tidy_lm_table$star <- dplyr::if_else(tidy_lm_table$p.value<0.001, "***",   ##create significance asterisks
                                dplyr::if_else(tidy_lm_table$p.value<0.01, "**",
                                        dplyr::if_else(tidy_lm_table$p.value<0.05, "*", "")))
  # tidy_lm_table <- purrrlyr::dmap_if(tidy_lm_table, is.numeric, round, 3)
  tidy_lm_table <- purrrlyr::dmap_if(tidy_lm_table, is.numeric, .f = ~format(round(.x, 3), nsmall = 3))
  if(b_beta=="b" || b_beta == "b_beta"){
  tidy_lm_table$estimate <- paste(tidy_lm_table$estimate," (",
                                  tidy_lm_table$std.error, ")",
                                  tidy_lm_table$star, sep = "")  ##add asterisks and std.error to estimate
  }
  if(b_beta=="beta"){
  tidy_lm_table$estimate <- paste(tidy_lm_table$estimate,
                                  tidy_lm_table$star, sep = "")  ##add asterisks to beta estimate
  }
  tidy_lm_table <- dplyr::select(tidy_lm_table, -std.error, -statistic, -p.value, -star)
   # browser()

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)==0){
    coef_list <- tidy_lm_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_lm_table <- tidy_lm_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))  ##drop rows containing ""

  }
  if(stringr::str_length(drop_var)==0 && stringr::str_length(keep_var)>0){
    coef_list <- tidy_lm_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()
    tidy_lm_table <- tidy_lm_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))  ##drop rows containing "drop_var"

  }

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)>0) stop("drop_var and keep_var can't be specified at the same time")

    lm_obs <- lm_object$model %>% dplyr::summarise(n())
  names(lm_obs) <- "obs"
  lm_obs <- data.frame(cbind("Obs", format(lm_obs[,1], big.mark = ",")))   ##make r squared into data frame
  if(ncol(lm_obs) < ncol(tidy_lm_table)){
    lm_obs <- tibble::add_column(lm_obs, x="", .after = 1)
  }
  names(lm_obs) <- names(tidy_lm_table)

  lm_r2 <- broom::glance(m) %>% dplyr::select(r.squared)  ##dplyr::select r squared
  lm_r2 <- data.frame(cbind("R2", as.character(format(round(lm_r2[1,1],3), nsmall = 3))))##make r squared into data frame
  if(ncol(lm_r2) < ncol(tidy_lm_table)){
    lm_r2 <- tibble::add_column(lm_r2, x="", .after = 1)
  }
  names(lm_r2) <- names(tidy_lm_table)  ##give r squared variable names to match the table

  tidy_lm_table <- (rbind(tidy_lm_table, lm_r2, lm_obs))  ##add r squared to table
  if(stringr::str_length(drop_var)>0 || stringr::str_length(keep_var)>0) {
    coef_list <- data.frame(cbind("Note:", paste0(coef_list, " terms not shown")))##make r squared into data frame
    if(ncol(coef_list) < ncol(tidy_lm_table)){
      coef_list <- tibble::add_column(coef_list, x="", .after = 1)
    }
    names(coef_list) <- names(tidy_lm_table)  ##give r squared variable names to match the table
    tidy_lm_table <- (rbind(tidy_lm_table, coef_list))  ##add r squared to table

  }

if(b_beta=="b_beta"){
  tidy_lm_table <- tidy_lm_table %>% dplyr::select(Variable, estimate, std.estimate)
  if(model_name==""){
    names(tidy_lm_table)[2] <- paste0(names(lm_object$model)[1]," OLS(b(se))")
    names(tidy_lm_table)[3] <- paste0(names(lm_object$model)[1]," OLS(beta)")
    }else {
      names(tidy_lm_table)[2] <- paste0(as.character(model_name)," OLS(b(se))")
      names(tidy_lm_table)[3] <- paste0(as.character(model_name)," OLS(beta)")
    }
  }else if(b_beta=="b"){
    if(model_name==""){
      names(tidy_lm_table)[2] <- paste0(names(lm_object$model)[1]," OLS(b(se))")
    }else {
      names(tidy_lm_table)[2] <- paste0(as.character(model_name)," OLS(b(se))")
    }
  }else if(b_beta=="beta"){
    if(model_name==""){
      names(tidy_lm_table)[2] <- paste0(names(lm_object$model)[1]," OLS(beta)")
    }else {
      names(tidy_lm_table)[2] <- paste0(as.character(model_name)," OLS(beta)")
    }
  }


  return(tidy_lm_table)

}




#' Clean glm poisson object results using broom
#'
#' @param poisson_glm_object object from glm model where "family = "poisson"".
#' @param model_name defult for model title is dependent var, add string to change
#' @param b_exp Display b or exponentiated (default) coefficients
#'
#' @param drop_var name of variable(s) to drop from table (as string). Multiple variables should be combined as strings using \code{c()}.
#' @param keep_var name of variable(s) to keep in table (as string). Multiple variables should be combined as strings using \code{c()}.
#'
#' @return data frame with term name and estimate (p value as asterisks), all values are charachters.
#'         adding McFadden pseudo R2 (like in STATA) and n.
#' @export
#'
#' @examples
#' m_p <- glm(breaks~wool+tension, data = warpbreaks , family = "poisson")
#' clean_poisson_res(m_p)
#' clean_poisson_res(m_p, model_name = "warpbreaks")
#' clean_poisson_res(m_p, model_name = "warpbreaks", b_exp = "b")
#' clean_poisson_res(m_p, model_name = "Wool X Tension")
clean_poisson_res <- function(poisson_glm_object, model_name="", b_exp="exp", drop_var="", keep_var="") {
  if(as.character(poisson_glm_object$call)[1]!="glm") stop(paste(as.character(substitute(poisson_glm_object)),
                                                             "is from",
                                                             as.character(poisson_glm_object$call)[1],
                                                             "model, not glm. function supports only glm poisson models",
                                                             sep = " "))

  if(poisson_glm_object$family$family!="poisson") stop(paste0("model is `",
                                                              poisson_glm_object$family$family,
                                                             "` glm, this function supports `family = `poisson`."))

  if(b_exp=="exp"){
    tidy_pois_table <- broom::tidy(poisson_glm_object, exponentiate = T)
  }
  if(b_exp=="b"){
    tidy_pois_table <- broom::tidy(poisson_glm_object, exponentiate = F)
  }
  if(b_exp!="b"& b_exp!="exp") stop("Please dplyr::select estimatie to display: `b` or `exp`")


  names(tidy_pois_table)[1] <- c("Variable")
  tidy_pois_table$Variable <- stringr:: str_replace(tidy_pois_table$Variable, ":", " X ")  ## interaction
  tidy_pois_table$star <- dplyr::if_else(tidy_pois_table$p.value<0.001, "***",   ##create significance asterisks
                                  dplyr::if_else(tidy_pois_table$p.value<0.01, "**",
                                          dplyr::if_else(tidy_pois_table$p.value<0.05, "*", "")))
  tidy_pois_table <- purrrlyr::dmap_if(tidy_pois_table, is.numeric, .f = ~format(round(.x, 3), nsmall = 3))

  # tidy_pois_table <- purrrlyr::dmap_if(tidy_pois_table, is.numeric, round, 3)
  tidy_pois_table$estimate <- paste(tidy_pois_table$estimate, tidy_pois_table$star, sep = "")  ##add asterisks to estimate
  tidy_pois_table <- dplyr::select(tidy_pois_table, -std.error, -statistic, -p.value, -star)
  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)==0){
    coef_list <- tidy_pois_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_pois_table <- tidy_pois_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))  ##drop rows containing "drop_var"
  }
  if(stringr::str_length(drop_var)==0 && stringr::str_length(keep_var)>0){
    coef_list <- tidy_pois_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_pois_table <- tidy_pois_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))  ##drop rows containing "drop_var"
  }

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)>0) stop("drop_var and keep_var can't be specified at the same time")

    lm_obs <- poisson_glm_object$model %>% dplyr::summarise(n())
  names(lm_obs) <- "obs"
  lm_obs <- data.frame(cbind("Obs", format(lm_obs[,1], big.mark = ",")))   ##make r squared into data frame
  names(lm_obs) <- names(tidy_pois_table)

  lm_r2 <- data.frame(pscl::pR2(poisson_glm_object)[4]) #extract McFadden pseudo r2
  lm_r2 <- data.frame(cbind("Pseudo R2", as.character(format(round(lm_r2[1,1],3), nsmall = 3))))##make r squared into data frame
  names(lm_r2) <- names(tidy_pois_table)  ##give r squared variable names to match the table

  tidy_pois_table <- (rbind(tidy_pois_table, lm_r2, lm_obs))  ##add r squared to table

  if(stringr::str_length(drop_var)>0 || stringr::str_length(keep_var)>0) {
    coef_list <- data.frame(cbind("Note:", paste0(coef_list, " terms not shown")))##make r squared into data frame
    names(coef_list) <- names(tidy_pois_table)  ##give r squared variable names to match the table
    tidy_pois_table <- (rbind(tidy_pois_table, coef_list))  ##add r squared to table

  }
  if(model_name==""){
    names(tidy_pois_table)[2] <- paste0(names(poisson_glm_object$model)[1]," Poisson(", b_exp, ")")
  }else {
    names(tidy_pois_table)[2] <- paste0(as.character(model_name)," Poisson(", b_exp, ")")
  }

  # names(tidy_pois_table)[2] <- paste(names(poisson_glm_object$model)[1],"Poisson(exp)", sep = " ")
  return(tidy_pois_table)

}


#' Clean glm logit object results using broom
#'
#' @param logit_glm_object object from glm model where "family = "binomial"".
#' @param model_name defult for model title is dependent var, add string to change
#' @param b_or Display b or exponentiated Odds ratio (default) coefficients
#' @param drop_var name of variable(s) to drop from table (as string). Multiple variables should be combined as strings using \code{c()}.
#' @param keep_var name of variable(s) to keep in table (as string). Multiple variables should be combined as strings using \code{c()}.
#'
#' @return data frame with term name and estimate (p value as asterisks), all values are charachters.
#'         Adding McFadden pseudo R2 (like in STATA) and n.
#' @export
#'
#' @examples
#' m_l <- glm(vs ~ wt + disp, data=mtcars, family=binomial)
#' clean_logit_res(m_l)
#' clean_logit_res(m_l, model_name = "model1", b_or="b")

clean_logit_res <- function(logit_glm_object, model_name="", b_or="or", drop_var="", keep_var="") {
  if(as.character(logit_glm_object$call)[1]!="glm") stop(paste(as.character(substitute(logit_glm_object)),
                                                                 "is from",
                                                                 as.character(logit_glm_object$call)[1],
                                                                 "model, not glm. function supports only glm logit models",
                                                                 sep = " "))

  if(logit_glm_object$family$family!="binomial") stop(paste0("model is `",
                                                             logit_glm_object$family$family,
                                                             "` glm, this function supports `family = `binomial`."))
  if(b_or=="or"){
    tidy_logit_table <- broom::tidy(logit_glm_object, exponentiate = T)
  }
  if(b_or=="b"){
    tidy_logit_table <- broom::tidy(logit_glm_object, exponentiate = F)
  }
  if(b_or!="b"& b_or!="or") stop("Please dplyr::select estimatie to display: `b` or `or`")


  names(tidy_logit_table)[1] <- c("Variable")
  tidy_logit_table$Variable <- stringr:: str_replace(tidy_logit_table$Variable, ":", " X ")  ## interaction
  tidy_logit_table$star <- dplyr::if_else(tidy_logit_table$p.value<0.001, "***",   ##create significance asterisks
                                   dplyr::if_else(tidy_logit_table$p.value<0.01, "**",
                                           dplyr::if_else(tidy_logit_table$p.value<0.05, "*", "")))
  tidy_logit_table <- purrrlyr::dmap_if(tidy_logit_table, is.numeric, .f = ~format(round(.x, 3), nsmall = 3))

  # tidy_logit_table <- purrrlyr::dmap_if(tidy_logit_table, is.numeric, round, 3)
  tidy_logit_table$estimate <- paste(tidy_logit_table$estimate, tidy_logit_table$star, sep = "")  ##add asterisks to estimate
  tidy_logit_table <- dplyr::select(tidy_logit_table, -std.error, -statistic, -p.value, -star)

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)==0){
    coef_list <- tidy_logit_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_logit_table <- tidy_logit_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))  ##drop rows containing "drop_var"
  }
  if(stringr::str_length(drop_var)==0 && stringr::str_length(keep_var)>0){
    coef_list <- tidy_logit_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_logit_table <- tidy_logit_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))  ##drop rows containing "drop_var"
  }

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)>0) stop("drop_var and keep_var can't be specified at the same time")

  lm_obs <- logit_glm_object$model %>% dplyr::summarise(n())
  names(lm_obs) <- "obs"
  lm_obs <- data.frame(cbind("Obs", format(lm_obs[,1], big.mark = ",")))   ##make r squared into data frame
  names(lm_obs) <- names(tidy_logit_table)

  lm_r2 <- data.frame(pscl::pR2(logit_glm_object)[4]) #extract McFadden pseudo r2
  lm_r2 <- data.frame(cbind("Pseudo R2", as.character(format(round(lm_r2[1,1],3), nsmall = 3))))##make r squared into data frame
  names(lm_r2) <- names(tidy_logit_table)  ##give r squared variable names to match the table

  tidy_logit_table <- (rbind(tidy_logit_table, lm_r2, lm_obs))  ##add r squared to table

  if(stringr::str_length(drop_var)>0 || stringr::str_length(keep_var)>0) {
    coef_list <- data.frame(cbind("Note:", paste0(coef_list, " terms not shown")))##make r squared into data frame
    names(coef_list) <- names(tidy_logit_table)  ##give r squared variable names to match the table
    tidy_logit_table <- (rbind(tidy_logit_table, coef_list))  ##add r squared to table

  }
  if(model_name==""){
    names(tidy_logit_table)[2] <- paste0(names(logit_glm_object$model)[1]," logit(", b_or, ")")
  }else {
    names(tidy_logit_table)[2] <- paste0(as.character(model_name)," logit(", b_or, ")")
  }

  # names(tidy_logit_table)[2] <- paste(names(logit_glm_object$model)[1],"Poisson(exp)", sep = " ")
  return(tidy_logit_table)

}



#' Clean glm Negative Binomial object results using broom
#'
#' @param nb_glm_object object from \code{MASS::glm.nb} model.
#' @param model_name defult for model title is dependent var, add string to change
#' @param b_exp Display b or exponentiated (default) coefficients
#' @param drop_var name of variable(s) to drop from table (as string). Multiple variables should be combined as strings using \code{c()}.
#' @param keep_var name of variable(s) to keep in table (as string). Multiple variables should be combined as strings using \code{c()}.
#'
#' @return data frame with term name and estimate (p value as asterisks), all values are charachters.
#'         adding McFadden pseudo R2 (like in STATA) and n.
#' @export
#'
#' @examples
#' m_nb <- MASS::glm.nb(breaks~wool+tension, data = warpbreaks)
#' clean_nb_res(m_nb)
#' clean_nb_res(m_nb, model_name = "warpbreaks")
#' clean_nb_res(m_nb, model_name = "warpbreaks", b_exp = "b")
#' m_nb2 <- MASS::glm.nb(breaks~wool*tension, data = warpbreaks)
#' clean_nb_res(m_nb2, model_name = "Wool X Tension")
#' @seealso \code{\link[MASS]{glm.nb}}
#'
clean_nb_res <- function(nb_glm_object, model_name="", b_exp="exp", drop_var="", keep_var="") {
  if(as.character(nb_glm_object$call)[1]!="MASS::glm.nb") stop(paste(as.character(substitute(nb_glm_object)),
                                                                     "is from",
                                                                     as.character(nb_glm_object$call)[1],
                                                                     "model, not MASS::glm.nb function supports only MASS::glm.nb models",
                                                                     sep = " "))

  if(stringr::str_sub(nb_glm_object$family$family, 1, 17)!="Negative Binomial") stop(paste0("model is `",
                                                                                   nb_glm_object$family$family,
                                                                                   "` glm, this function supports MASS::glm.nb."))

  if(b_exp=="exp"){
    tidy_nb_table <- broom::tidy(nb_glm_object, exponentiate = T)
  }
  if(b_exp=="b"){
    tidy_nb_table <- broom::tidy(nb_glm_object, exponentiate = F)
  }
  if(b_exp!="b"& b_exp!="exp") stop("Please dplyr::select estimatie to display: `b` or `exp`")


  names(tidy_nb_table)[1] <- c("Variable")
  tidy_nb_table$Variable <- stringr:: str_replace(tidy_nb_table$Variable, ":", " X ")  ## interaction
  tidy_nb_table$star <- dplyr::if_else(tidy_nb_table$p.value<0.001, "***",   ##create significance asterisks
                                dplyr::if_else(tidy_nb_table$p.value<0.01, "**",
                                        dplyr::if_else(tidy_nb_table$p.value<0.05, "*", "")))
  tidy_nb_table <- purrrlyr::dmap_if(tidy_nb_table, is.numeric, .f = ~format(round(.x, 3), nsmall = 3))

  # tidy_nb_table <- purrrlyr::dmap_if(tidy_nb_table, is.numeric, round, 3)
  tidy_nb_table$estimate <- paste(tidy_nb_table$estimate, tidy_nb_table$star, sep = "")  ##add asterisks to estimate
  tidy_nb_table <- dplyr::select(tidy_nb_table, -std.error, -statistic, -p.value, -star)

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)==0){
    coef_list <- tidy_nb_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_nb_table <- tidy_nb_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("^((?!",paste(drop_var, collapse = "|"), ").)*$")))  ##drop rows containing "drop_var"
  }
  if(stringr::str_length(drop_var)==0 && stringr::str_length(keep_var)>0){
    coef_list <- tidy_nb_table %>% dplyr::mutate(test = stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))
    # coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% paste(collapse = ",")
    coef_list <- coef_list %>% dplyr::filter(test == F) %>% dplyr::pull(Variable) %>% length()

    tidy_nb_table <- tidy_nb_table %>% dplyr::filter(stringr::str_detect(Variable, paste0("Intercept|", "^", paste(keep_var, collapse = "|^"))))  ##drop rows containing "drop_var"
  }

  if(stringr::str_length(drop_var)>0 && stringr::str_length(keep_var)>0) stop("drop_var and keep_var can't be specified at the same time")

  lm_obs <- nb_glm_object$model %>% dplyr::summarise(n())
  names(lm_obs) <- "obs"
  lm_obs <- data.frame(cbind("Obs", format(lm_obs[,1], big.mark = ",")))   ##make r squared into data frame
  names(lm_obs) <- names(tidy_nb_table)

  lm_r2 <- data.frame(pscl::pR2(nb_glm_object)[4]) #extract McFadden pseudo r2
  lm_r2 <- data.frame(cbind("McFadden's Pseudo R2", as.character(format(round(lm_r2[1,1],3), nsmall = 3))))##make r squared into data frame
  names(lm_r2) <- names(tidy_nb_table)  ##give r squared variable names to match the table

  tidy_nb_table <- (rbind(tidy_nb_table, lm_r2, lm_obs))  ##add r squared to table

  if(stringr::str_length(drop_var)>0 || stringr::str_length(keep_var)>0) {
    coef_list <- data.frame(cbind("Note:", paste0(coef_list, " terms not shown")))##make r squared into data frame
    names(coef_list) <- names(tidy_nb_table)  ##give r squared variable names to match the table
    tidy_nb_table <- (rbind(tidy_nb_table, coef_list))  ##add r squared to table

  }

  if(model_name==""){
    names(tidy_nb_table)[2] <- paste0(names(nb_glm_object$model)[1]," Negative Binomial(", b_exp, ")")
  }else {
    names(tidy_nb_table)[2] <- paste0(as.character(model_name)," Negative Binomial(", b_exp, ")")
  }

  # names(tidy_nb_table)[2] <- paste(names(nb_glm_object$model)[1],"Negative Binomial(exp)", sep = " ")
  return(tidy_nb_table)

}
