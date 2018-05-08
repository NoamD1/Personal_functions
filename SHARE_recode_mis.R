
#' Recode missing Variables in SHARE
#' 
#' This function recodes "don't know", "refusal" and NaN to NA, while preserving
#' the labelled class (so future transformation using \code{as_factor} is possible).
#' In variables such as hnetw, -9999992:-9999991 is used as indicator for missing
#' casese. 
#' @param df data frame with share data
#'
#' @return data frame
#' @export
#'
#' @examples library(NoamR)
#' library(tidyverse)
#' df5 <- NoamR::SHARE_addVars(eurod, casp, wave = 5,module = gv_health)
#' df5 <- NoamR::SHARE_addVars(country, adl2, gali, wave = 5,module = gv_health, convert_fct = T, add_to_df = df5)
#' df5 <- NoamR::SHARE_addVars(hnetw, wave = 5, module = imputations_uniqe, add_to_df = df5)
#' purrr::map_chr(df5, class)
#' df5 <- NoamR::SHARE_recode_mis(df5)
#' purrr::map_chr(df5, class)

SHARE_recode_mis <- function(df) {
  # browser()
  env <- environment()
  pb <- txtProgressBar(min = 0, max =  ncol(df)-1, initial = 0,style = 3)
  counter <- 0
  var_names <- variable.names(df)
    for (x in var_names) {
      if(is.numeric(df[[x]])==T) {
        if(-99>min(df[[x]], na.rm = T)) {
          df[[x]] <- car::recode(df[[x]], "'Refusal'=NA; 'Don\\'t know'=NA;-9999992:-9999991=NA;NaN=NA")
        } else {
        df[[x]] <- car::recode(df[[x]], "'Refusal'=NA; 'Don\\'t know'=NA;-99:-1=NA;NaN=NA")
        }
      } else {
        df[[x]] <- car::recode(df[[x]], "'Refusal'=NA; 'Don\\'t know'=NA;-99:-1=NA;NaN=NA")
        if(is.factor(df[[x]])){
          df[[x]] <- droplevels(df[[x]])
        }
      }
      
      #print(paste0(x, " RF DK recoded to NA "))
      curVal <- get("counter", envir = env)
      assign("counter", curVal +1 ,envir= env)
      setTxtProgressBar(get("pb", envir= env),
                        curVal +1)
      
    }
  close(pb)
  
  df
}

