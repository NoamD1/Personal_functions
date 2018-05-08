#' pwcorr: Wrapper for \code{\link{Hmisc::rcorr}}
#'
#' @param df data frame
#' @param ... Variable names
#'
#' @return see \code{\link{Hmisc::rcorr}} "rcorr returns a list with elements r, the matrix of correlations, n the matrix of number of observations used in analyzing each pair of variables, and P, the asymptotic P-values. Pairs with fewer than 2 non-missing values have the r values set to NA. The diagonals of n are the number of non-NAs for the single variable corresponding to that row and column."
#' @export
#'
#' @examples df <- data.frame(x=rnorm(50, 18, 6.3), y=rnorm(n = 50, 50, 18.31), z=rnorm(n = 50, 22.3, 15.2))
#' pwcorr(df, x, y)
#' pwcorr(df, x, y, z)
#'
pwcorr <- function(df,...){
  Vars <- eval(substitute(alist(...)))
  if(length(Vars)==0){
    df_cor <- df
  }
  if(length(Vars)!=0){
    df_cor <- dplyr::select_(df, .dots = Vars)
  }
          # Vars2 <- map_chr(Vars, as.character)
          # Vars2 <- str_c(Vars2, sep = ", ")
          df_cor <- as.matrix(df_cor)
          Hmisc::rcorr(df_cor)
}
