# require(tidyverse)
# suppressMessages(library(haven))
# suppressMessages(library(stringr))

#' SHARE_addVars
#' This function allows you to merges specific item(s) from SHARE data and returns a R tibble.
#'
#' @param ... variables to add, could be full name (e.g ac012_), or start of names (e.g ac01)
#'            EIther way, the function will bring every variable that starts with
#'            those charachters.
#' @param wave SHARE wave to use.
#' @param module which module to take data from. If module is not specified, the
#'               module is taken from the first variable name (e.g. ac012="ac").
#' @param add_suffix Should wave number be added as suffix to each variable name? Defaults to TRUE.
#' @param add_to_df  To add variabels to exisiting data-frame, specify the name of data-frame.
#' @param convert_fct Variable are imported as class(labelled) using read_dta from Haven
#'                      package. If set to TRUE all variables specified will be convert to factor
#'
#' @return tb_df
#' @export
#'
#' @examples df <- SHARE_addVars(country, adl2, wave=5, module = gv_health, add_suffix = T, convert_fct = T)
#'           df2 <- SHARE_addVars(ac012, wave=5, add_to_df = df)



SHARE_addVars <- function(...,wave, module=NULL,add_to_df=NULL, convert_fct=F, convert_num=F, add_suffix=F) {
  current_wd <- getwd()
  # browser()
  if(missing(wave)) stop("Please specify wave to use")
  wave_data <- wave
  wd <- paste0("C:/Users/owner/Documents/SHARE-Noam/SHARE files/Wave ",
               wave_data,
               " - All/Wave ",
               wave_data,
               " - All") # define folder with data

    vars <- eval(substitute(alist(...))) # turn variable names to evaluated list

  ## Turn unevaluated module name to string charachter, if NULL next step will read
  ## the module name from first variable.
  modul <- deparse(substitute(module))
  if(modul=="NULL"){
    modul <- stringr::str_sub(deparse(vars[[1]]),1,2)
  }

  ## Extract the file name, by listing files in defined folder with module name
  ## pattern. stop if module no found, load file if do.
  # file_name <- list.files(path="~",
  #                         pattern = paste0("sharew", wave_data, "_rel6-1-0_", modul,".dta"),
  #                         recursive = T,
  #                         full.names = T) # Option 1: start looking for files from root directory, will work on any computer with SHARE data, slows function.
  file_name <- list.files(path="C:/Users/owner/Documents/SHARE-Noam/SHARE files",
                          pattern = paste0("sharew", wave_data, "_rel6-1-0_", modul,".dta"),
                          recursive = T,
                          full.names = T) # Option 2: start looking for files from specific directory, speeds up function.
  # file_name <- list.files(path = "C:/Users/owner/Documents/SHARE-Noam/SHARE files", pattern = paste0(".+(", modul,".dta)"))
  if(length(file_name)==0) stop("error: Module wasn't found!")
  # data <- haven::read_dta(paste0(wd, "/",file_name))
  data <- haven::read_dta(file_name[1])
  print(paste0("file loaded: ", file_name[1]))

  ## create empty DF with mergeid, all variables will merge to this DF
  df_vars <- dplyr::select_(data, "mergeid")

  for(x in vars){
    tryCatch({ #added so command wouldn't stop with error in one loo.
        x <- deparse(substitute(x)) # turn variable names from symbol to character.

        ## look for variable/pattern in colnames of module data, stop if not found.
        if(!any(grepl(x, colnames(data),fixed = TRUE))) {
          stop(paste0(x, " is not found in ", modul, " module"), call. = F)
        }
        ## look for variable/pattern in colnames of df_vars, stop if variable is found.
        if(any(grepl(x, colnames(df_vars),fixed = TRUE))) {
          stop(paste0(x, " is repeated twice in varlist, please check list"), call. = F)
        }

        df <- dplyr::select_(data, "mergeid", lazyeval::interp(~dplyr::starts_with(x)))

        if(convert_fct==T) {
          df <- purrrlyr::dmap_at(df,2:ncol(df), haven::as_factor)
          df <- purrrlyr::dmap_at(df,2:ncol(df), haven::zap_missing)
          }
        if(convert_num==T) {
          df <- purrrlyr::dmap_at(df,2:ncol(df), haven::zap_labels)
          df <- purrrlyr::dmap_at(df,2:ncol(df), haven::zap_missing)

        }

        if(add_suffix==T) {
        colnames(df)[2:ncol(df)] <- paste(colnames(df[,c(2:ncol(df))]),"_w", wave_data, sep = "")
        }

        if(ncol(df)==1) {stop("variable didn't merge")}

        if(ncol(df)>=1 && convert_fct==F && convert_num==F) {
          print(paste0("Module ", modul, ", wave ", wave_data,
                         ", added as class(labelled): ", paste(colnames(df[2:ncol(df)]), collapse = ", ")))
          }

        if(ncol(df)>=1 & convert_fct==T) {
          print(paste0("Module ", modul, ", wave ", wave_data,
                       ", added as class(factor): ", paste(colnames(df[2:ncol(df)]), collapse = ", ")))
        }

        if(ncol(df)>=1 & convert_num==T) {
          print(paste0("Module ", modul, ", wave ", wave_data,
                         ", added as class(numeric): ", paste(colnames(df[2:ncol(df)]), collapse = ", ")))
        }

         df_vars <- dplyr::inner_join(df_vars, df, by="mergeid")
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

  }

  if(is.null(add_to_df)){
    return(df_vars)
  }

  ## adds merged variables to data frame in the .GlobalEnv.
  if(!is.null(add_to_df)){
    df_name <- deparse(substitute(add_to_df))
    global_df <- get(df_name, envir = .GlobalEnv)
    df_res <- dplyr::inner_join(global_df, df_vars, by="mergeid")
    if((ncol(df_res)>ncol(global_df))) print(paste0("Variable(s) added to: ", df_name))

    return(df_res)

  }
  rm(list=ls())
}

