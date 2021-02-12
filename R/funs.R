#' Replacement of str_remove
#' @noRd
chr_remove <- function(string, pattern){
     gsub(pattern = pattern, replacement = "", x = string)
}

#' Replacement of str_match
#' @noRd
chr_match <- function(string, pattern){
  matches <- regexec(pattern = pattern, text = string)
  list_matches <- lapply(regmatches(x = string, m = matches),
                         function(x)    if(length(x)==0) NA else x )
  do.call("rbind",list_matches )
}
#' Replacement of str_detect
#' @noRd
chr_detect <- function(string, pattern, ignore.case = FALSE){
  grepl(pattern = pattern, x = string, ignore.case = ignore.case)
}
#' Replacement of str_extract
#' @noRd
chr_extract_all <- function(string, pattern, ignore.case = FALSE, perl = TRUE, ...){
  matches <- gregexpr(pattern = pattern, text = string, ignore.case = ignore.case,perl = perl, ...)
  list_matches <- lapply(regmatches(string, m = matches),
                         function(x)    if(length(x)==0) character(0) else x )
  list_matches
}

#' @noRd
chr_extract <- function(string, pattern, ignore.case = FALSE, perl = TRUE, ...){
  matches <- gregexpr(pattern = pattern, text = string, ignore.case = ignore.case,perl = perl, ...)
  list_matches <- lapply(regmatches(string, m = matches),
                         function(x)    if(length(x)==0) NA else x[[1]] )
  unlist(list_matches)
}
