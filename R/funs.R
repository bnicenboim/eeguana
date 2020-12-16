#' Replacement of str_remove
#' @noRd
chr_remove <- function(string, pattern){
     gsub(pattern = pattern, replacement = "", x = string)
}

#' Replacement of str_match
#' @noRd
chr_match <- function(string, pattern){
  matches <- regexec(pattern = pattern, text = string)
  list_matches <- lapply(regmatches(x = string, m = matches), function(x)    if(length(x)==0) NA else x )
  do.call("rbind",list_matches )
}
#' Replacement of str_match
#' @noRd
chr_detect <- function(string, pattern, ignore.case = FALSE){
  grepl(pattern = pattern, x = string, ignore.case = ignore.case)
}

