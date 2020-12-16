#' Replacement of str_remove
#' @noRd
chr_remove <- function(string, pattern){
     gsub(pattern = pattern, replacement = "", x = string)
}

#' Replacement of str_detect
#' @noRd
chr_detect <- function(string, pattern){
  matches <- regexec(pattern = noun, text = head(sentences))
  list_matches <- lapply(regmatches(x = head(sentences), m = matches), function(x)    if(length(x)==0) NA else x )
  do.call("rbind",list_matches )
}
