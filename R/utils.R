seq_len <- function(length.out){
	if(length(length.out) == 0){
		base::seq_len(0)
	} else if (length.out ==-Inf){
		warning("length.out is -Inf, using 0 instead.")
		base::seq_len(0)
	} else {
		base::seq_len(length.out)
	}
}