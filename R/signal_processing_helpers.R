#' wrapper for signal::decimate that allows a vector in q, for decimating several times serially
#' When using IIR downsampling, it is recommended to call decimate multiple times for downsampling factors higher than 13. reference: https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.decimate.html
#' @noRd
decimate_ch <- function (.channel, q, n = if (ftype == "iir") 8 else 30, ftype = "iir") {
    attrs <- attributes(.channel)
    class(.channel) <- NULL
    if(anyNA(.channel)){
        r <- .channel[seq(1, length(.channel), by = prod(q))]
    } else if(length(q)>1){
        r<- Reduce(function(x,q) signal::decimate(x=x,q=q, n=n,ftype = ftype), x = q, init = .channel)
    } else {
        r <- signal::decimate(x =.channel, q=q, n = n , ftype = ftype)
    }
    mostattributes(r) <- attrs
    r
}


