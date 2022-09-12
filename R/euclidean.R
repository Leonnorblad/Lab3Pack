#' Euclidean
#'
#' Finds the greatest common divisor following the euclidean algorithm.
#'
#'
#' @param a,b are numeric scalars or integers.
#' 
#' @return The greatest common divisor of \code{a} and \code{b}.
#' 
#' @examples
#' euclidean(1013253152, 1251514)
#' euclidean(85121215, 11182625192620156)
#'
#' @references
#' Wikipedia contributors. (2022, August 17). Euclidean algorithm. In Wikipedia, The Free Encyclopedia. Retrieved 11:30, September 9, 2022, from \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
#' @export

euclidean <-
function(a, b){
    # checks the input
    if(any(!is.numeric(c(a, b))) |    # a and b has to be a number
       any(length(a)!=1|length(b)!=1) # a and b has to be a single value
    ) {stop()}
    
    while (b != 0) { # it runs as long as b is not zero (a smaller divider still exists)
        t <- b         # stores t (temporary value) as the remainder (b from input the first time)
        b <- a %% b    # calculates the remainder from the division of a and b and stores it as b
        a <- t         # stores a as the temporary value for the next division (or the greatest common divisor) 
    }
    return(a)        # returns the greatest common divisor
}
