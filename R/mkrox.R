#' make roxygen-style file from function code
#' 
#' make roxygen-style file from function code.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param func character
#' @param pkgdir nedd a full path like "paste(sep="/", getwd(), ''/batade/"
#' @return NULL ( a roxygen-style R source code)
#' @note %% ~~further notes~~
#' @author Ichikawa Daisuke
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 

#' plot_num <- function(n){
#'	num <- runif(n)
#'	plot(num)
#'	}
#'dir.create("sample")
#'dir.create("sample/R")
#'dir.create("sample/man")
#'mkrox("plot_num",pkgdir=str_c(getwd(), "/sample"))

mkrox <-
function(func, pkgdir){
    if(!is.character(func)){
    	stop("func must be character")
    }
    require(stringr)
    require(Rd2roxygen)
    mandir <- str_c(pkgdir, "/man/")
    codedir <- str_c(pkgdir, "/R/")
    
    #output Rd file
    Rdname <- str_c(mandir, func, ".Rd")
    obj <- eval(parse(text=func))
    prompt(obj, Rdname)

    #output roxygen code
    codename <- str_c(codedir, func, ".R")
    parse_and_save(path=Rdname, file=codename)

    #join roxygen and function code
    dump(func, file=codename, append=TRUE) 
    }

