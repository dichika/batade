#' %% ~~function to do ... ~~
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param func %% ~~Describe \code{func} here~~
#' @param pkgdir %% ~~Describe \code{pkgdir} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#'   \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#'   'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function(func, pkgdir){
#'     if(!is.character(func)){
#'     	stop("func must be character")
#'     }
#'     require(stringr)
#'     require(Rd2roxygen)
#'     mandir <- str_c(pkgdir, "/man/")
#'     codedir <- str_c(pkgdir, "/R/")
#'     
#'     #output Rd file
#'     Rdname <- str_c(mandir, func, ".Rd")
#'     obj <- eval(parse(text=func))
#'     prompt(obj, Rdname)
#' 
#'     #output roxygen code
#'     codename <- str_c(codedir, func, ".R")
#'     parse_and_save(path=Rdname, file=codename)
#' 
#'     #join roxygen and function code
#'     dump(func, file=codename, append=TRUE) 
#'     }
#' 

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

