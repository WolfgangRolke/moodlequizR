#' make.xml
#' 
#' This function is a wrapper for genquiz. It reads file from folder and runs genquiz
#' @param fun name of function that makes a quiz
#' @param folder folder were fun.R is located
#' @param k how many quizzes?
#' @param delete.fun should file be deleted from workspace after we are done?
#' @param ... further arguments passed to fun
#' @return None
#' @export
#' @examples
#' # not run
#' # make.xml(example1, 10, problem=1)

make.xml <-
function (fun, folder=getwd(), k=1, delete.fun=TRUE, ...) 
{
    funname <- deparse(substitute(fun))    
    source(paste0(folder, funname, ".R"))
    genquiz(k, fun, folder=folder, ...)
    if(delete.fun)
        remove(list=funname, pos=.GlobalEnv)

}
