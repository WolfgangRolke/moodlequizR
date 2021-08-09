#' mc
#' 
#' This function generates the code for a multiple choice CLOZE question
#' @param options  vector of choices
#' @param w  vector of weights
#' @param which.true either which of the options gets 100%, 
#'   or a logical value TRUE=first option, False=second option
#' @param pts  how many points is question worth?
#' @return a list with the elements for qmc and amc
#' @export
#' @examples
#' mc(c("Yes", "No"), c(100, 0), 10)

mc <- 
function(options, w, which.true, pts = 1) {
  option.list <- list( 
    o1=c("lower", "not equal to", "higher", "can't tell"),
    o2=c("lower", "not equal to", "higher"),  
    o3=c("is statistically significant", "is not statistically significant"),
    o4=c("is statistically significant", "is not statistically significant", "can't tell"),
    o5=c("is", "is not"), 
    o6=c("Male", "Female"),
    o7=c("true", "false"),
    o8=c("has", "does not have"),
    o9=c("!=", "<", ">", "can't tell"),
    o10=c("!=", "<", ">"), 
    o11=c("&mu;", "&pi;", "&sigma;", "&lambda;", "&rho;", "other")   
  )
  if(is.numeric(options)) options <- option.list[[options]]
  if(!missing(which.true)) {
    if(is.logical(which.true)) {
      if(which.true) which.true <- 1
      else which.true <- 2
    }
    w <- rep(0, length(options))
    w[which.true] <- 100
  }
  qmc <- paste0("{", pts, ":MC:", paste0("%", w, "%", options, "~", collapse = ""))
  qmc <- paste0(substring(qmc, 1, nchar(qmc)-1), "}")
  list(qmc=qmc, amc=options[w==100])
}
