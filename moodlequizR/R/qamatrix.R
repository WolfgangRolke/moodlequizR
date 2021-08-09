#' qamatrix
#'
#' This function takes a matrix and generates the html code for questions and answers in a
#' moodle quiz 
#' @param tbl a matrix
#' @param points Points for correct answers
#' @param precision required 
#' @param before text that appears before question
#' @param after text that appears after question
#' @return a list for the qmc and amc portions of genquiz
#' @export
#' @examples
#' p=matrix(1:6,2,3)
#' qamatrix(p)
#' qamatrix(p, c(100,80), c(0,0.1))

qamatrix <- function(tbl, points=100, precision=0, before, after) {
  if(missing(tbl)) {
    tbl=matrix(" ", nrow(before),ncol(before))
    dimnames(tbl)=dimnames(before)
  }
  if(is.table(tbl) & is.na(ncol(tbl))) tbl=t(tbl)
  if(missing(before)) {
    before=matrix(" ", nrow(tbl),ncol(tbl))
    dimnames(before)=dimnames(tbl)
  }  
  if(missing(after)) {
    after=matrix(" ", nrow(tbl),ncol(tbl))
    dimnames(after)=dimnames(tbl)
  }    
  nr <- nrow(tbl)
  rnames=rownames(tbl)
  nc <- ncol(tbl)
  cnames=colnames(tbl)
  dimnames(tbl)=NULL
  if(!is.null(cnames)) {
    tmp=paste0(cnames, collapse="</th>&nbsp;&nbsp;<th>")
    if(!is.null(rnames)) tmp=paste0("</th>&nbsp;&nbsp;<th>", tmp)
    qtxt = paste0("<tr><th>&nbsp;&nbsp;", tmp, "&nbsp;&nbsp;</th></tr>")   
  }  
  else qtxt=NULL 
  atxt=qtxt  
  for(i in 1:nr) {   
    tmp=ifelse(is.null(rnames), "", paste0("<th>&nbsp;&nbsp;", rnames[i],"&nbsp;&nbsp;</th>"))
    qtmp = paste0(tmp, "<td>&nbsp;&nbsp;", paste0(before[i, ], unlist(lapply(tbl[i, ], moodleR::nm, w=points, eps=precision)), after[i, ], collapse="</td><td>"), "</td>")
    atmp = paste0(tmp, "<td>&nbsp;&nbsp;", paste0(before[i, ], tbl[i, ], after[i, ], collapse="</td>&nbsp;&nbsp;<td>"), "</td>")
    qtxt <- paste0(qtxt, "<tr>", qtmp, "</tr>")        
    atxt <- paste0(atxt, "<tr>", atmp, "</tr>")
  }
  qtxt <- paste0("<p><table Border=1>", qtxt, "</table>")
  atxt <- paste0("<p><table Border=1>", atxt, "</table>")
  list(qtxt=qtxt,atxt=atxt)
}