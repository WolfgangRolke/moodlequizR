#' sa 
#' 
#' This function creates a text question for moodle in CLOZE format.
#' @param txt  character vector with possible answers
#' @param w vector of weights
#' @param caps keep capital letters
#' @param pts points for answers
#' @return a character vector
#' @export
#' @examples 
#' sa("Los Angeles")
#' sa(c("Los Angeles", "San Francisco"), w=c(100, 80))

sa <-
function (txt, w=100, caps=TRUE, pts=1) 
{
    txt <- gsub(" ", "*", txt)
    txt <- gsub("\\(", "\\(*", txt)
    txt <- gsub("\\)", "*\\)", txt)
    txt <- gsub(",", "*,*", txt)
    if(caps)
        out <- paste0("{", pts, ":SAC:", paste0("%", w, "%", txt, "~", collapse = ""))
    else
        out <- paste0("{", pts, ":SA:", paste0("%", w, "%", txt, "~", collapse = ""))
    out <- paste0(substring(out, 1, nchar(out)-1), "}")
    out
}
