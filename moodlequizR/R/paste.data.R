#' paste.data
#'
#' This function is used to read data from moodle into R
#' @param sep symbol used for separation
#' @param header does data have a header?
#' @param is.table is data a table? Needed if all data is character.
#' @return the data in the clipboard
#' @export
#' @examples
#' # not run
#' # paste.data()

paste.data = function(sep="", header=TRUE, is.table=FALSE) {
.gmd.win = function(sep="", header=TRUE, is.table=FALSE) {
  options(warn=-1)
  txt <- "Data has been attached"
  if(is.table) {
    df <- utils::read.table("clipboard", header=header)
  }
  else {
    x <- scan("clipboard", what="character", sep=sep, quiet=TRUE)
    if(any(is.na(as.numeric(x))) & any(!is.na(as.numeric(x)))) {
      df <- utils::read.table("clipboard", header=header)
    }
    else {
      if(all(!is.na(as.numeric(x)))) {
        txt <- ""
        df <- as.numeric(x)    
      }  
      else {
        z <- table(x)
        z <- z[z==1]
        if(length(z)>1) 
          df <- utils::read.table("clipboard", header=header)
        else  {
          txt <- ""
          df <- x
        }
          
      }
    }
  }
  repeat {
    if("x" %in% search()) detach(x) 
    else break
  }
  cat("Data begins with:\n")
  print(utils::head(df))
  cat("\n")
  assign("x", df, pos = .GlobalEnv)
  cat("Data has been saved as x\n\n")
  if(txt!="") {
    attach(df, pos=-1, name="x")
    cat(txt);cat("\n")
  }
  options(warn=0)
   
}

.gmd.other = function(sep="", header=TRUE, is.table=FALSE) {
  options(warn=-1)
  txt <- "Data has been attached"
  if(is.table) {
    df <- utils::read.table(pipe("pbpaste"), header=header)
  }
  else {
    x <- scan(pipe("pbpaste"), what="character", sep=sep, quiet=TRUE)
    if(any(is.na(as.numeric(x))) & any(!is.na(as.numeric(x)))) {
      df <- utils::read.table(pipe("pbpaste"), header=header)
    }
    else {
      if(all(!is.na(as.numeric(x)))) {
        txt <- ""
        df <- as.numeric(x)    
      }  
      else {
        z <- table(x)
        z <- z[z==1]
        if(length(z)>1) 
          df <- utils::read.table(pipe("pbpaste"), header=header)
        else  {
          txt <- ""
          df <- x
        }
        
      }
    }
  }
  repeat {
    if("x" %in% search()) detach(x) 
    else break
  }
  cat("Data begins with:\n")
  print(utils::head(df))
  cat("\n")
  assign("x", df, pos = .GlobalEnv)
  cat("Data has been saved as x\n\n")
  if(txt!="") {
    attach(df, pos=-1, name="x")
    cat(txt);cat("\n")
  }
  options(warn=0)
  
}

   if(.Platform$`OS.type`=="windows")
      .gmd.win(sep=sep, header=header, is.table=is.table)
   else
      .gmd.other(sep=sep, header=header, is.table=is.table)
   
}



