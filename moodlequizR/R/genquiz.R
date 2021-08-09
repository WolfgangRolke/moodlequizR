#' genquiz
#' 
#' This function generates an xml file for import into moodle
#' @param B how many quizzes?
#' @param fun name of the R routine that makes a quiz
#' @param folder where is the .R located?
#' @param Show (optional)want to see what it looks like?
#' @param problem (optional) which problem should be done?
#' @param ... further arguments passed to fun
#' @return None 
#' @export
#' @examples
#' # not run
#' # genquiz(10, example1)

genquiz <-
function(B = 1, fun, folder, problem=0, Show = FALSE, ...) {
      if(missing(folder)) folder <- getwd()
    outfile <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
             "<quiz>")

    for(j in problem) {
      for(i in 1:B) {
        if(problem[1]==0) info <- fun(...)
        else info <- fun(problem=j, ...)
        if(Show) print(info)
        lns <- c("<!-- question: 0   -->", 
          "<question type=\"category\">",
              "<category>", 
                 paste0("<text>$course$/", info$category, "</text>"),
              "</category>", 
            "</question>", " ",
            
            "<!-- question: ", sample(1:1e6,1), "  -->",
              "<question type=\"cloze\">", 
              "<name>",
                   paste0("<text>", info$quizname,  i, "</text>"),
              "</name>",  
              "<questiontext format=\"html\">",
                   paste0("<text><![CDATA[", info$qtxt, "]]></text>"),
               "</questiontext>",  
               "<generalfeedback format=\"html\">",
                   paste0("<text><![CDATA[", info$atxt, "]]></text>"),
               "</generalfeedback>",
               "<penalty>0.0000000</penalty>",
               "<hidden>0</hidden>", 
               "<hint format=\"html\">",
                   paste0("<text><![CDATA[", info$htxt, "]]></text>"),
               "</hint>",
          "</question>")
        outfile <- c(outfile, "", lns)      
      }  
    }        
    outfile <- c(outfile, "</quiz>")  
    write(outfile, paste0(folder,"/newquiz.xml") )  
    
}
