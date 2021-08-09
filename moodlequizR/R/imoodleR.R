#' shinymoodlequizR 
#' 
#' This function runs the moodlequizR shiny app
#' @return None
#' @export
#' @examples 
#' # not run
#' # shinymoodlequizR()


shinymoodlequizR <- function () 
{
    shiny::runApp(system.file('shinymoodlequizR', package='moodlequizR'))
    
}
