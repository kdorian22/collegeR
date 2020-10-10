args = commandArgs(trailingOnly=TRUE)
sprays <- function(code, year){

  loadCodes <- function(){
    ##loadCodes is a function used to load the stats.ncaa.org team codes 
    
    ##loadCodes takes no arguments
    
    ##loadCodes returns a dataframe of teamCodes
    library(XML)
    Codes <- readHTMLTable('http://stats.ncaa.org/game_upload/team_codes')
    teamCodes <- as.data.frame(Codes[1])
    colnames(teamCodes) <- c('Code', 'School')
    teamCodes <- teamCodes[-c(1:4),]
    teamCodes$Code <- as.character(teamCodes$Code)
    teamCodes$School <- as.character(teamCodes$School)
    return(teamCodes)
  }
  teamCodes <- loadCodes()
  
  myTeam <- teamCodes$School[which(teamCodes$Code == code)]
  
  
  setwd("/home/kdorian/creative/app/static")
  pdf(file = paste0('sprays.pdf'), height = 11, width = 8.5)
  plot(1,1)
  title(myTeam)
  dev.off()
  
  }
sprays(args[1], args[2])


