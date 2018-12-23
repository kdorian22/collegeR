args = commandArgs(trailingOnly=TRUE)

sprays <- function(code, year){
  library(XML)
  library(dplyr)
  library(tidyr)
  library(rvest)
  
  
  loadCodes <- function(){
    ##loadCodes is a function used to load the stats.ncaa.org team codes 
    
    ##loadCodes takes no arguments
    
    ##loadCodes returns a dataframe of teamCodes
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
  
  loadRoster <- function(Code, year){
    ##loadRoster is a function used to scrape an entire roster from stats.ncaa.org
    
    ##loadRoster takes as an argument the team code associated with a given team 
    
    ##loadRoster returns the entire roster for that team
    Roster <- readHTMLTable(paste0('http://stats.ncaa.org/team/', Code, '/roster/', year))
    Roster <- as.data.frame(Roster[1])
    Roster <- extract(Roster, stat_grid.Player, c("LastName", "FirstName"), "([^ ]+) (.*)")
    Roster <- c(Roster[,c(1:5)])
    Roster$LastName <- gsub(',', '', Roster$LastName)
    Roster$Team <- teamCodes$School[which(teamCodes$Code==Code)]
    Roster <- as.data.frame(apply(as.data.frame(Roster), 2, function(x){as.character(x)}))
    colnames(Roster) <- c('Number', 'LastName', 'FirstName', 'Position', 'Year', 'Team')
    Roster$LastName <- gsub( "'", "",Roster$LastName)
    Roster$FirstName <- gsub( "'", "",Roster$FirstName)
    return((Roster))}
  roster <- loadRoster(code, year)
  
  quickStats <- function(code, year){
    ##quickStats is a function used to scrape basic offensive data from stats.ncaa.org 
    
    ##quickStats takes as an argument the code of the one team you want to look at,
    
    ##quickStats returns a dataframe of offensive statistics
    URL <- paste0('http://stats.ncaa.org/team/', code, '/stats/', year)
    myTeam <- as.data.frame(readHTMLTable(URL)[3])
    myTeam$Team <- teamCodes$School[which(teamCodes$Code==code)]
    myTeam[,(5:(ncol(myTeam)-1))] <- (apply(myTeam[,(5:(ncol(myTeam)-1))],2, function(x){as.numeric(as.character(x))}))
    myTeam[,(2:4)] <- (apply(myTeam[,(2:4)],2, function(x){as.character(x)}))
    myTeam[,2] <-gsub(',', '', myTeam[,2])
    myTeam <- extract(myTeam, `stat_grid.Player`, c("LastName", "FirstName"), "([^ ]+) (.*)")
    for(i in 1:nrow(myTeam)){
      for(j in 6:ncol(myTeam)){
        if(is.na(myTeam[i,j])==T){
          myTeam[i,j] <- 0}  
      }
    }
    colnames(myTeam) <- c('Number', 'LastName', 'FirstName', 'Year', 'Pos', 'GP', 'GS', 'G', 'BA', 'OBP', 'SLG',
                          'R', 'AB', 'H', '2B', '3B', 'TB', 'HR', 'RBI', 'BB', 'HBP', 'SF', 'SH', 'K', 'DP',
                          'CS', 'Picked', 'SB', '2outRBI','Team')
    myTeam <- myTeam[,c(30,1:7, 9:21, 24:26, 28)]
    myTeam$wOBA <- round((0.688*myTeam$BB + 0.72*myTeam$HBP + 0.89*(myTeam$`H`-myTeam$`2B`-myTeam$`3B`-myTeam$`HR`)+ 1.27*myTeam$`2B`
                          + 1.62*myTeam$`3B`+2.1*myTeam$HR)/(myTeam$AB+myTeam$BB+myTeam$HBP),3)
    return(myTeam)
  }
  stats <- quickStats(code, year)
  
  roster$LastName <- as.character(roster$LastName)
  roster$FirstName <- as.character(roster$FirstName)
  stats$LastName <- as.character(stats$LastName)
  stats$FirstName <- as.character(stats$FirstName)
  roster <- left_join(roster, stats, c('LastName' = 'LastName', 'FirstName'='FirstName'))                       
  roster2 <- roster[,c(1,2,3,4,5,17,6)]
  colnames(roster2) <- c('Number', 'LastName', 'FirstName','Position', 'Year', 'AB', 'Team')
  
  
  getLinks <- function(code, year){
    init <- paste0('https://stats.ncaa.org/team/', code, '/roster/', year)
    pg <- read_html(init)
    match <- '/teams'
    allLinks <- html_attr(html_nodes(pg, "a"), "href")
    team <- paste0('http://stats.ncaa.org',allLinks[grep(match, allLinks)])[3]
    
    pg <- read_html(team)
    match <- '/game/index'
    allLinks <- html_attr(html_nodes(pg, "a"), "href")
    team <- paste0('http://stats.ncaa.org',allLinks[grep(match, allLinks)])
    team <- as.vector(paste0(substr(team, 1, 27), 'play_by_play/', substr(team, 34, 40)))
    return(team)
  }
  URLs <- getLinks(code, year)
  
  PlayScrape <- function(URL, team){
    ##PlayScrape is a function used to scrape play by play data off the stats.ncaa webstie
    
    ##PlayScrape takes as arguments a vector of URLs of all the games you want the play by play data for,
    ##the official stats.ncaa team name
    
    ##PlayScrape returns a matrix with the plays for an entire team from the games provided in the RUL vector
    AllPlays <- c()
    Plays <- c()
    AllPlays <- (readHTMLTable(URL[1]))
    for(i in 2:length(URL)){
      AllPlays <- c(AllPlays, readHTMLTable(URL[i]))
      print(paste(i,'success'))}
    AllPlays <- AllPlays[-c(1:5)]
    AllPlays <- AllPlays[-which(sapply(AllPlays, is.null))]
    for(i in 1:length(AllPlays)){
      AllPlays[[i]] <- AllPlays[[i]][-2]
    }
    Plays <- list()
    for(i in 1:length(AllPlays)){
      Plays[[i]] <- as.data.frame((AllPlays[[i]][,which(names(AllPlays[[i]]) %in% team)]))}
    All <- data.frame()
    for(i in 1:length(Plays)){
      All <- rbind(All, Plays[[i]])
    }
    All <- (apply(All, 1, as.character))
    All <- (All[-which(All=='')])
    All <- as.data.frame(All)
    colnames(All) <- 'Description'
    All$Description <- gsub("'", "",All$Description)
    return(All)
  }
  plays <- PlayScrape(URLs, myTeam)
  
  
  loadPoints <- function(file){
    ##loadPoints is a function used to read in a csv file with the graphing instructions for the 
    ##makeSpray function
    
    ##loadPoints takes as an argument the name of the file you want to read in
    
    ##loadPoints returns nothing, but ut saves the file you read in to your Global Environment
    
    ##Graphical Point is the suggested file to use
    Points <- read.csv(file)
    Points[,c(1:4)] <- apply(Points[,c(1:4)], 2, function(x){as.character(x)})
    return(Points)}
  Points <- loadPoints("/home/kdorian/creative/app/Graphical Point.csv")
 
  sprayInfo<-function(Plays, Points, Roster, FirstName, LastName, Title=TRUE, groundx=1, 
                      groundy=1, airx=1, airy=1, Point = TRUE, Legend = T){
    ##makeSpray is a function used to make a visual spray chart using non-specfic play-by-play data
    
    ##makeSpray takes as arguments a matrix with the play-by-play data either provided by the user
    ##or scraped using playScrape, a dataframe with the plotting inctructions, the first
    ##and last name and handedness of the player being studied, whether the user wants a title, 
    ##the factor by which the data needs to be 
    ##jittered (use trial and error in most cases so the points all fit in fair territory), and whether the user
    ##wants points or lines to show where the ball is estimated to have gone
    
    ##makeSpray returns a visualization of a hitter's directional tendencies
    Player <- (grep(LastName, as.character(Plays$Description), value = T))
    Player <- as.data.frame(Player)
    outcomes <- "grounded|muffed throw|error|line|lined|lined|flied|force|single|pop|double|triple|home|choice|out at|foul|bunt"
    locations <- "1b|2b|3b| ss| p | p\\.| p\\,| c | c\\.| c\\,|catcher|pitcher|lf|rf|cf|shortstop|center|left|right|left center|right field line|lf line|rf line|left field line|right center|through the left side|through the right side|middle|1B line|3B line|third base|first base|second base"
    unwanted <- "picked off|caught stealing|struck"
    Player[,1] <- gsub(";.*", "", Player[,1])
    Player[,2] <- gsub("([A-Za-z]+).*", "\\1", Player[,1])
    Player[,2] <- gsub("(.*);.*", "\\1", Player[,2])
    Player$Player <- gsub('Wolff', 'Woff', Player$Player)
    Player$Player <- gsub('Bernsdorf', 'Bernsdof', Player$Player)
    Player[,3] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(outcomes, Player[,1]))))
    Player[,4] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(locations, Player[,1]))))
    Player[,5] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(unwanted, Player[,1]))))
    Player <- subset(Player, V1.2!='caught stealing' & V1.2!='picked off' & V1.2!='struck')
    Player <- Player[,-5]
    Player <- Player[(which(Player$V2 == FirstName | Player$V2 == LastName | Player$V2 == substr(FirstName,1,1) | Player$V2 == substr(LastName,1,1))),]
    Player <- Player[lapply(Player$V1,length)>0,]
    Player <- Player[lapply(Player$V1.1,length)>0,]
    Player[,3] <- sapply(Player[,3], function(x){x <- x[1]})
    ifelse(Player[,3]=='muffed throw', Player[which(Player[,3]=='muffed throw'),4] <- Player[which(Player[,3]=='muffed throw'),4][[1]][2],x <- 0)
    offCenter <- c(grep('cf to right center', Player$Player), grep('cf to left center', Player$Player),
                   grep('rf to right center', Player$Player), grep('lf to left center', Player$Player))
    offCenter <- sort(offCenter, decreasing=F)
    if(length(offCenter)>0){for(i in 1:length(offCenter)){Player$V1.1[offCenter[i]][[1]] <- Player$V1.1[offCenter[i]][[1]][2]}}
    Player[,4] <- sapply(Player[,4], function(x){x <- x[1]})
    Player[,5] <- paste(Player[,3], Player[,4])
    Player <- Player[Player$V5 %in% Points$Total,]
    sprayInfo <- Player
    x <- c()  
    y <- c()
    colors <- c()
    type <- c()
    line.type <- c()
    for(i in 1:length(Player[,5])){
      x[i] <- Points$x[which(Points$Total==Player$V5[i])]
      y[i] <- Points$y[which(Points$Total==Player$V5[i])]
      colors[i] <- Points$color[which(Points$Total==Player$V5[i])]
      type[i] <- Points$type[which(Points$Total==Player$V5[i])]
      line.type[i] <- Points$line.type[which(Points$Total==Player$V5[i])]
    }
    data <- as.data.frame(matrix(c(x,y,colors,type,line.type),length(x),5))
    colnames(data) <- c('x','y','color','type','line.type')
    return(data)
  } 
  
  makeSpray<-function(Plays, Points, Roster, FirstName, LastName, Title=TRUE, groundx=1, 
                      groundy=1, airx=1, airy=1, Point = TRUE, Legend = T){
    ##makeSpray is a function used to make a visual spray chart using non-specfic play-by-play data
    
    ##makeSpray takes as arguments a matrix with the play-by-play data either provided by the user
    ##or scraped using playScrape, a dataframe with the plotting inctructions, the first
    ##and last name and handedness of the player being studied, whether the user wants a title, 
    ##the factor by which the data needs to be 
    ##jittered (use trial and error in most cases so the points all fit in fair territory), and whether the user
    ##wants points or lines to show where the ball is estimated to have gone
    
    ##makeSpray returns a visualization of a hitter's directional tendencies
    Player <- (grep(LastName, as.character(Plays$Description), value = T))
    Player <- as.data.frame(Player)
    outcomes <- "grounded|muffed throw|error|line|lined|lined|flied|force|single|pop|double|triple|home|choice|out at|foul|bunt"
    locations <- "1b|2b|3b| ss| p | p\\.| p\\,| c | c\\.| c\\,|catcher|pitcher|lf|rf|cf|shortstop|center|left|right|left center|right field line|lf line|rf line|left field line|right center|through the left side|through the right side|middle|1B line|3B line|third base|first base|second base"
    unwanted <- "picked off|caught stealing|struck"
    Player[,1] <- gsub(";.*", "", Player[,1])
    Player[,2] <- gsub("([A-Za-z]+).*", "\\1", Player[,1])
    Player[,2] <- gsub("(.*);.*", "\\1", Player[,2])
    Player$Player <- gsub('Wolff', 'Woff', Player$Player)
    Player$Player <- gsub('Bernsdorf', 'Bernsdof', Player$Player)
    Player[,3] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(outcomes, Player[,1]))))
    Player[,4] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(locations, Player[,1]))))
    Player[,5] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(unwanted, Player[,1]))))
    Player <- subset(Player, V1.2!='caught stealing' & V1.2!='picked off' & V1.2!='struck')
    Player <- Player[,-5]
    Player <- Player[(which(Player$V2 == FirstName | Player$V2 == LastName | Player$V2 == substr(FirstName,1,1) | Player$V2 == substr(LastName,1,1))),]
    Player <- Player[lapply(Player$V1,length)>0,]
    Player <- Player[lapply(Player$V1.1,length)>0,]
    Player[,3] <- sapply(Player[,3], function(x){x <- x[1]})
    ifelse(Player[,3]=='muffed throw', Player[which(Player[,3]=='muffed throw'),4] <- Player[which(Player[,3]=='muffed throw'),4][[1]][2],x <- 0)
    offCenter <- c(grep('cf to right center', Player$Player), grep('cf to left center', Player$Player),
                   grep('rf to right center', Player$Player), grep('lf to left center', Player$Player))
    offCenter <- sort(offCenter, decreasing=F)
    if(length(offCenter)>0){for(i in 1:length(offCenter)){Player$V1.1[offCenter[i]][[1]] <- Player$V1.1[offCenter[i]][[1]][2]}}
    Player[,4] <- sapply(Player[,4], function(x){x <- x[1]})
    Player[,5] <- paste(Player[,3], Player[,4])
    Player <- Player[Player$V5 %in% Points$Total,]
    x <- c()  
    y <- c()
    colors <- c()
    type <- c()
    line.type <- c()
    for(i in 1:length(Player[,5])){
      x[i] <- Points$x[which(Points$Total==Player$V5[i])]
      y[i] <- Points$y[which(Points$Total==Player$V5[i])]
      colors[i] <- Points$color[which(Points$Total==Player$V5[i])]
      type[i] <- Points$type[which(Points$Total==Player$V5[i])]
      line.type[i] <- Points$line.type[which(Points$Total==Player$V5[i])]
    }
    data <- as.data.frame(matrix(c(x,y,colors,type,line.type),length(x),5))
    colnames(data) <- c('x','y','color','type','line.type')
    data$x <- as.numeric(as.character(data$x))
    data$y <- as.numeric(as.character(data$y))
    data$color <- as.character(data$color)
    data$type <- as.numeric(as.character(data$type))
    data$line.type <- as.numeric(as.character(data$line.type))
    ground <- subset(data, color=='blue')
    air <- subset(data,color!='blue')
    curve(-0.375*x^2 + 4, -2.194,2.194, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
    par(new=T)
    curve(-0.575*x^2 + 2, -1.188,1.188, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
    segments(x0=0, x1=c(-2.194,-0.69,0,0.69,2.194),y0=0, y1=c(2.194, 1.734, 2, 1.734, 2.194), lty=c(1,2,2,2,1))
    segments(x0=c(0,-0.84,0,0.84), x1=c(-0.84,0,0.84,0),y0=c(0,0.84,1.66,0.84), y1=c(0.84, 1.66, 0.84,0), lty=1)
    segments(x0=c(-0.5,0.5), x1=c(-1,1),y0=c(1.82,1.82), y1=c(3.625, 3.625), lty=2)
    if(Point == TRUE){points(x = c(jitter(ground$x, groundx), jitter(air$x, airx)), 
                             y = c(jitter(ground$y, groundy), jitter(air$y, airy)), 
                             col = c(ground$color, air$color), pch = c(ground$type, air$type))}
    else{segments(x0=0, y0=0, c(jitter(ground$x, groundx), jitter(air$x, airx)), 
                  y1=c(jitter(ground$y, groundy), jitter(air$y, airy)), col=c(ground$color, air$color), lty = c(ground$line.type, air$line.type))}
    if(Title==T){title(main = paste(FirstName, LastName)) 
      mtext(paste0('#', Roster$Number[which(Roster$LastName == LastName && Roster$FirstName == FirstName)], ' ',
                   Roster$Position[which(Roster$LastName == LastName  && Roster$FirstName == FirstName)]))}
    if(Legend==T){legend('bottomleft', legend=c('groundout', 'flyout', 'lineout', 'ground.hit', 'single', 'double', 'triple', 'homerun'), 
                         fill=c('white', 'white', 'white', 'blue','green', 'orange', 'purple', 'turquoise'), border=c('blue','red', rep('black',6)),cex=.5)}
    sprayInfo <- Player
    return(sprayInfo)
  }
  
  makeShades <- function(Plays, Points, Roster, FirstName, LastName, Title=T){
    ##makeShades is a function used to make a visual spray chart using non-specfic play-by-play data
    
    ##makeSpray takes as arguments a matrix with the play-by-play data either provided by the user
    ##or scraped using playScrape, a set of points dataframe with the plotting inctructions, 
    ##whether the user wants a title on the plot, and the first and last name and handedness of the player being studied
    
    ##makeSpray returns a shaded visualization of a hitter's directional tendencies
    Player <- (grep(LastName, as.character(Plays$Description), value = TRUE))
    Player <- as.data.frame(Player)
    outcomes <- "grounded|muffed throw|error|line|lined|lined|flied|force|single|pop|double|triple|home|choice|out at|foul|bunt"
    locations <- "1b|2b|3b| ss| p | p\\.| p\\,| c | c\\.| c\\,|catcher|pitcher|lf|rf|cf|shortstop|center|left|right|left center|right field line|lf line|rf line|left field line|right center|through the left side|through the right side|middle|1B line|3B line|third base|first base|second base"
    unwanted <- "picked off|caught stealing|struck"
    Player[,1] <- gsub(";.*", "", Player[,1])
    Player[,2] <- gsub("([A-Za-z]+).*", "\\1", Player[,1])
    Player[,2] <- gsub("(.*);.*", "\\1", Player[,2])
    Player$Player <- gsub('Wolff', 'Woff', Player$Player)
    Player[,3] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(outcomes, Player[,1]))))
    Player[,4] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(locations, Player[,1]))))
    Player[,5] <- as.data.frame(as.matrix(regmatches(Player[,1], gregexpr(unwanted, Player[,1]))))
    Player <- subset(Player, V1.2!='caught stealing' & V1.2!='picked off' & V1.2!='struck')
    Player <- Player[,-5]
    Player <- Player[(which(Player$V2 == FirstName | Player$V2 == LastName | Player$V2 == substr(FirstName,1,1) | Player$V2 == substr(LastName,1,1))),]
    Player <- Player[lapply(Player$V1,length)>0,]
    Player <- Player[lapply(Player$V1.1,length)>0,]
    Player[,3] <- sapply(Player[,3], function(x){x <- x[1]})
    ifelse(Player[,3]=='muffed throw', Player[which(Player[,3]=='muffed throw'),4] <- Player[which(Player[,3]=='muffed throw'),4][[1]][2],
           x <- 0)
    offCenter <- c(grep('cf to right center', Player$Player), grep('cf to left center', Player$Player),
                   grep('rf to right center', Player$Player), grep('lf to left center', Player$Player))
    offCenter <- sort(offCenter, decreasing=F)
    if(length(offCenter)>0){for(i in 1:length(offCenter)){Player$V1.1[offCenter[i]][[1]] <- Player$V1.1[offCenter[i]][[1]][2]}}
    Player[,4] <- sapply(Player[,4], function(x){x <- x[1]})
    Player[,5] <- paste(Player[,3], Player[,4])
    Player <- Player[Player$V5 %in% Points$Total,]
    infield <- Player[which(Player$V1.1=='shortstop'|Player$V1.1==' ss'|Player$V1.1=='2b'|
                              Player$V1.1=='1b'|Player$V1.1=='3b'|Player$V1.1==' p '|
                              Player$V1.1==' p.'|Player$V1.1==' p,'|Player$V1.1=='pitcher'|
                              Player$V1.1==' c '|Player$V1.1==' c.'|Player$V1.1==' c,'|
                              Player$V1.1=='catcher'|Player$V1.1=='first base'|Player$V1.1=='second base'|
                              Player$V1.1=='third base'|Player$V1.1=='through the right side'|
                              Player$V1.1=='through the left side'),]
    infield$type <- 'infield'
    outfield <- Player[which(Player$V1.1=='lf'|Player$V1.1=='rf'|Player$V1.1=='cf'|Player$V1.1=='middle'|
                               Player$V1.1=='left'|Player$V1.1=='right'|Player$V1.1=='center'|
                               Player$V1.1=='left center'|Player$V1.1=='right center'|Player$V1.1=='right field line'|
                               Player$V1.1=='left field line'|Player$V1.1=='lf line'|Player$V1.1=='rf line'),]
    outfield$type <- 'outfield'
    nrow(outfield)
    hit <- outfield[which(Player$V1=='single'|Player$V1=='double'|Player$V1=='triple'|Player$V1=='home'),]
    hit$type <- 'hit'
    short <- infield[which(infield$V1.1==' ss' | infield$V1.1=='through the left side'| infield$V1.1=='shortstop'|
                             infield$V1.1==' p '|infield$V1.1==' c.'|infield$V1.1==' c,'|infield$V1.1==' c '|infield$V1.1=='catcher'),]
    second <- infield[which(infield$V1.1=='2b' | infield$V1.1=='second base'| infield$V1.1=='through the right side'|infield$V1.1==' p.'|infield$V1.1==' p,'|infield$V1.1=='pitcher'),]
    first <- infield[which(infield$V1.1=='1b'|infield$V1.1=='first base'),]
    third <- infield[which(infield$V1.1=='3b'|infield$V1.1=='third base'),]
    colshort <- paste0('gray', round(1-nrow(short)/nrow(infield), 2)*100)
    colsecond <- paste0('gray', round(1-nrow(second)/nrow(infield), 2)*100)
    colfirst <- paste0('gray', round(1-nrow(first)/nrow(infield), 2)*100)
    colthird <- paste0('gray', round(1-nrow(third)/nrow(infield), 2)*100)
    left <- outfield[which(outfield$V1.1=='left'|outfield$V1.1=='lf'|outfield$V1.1=='left center'|outfield$V1.1=='lf line'),]
    right <- outfield[which(outfield$V1.1=='right'|outfield$V1.1=='rf'|outfield$V1.1=='right center'|outfield$V1.1=='rf line'),]
    center <- outfield[which(outfield$V1.1=='cf'|outfield$V1.1=='middle'|outfield$V1.1=='center'),]
    colleft <- paste0('gray', round(1-nrow(left)/nrow(outfield), 2)*100)
    colright <- paste0('gray', round(1-nrow(right)/nrow(outfield), 2)*100)
    colcenter <- paste0('gray', round(1-nrow(center)/nrow(outfield), 2)*100)
    curve(-0.375*x^2 + 4, -2.194,2.194, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
    segments(0,0,seq(-1.188,-0.69,0.001),-0.575*(seq(-1.188,-0.69,0.001))^2+2,col=colthird)
    segments(0,0,seq(-0.69,0,0.001), -0.575*seq(-0.69,0,0.001)^2+2, col=colshort)
    segments(0,0,seq(0,0.69,0.001), -0.575*seq(0,0.69,0.001)^2+2, col=colsecond)
    segments(0,0,seq(0.69,1.188,0.001), -0.575*seq(0.69,1.188,0.001)^2+2, col=colfirst)
    segments(seq(-1.188,-0.5,0.001), -0.575*(seq(-1.188,-0.5,0.001))^2+2,
             seq(-2.194,-1,0.001732946), -0.375*(seq(-2.194,-1,0.001732946))^2+4, col=colleft)
    segments(seq(-0.5,0.5,0.001), -0.575*(seq(-0.5,0.5,0.001))^2+2,
             seq(-1,1,0.001998002), -0.375*(seq(-1,1,0.001998002))^2+4, col=colcenter)
    segments(seq(0.5,1.188,0.001), -0.575*(seq(0.5,1.188,0.001))^2+2,
             seq(1,2.194,0.001732946), -0.375*(seq(1,2.194,0.001732946))^2+4, col=colright)
    segments(x0=0, x1=c(-2.194,-0.69,0,0.69,2.194),y0=0, y1=c(2.194, 1.734, 2, 1.734, 2.194), lty=c(1,2,2,2,1))
    segments(x0=c(0,-0.84,0,0.84), x1=c(-0.84,0,0.84,0),y0=c(0,0.84,1.66,0.84), y1=c(0.84, 1.66, 0.84,0), lty=1)
    segments(x0=c(-0.5,0.5), x1=c(-1,1),y0=c(1.82,1.82), y1=c(3.625, 3.625), lty=2)
    third <- paste0(round(nrow(third)/nrow(infield),3)*100, '%')
    short <- paste0(round(nrow(short)/nrow(infield),3)*100, '%')
    second <- paste0(round(nrow(second)/nrow(infield),3)*100, '%')
    first <- paste0(round(nrow(first)/nrow(infield),3)*100, '%')
    left <- paste0(round(nrow(left)/nrow(outfield),3)*100, '%')
    right <- paste0(round(nrow(right)/nrow(outfield),3)*100, '%')
    center <- paste0(round(nrow(center)/nrow(outfield),3)*100, '%')
    text(-0.75,1.25, third, font=2, cex=.95)
    text(-0.3,1.63, short, font=2, cex=.95)
    text(0.3,1.63, second, font=2, cex=.95)
    text(0.75,1.25, first, font=2, cex=.95)
    text(-1.5,2.5, left, font=2, cex=1.2)
    text(1.5,2.5, right, font=2, cex=1.2)
    text(0,3, center, font=2, cex=1.2)
    par(new=TRUE)
    curve(-0.375*x^2 + 4, -2.194,2.194, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
    par(new=TRUE)
    curve(-0.575*x^2 + 2, -1.188,1.188, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
    if(Title==TRUE){title(main = paste(FirstName, LastName))
      mtext(paste0('#', Roster$Number[which(Roster$LastName == LastName & Roster$FirstName == FirstName)], ' ',
                   Roster$Position[which(Roster$LastName == LastName & Roster$FirstName == FirstName)],' (', Roster$Year[which(Roster$LastName==LastName & Roster$FirstName == FirstName)], ')'))
    }
    results <- as.data.frame(matrix(c(third,short,second,first,left,center,right), 7,1))
    rownames(results) <- c('3B','SS','2B','1B','LF','CF','RF')
    colnames(results) <- 'Percentage'
    return(results)
  }

  setwd("/home/kdorian/creative/app/static")
  pdf(file = paste0('sprays.pdf'), height = 11, width = 8.5)
  roster2$AB <- ifelse(is.na(roster2$AB), 0, roster2$AB)
  for(i in 1:nrow(roster2)){
    if(roster2$AB[i] > 10){
      info <- sprayInfo(plays, Points, roster2, roster2$FirstName[i], roster2$LastName[i], T)
      ground <- sum(as.numeric(as.character(info$y))<=1.6)
      air <- nrow(info)-ground
      if(ground > 5 & air > 5){
        groundx <- ifelse(ground > 50, 18, ifelse(ground > 40, 15, ifelse(ground > 25, 13, ifelse(ground > 10, 10, ifelse(ground > 7, 4, 2)))))
        groundy <- max(groundx - 2, 1)
        airx <- ifelse(air > 70, 20, ifelse(air > 50, 17, ifelse(air > 30, 14, ifelse(air > 20, 10, ifelse(air>10, 6, 3)))))
        airy <- max(airx-2, 1)
        layout(matrix(c(1,2), 2,1, byrow=T))
        par(mar = c(.1,.1,3,.1))
        makeShades(plays, Points, roster2, roster2$FirstName[i], roster2$LastName[i], T)
        makeSpray(plays, Points, roster2, roster2$FirstName[i], roster2$LastName[i], F, groundx,groundy, airx, airy, T, T)
      }
    }
    
  }
  dev.off()
}
sprays(args[1], args[2])



