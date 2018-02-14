loadCodes <- function(){
  ##loadCodes is a function used to load the stats.ncaa.org team codes 
  
  ##loadCodes takes no arguments
  
  ##loadCodes doesn't return anything, but it saves the object called teamCodes to your global environment 
  require(XML)
  Codes <- readHTMLTable('http://stats.ncaa.org/game_upload/team_codes')
  teamCodes <- as.data.frame(Codes[1])
  colnames(teamCodes) <- c('Code', 'School')
  teamCodes <- teamCodes[-c(1:4),]
  teamCodes$Code <- as.character(teamCodes$Code)
  teamCodes$School <- as.character(teamCodes$School)
  return(teamCodes)
}


findName <- function(schools){
  ##findName is a function used to find the specific name given to a team by the NCAA
  
  ##findName takes as an argument a vector with the first word of the 
  ##schools you wan to know the names of
  
  ##Enter just the first letter, if what you are looking for 
  ##doens't appear when entering the first word
  
  ##findName returns a list of the possible names
  sapply(schools, function(x){teamCodes[grepl(x, teamCodes$School, ignore.case=T), 2]})
}


nameCodes <- function(Teams, teamCodes){
  ##nameCodes is a function that privides the team code associated with any NCAA team
  
  ##neameCodes takes as arguments a vector of the the specific names given to each team by stats.ncaa.org,
  ##and the list of NCAA team codes
  
  ##Use findName to find the specific spellings/wordings
  
  ##nameCodes returns a list of the codes
  sapply(Teams, function(x){teamCodes$Code[which(teamCodes$School == x)]})
}


loadRoster <- function(Code){
  ##loadRoster is a function used to scrape an entire roster from stats.ncaa.org
  
  ##loadRoster takes as an argument the team code associated with a given team 
  
  ##loadRoster returns the entire roster for that team
  require(XML)
  require(tidyr)
  Roster <- readHTMLTable(paste0('http://stats.ncaa.org/team/', Code, '/roster/12560'))
  Roster <- as.data.frame(Roster[1])
  Roster <- extract(Roster, stat_grid.Player, c("LastName", "FirstName"), "([^ ]+) (.*)")
  Roster <- c(Roster[,c(1:5)])
  Roster$LastName <- gsub(',', '', Roster$LastName)
  Roster <- as.data.frame(apply(as.data.frame(Roster), 2, function(x){as.character(x)}))
  colnames(Roster) <- c('Number', 'LastName', 'FirstName', 'Position', 'Year')
  return((Roster))}

PlayScrape <- function(URL, team){
  ##PlayScrape is a function used to scrape play by play data off the stats.ncaa webstie
  
  ##PlayScrape takes as arguments a vector of URLs of all the games you want the play by play data for,
  ##the official stats.ncaa team name
  ##PlayScrape returns a matrix with the plays for an entire team from the games provided in the RUL vector
  require(XML)
  AllPlays <- c()
  Plays <- c()
  AllPlays <- (readHTMLTable(URL[1]))
  for(i in 2:length(URL)){
    AllPlays <- c(AllPlays, readHTMLTable(URL[i]))}
  AllPlays <- AllPlays[-c(1:5)]
  AllPlays <- AllPlays[-which(sapply(AllPlays, is.null))]
  for(i in 1:length(AllPlays)){
    AllPlays[[i]] <- AllPlays[[i]][-2]
  }
  Plays <- list()
  
  All <- data.frame()
  for(i in 1:length(Plays)){
    All <- rbind(All, Plays[[i]])
  }
  All <- (apply(All, 1, as.character))
  All <- (All[-which(All=='')])
  All <- as.data.frame(All)
  colnames(All) <- 'Description'
  return(All)
}



loadPoints <- function(file){
  ##loadPoints is a function used to read in a csv file with the graphing instructions for the 
  ##makeSpray function
  
  ##loadPoints takes as an argument the name of the file you want to read in
  
  ##loadPoints returns nothing, but ut saves the file you read in to your Global Environment
  
  ##Graphical Point is the suggested file to use
Points <- read.csv(file)
Points[,c(1:4)] <- apply(Points[,c(1:4)], 2, function(x){as.character(x)})
return(Points)}


makeSpray<-function(Plays, Points, Roster, FirstName, LastName, Hand, Title=TRUE, groundx=1, 
                     groundy=1, airx=1, airy=1, Point = TRUE){
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
  ground <- subset(data,color=='blue')
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
  if(Title==T){title(main = paste(FirstName, LastName, paste0('(', Hand, ')'))) 
  mtext(paste0('#', Roster$Number[which(Roster$LastName == LastName)], ' ',
               Roster$Position[which(Roster$LastName == LastName)]))}
  if(Point==T){legend('bottomleft', legend=c('groundout', 'flyout', 'lineout', 'ground.hit', 'single', 'double', 'triple', 'homerun'), 
         fill=c('white', 'white', 'white', 'blue','green', 'orange', 'purple', 'turquoise'), border=c('blue','red', rep('black',6)),cex=0.75)}
  else{legend('bottomleft', legend=c('groundout', 'flyout', 'lineout', 'ground.hit', 'single', 'double', 'triple', 'homerun'), 
                       col=c('blue', 'red', 'black', 'blue', 'green', 'orange', 'purple', 'turquoise'), lty=c(2,2,2,rep(1,6)),cex=0.75)}
  
  sprayInfo <- Player
  return(sprayInfo)
}



groundRates <- function(Plays, Points, FirstName, LastName){
  Player <- (grep(LastName, as.character(Plays$Description), value = T))
  Player <- as.data.frame(Player)
  outcomes <- "grounded|muffed throw|error|line|lined|lined|flied|force|single|pop|double|triple|home|choice|out at|foul|bunt"
  locations <- "1b|2b|3b| ss| p | p\\.| p\\,| c | c\\.| c\\,|catcher|pitcher|lf|rf|cf|shortstop|center|left|right|left center|right field line|lf line|rf line|left field line|right center|through the left side|through the right side|middle|1B line|3B line|third base|first base|second base"
  unwanted <- "picked off|caught stealing|struck"
  Player[,1] <- gsub(";.*", "", Player[,1])
  Player[,2] <- gsub("([A-Za-z]+).*", "\\1", Player[,1])
  Player[,2] <- gsub("(.*);.*", "\\1", Player[,2])
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
  colors <- c()
  type <- c()
  for(i in 1:length(Player[,5])){
    x[i] <- Points$x[which(Points$Total==Player$V5[i])]
    colors[i] <- Points$color[which(Points$Total==Player$V5[i])]
    type[i] <- Points$type[which(Points$Total==Player$V5[i])]
  }
  data <- as.data.frame(matrix(c(x,colors, type),length(x),3))
  colnames(data) <- c('x','color', 'type')
  data$x <- as.numeric(as.character(data$x))
  infield <- data[which(data$color=='blue'),]
  groundRight <- as.numeric(round(nrow(data[which(data$color=='blue' & data$x > 0),])/nrow(infield),3))
  groundMiddle <- as.numeric(round(nrow(data[which(data$color=='blue' & data$x == 0),])/nrow(infield),3))
  groundLeft <- as.numeric(round(nrow(data[which(data$color=='blue' & data$x < 0),])/nrow(infield),3))
  groundoutLeft <- as.numeric(round(nrow(data[which(data$color=='blue' & data$x < 0 & data$type==1),])))
  groundoutRight <- as.numeric(round(nrow(data[which(data$color=='blue' & data$x > 0 & data$type==1),])))
  throughRight <- round(nrow(Player[which(Player[4]=='through the right side'),]),0)
  throughLeft <- round(nrow(Player[which(Player[4]=='through the left side'),]),0)
  hits <- round(nrow(data[which(data$type==19),]),0)
  infield <- c(groundLeft, groundMiddle, groundRight, groundLeft, throughLeft, throughRight, hits, groundoutLeft, groundoutRight)
  return(infield)
}


makeShades <- function(Plays, Points, Roster, FirstName, LastName, Hand, Title=T){
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
  colshort <- paste0('gray', round(1-nrow(short)/nrow(Player), 2)*100)
  colsecond <- paste0('gray', round(1-nrow(second)/nrow(Player), 2)*100)
  colfirst <- paste0('gray', round(1-nrow(first)/nrow(Player), 2)*100)
  colthird <- paste0('gray', round(1-nrow(third)/nrow(Player), 2)*100)
  left <- outfield[which(outfield$V1.1=='left'|outfield$V1.1=='lf'|outfield$V1.1=='left center'|outfield$V1.1=='lf line'),]
  right <- outfield[which(outfield$V1.1=='right'|outfield$V1.1=='rf'|outfield$V1.1=='right center'|outfield$V1.1=='rf line'),]
  center <- outfield[which(outfield$V1.1=='cf'|outfield$V1.1=='middle'|outfield$V1.1=='center'),]
  colleft <- paste0('gray', round(1-nrow(left)/nrow(Player), 2)*100)
  colright <- paste0('gray', round(1-nrow(right)/nrow(Player), 2)*100)
  colcenter <- paste0('gray', round(1-nrow(center)/nrow(Player), 2)*100)
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
  third <- paste0(round(nrow(third)/nrow(Player),3)*100, '%')
  short <- paste0(round(nrow(short)/nrow(Player),3)*100, '%')
  second <- paste0(round(nrow(second)/nrow(Player),3)*100, '%')
  first <- paste0(round(nrow(first)/nrow(Player),3)*100, '%')
  left <- paste0(round(nrow(left)/nrow(Player),3)*100, '%')
  right <- paste0(round(nrow(right)/nrow(Player),3)*100, '%')
  center <- paste0(round(nrow(center)/nrow(Player),3)*100, '%')
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
  if(Title==TRUE){title(main = paste(FirstName, LastName, paste0('(', Hand, ')'))) 
  mtext(paste0('#', Roster$Number[which(Roster$LastName == LastName)], ' ',
               Roster$Position[which(Roster$LastName == LastName)]))}
  results <- as.data.frame(matrix(c(third,short,second,first,left,center,right), 7,1))
  rownames(results) <- c('3B','SS','2B','1B','LF','CF','RF')
  colnames(results) <- 'Percentage'
  return(results)
}


powerNumbers <- function(Plays, Points, Roster, FirstName, LastName, Hand, Title=TRUE, Legend=TRUE){
  ##powerNumbers is a function used to visualize a player's power directional tendencies using non-specfic play-by-play data
  
  ##powerNumbers takes as arguments a matrix with the play-by-play data either provided by the user
  ##or scraped using playScrape, a set of points dataframe with the plotting inctructions, the first and last name 
  ##and handedness of the player being studied, and whether the user wants a legend and a title on the plot 
  
  ##powerNumbers returns a plot of a field with numeric annotations
  Player <- (grep(LastName, as.character(Plays$Description), value = TRUE))
  Player <- as.data.frame(Player)
  outcomes <- "grounded|muffed throw|error|line|lined|lined|flied|force|single|pop|double|triple|home|choice|out at|foul|bunt"
  locations <- "1b|2b|3b| ss| p | p\\.| p\\,| c | c\\.| c\\,|catcher|pitcher|lf|rf|cf|shortstop|center|left|right|left center|right field line|lf line|rf line|left field line|right center|through the left side|through the right side|middle|1B line|3B line|third base|first base|second base"
  unwanted <- "picked off|caught stealing|struck"
  Player[,1] <- gsub(";.*", "", Player[,1])
  Player[,2] <- gsub("([A-Za-z]+).*", "\\1", Player[,1])
  Player[,2] <- gsub("(.*);.*", "\\1", Player[,2])
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
  hit <- Player[which(Player$V1=='double'|Player$V1=='triple'|Player$V1=='home'),]
  left <- hit[which(hit$V1.1=='left'|hit$V1.1=='lf'|hit$V1.1=='lf line'),]
  lc <- hit[which(hit$V1.1=='left center'),]
  right <- hit[which(hit$V1.1=='right'|hit$V1.1=='rf'|hit$V1.1=='rf line'),]
  rc <- hit[which(hit$V1.1=='right center'),]
  center <- hit[which(hit$V1.1=='cf'|hit$V1.1=='middle'|hit$V1.1=='center'),]
  l2 <- nrow(left[which(left$V1=='double'),])
  r2 <- nrow(right[which(right$V1=='double'),])
  c2 <- nrow(center[which(center$V1=='double'),])
  lc2 <- nrow(lc[which(lc$V1=='double'),])
  rc2 <- nrow(rc[which(rc$V1=='double'),])
  l3 <- nrow(left[which(left$V1=='triple'),])
  r3 <- nrow(right[which(right$V1=='triple'),])
  c3 <- nrow(center[which(center$V1=='triple'),])
  lc3 <- nrow(lc[which(lc$V1=='triple'),])
  rc3 <- nrow(rc[which(rc$V1=='triple'),])
  l4 <- nrow(left[which(left$V1=='home'),])
  r4 <- nrow(right[which(right$V1=='home'),])
  c4 <- nrow(center[which(center$V1=='home'),])
  lc4 <- nrow(lc[which(lc$V1=='home'),])
  rc4 <- nrow(rc[which(rc$V1=='home'),])
  curve(-0.375*x^2 + 4, -2.194,2.194, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
  segments(x0=c(0,-0.84,0,0.84), x1=c(-0.84,0,0.84,0),y0=c(0,0.84,1.66,0.84), y1=c(0.84, 1.66, 0.84,0), lty=1)
  segments(x0=0, x1=c(-2.194,2.194),y0=0, y1=c(2.194, 2.194), lty=1)
  par(new=T)
  curve(-0.575*x^2 + 2, -1.188,1.188, xlim=c(-3,3), ylim=c(0,4.5), xlab='', ylab='', axes=F)
  text(c(-1.5,-1.5,-1.5,-0.8,-0.8,-0.8,1.5,1.5,1.5,0.8,0.8,0.8,0,0,0),
       c(2.1,2.4,2.7,2.6,2.9,3.2,2.1,2.4,2.7,2.6,2.9,3.2,3.1,3.4,3.7),
       c(l2,l3,l4,lc2,lc3,lc4,r2,r3,r4,rc2,rc3,rc4,c2,c3,c4), font=2, cex=1,
       col=c('orange','purple','turquoise'))
  if(Legend==TRUE){legend('bottomleft', legend=c('doubles', 'triples', 'homeruns'), 
         fill=c('orange', 'purple', 'turquoise'), cex=0.75)}
  if(Title==TRUE){title(main = paste(FirstName, LastName, paste0('(', Hand, ')')))
  mtext(paste0('#', Roster$Number[which(Roster$LastName == LastName)], ' ',
               Roster$Position[which(Roster$LastName == LastName)]))}
  }


makeTitle <- function(Roster, FirstName, LastName){
  ##makeTitle is a function used to create a text box with a player's name, number, and position
   
  ##makeTitle takes as an argument the Roster of the player, his firstname, and his last name
  
  ##powerNumbers returns a text box with the player's name, number, and position
  plot(c(0, 200), c(0,200), type= "n", xlab = "", ylab = "",axes=F)
  text(100,160, paste(FirstName, LastName),cex=1.5)
  text(100,140,paste0('#', Roster$Number[which(Roster$LastName == LastName)], ' ',
           Roster$Position[which(Roster$LastName == LastName)]),cex=1.5)
  text(80, 80, 'Notes:',cex=1.5)
  }

matchUp <- function(FirstName, LastName, Plays){
  ##matchUp is a funtion used to extract the at bats against a certain relief pitcher
  
  ##matchUp takes as arguments the first and last name of the pitcher and the set of plays
  ##the user wants to subset
  
  ##matchUp outputs a dataframe with the play by play dexriptions of all at bats against
  ##the desired pitcher
  Start <- c(grep(paste0(LastName, ' to p for'), Plays1$Description), 
    grep(paste0(LastName, ', ', FirstName , ' to p for'), Plays1$Description),
    grep(paste0(LastName, ', ', substr(FirstName,1,1), '.' , ' to p for'), Plays1$Description),
    grep(paste0(FirstName, ' ', LastName, ' to p for'), Plays1$Description),
    grep(paste0(substr(FirstName,1,1),'. ', LastName, ' to p for'), Plays1$Description))
  Start <- sort(unique(Start), decreasing=F)
  Stop <-  c(grep(paste0('to p for ', FirstName, ' ', LastName), Plays1$Description),
             grep(paste0('to p for ', LastName), Plays1$Description),
             grep(paste0('to p for ', LastName, ', ',substr(FirstName,1, 1), '.'), Plays1$Description),
             grep(paste0('to p for ', substr(FirstName,1, 1), ('. '), LastName), Plays1$Description),
             grep(paste0('to p for ', LastName, ', ', FirstName), Plays1$Description))
  Stop <- sort(unique(Stop), decreasing=F)
  lenStart <- length(Start)
  lenStop <- length(Stop)
  allPlays <- c()
 for(i in 1:length(Stop)){
    allPlays <- c(allPlays, c(Start[i]:Stop[i]))}
  MatchUp <- Plays[allPlays,]
  MatchUp <- as.data.frame(as.character(MatchUp))
  colnames(MatchUp) <- 'Description'
  return(MatchUp)
  }

####################  Stats Scraping  #####################
d3SOS <- function(){
  ##d3SOS is a function used to scrape the d3baseball.com strength of schedule data
  
  ##d3SOS take no arguments
  
  ##d3SOS returns a dataframe with the strength of schedule data to your Global Evironment
  require(rvest) 
  URL <- 'http://d3baseball.com/seasons/2017/schedule?tmpl=sos-template'
  temp <- URL %>% 
    read_html %>%
    html_nodes("table")
  allTables <- html_table(temp)
  SOS <- as.data.frame(allTables[1])
  colnames(SOS) <- c('Team', 'Record', 'Win%', 'OWP', 'OOWP', 'SOS')
  SOS <- SOS[-1,]
  SOS <- apply(SOS, 1, function(x){sub(".*  ","",x)})
  SOS <- t(SOS)
  rownames(SOS) <- c(1:nrow(SOS))
  SOS <- as.data.frame(SOS)
  SOS <- SOS[order(SOS$Team),]
  Names <- read.csv('/Users/kennydorian/Documents/Baseball/NCAA-Package/Team Names.csv')
  SOS[,1] <- Names
  SOS <- SOS[order(SOS$SOS, decreasing = T),]
  return(SOS)
  }

loadSOS <- function(Division = 3){
  ##loadSOS scrapes the strength of schedule data from stats.ncaa website 
  
  ##loadSOS takes as an argument the division of the strengths of schedule you are seeking
  
  ##It returns a data frame with every team's strength of schedule
  require(XML)
  if(Division == 3) {code <- 16067}
  if(Division == 2){code <- 16122}
  if(Division == 1) {code <- 16002}
  SOS <- as.data.frame(readHTMLTable(paste0('http://stats.ncaa.org/reports/toughest_schedule?id=', code))[1])
  SOS <- SOS[,c(2,15)]
  colnames(SOS) <- c('Team', 'SOS')
  return(SOS)}

Offense <- function(codes, SOS){
  ##Offense is a function used to scrape basic offensive data from stats.ncaa.org 
  ##and calculate several advaanced metrics.
  
  ##Offense takes as an argument the team codes of the teams in the league you want to look at,
  ##and the NCAA division of those teams
  
  ##The calculation of the advanced metrics depends on more than one team being used
  
  ##Offense returns a dataframe of offensive statistics
  require(XML)
  require(tidyr)
  getSites <- function(codes){
    Sites <- c()
    for(i in codes){
      base <- 'http://stats.ncaa.org/team/'
      Sites <- c(Sites, paste0(base, i, '/stats/12560'))}
    return(Sites)
  }

  myTeam <- cbind(as.data.frame(readHTMLTable(getSites(codes)[1])[3]), 
                  rep(teamCodes$School[which(teamCodes$Code == codes[1])], nrow(as.data.frame(readHTMLTable(getSites(codes)[1])[3]))))
  colnames(myTeam) <- (1:ncol(myTeam))
  otherTeams <- NULL
  for(i in 2:length(codes)){
    otherTeam <- (cbind(as.data.frame(readHTMLTable(getSites(codes)[i])[3]), 
                        rep(teamCodes$School[which(teamCodes$Code == codes[i])],
                            nrow(as.data.frame(readHTMLTable(getSites(codes)[i])[3])))))
    colnames(otherTeam) <- (1:ncol(otherTeam))
    otherTeams <- rbind(otherTeams, otherTeam)}
  myLeague <- rbind(myTeam, otherTeams)
  myLeague[,(5:(ncol(myLeague)-1))] <- (apply(myLeague[,(5:(ncol(myLeague)-1))],2, function(x){as.numeric(as.character(x))}))
  myLeague[,(2:4)] <- (apply(myLeague[,(2:4)],2, function(x){as.character(x)}))
  myLeague[,2] <-gsub(',', '', myLeague[,2])
  myLeague <- extract(myLeague, `2`, c("LastName", "FirstName"), "([^ ]+) (.*)")
  for(i in 1:nrow(myLeague)){
    for(j in 6:ncol(myLeague)){
      if(is.na(myLeague[i,j])==T){
        myLeague[i,j] <- 0}  
    }
  }
  colnames(myLeague) <- c('Number', 'LastName', 'FirstName', 'Year', 'Pos', 'GP', 'GS', 'G', 'BA', 'OBP', 'SLG',
                          'R', 'AB', 'H', '2B', '3B', 'TB', 'HR', 'RBI', 'BB', 'HBP', 'SF', 'SH', 'K', 'DP',
                          'CS', 'Picked', 'SB', '2outRBI','Team')
  myLeague$ISO <- myLeague$SLG-myLeague$BA
  myLeague$`1B` <- myLeague$H-myLeague$HR-myLeague$`3B`-myLeague$`2B`
  myLeague$PA <- myLeague$AB + myLeague$BB + myLeague$HBP + myLeague$SF + myLeague$SH
  myLeague$wOBA <- (0.688*myLeague$BB + 0.72*myLeague$HBP + 0.89*myLeague$`1B`+ 1.27*myLeague$`2B`
                    + 1.62*myLeague$`3B`+2.1*myLeague$HR)/(myLeague$AB+myLeague$BB+myLeague$HBP)
  myLeague <- myLeague[-which(is.nan(myLeague$wOBA)==T),]
  lgwOBA <- mean(myLeague$wOBA)
  myLeague$wRAA <- (myLeague$wOBA - lgwOBA)*myLeague$PA
  lgRPA <- sum(myLeague$R)/sum(myLeague$PA)
  myLeague$wRC <- (((myLeague$wOBA-lgwOBA)+lgRPA))*myLeague$PA
  lgwRCPA <- sum(myLeague$wRC)/sum(myLeague$PA)
  unadjBattingRuns <- (myLeague$wRAA + ((lgRPA - lgwRCPA)*myLeague$PA))
  SOS <- SOS
    #
  Add <- c()
  for(i in 1:nrow(myLeague)){
    Add[i] <- as.numeric(as.character(SOS$SOS))[which(as.character(myLeague$Team[i])==as.character(SOS$Team))]
  }
  Add <- (5*Add)-1
  myLeague$BattingRuns <- (unadjBattingRuns+Add)
  runSB <- 0.2
  runCS <- -0.4
  lgSB <- mean(myLeague$SB)
  lgCS <- mean(myLeague$CS)
  lgwSB <- (lgSB*runSB + lgCS*runCS)/
    (mean(myLeague$`1B`)+mean(myLeague$BB)+mean(myLeague$HBP)) 
  myLeague$BaseRunningRuns <- (myLeague$SB*runSB + myLeague$CS*runCS)-
    (lgwSB*(myLeague$`1B`+ myLeague$BB + myLeague$HBP))
  lgAdjust <- ((-1)*(mean(myLeague$BattingRuns)+mean(myLeague$BaseRunningRuns))/
                 sum(myLeague$PA))*myLeague$PA
  runsWin <- 9*((sum(myLeague$R))/sum((Pitching(codes))$IP))*1.5+3
  TotGames <- max(myLeague$GP)
  ReplacementRuns <- (((TotGames*(length(codes)/2)/2430)*570))*((mean(sapply(unique(myLeague$Team), 
                                                                             function(x){max(myLeague$GP[which(myLeague$Team==x)])}))*
                                                                   (length(codes)/2))/(TotGames*(length(codes))/2))*(runsWin/(sum(myLeague$PA)))*myLeague$PA
  myLeague$WAR <- (myLeague$BattingRuns+myLeague$BaseRunningRuns+lgAdjust+ReplacementRuns)/runsWin
  rownames(myLeague) <- c(1:nrow(myLeague))
  myLeague <- myLeague[,-c(8, 27, 29)]
  nrow(myLeague)
  myLeague <- myLeague[,c(27, 1:13, 29, 14:26, 28, 30:36)]
}
UAA <- Offense(nameCodes(c('Washington-St. Louis', 'Emory', 'CWRU', 'NYU', 'Greenville', 'Brandeis'), teamCodes), 3)

quickStats <- function(code){
  ##quickStats is a function used to scrape basic offensive data from stats.ncaa.org 
  
  ##quickStats takes as an argument the code of the one team you want to look at,
  
  ##quickStats returns a dataframe of offensive statistics
  require(XML)
  require(tidyr)
  URL <- paste0('http://stats.ncaa.org/team/', code, '/stats/12560')
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


multiStats <- function(codes, Division){
  ##multiStats is a function used to scrape basic offensive data from stats.ncaa.org 
  
  ##multiStats takes as an argument the team codes of the teams in the league you want to look at,
  ##and the NCAA division of those teams
  
  ##multiStats returns a dataframe of offensive statistics
  require(XML)
  require(tidyr)
  getSites <- function(codes){
    Sites <- c()
    for(i in codes){
      base <- 'http://stats.ncaa.org/team/'
      Sites <- c(Sites, paste0(base, i, '/stats/12560'))}
    return(Sites)
  }
  getSites(codes)
  myTeam <- cbind(as.data.frame(readHTMLTable(getSites(codes)[1])[3]), 
                  rep(teamCodes$School[which(teamCodes$Code == codes[1])], nrow(as.data.frame(readHTMLTable(getSites(codes)[1])[3]))))
  colnames(myTeam) <- (1:ncol(myTeam))
  otherTeams <- NULL
  for(i in 2:length(codes)){
    otherTeam <- (cbind(as.data.frame(readHTMLTable(getSites(codes)[i])[3]), 
                        rep(teamCodes$School[which(teamCodes$Code == codes[i])],
                            nrow(as.data.frame(readHTMLTable(getSites(codes)[i])[3])))))
    colnames(otherTeam) <- (1:ncol(otherTeam))
    otherTeams <- rbind(otherTeams, otherTeam)}
  myLeague <- rbind(myTeam, otherTeams)
  myLeague[,(5:(ncol(myLeague)-1))] <- (apply(myLeague[,(5:(ncol(myLeague)-1))],2, function(x){as.numeric(as.character(x))}))
  myLeague[,(2:4)] <- (apply(myLeague[,(2:4)],2, function(x){as.character(x)}))
  myLeague[,2] <-gsub(',', '', myLeague[,2])
  myLeague <- extract(myLeague, `2`, c("LastName", "FirstName"), "([^ ]+) (.*)")
  for(i in 1:nrow(myLeague)){
    for(j in 6:ncol(myLeague)){
      if(is.na(myLeague[i,j])==T){
        myLeague[i,j] <- 0}  
    }
  }
  colnames(myLeague) <- c('Number', 'LastName', 'FirstName', 'Year', 'Pos', 'GP', 'GS', 'G', 'BA', 'OBP', 'SLG',
                          'R', 'AB', 'H', '2B', '3B', 'TB', 'HR', 'RBI', 'BB', 'HBP', 'SF', 'SH', 'K', 'DP',
                          'CS', 'Picked', 'SB', '2outRBI','Team')
  myLeague <- myLeague[,c(30,1:7, 9:29)]
  myLeague$wOBA <- round((0.688*myLeague$BB + 0.72*myLeague$HBP + 0.89*(myLeague$`H`-myLeague$`2B`-myLeague$`3B`-myLeague$`HR`)+ 1.27*myLeague$`2B`
                    + 1.62*myLeague$`3B`+2.1*myLeague$HR)/(myLeague$AB+myLeague$BB+myLeague$HBP),3)
  

quickPitch <- function(code){
  require(XML)
  require(tidyr)
  URL <- paste0('http://stats.ncaa.org/team/', code, '/stats?id=12560&year_stat_category_id=11001')
  myTeam <- as.data.frame(readHTMLTable(URL)[3])
  myTeam$Team <- teamCodes$School[which(teamCodes$Code==code)]
  myTeam$stat_grid.App <- as.numeric(as.character(myTeam$stat_grid.App))
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
  myTeam <- myTeam[which(myTeam$stat_grid.App>0),] 
  colnames(myTeam) <- c('Number', 'Lastname', 'Firstname', 'Year', 'Position', 'GP', 'GS', 'G', 
                            'App', 'Starts', 'ERA', 'IP', 'CG', 'H', 'R', 'ER', 'BB', 'SO', 'SHO', 'BF', 'OAB',
                            '2B', '3B', 'Bk', 'HR', 'WP', 'HB', 'IBB', 'InhRun', 'InhRunScore', 'SHA', 'SFA',
                            'Pitches', 'GO', 'FO', 'W', 'L', 'Saves', 'KL', 'Team')

  myTeam <- myTeam[, -c(6,7,8)]
  rownames(myTeam) <- c(1:nrow(myTeam))
  myTeam <- myTeam[c(37,1:36)]
}

Pitching <- function(codes){
  ##Pitching is a function used to scrape basic pitching data from stats.ncaa.org
  
  ##Pitching takes as an argument the team codes of the teams in your league
  
  ##Pitching returns a dataframe with basic pitching stats
  require(tidyr)
  getSites <- function(codes){
    Sites <- c()
    for(i in codes){
      base <- 'http://stats.ncaa.org/team/'
      Sites <- c(Sites, paste0(base, i, '/stats?id=12560&year_stat_category_id=11001'))}
    return(Sites)
  }
  myTeam <- cbind(as.data.frame(readHTMLTable(getSites(codes)[1])[3]), 
                  rep(teamCodes$School[which(teamCodes$Code == codes[1])], nrow(as.data.frame(readHTMLTable(getSites(codes)[1])[3]))))
  colnames(myTeam) <- (1:ncol(myTeam))
  otherTeams <- NULL
  for(i in 2:length(codes)){
    otherTeam <- (cbind(as.data.frame(readHTMLTable(getSites(codes)[i])[3]), 
                        rep(teamCodes$School[which(teamCodes$Code == codes[i])],
                            nrow(as.data.frame(readHTMLTable(getSites(codes)[i])[3])))))
    colnames(otherTeam) <- (1:ncol(otherTeam))
    otherTeams <- rbind(otherTeams, otherTeam)}
  myPitching <- rbind(myTeam, otherTeams)
  myPitching <- myPitching[which((myPitching$`4`)=='P'),]
  myPitching[,2] <-gsub(',', '', myPitching[,2])
  myPitching[,(2:4)] <- (apply(myPitching[,(2:4)],2, function(x){(as.character(x))}))
  myPitching[,(6:(ncol(myPitching))-1)] <- (apply(myPitching[,(6:(ncol(myPitching))-1)],2, function(x){as.numeric(as.character(x))}))
  myPitching[,1] <- as.character(myPitching[,1])
  myPitching <- extract(myPitching, `2`, c("LastName", "FirstName"), "([^ ]+) (.*)")
  for(i in 1:nrow(myPitching)){
    for(j in 6:ncol(myPitching)){
      if(is.na(myPitching[i,j])==T){
        myPitching[i,j] <- 0}  
    }
  }  
  colnames(myPitching) <- c('Number', 'Lastname', 'Firstname', 'Year', 'Position', 'GP', 'GS', 'G', 
                            'App', 'Starts', 'ERA', 'IP', 'CG', 'H', 'R', 'ER', 'BB', 'SO', 'SHO', 'BF', 'OAB',
                            '2B', '3B', 'Bk', 'HR', 'WP', 'HB', 'IBB', 'InhRun', 'InhRunScore', 'SHA', 'SFA',
                            'Pitches', 'GO', 'FO', 'W', 'L', 'Saves', 'KL', 'Team')
  myPitching <- myPitching[, -c(6,7,8)]
  rownames(myPitching) <- c(1:nrow(myPitching))
  myPitching <- myPitching[c(37,1:36)]
  return(myPitching)
}
pitchUAA <- Pitching(nameCodes(c('Washington-St. Louis', 'Emory', 'CWRU', 'NYU', 'Brandeis'), teamCodes))

##### Working on Pitcher Handedness ######
a <- grep("R:",Plays1$Description)
Plays1$inning <- ''
for(i in 1:nrow(Plays1)){
  if(i%in%a){
    Plays1$inning[i] <- 'change'
  }
}

