library(readxl)
library(dplyr)
library(Hmisc)

pitchSequence <- function(info, IDs, batter, pitcher, atBat, date){
  merge <- merge(IDs, info)
  merge$date <- as.character(merge$date)
  ID <- unique(merge$atBatID[which(merge$batter==batter & merge$pitcher==pitcher & merge$`at bat number`== atBat & merge$date==date)])
  a <- subset(merge, merge$atBatID==ID)
  colors <- c('red', 'blue', 'purple', 'green','orange','orange','orange')
  cols <- c()
  for(i in 1:max(a$`pitch of at bat`)){
     cols[i] <- colors[a$`pitch type`[which(a$`pitch of at bat`==i)]]
  }
  x <- 1
  for(i in 1:max(a$`pitch of at bat`)){
      if(a$`strike/ball`[which(a$`pitch of at bat`==i)]==1){
        plot(x,1,col=cols[i],pch=15,cex=4,axes=FALSE,xlab='',ylab='', xlim=range(0,10),ylim=range(0,2))
      }
      else{
        plot(x,0.7,col=cols[i],pch=15,cex=4,axes=FALSE,xlab='',ylab='', xlim=range(0,10),ylim=range(0,2))
      }
    x <- x + 1
    par(new=T)
  }
  text(x+1, 0.85, a$result[which(a$`pitch of at bat`==max(a$`pitch of at bat`))], cex=2)
  text(c(0,0),c(1,0.7),c('S','B'), cex=1.5, font=4)
  text(0,1.3, pos=4,paste0("P ", capitalize(a$pitcher[which(a$atBatID==a$atBatID[1])]), ";  B ", 
                           capitalize(a$batter[which(a$atBatID==a$atBatID[1])]), ";  AB ", 
                           (a$`at bat number`[which(a$atBatID[1]==a$atBatID[1])]), "; ", 
                           a$date[which(a$atBatID==a$atBatID[1])]))

  legend('bottomright', legend=c('FB', 'CB', 'SL', 'CH', '2S'), fill=c('red', 'blue', 'purple', 'green', 'orange'))
}


plotPitch <- function(teamSheet, teamIDSheet, ID){
  pitchSequence(teamSheet, teamIDSheet, Hid$batter[which(Hid$atBatID==ID)], Hid$pitcher[which(Hid$atBatID==ID)], 
                Hid$`at bat number`[which(Hid$atBatID==ID)], Hid$date[which(Hid$atBatID==ID)])
}
