args = commandArgs(trailingOnly=TRUE)


sequence <- function(file){
    library(readxl)
    library(tidyr)
    library(dplyr)
    library(R.utils)
    library(tibble)
    info <- read_excel(file)
    IDs <- read_excel(file, sheet=2)
    IDs$date <- as.character(IDs$date)
    IDs <- arrange(IDs, desc(date))
    IDs <- IDs[,c(1:5)]
    
    
    pitchSequence <- function(info, IDs, batter, pitcher, atBat, date){
      merge <- merge(IDs, info)
      merge$date <- as.character(merge$date)
      ID <- unique(merge$atBatID[which(merge$batter==batter & merge$pitcher==pitcher & merge$`at bat number`== atBat & merge$date==date)])
      a <- subset(merge, merge$atBatID==ID)
      colors <- c('black', 'yellow', 'darkgreen', 'red','orange','dodgerblue3','dodgerblue3')
      cols <- c()
      for(i in 1:max(a$`pitch of at bat`)){
        cols[i] <- colors[a$`pitch type`[which(a$`pitch of at bat`==i)]]
      }
      x <- 1
      i <- 1
      if(max(a$`pitch of at bat`)<=7){
        for(i in 1:max(a$`pitch of at bat`)){
          if(a$`strike/ball`[which(a$`pitch of at bat`==i)]==0){
            plot(x,1,col=cols[i],pch=15,cex=4,axes=FALSE,xlab='',ylab='', xlim=range(0,10),ylim=range(0,2))
          }else{
            plot(x,0.7,col=cols[i],pch=15,cex=4,axes=FALSE,xlab='',ylab='', xlim=range(0,10),ylim=range(0,2))
          }
          x <- x + 1
          par(new=T)
        }
        text(-.5,1.3, pos=4,paste0("P ", capitalize(a$pitcher[which(a$atBatID==a$atBatID[1])]), ";  B ", 
                                   capitalize(a$batter[which(a$atBatID==a$atBatID[1])]), ";  AB ", 
                                   (a$`at bat number`[which(a$atBatID[1]==a$atBatID[1])]), "; ", 
                                   a$date[which(a$atBatID==a$atBatID[1])]))
        
      }
      else{
        for(i in 1:7){
          if(a$`strike/ball`[which(a$`pitch of at bat`==i)]==0){
            plot(x,1,col=cols[i],pch=15,cex=4,axes=FALSE,xlab='',ylab='', xlim=range(0,10),ylim=range(0,2))
          }else{
            plot(x,0.7,col=cols[i],pch=15,cex=4,axes=FALSE,xlab='',ylab='', xlim=range(0,10),ylim=range(0,2))
          }
          x <- x + 1
          par(new=T)
        }
        text(-.5,1.3, pos=4,paste0("P ", capitalize(a$pitcher[which(a$atBatID==a$atBatID[1])]), ";  B ", 
                                   capitalize(a$batter[which(a$atBatID==a$atBatID[1])]), ";  AB ", 
                                   (a$`at bat number`[which(a$atBatID[1]==a$atBatID[1])]), "; ", 
                                   a$date[which(a$atBatID==a$atBatID[1])], '; (', (max(a$`pitch of at bat`)), 'p)'), cex=.9)
      }
      for(i in 1:8){
        lines(c(i-0.5, i-0.5), c(1.2,0.5))
      }
      lines(c(-.39, -.39), c(1.4,0.5))
      lines(c(-0.5,7.5), c(0.85,0.85))
      lines(c(-0.39,9.5), c(1.4,1.4))
      lines(c(-.39,9.5), c(0.5,0.5))
      lines(c(-.5,9.5), c(1.2,1.2))
      lines(c(9.5,9.5),c(0.5,1.4))
      text(8.5, 0.85, a$result[which(a$`pitch of at bat`==max(a$`pitch of at bat`))], cex=2)
      text(c(0,0),c(1,0.7),c('B','S'), cex=1.5, font=4)
      #legend('bottomright', legend=c('FB', '2S', 'CB', 'S', 'CH', 'CU'), fill=c('black', 'dodgerblue3', 'yellow', 'darkgreen', 'red', 'orange'))
    }
    
    plotPitch <- function(teamSheet, teamIDSheet, ID){
      pitchSequence(teamSheet, teamIDSheet, teamIDSheet$batter[which(teamIDSheet$atBatID==ID)], teamIDSheet$pitcher[which(teamIDSheet$atBatID==ID)], 
                    teamIDSheet$`at bat number`[which(teamIDSheet$atBatID==ID)], teamIDSheet$date[which(teamIDSheet$atBatID==ID)])
    }
  setwd("/home/kdorian/creative/app/static")
  pdf(file = paste0('sequence.pdf'), height = 11, width = 8.5)
  for(i in 1:length(unique(IDs$batter))){
    atBats <- unique(IDs$atBatID[which(IDs$batter==unique(IDs$batter)[i])])
    if(length(atBats) > 13){
      atBats <- atBats[1:13]
    }
    par(mfrow=c(max(ceiling((length(atBats)+1)/2), 5),2), mar=c(.3,1,.01,.01))
    for(j in 1:length(atBats)){
      plotPitch(info, IDs, atBats[j])
      print(i)
    }
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
    legend('left', legend =c('FB', '2S', 'CB',
                             'SL', 'CH', 'CU'), pch=15, pt.cex = 3, cex=1.3, ncol = 3,
           col = c('black', 'dodgerblue3', 'yellow', 'darkgreen', 'red', 'orange'))
  }
  dev.off()
}
sequence(args[1])
