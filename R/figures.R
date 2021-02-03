
dataPlot <- function() {
  
  inauguration_2021 = c("#5445b1", "#749dae", "#f3c483", "#5c1a33", "#cd3341","#f7dc6a")
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  OKparticipants <- participants$participant[which(participants$used == TRUE)]
  
  pdf(file='doc/data_plots.pdf', width=8, height=11)
  
  layout(matrix(c(1:6),nrow=3,byrow=FALSE))
  
  for (rotation in c(30,60)) {
    
    for (condition in c('abrupt', 'ramp', 'steps')) {
      
      df <- loadReachData(condition,rotation)
      
      df <- df[which(df$participant %in% OKparticipants),]

      #df <- read.csv(sprintf('data/reachdevs_%s_%d.csv',condition,rotation), stringsAsFactors=F)
      
      participants <- unique(df$participant)
      N <- length(participants)
      
      plot(-1000,-1000,
           main=sprintf('%s %d° (N=%d)',condition,rotation,N),xlab='trial',ylab='reach deviation [°]',
           xlim=c(0,225),ylim=c(-1.25*rotation,1.25*rotation),
           bty='n',ax=F)
      
      if (condition == 'abrupt') {
        lines(c(1,33,      33,     193,        193,        201, 201, 224),
              c(0, 0,rotation,rotation,-1*rotation,-1*rotation,   0,   0),
              col='#AAAAAA')
      } else if (condition == 'ramp') {
        lines(c(1,33,     113,     193,        193,        201, 201, 224),
              c(0, 0,rotation,rotation,-1*rotation,-1*rotation,   0,   0),
              col='#AAAAAA')
      } else {
        lines(c(1,33,   33, 73,73,113,113,153,153,193,                  193,        201, 201, 224),
              c(0, 0,c(.25,.25,.5, .5,.75,.75,  1,  1)*rotation,-1*rotation,-1*rotation,   0,   0),
              col='#AAAAAA')
      }
      
      # plot individual participants:
      for (participant in participants) {
        
        pdf <- df[which(df$participant == participant),]
        lines(pdf$reachdev,col='#FF00FF06')
        
      }
      
      # plot the median reaches across participants
      med_reachdevs <- aggregate(reachdev ~ trialno, data=df, FUN=median)
      lines(med_reachdevs,col='#FF00FF')
      
      # fit a two-rate model using the schedule and median reaches
      schedule <- df$rotation[which(df$participant == participants[1])]
      fit <- SMCL::twoRateFit(schedule       = schedule,
                              reaches        = med_reachdevs$reachdev,
                              gridfits       = 4, 
                              gridpoints     = 9,
                              checkStability = TRUE)
      
      cat(sprintf('%s %d\n',condition,rotation))
      print(fit)
      
      # get the model's prediction
      modelreaches <- SMCL::twoRateModel(par=fit, schedule=schedule)
      
      lines(modelreaches$slow, col='orange')
      lines(modelreaches$fast, col='blue')
      lines(modelreaches$total, col='dark red')
      #print(str(modelreaches))
      
      axis(side=1,at=c(1,33,193,201,224),las=2)
      axis(side=2,at=rotation*c(-1,-.5,0,.5,1),las=1)
      
    }
    
  }
  
  dev.off()
  
}