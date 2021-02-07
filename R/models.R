


getTwoRateFits <- function() {
  
  inauguration_2021 = c("#5445b1", "#749dae", "#f3c483", "#5c1a33", "#cd3341","#f7dc6a")
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  OKparticipants <- participants$participant[which(participants$used == TRUE)]
  
  for (rotation in c(30,60)) {
    
    for (condition in c('abrupt', 'ramp', 'steps')) {
      
      df <- loadReachData(condition,rotation)
      
      df <- df[which(df$participant %in% OKparticipants),]
      
      participants <- unique(df$participant)
      N <- length(participants)
      
      if (file.exists(sprintf('data/models_%s_%d.csv',condition,rotation))) {
        fits <- read.csv(sprintf('data/models_%s_%d.csv',condition,rotation), stringsAsFactors = F)
      } else {
        fits <- NA
      }
      
      schedule <- df$rotation[which(df$participant == participants[1])]
      
      for (participant in participants) {
        if (participant %in% fits$participant) {
          next
        }
        pdf <- df[which(df$participant == participant),]
        print(participant)
        fit <- SMCL::twoRateFit(reaches = as.vector(unlist(pdf$reachdev)),
                                                schedule       = schedule,
                                                gridfits       = 4,
                                                gridpoints     = 9,
                                                checkStability = TRUE)
        
        fit <- as.data.frame(as.list(fit))
        
        fit$participant <- participants
        fit$rotation <- rotation
        fit$condition <- condition
        
        if (is.data.frame(fits)) {
          fits <- rbind(fits, fit)
        } else {
          fits <- fit
        }
        
      }
      
      write.csv(fits, sprintf('data/models_%s_%d.csv',condition,rotation), row.names = F)
      
      
    }
    
  }
  
}


logisticRegressions <- function() {
  
  inauguration_2021 = c("#5445b1", "#749dae", "#f3c483", "#5c1a33", "#cd3341","#f7dc6a")
  
  allFits <- list()
  
  for (rotation in c(30,60)) {
    
    for (condition in c('abrupt', 'ramp', 'steps')) {
      
      fits <- read.csv(sprintf('data/models_%s_%d.csv',condition,rotation), stringsAsFactors = F)
      
      allFits[[sprintf('%s_%d',condition,rotation)]] <- fits
      
    }
    
  }
  
  # compare rotations within condition:
  
  for (condition in c('abrupt', 'ramp', 'steps')) {
    
    df <- NA
    
    for (par_no in c(0,1)) {
      
      rotation <- c(30,60)[par_no+1]
      
      par_df <- allFits[[sprintf('%s_%d',condition,rotation)]]
      
      par_df$parameter <- par_no
      
      if (is.data.frame(df)) {
        df <- rbind(df, par_df)
      } else {
        df <- par_df
      }
      
    }
    
    #print(str(df))
    
    cat(sprintf('\nLOGISTIC REGRESSION: separate 30 and 60 degree rotations in %s schedule\n\n',condition))
    
    logit <- glm( parameter ~ Rs + Ls + Rf + Lf, family=binomial(link="logit"), data=df )
    print(summary(logit))
    
  }
  
  for (rotation in c(30,60)) {
    
    for (alternative in c('ramp','steps')) {
      
      df <- allFits[[sprintf('abrupt_%d',rotation)]]
      df$parameter <- 0
      
      adf <- allFits[[sprintf('%s_%d',alternative,rotation)]]
      adf$parameter <- 1
      
      df <- rbind(df,adf)
      
      cat(sprintf('\nLOGISTIC REGRESSION: separate ABRUPT and %s schedule in %d rotation\n\n',toupper(alternative),rotation))
      
      logit <- glm( parameter ~ Rs + Ls + Rf + Lf, family=binomial(link="logit"), data=df )
      print(summary(logit))
      
    }
    
  }
  
  for (rotation in c(30, 60)) {
    
    dfr <- allFits[[sprintf('ramp_%d',rotation)]]
    dfr$parameter <- 0
    dfs <- allFits[[sprintf('steps_%d',rotation)]]
    dfs$parameter <- 1
    
    df <- rbind(dfr, dfs)
    
    cat(sprintf('\nLOGISTIC REGRESSION: separate RAMP and STEPS schedule in %d rotation\n\n',rotation))
    
    logit <- glm( parameter ~ Rs + Ls + Rf + Lf, family=binomial(link="logit"), data=df )
    print(summary(logit))
    
  }

}
