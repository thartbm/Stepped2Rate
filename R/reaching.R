
checkLearning <- function(ppData, percentage=0.66666) {
  
  ppData <- ppData[-which(ppData$phase == ''),]
  ppData <- ppData[-c(1:24),]
  
  taskVer <- ppData$taskVer[1]
  
  # 0: abrupt
  # 1: ramp
  # 2: step
  # 3: abrupt
  # 4: ramp
  # 5: step
  
  condition <- c('abrupt','ramp','steps','abrupt','ramp','steps')[taskVer+1]
  rotation <- max(abs(ppData$rotation),na.rm=TRUE)
  flip = FALSE
  if (rev(which(ppData$rotation == rotation))[1] == 192) { flip = TRUE }
  
  # now we can check if the data has already been pre-processed:
  if (file.exists(sprintf('data/reachdevs_%s_%d.csv',condition,rotation))) {
    reachdevs <- read.csv(sprintf('data/reachdevs_%s_%d.csv',condition,rotation),stringsAsFactors = F)
    participant <- ppData$participant[1]
    if (participant %in% unique(reachdevs$participant)) {
      preachdevs <- reachdevs[which(reachdevs$participant == participant),]
    } else {
      preachdevs <- getReachDevs(ppData, condition, rotation, flip)
    }
  } else {
    preachdevs <- getReachDevs(ppData, conditionname=condition, maxrotation=rotation, flip)
  }
  
  #print(ppData$participant[1])
  #print(preachdevs$reachdev)
  
  if (mean(preachdevs$reachdev[173:192], na.rm=T)/rotation >= percentage) {
    use = TRUE
  } else {
    use = FALSE
  }

  return(use)
  
}

getReachDevs <- function(ppData, conditionname, maxrotation, flip) {
  
  ppname <- ppData$participant[1]
  print(ppname)
  participant <- c()
  trialno <- c()
  rotation <- c()
  target <- c()
  reachdev <- c()
  
  for (rown in c(1:dim(ppData)[1])) {
    
    participant <- c(participant, ppname)
    trialno <- c(trialno, rown)
    rotation <- c(rotation, ppData$rotation[rown])
    target <- c(target, ppData$targetangle_deg[rown])
    
    X <- convertCellToNumVector(ppData$cursorx_rel[rown])
    Y <- convertCellToNumVector(ppData$cursory_rel[rown])
    step <- convertCellToNumVector(ppData$step[rown])
    
    coords <- rotateTrajectory(X,Y,-1*ppData$targetangle_deg[rown])
    
    X <- coords[which(step==2),1]
    Y <- coords[which(step==2),2]
    # dist <- sqrt(X^2 + Y^2)
    idx <- which(sqrt(X^2 + Y^2) > 0.33333)[1]
    reachdev <- c(reachdev, (atan2(Y[idx],X[idx])/pi)*180)
    
  }
  
  if (flip) {
    reachdev <- -1 * reachdev
    rotation <- -1 * rotation
  }
  
  reachdev <- reachdev - mean(reachdev[17:32])
  
  hi <- rotation
  hi[which(is.na(rotation))] <- max(rotation, na.rm=T)
  # hi <- hi * -1
  # lo <- rotation
  # lo[which(is.na(rotation))] <- min(rotation, na.rm=T)
  # lo <- lo * -1
  # # outlier removal?
  # hi <- pmax(hi,rep(0,length(rotation)))
  # lo <- pmin(lo,rep(0,length(rotation)))
  # #print(hi-reachdev)
  # #print(lo-reachdev)
  # reachdev[which( (hi-reachdev) >  30)] <- NA
  # reachdev[which( (lo-reachdev) < -30)] <- NA
  # 
  # print(length(which(is.na(reachdev))))
  
  preachdevs <- data.frame(participant, trialno, rotation, target, reachdev)
  
  filename <- sprintf('data/reachdevs_%s_%d.csv',conditionname,maxrotation)
  
  if (file.exists(filename)) {
    allreachdevs <- read.csv(filename, stringsAsFactors = F)
    allreachdevs <- rbind(allreachdevs, preachdevs)
  } else {
    allreachdevs <- preachdevs
  }
  
  write.csv(allreachdevs, filename, row.names=F)
  
  return(preachdevs)
  
}


convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}

rotateTrajectory <- function(X,Y,angle) {
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix as well
  coordinates <- matrix(data=c(X,Y),ncol=2)
  
  # rotate the coordinates
  Rcoordinates <- coordinates %*% R
  
  # return the rotated reach
  return(Rcoordinates)
  
}


loadReachData <- function(condition,rotation) {
  
  df <- read.csv(sprintf('data/reachdevs_%s_%d.csv',condition,rotation), stringsAsFactors=F)  
  
  participants <- read.csv('data/participants.csv', stringsAsFactors = F)
  
  good_participants <- participants$participant[which(participants$used == TRUE)]
  
  df <- df[which(df$participant %in% good_participants),]
  
  return(df)
  
}


