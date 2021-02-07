
selectPavloviaParticipants <- function(rotation_groups=list(30,60)) {
  
  if (file.exists('data/pavlovia_participants.csv')) {
    participants <- read.csv('data/pavlovia_participants.csv',stringsAsFactors = F)
    
    participant <- as.character(participants$participant)
    timestamp <- as.character(participants$timestamp)
    OS <- as.character(participants$OS)
    frameRate <- participants$frameRate
    ttotal <- participants$ttotal
    condition <- participants$condition
    rotation <- participants$rotation
    used <- participants$used
    
  } else {
    
    participant <- c()
    timestamp <- c()
    OS <- c()
    frameRate <- c()
    ttotal <- c()
    condition <- c()
    rotation <- c()
    used <- c()
    
  }

  for (rot_group in rotation_groups) {
    
    csv_files <- list.files(sprintf('../tworate%ddeg/data/',rot_group), pattern='*.csv')
    
    for (csv_file in csv_files) {
      
      #cat(sprintf('file: %s', csv_file))
      
      # analyse string to get participant ID and timestamp
      Apos <- gregexpr(pattern=sprintf('_tworate%ddeg_',rot_group), csv_file)[[1]][1]
      pp <- substr(csv_file, 1, Apos-1)
      ts <- substr(csv_file, Apos+14, nchar(csv_file)-4)
      
      # if we already have their data, don't add them again...
      if (pp %in% participant) {
        #cat('  participant known\n')
        next
      }
      
      #cat('  participant unknown')
      filename <- sprintf('../tworate%ddeg/data/%s', rot_group, csv_file)
      
      if (!file.exists(filename)) {
        #cat('  file no longer exists...\n')
        next
      }
      
      # check how many trials in file? remove if not 
      csv_lines <- readLines(filename)
      if (length(csv_lines) < 250) {
        #cat('  task not finished\n')
        participant <- c(participant, pp)
        timestamp <- c(timestamp, ts)
        OS <- c(OS, NA)
        frameRate <- c(frameRate, NA)
        ttotal <- c(ttotal, NA)
        condition <- c(condition, NA)
        rotation <- c(rotation, rot_group)
        used <- c(used, FALSE)
        next
      }
      
      ppData <- read.csv(filename, stringsAsFactors = F)
      
      OK <- checkLearning(ppData, percentage=0.66666, window=45)
      
      participant <- c(participant, pp)
      timestamp <- c(timestamp, ts)
      OS <- c(OS, ppData$OS[1])
      frameRate <- c(frameRate, ppData$frameRate[1])
      ttotal <- c(ttotal, ppData$cumulativetime[249])
      condition <- c(condition, ppData$taskVer[1])
      rotation <- c(rotation, rot_group)
      used <- c(used, OK)
      
    }
    
  }
  
  pavlovia <- data.frame(participant,timestamp,OS,frameRate,ttotal,condition,rotation,used)
  
  write.csv(pavlovia, 
            'data/pavlovia_participants.csv',
            row.names=F)
  
  return(pavlovia)
  
}


selectPavloviaRAEParticipants <- function() {
  
  if (file.exists('data/pavlovia_RAE_participants.csv')) {
    participants <- read.csv('data/pavlovia_RAE_participants.csv',stringsAsFactors = F)
    
    participant <- as.character(participants$participant)
    timestamp <- as.character(participants$timestamp)
    OS <- as.character(participants$OS)
    frameRate <- participants$frameRate
    ttotal <- participants$ttotal
    condition <- participants$condition
    rotation <- participants$rotation
    used <- participants$used
    
  } else {
    
    participant <- c()
    timestamp <- c()
    OS <- c()
    frameRate <- c()
    ttotal <- c()
    condition <- c()
    rotation <- c()
    used <- c()
    
  }
  
  csv_files <- list.files('../steps/data/', pattern='*.csv')
  
  for (csv_file in csv_files) {
    
    #cat(sprintf('file: %s', csv_file))
    
    # analyse string to get participant ID and timestamp
    Apos <- gregexpr(pattern=sprintf('_steps_'), csv_file)[[1]][1]
    pp <- substr(csv_file, 1, Apos-1)
    ts <- substr(csv_file, Apos+7, nchar(csv_file)-4)
    
    # if we already have their data, don't add them again...
    if (pp %in% participant) {
      #cat('  participant known\n')
      next
    }
    
    #cat('  participant unknown')
    filename <- sprintf('../steps/data/%s', csv_file)
    
    if (!file.exists(filename)) {
      #cat('  file no longer exists...\n')
      next
    }
    
    # check how many trials in file? remove if not
    csv_lines <- readLines(filename)
    if (length(csv_lines) < 305) {
      #cat('  task not finished\n')
      participant <- c(participant, pp)
      timestamp <- c(timestamp, ts)
      OS <- c(OS, NA)
      frameRate <- c(frameRate, NA)
      ttotal <- c(ttotal, NA)
      condition <- c(condition, NA)
      rotation <- c(rotation, NA)
      used <- c(used, FALSE)
      next
    }
    
    ppData <- read.csv(filename, stringsAsFactors = F)
    
    OK <- checkLearningRAE(ppData, percentage=0.66666, window=45)
    
    participant <- c(participant, pp)
    timestamp <- c(timestamp, ts)
    OS <- c(OS, ppData$OS[1])
    frameRate <- c(frameRate, ppData$frameRate[1])
    ttotal <- c(ttotal, ppData$cumulativetime[304])
    condition <- c(condition, ppData$condition[1])
    rotation <- c(rotation, 60)
    used <- c(used, OK)
    
  }
  
  # print(length(participant))
  # print(length(timestamp))
  # print(length(OS))
  # print(length(frameRate))
  # print(length(ttotal))
  # print(length(condition))
  # print(length(rotation))
  # print(length(used))
  
  pavlovia <- data.frame(participant,timestamp,OS,frameRate,ttotal,condition,rotation,used)
  
  write.csv(pavlovia, 
            'data/pavlovia_RAE_participants.csv',
            row.names=F)
  
  return(pavlovia)
  
}

selectQualtricsParticipants <- function(rotation_groups=list(30,60)) {
  
  qualtrics <- NA
  
  for (rot_group in rotation_groups) {
    
    qualtric <- read.csv(sprintf('../data/qualtrics_tworate%ddeg.csv',rot_group), stringsAsFactors = F)
    
    removecolumns <- c('Status', 'IPAddress', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 
                       'ExternalReference', 'LocationLatitude', 'LocationLongitude', 
                       'DistributionChannel', 'UserLanguage',
                       'FL_10_DO_FL_20',	'FL_10_DO_FL_21',	'FL_10_DO_FL_17',	'FL_10_DO_FL_48',	'FL_10_DO_FL_49',	'FL_10_DO_FL_50', 'FL_10_DO_FL_18', 'FL_10_DO_FL_33', 'FL_10_DO_FL_34')
                       
    qualtric <- qualtric[,-which(names(qualtric) %in% removecolumns)]
    
    columns <- list('Q1'  = 'consent',
                    'Q2'  = 'age',
                    'Q3'  = 'sex',
                    'Q4'  = 'bodylength_cm',
                    'Q6'  = 'handedness',
                    'Q8'  = 'device',
                    'Q9'  = 'cursorcontrol',
                    'Q5'  = 'needvisioncorrection',
                    'Q7'  = 'wearvisioncorrection',
                    'Q11' = 'usedhand',
                    'Q19' = 'reappearingcursor',
                    'Q10' = 'comments')
    
    for (cl in names(columns)) {
      names(qualtric)[which(names(qualtric) == cl)] <- columns[[cl]]
    }
    
    initialrows <- dim(qualtric)[1]
    
    # only participants who finished the experiment:]
    qualtric <- qualtric[which(qualtric$Finished == TRUE),]
    
    # consent = given
    qualtric <- qualtric[which(qualtric$consent == 'I agree to participate in the study'),]
    
    # remove NO on wearing corrective device
    if (length(which(qualtric$wearvisioncorrection == 'No')) > 0) {
      qualtric <- qualtric[-which(qualtric$wearvisioncorrection == 'No'),]
    }
    
    # remove Touchscreen
    if (length(which(qualtric$cursorcontrol == 'Touchscreen')) > 0) {
      qualtric <- qualtric[-which(qualtric$cursorcontrol == 'Touchscreen'),]
    }
    
    # remove switched between hands + unanswered
    qualtric <- qualtric[which(qualtric$usedhand %in% c('Right','Left')),]
    
    finalrows <- dim(qualtric)[1]
    
    cat(sprintf('Qualtrics: removed %d out of %d participants in the %d condition\n',(initialrows-finalrows),initialrows,rot_group))
    
    if (is.data.frame(qualtrics)) {
      qualtrics <- rbind(qualtrics, qualtric)
    } else {
      qualtrics <- qualtric
    }
    
  }
  
  return(qualtrics)
  
}

selectQualtricsRAEParticipants <- function() {
  
  qualtrics <- NA
  
  qualtric <- read.csv(sprintf('../data/qualtrics_RAE60deg.csv'), stringsAsFactors = F)
  
  removecolumns <- c('Status', 'IPAddress', 'ResponseId', 'RecipientLastName', 'RecipientFirstName', 'RecipientEmail', 
                     'ExternalReference', 'LocationLatitude', 'LocationLongitude', 
                     'DistributionChannel', 'UserLanguage',
                     'FL_10_DO_FL_20', 'FL_10_DO_FL_21', 'FL_10_DO_FL_17', 'FL_10_DO_FL_18', 'FL_10_DO_FL_33', 'FL_10_DO_FL_34', 'FL_10_DO_FL_48', 'FL_10_DO_FL_49', 'FL_10_DO_FL_50')
  
  qualtric <- qualtric[,-which(names(qualtric) %in% removecolumns)]
  
  columns <- list('Q1'  = 'consent',
                  'Q2'  = 'age',
                  'Q3'  = 'sex',
                  'Q4'  = 'bodylength_cm',
                  'Q6'  = 'handedness',
                  'Q8'  = 'device',
                  'Q9'  = 'cursorcontrol',
                  'Q5'  = 'needvisioncorrection',
                  'Q7'  = 'wearvisioncorrection',
                  'Q11' = 'usedhand',
                  'Q19' = 'reappearingcursor',
                  'Q10' = 'comments')
  
  for (cl in names(columns)) {
    names(qualtric)[which(names(qualtric) == cl)] <- columns[[cl]]
  }
  
  initialrows <- dim(qualtric)[1]
  
  # only participants who finished the experiment:]
  qualtric <- qualtric[which(qualtric$Finished == TRUE),]
  
  # consent = given
  qualtric <- qualtric[which(qualtric$consent == 'I agree to participate in the study'),]
  
  # remove NO on wearing corrective device
  if (length(which(qualtric$wearvisioncorrection == 'No')) > 0) {
    qualtric <- qualtric[-which(qualtric$wearvisioncorrection == 'No'),]
  }
  
  # remove Touchscreen
  if (length(which(qualtric$cursorcontrol == 'Touchscreen')) > 0) {
    qualtric <- qualtric[-which(qualtric$cursorcontrol == 'Touchscreen'),]
  }
  
  # remove switched between hands + unanswered
  qualtric <- qualtric[which(qualtric$usedhand %in% c('Right','Left')),]
  
  finalrows <- dim(qualtric)[1]
  
  cat(sprintf('Qualtrics: removed %d out of %d participants in the RAE task\n',(initialrows-finalrows),initialrows))
  
  if (is.data.frame(qualtrics)) {
    qualtrics <- rbind(qualtrics, qualtric)
  } else {
    qualtrics <- qualtric
  }
  
  
  return(qualtrics)
  
}

selectParticipants <- function(rotation_groups=c(30,60)) {
  
  # two-rate paradigm:
  
  PaP <- selectPavloviaParticipants(rotation_groups=rotation_groups)
  QuP <- selectQualtricsParticipants(rotation_groups=rotation_groups)
  
  participants <- merge(PaP,QuP,by.x='participant',by.y='id',all=F)
  
  write.csv(participants,'data/participants.csv',row.names=F)
  
  # reach aftereffects paradigm:
  
  QuRAE <- selectQualtricsRAEParticipants()
  QuRAE <- QuRAE[,-which(names(QuRAE) %in% c('condition'))]
  PaRAE <- selectPavloviaRAEParticipants()
  
  participantsRAE <- merge(PaRAE, QuRAE, by.x='participant', by.y='id', all=F)
  
  write.csv(participantsRAE,'data/participants_RAE.csv',row.names=F)
  
  return(participants)
  
}

# sampleSizes <- function() {
#   
#   participants <- read.csv('data/participants.csv', stringsAsFactors = F)
#   
#   cond <- c('abrupt','ramp','steps','abrupt','ramp','steps')[participants$condition+1]
#   
#   rotation <- c()
#   condition <- c()
#   N <- c()
#   
#   for (rot in c(30,60)) {
#     
#     for (conditionname in c('abrupt','ramp','steps')) {
#       
#       rotation <- c(rotation, rot)
#       condition <- c(condition, conditionname)
#       N <- c(N, length(which(participants$rotation == rot & cond == conditionname)))
#       
#     }
#     
#   }
#   
#   print(data.frame(rotation, condition, N))
#   
#   
# }