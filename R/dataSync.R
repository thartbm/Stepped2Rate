

pullPavlovia <- function() {
  
  tr30 <- system('cd ../tworate30deg/; git pull', intern=TRUE)
  cat(paste0(tr30,'\n'))
  tr60 <- system('cd ../tworate60deg/; git pull', intern=TRUE)
  cat(paste0(tr60,'\n'))
  RAE <- system('cd ../steps/; git pull', intern=TRUE)
  cat(paste0(RAE,'\n'))
  
  return(c('tworate30deg'=tr30,
           'tworate60deg'=tr60,
           'RAE60deg'=RAE))
  
}

library('qualtRics')
library('tibble')

fetchQualtricsData <- function(force_request=F, pullResults=c('','','')) {
  
  survey_ids <- list('tworate30deg'="SV_eaFKe3T9GjtbvKZ", 
                     'tworate60deg'="SV_6rKxITgVme2vQUJ",
                     'RAE60deg'='SV_bDfcK7LDlAitPjE')
  
  for ( rotation in names(survey_ids) ) {
    
    sur_id <- survey_ids[[rotation]]
    
    if (rotation %in% names(pullResults)) {
      if (pullResults[rotation] == "Already up to date.") {
        # no need to sync the Qualtrics questionnaire as there is no new data (with high probability)
        next
      }
    }
    
    data <- fetch_survey(surveyID=sur_id, force_request = force_request)
    
    write.csv(data, file=sprintf('../data/qualtrics_%s.csv',rotation), row.names=F)
    
  }
  
}

downloadData <- function() {
  
  pullResults <- pullPavlovia()
  fetchQualtricsData(force_request = T, pullResults=pullResults)
  
}