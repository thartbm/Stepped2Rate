

pullPavlovia <- function() {
  
  system('cd ../tworate30deg/; git pull')
  system('cd ../tworate60deg/; git pull')
  
}

library('qualtRics')
library('tibble')

fetchQualtricsData <- function(force_request=F) {
  
  
  survey_ids <- list('30'="SV_eaFKe3T9GjtbvKZ", 
                     '60'="SV_6rKxITgVme2vQUJ" )
  
  for ( rotation in names(survey_ids) ) {
    
    sur_id <- survey_ids[[rotation]]
    data <- fetch_survey(surveyID=sur_id, force_request = force_request)
    
    write.csv(data, file=sprintf('../data/qualtrics_tworate%sdeg.csv',rotation), row.names=F)
    
  }
  
}