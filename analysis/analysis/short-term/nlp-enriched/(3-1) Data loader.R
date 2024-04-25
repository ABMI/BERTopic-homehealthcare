########################load plpData #############################

# for(i in 1:nrow(referenceTable)){
#i <- 1

i <- 1

plpDataFolder <- referenceTable$plpDataFolder[i]
if(!dir.exists(plpDataFolder)){
  ParallelLogger::logTrace(paste0('Running setting ', i ))
  
  oind <- referenceTable$cohortId==referenceTable$cohortId[i] & 
    referenceTable$covariateSettingId==referenceTable$covariateSettingId[i]
  outcomeIds <- unique(referenceTable$outcomeId[oind])
  
  plpDataSettings$cohortId <- referenceTable$cohortId[i]
  plpDataSettings$outcomeIds <- outcomeIds 
  plpDataSettings$covariateSettings <- modelAnalysisList$covariateSettings[[referenceTable$covariateSettingId[i]]]
  plpData <- tryCatch(do.call(PatientLevelPrediction::getPlpData, plpDataSettings),
                      finally= ParallelLogger::logTrace('Done plpData.'),
                      error= function(cond){ParallelLogger::logInfo(paste0('Error with getPlpData:',cond));return(NULL)})
  
  if(!is.null(plpData)){
    
    ParallelLogger::logTrace(paste0('Saving data in setting ', i ))
    PatientLevelPrediction:::savePlpData(plpData, referenceTable$plpDataFolder[i])
    #plpData <- loadPlpData(referenceTable$plpDataFolder[i])
  } else{
    ParallelLogger::logInfo('No plpData - probably empty cohort issue')
  }
} else{
  ParallelLogger::logTrace(paste0('Loading data in setting ', i ))
  plpData <- PatientLevelPrediction::loadPlpData(referenceTable$plpDataFolder[i])
}

if(!file.exists(referenceTable$studyPopFile[i])){#studyPop[i])){
  ParallelLogger::logTrace(paste0('Setting population settings for setting ', i ))
  # get pop and save to referenceTable$popFile
  popSettings <- modelAnalysisList$populationSettings[[referenceTable$populationSettingId[i]]]
  popSettings$outcomeId <- referenceTable$outcomeId[i] 
  popSettings$plpData <- plpData
  population <- tryCatch(do.call(PatientLevelPrediction:::createStudyPopulation, popSettings),
                         finally= ParallelLogger::logTrace('Done pop.'), 
                         error= function(cond){ParallelLogger::logTrace(paste0('Error with pop:',cond));return(NULL)})
  if(!is.null(population)){
    ParallelLogger::logTrace(paste0('Saving population for setting ', i ))
    saveRDS(population, referenceTable$studyPopFile[i])#studyPop[i])
  }
} else{
  ParallelLogger::logTrace(paste0('Loading population for setting', i ))
  population <- readRDS(referenceTable$studyPopFile[i])#studyPop[i])
}

plpResultFolder = file.path(referenceTable$plpResultFolder[i],'plpResult')

if(!dir.exists(plpResultFolder) && !onlyFetchData){
  ParallelLogger::logTrace(paste0('Running runPlp for setting ', i ))
  dir.create(referenceTable$plpResultFolder[i], recursive = T)
  # runPlp and save result to referenceTable$plpResultFolder
  runPlpSettings$modelSettings <- modelAnalysisList$models[[referenceTable$modelSettingId[i]]]
  runPlpSettings$plpData <- plpData
  runPlpSettings$population <- population
  runPlpSettings$saveDirectory <- gsub(paste0('/Analysis_',referenceTable$analysisId[i]),'',referenceTable$plpResultFolder[i])
  runPlpSettings$analysisId <- paste0('Analysis_',referenceTable$analysisId[i])
  runPlpSettings$savePlpData <- F
  runPlpSettings$savePlpResult <- T
  runPlpSettings$savePlpPlots <- F
  runPlpSettings$saveEvaluation <- F
  # result <- tryCatch(do.call(runPlp, runPlpSettings),
  #                    finally= ParallelLogger::logTrace('Done runPlp.'), 
  #                    error= function(cond){ParallelLogger::logTrace(paste0('Error with runPlp:',cond));return(NULL)})
}



