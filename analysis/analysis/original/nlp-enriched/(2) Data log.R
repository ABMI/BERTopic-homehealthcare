# start log:

if(!dir.exists(outputFolder)){dir.create(outputFolder,recursive=T)}
logFileName = paste0(outputFolder,'/plplog.txt')
logger <- ParallelLogger::createLogger(name = "Multple PLP Log",
                                       threshold = verbosity,
                                       appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                           fileName = logFileName)))
ParallelLogger::registerLogger(logger)

if (missing(outcomeIds)){
  stop("Need to specify outcome ids")
}
if (missing(cohortIds)){
  stop("Need to specify cohort ids")
}
if (missing(connectionDetails)){
  stop("Need to specify connectionDetails")
}
if (missing(cdmDatabaseSchema)){
  stop("Need to specify cdmDatabaseSchema")
}
if (missing(cdmDatabaseName)){
  stop("Need to specify cdmDatabaseName - a shareable name for the database")
}
if (missing(modelAnalysisList)){
  stop("Need to specify modelAnalysisList")
}
# check input types
plpDataSettings <- list(connectionDetails = connectionDetails,
                        cdmDatabaseSchema = cdmDatabaseSchema,
                        oracleTempSchema = oracleTempSchema, 
                        cohortDatabaseSchema = cohortDatabaseSchema,
                        cohortTable = cohortTable,
                        outcomeDatabaseSchema = outcomeDatabaseSchema,
                        outcomeTable = outcomeTable,
                        cdmVersion = cdmVersion,
                        firstExposureOnly = F,
                        washoutPeriod = washoutPeriod,
                        sampleSize = maxSampleSize
)

runPlpSettings <- list(minCovariateFraction = minCovariateFraction,
                       normalizeData = normalizeData,
                       testSplit = testSplit,
                       testFraction = testFraction,
                       splitSeed = splitSeed,
                       nfold = nfold,
                       verbosity = verbosity )

if (!dir.exists(outputFolder)){
  dir.create(outputFolder)
}
if (!dir.exists(file.path(outputFolder,'Validation'))){
  dir.create(file.path(outputFolder,'Validation'), recursive = T)
}

analyses <- expand.grid(cohortId = cohortIds,
                        outcomeId = outcomeIds,
                        modelSettingsId = modelAnalysisList$settingLookupTable$lookupId)

# remove rows with same cohortId and outcomeId
removeInd <- analyses$cohortId == analyses$outcomeId
analyses <- analyses[!removeInd, ]

analyses$analysisId <- 1:nrow(analyses)
analyses$devDatabase <- cdmDatabaseName
analyses <- merge(analyses, modelAnalysisList$settingLookupTable, 
                  by.x='modelSettingsId', by.y='lookupId', all.x=T)

# TODO: replace outputFolder with '.' to make relative positions
analyses$plpDataFolder <- file.path(outputFolder,
                                    paste0('PlpData_L',analyses$covariateSettingId,'_T',analyses$cohortId))
analyses$studyPopFile <- file.path(outputFolder,
                                   paste0('StudyPop_L',analyses$covariateSettingId,'_T',analyses$cohortId,'_O',analyses$outcomeId,'_P',analyses$populationSettingId,'.rds'))
analyses$plpResultFolder <- file.path(outputFolder,
                                      paste0('Analysis_',analyses$analysisId))


createPlpReferenceTable <- function(modelAnalysisList,
                                    cohortIds,
                                    outcomeIds,
                                    outputFolder, cdmDatabaseName){
  
  #analysisId, cohortId, outcomeId, settingsFile, plpDataFolder, studyPopFile, plpResultFolder
  
  analyses <- expand.grid(cohortId = cohortIds,
                          outcomeId = outcomeIds,
                          modelSettingsId = modelAnalysisList$settingLookupTable$lookupId)
  # remove rows with same cohortId and outcomeId
  removeInd <- analyses$cohortId == analyses$outcomeId
  analyses <- analyses[!removeInd, ]
  analyses$analysisId <- 1:nrow(analyses)
  analyses$devDatabase <- cdmDatabaseName
  analyses <- merge(analyses, modelAnalysisList$settingLookupTable, 
                    by.x='modelSettingsId', by.y='lookupId', all.x=T)
  
  # TODO: replace outputFolder with '.' to make relative positions
  analyses$plpDataFolder <- file.path(outputFolder,
                                      paste0('PlpData_L',analyses$covariateSettingId,'_T',analyses$cohortId))
  analyses$studyPopFile <- file.path(outputFolder,
                                     paste0('StudyPop_L',analyses$covariateSettingId,'_T',analyses$cohortId,'_O',analyses$outcomeId,'_P',analyses$populationSettingId,'.rds'))
  analyses$plpResultFolder <- file.path(outputFolder,
                                        paste0('Analysis_',analyses$analysisId))
  return(analyses)  
}


#cohorts&outcome reference table 

ParallelLogger::logTrace(paste0('Creating reference table'))
referenceTable <- tryCatch({createPlpReferenceTable(modelAnalysisList,
                                                    cohortIds,
                                                    outcomeIds,
                                                    outputFolder, cdmDatabaseName)},
                           error = function(cont){ParallelLogger::logTrace(paste0('Creating reference table error:', cont)); stop()})
if(!missing(cohortNames)){
  if(!is.null(cohortNames))
    if(length(cohortNames)!=length(cohortIds)){
      stop('cohortNames entered but different length to cohortIds')
    }
  cnames <- data.frame(cohortId=cohortIds, cohortName=cohortNames)            #cohort names  
  referenceTable <- merge(referenceTable, cnames, by='cohortId', all.x=T)
}
if(!missing(outcomeNames)){
  if(!is.null(outcomeNames))
    if(length(outcomeNames)!=length(outcomeIds)){            
      stop('outcomeNames entered but different length to outcomeIds')   #outcome names 
    }
  onames <- data.frame(outcomeId=outcomeIds, outcomeName=outcomeNames)
  referenceTable <- merge(referenceTable, onames, by='outcomeId', all.x=T)
}

# if settings are there restrict to these:
if(!is.null(settings)){
  if(nrow(settings) != 0){
    ParallelLogger::logInfo('Restricting to specified settings...')
    
    # if transpose fix it 
    if(sum(row.names(settings)%in%c('cohortId', 'outcomeId', 'populationSettingId',
                                    'modelSettingId', 'covariateSettingId'))==5){
      settings <- t(settings)
    }
    
    referenceTable <- merge(settings, referenceTable, by = c('cohortId', 
                                                             'outcomeId', 'populationSettingId',
                                                             'modelSettingId', 'covariateSettingId'))
  }
}

if(!file.exists(file.path(outputFolder,'settings.csv'))){
  ParallelLogger::logTrace(paste0('Writing settings csv to ',file.path(outputFolder,'settings.csv') ))
  utils::write.csv(referenceTable,
                   file.path(outputFolder,'settings.csv'), 
                   row.names = F )
}

