minCovariateFraction = runPlpSettings$minCovariateFraction
normalizeData= runPlpSettings$normalizeData
modelSettings = runPlpSettings$modelSettings
testSplit = runPlpSettings$testSplit
testFraction= runPlpSettings$testFraction
trainFraction = NULL
splitSeed = splitSeed
nfold= runPlpSettings$nfold
indexes= NULL
saveDirectory=runPlpSettings$saveDirectory
savePlpData=F
savePlpResult=T
savePlpPlots = F
saveEvaluation = F
timeStamp=FALSE
analysisId=NULL
runCovariateSummary = F
#runCovariateSummary = T
save=NULL
##################################################################
if(!missing(save)){
  warning('save has been replaced with saveDirectory - please use this input from now on')
  if(is.null(saveDirectory)){saveDirectory <- save}
}

if(missing(verbosity)){
  verbosity <- "INFO"
} else{
  if(!verbosity%in%c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR", "NONE")){
    stop('Incorrect verbosity string')
  }
}
##################################################################
# log the start time:
ExecutionDateTime <- Sys.time()

# create an analysisid and folder to save the results
start.all <- Sys.time()
if(is.null(analysisId))
  analysisId <- gsub(':','',gsub('-','',gsub(' ','',start.all)))

if(is.null(saveDirectory)){
  analysisPath <- file.path(getwd(),analysisId)
} else {
  analysisPath <- file.path(saveDirectory,analysisId) 
}

if(verbosity!="NONE"){
  if(!dir.exists(analysisPath)){dir.create(analysisPath,recursive=T)}
}
logFileName = paste0(analysisPath,'/plplog.txt')

# write log to both console and file (tee). 
# note other appenders can be created, e.g., to webservice or database!
if(verbosity!="NONE"){
  logger <- ParallelLogger::createLogger(name = "PLP Log",
                                         threshold = verbosity,
                                         appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                             fileName = logFileName)))
  ParallelLogger::registerLogger(logger)
}

ParallelLogger::logInfo(paste0('Patient-Level Prediction Package version ', utils::packageVersion("PatientLevelPrediction")))

# get ids
cohortId <- attr(population, "metaData")$cohortId
outcomeId <- attr(population, "metaData")$outcomeId

# add header to analysis log
ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
ParallelLogger::logInfo(sprintf('%-20s%s', 'CohortID: ', cohortId))
ParallelLogger::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
ParallelLogger::logInfo(sprintf('%-20s%s', 'Cohort size: ', nrow(plpData$cohorts)))
ParallelLogger::logInfo(sprintf('%-20s%s', 'Covariates: ', nrow(plpData$covariateData$covariateRef)))
ParallelLogger::logInfo(sprintf('%-20s%s', 'Population size: ', nrow(population)))
ParallelLogger::logInfo(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount>0)))


source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/ParamChecks.R', echo=TRUE)
#source('~/PatientLevelPrediction/R/ParamChecks.R', echo=TRUE)

# check parameters
ParallelLogger::logTrace('Parameter Check Started')
ParallelLogger::logDebug(paste0('testSplit: ', testSplit))
checkInStringVector(testSplit, c('person','time', 'stratified','subject'))
ParallelLogger::logDebug(paste0('outcomeCount: ', sum(population[,'outcomeCount']>0)))
checkHigherEqual(sum(population[,'outcomeCount']>0), 25)
ParallelLogger::logDebug(paste0('plpData class: ', class(plpData)))
checkIsClass(plpData, c('plpData'))
ParallelLogger::logDebug(paste0('testfraction: ', testFraction))
checkIsClass(testFraction, c('numeric','integer'))
checkHigher(testFraction,0)
checkHigher(-1*testFraction,-1)
ParallelLogger::logDebug(paste0('nfold class: ', class(nfold)))
ParallelLogger::logDebug(paste0('nfold: ', nfold))
checkIsClass(nfold, c('numeric','integer'))
checkHigher(nfold, 0)

# if savePlpData
if(savePlpData){
  ParallelLogger::logInfo(sprintf('%-20s%s', 'Saving plpData to ', file.path(analysisPath,'plpData')))
  savePlpData(plpData, file.path(analysisPath,'plpData'))
}


source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/DataSplitting.R', echo=TRUE)

# construct the settings for the model pipeline
if(is.null(indexes)){
  if(testSplit=='stratified'){
    ParallelLogger::logTrace('Dataset stratified split started')
    if(is.null(splitSeed)){ #keep record of splitSeed
      splitSeed <- sample(20000000,1)-10000000
      ParallelLogger::logInfo(paste0('splitSeed: ', splitSeed))
    } 
    indexes <-tryCatch(randomSplitter(population, test=testFraction, train = trainFraction, nfold=nfold, seed=splitSeed),
                       finally=ParallelLogger::logTrace('Done.'))
  }
  if(testSplit=='subject'){
    ParallelLogger::logTrace('Dataset subject split started')
    if(is.null(splitSeed)){ #keep record of splitSeed
      splitSeed <- sample(20000000,1)-10000000
      ParallelLogger::logInfo(paste0('splitSeed: ', splitSeed))
    } 
    indexes <-tryCatch(subjectSplitter(population, test=testFraction, train = trainFraction, nfold=nfold, seed=splitSeed),
                       finally=ParallelLogger::logTrace('Done.'))
  }
  if(testSplit=='time'){
    ParallelLogger::logTrace('Dataset time split started')
    indexes <-tryCatch(timeSplitter(population, test=testFraction, train = trainFraction, nfold=nfold),
                       finally=ParallelLogger::logTrace('Done.'))
  }
  if(testSplit=='person'){
    ParallelLogger::logTrace('Dataset person split started')
    if(is.null(splitSeed)){ #keep record of splitSeed
      splitSeed <- sample(20000000,1)-10000000
      ParallelLogger::logInfo(paste0('splitSeed: ', splitSeed))
    } 
    indexes <- tryCatch(personSplitter(population, test=testFraction, train = trainFraction, nfold=nfold, seed=splitSeed),
                        finally= ParallelLogger::logTrace('Done.')
    )
  }
}

if(nrow(population)!=nrow(indexes)){
  ParallelLogger::logError(sprintf('Population dimension not compatible with indexes: %d <-> %d', nrow(population), nrow(indexes)))
  stop('Population dimension not compatible with indexes')
}

