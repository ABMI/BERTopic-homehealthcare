param =modelSettings$param

# check logger
if(length(ParallelLogger::getLoggers())==0){
  logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                         threshold = "INFO",
                                         appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
  ParallelLogger::registerLogger(logger)
}

# check plpData is coo format:
if (!FeatureExtraction::isCovariateData(plpData$covariateData)){
  stop("Needs correct covariateData")
}

metaData <- attr(population2, 'metaData')

if(!is.null(population2$indexes)){
  population <- population2 %>% filter(indexes > 0)
  attr(population2, 'metaData') <- metaData
}


#restrict to pop
if(length(population$rowId)<200000){
  plpData$covariateData <- PatientLevelPrediction:::limitCovariatesToPopulation(plpData$covariateData,
                                                                                population$rowId)
} else{
  plpData$covariateData <- batchRestrict(plpData$covariateData,
                                         data.frame(rowId = population$rowId),
                                         sizeN = 10000000)
}

#TODO - how to incorporate indexes?
#variance <- 0.003
#if(!is.null(param$variance )) variance <- param$variance
variance <- 0.01
includeCovariateIds <- param$includeCovariateIds
lowerLimit <- param$lowerLimit
upperLimit <- param$upperLimit
start <- Sys.time()
noShrinkage <- param$noShrinkage

# add pre-processing details
plpModel$metaData$preprocessSettings <- attr(plpData$covariateData, "metaData")

ParallelLogger::logTrace('Creating prediction function')

source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/Fit.R', echo=TRUE)
#source('~/PatientLevelPrediction/R/Fit.R', echo=TRUE)
plpModel$predict <- createTransform(plpModel)

ParallelLogger::logTrace('Adding index')

plpModel$index <- population$indexes  ##?- dont think we need this, just the seed instead
class(plpModel) <- 'plpModel'

return(plpModel)
model <- plpModel #Train set training 
#############################################################################
model$analysisId <- analysisId # adding this so we can link validation to models

# get train prediction and remove it from model
predictionTrain <- model$predictionTrain
model$predictionTrain <- NULL

# create test subset of population
population2 <- settings$population
populationTest <- population2[population2$index<0,]
attr(populationTest, 'metaData') <- attr(population2, 'metaData')

# calculate metrics
ParallelLogger::logTrace('Prediction')

source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/AndromedaHelperFunctions.R', echo=TRUE)

source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/Predict.R', echo=TRUE)
ParallelLogger::logTrace('predict.plp - predictingProbabilities start')

#probabilities
predictionTest <- PatientLevelPrediction:::predictProbabilities(plpModel$model, populationTest, 
                                                                plpData2$covariateData)
ParallelLogger::logTrace('predict.plp - predictingProbabilities end')

predictionTest2 <- predictionTest %>% select (-outcomeCount)
predictionTrain2 <- predictionTrain %>% select (-outcomeCount)

prediction2 <- rbind(predictionTest2, predictionTrain2)
outcomeCount <- settings$population %>% select (rowId, subjectId, outcomeCount)

prediction <- merge(prediction2, outcomeCount, by = c("rowId", "subjectId"), all.x = T)

FileName = paste0(outputFolder,'/prediction.csv')
write.csv(prediction, file = FileName)

# prediction <- rbind(predictionTest, predictionTrain[,colnames(predictionTest)])

ParallelLogger::logDebug(paste0('prediction null: ', is.null(prediction)))
ParallelLogger::logDebug(paste0('prediction unique values: ', length(unique(prediction$value))))
if(ifelse(is.null(prediction), FALSE, length(unique(prediction$value))>1)){
  
  # add analysisID
  attr(prediction, "metaData")$analysisId <- analysisId
  
  source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/EvaluatePlp.R', echo=TRUE)
  
  ParallelLogger::logInfo('Train set evaluation')
  attr(prediction, "metaData")$predictionType <- 'binary'
  
  performance.train <- PatientLevelPrediction::evaluatePlp(prediction[prediction$index>0,], plpData)
  ParallelLogger::logTrace('Done.')
  ParallelLogger::logInfo('Test set evaluation')
  
  performance.test <- PatientLevelPrediction::evaluatePlp(prediction[prediction$index<0,], plpData)
  ParallelLogger::logTrace('Done.')
  
  # now combine the test and train data and add analysisId
  
  source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/Formatting.R', echo=TRUE)
  performance <- reformatPerformance(train=performance.train, test=performance.test, analysisId = 'Analysis_1')
  saveEvaluation <- T
  if(saveEvaluation){
    ParallelLogger::logTrace('Saving evaluation csv files')
    if(!dir.exists( file.path(analysisPath, 'evaluation') ))
      dir.create(file.path(analysisPath, 'evaluation'))
    tryCatch(utils::write.csv(performance$evaluationStatistics, file.path(analysisPath, 'evaluation', 'evaluationStatistics.csv'), row.names=F ),
             finally= ParallelLogger::logTrace('Saved EvaluationStatistics.')
    )
    tryCatch(utils::write.csv(performance$thresholdSummary, file.path(analysisPath, 'evaluation', 'thresholdSummary.csv'), row.names=F ),
             finally= ParallelLogger::logTrace('Saved ThresholdSummary.')
    )
    tryCatch(utils::write.csv(performance$demographicSummary, file.path(analysisPath, 'evaluation', 'demographicSummary.csv'), row.names=F),
             finally= ParallelLogger::logTrace('Saved DemographicSummary.')
    )
    tryCatch(utils::write.csv(performance$calibrationSummary, file.path(analysisPath, 'evaluation', 'calibrationSummary.csv'), row.names=F),
             finally= ParallelLogger::logTrace('Saved CalibrationSummary.')
    )
    tryCatch(utils::write.csv(performance$predictionDistribution, file.path(analysisPath, 'evaluation', 'predictionDistribution.csv'), row.names=F),
             finally= ParallelLogger::logTrace('Saved PredictionDistribution.')
    )
  }
  
}else{
  ParallelLogger::logWarn(paste0('Evaluation not possible as prediciton NULL or all the same values'))
  performance.test <- NULL
  performance.train <- NULL
  performance <- NULL
}

# log the end time:
endTime <- Sys.time()
TotalExecutionElapsedTime <- difftime(endTime, ExecutionDateTime, units='mins')

# 1) input settings:
inputSetting <- list(dataExtrractionSettings=plpData$metaData$call,
                     populationSettings=attr(population, "metaData"),
                     modelSettings = modelSettings,
                     testSplit = testSplit, 
                     testFraction= testFraction,
                     nfold=nfold,
                     splitSeed = splitSeed)

# 2) Executionsummary details:
executionSummary <- list(PackageVersion = list(rVersion= R.Version()$version.string,
                                               packageVersion = utils::packageVersion("PatientLevelPrediction")),
                         PlatformDetails= list(platform= R.Version()$platform,
                                               cores= Sys.getenv('NUMBER_OF_PROCESSORS'),
                                               RAM=benchmarkme::get_ram()),
                         # Sys.info()
                         TotalExecutionElapsedTime = TotalExecutionElapsedTime,
                         ExecutionDateTime = ExecutionDateTime,
                         Log = logFileName # location for now
                         #Not available at the moment: CDM_SOURCE -  meta-data containing CDM version, release date, vocabulary version
)

population =population2

if(runCovariateSummary){
  ParallelLogger::logInfo(paste0('Calculating covariate summary @ ', Sys.time()))
  ParallelLogger::logInfo('This can take a while...')
  
  source('~/storage/PatientLevelPrediction/R/RunPlp.R', echo=TRUE)
  #source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/RunPlp.R', echo=TRUE)
  covSummary <- covariateSummary(plpData, population, model)
  
  if(saveEvaluation){
    ParallelLogger::logTrace('Saving covariate summary as csv')
    if(!dir.exists( file.path(analysisPath, 'evaluation') ))
      dir.create(file.path(analysisPath, 'evaluation'))
    tryCatch(utils::write.csv(covSummary, file.path(analysisPath, 'evaluation', 'covariateSummary.csv'), row.names=F ),
             finally= ParallelLogger::logTrace('Saved covariate summary.')
    )
  }
  ParallelLogger::logInfo(paste0('Finished covariate summary @ ', Sys.time()))
  
} else{
  ParallelLogger::logInfo('Skipping covariate summary')
  covSummary <- NULL
}



results <- list(inputSetting=inputSetting,
                executionSummary=executionSummary,
                model=model,
                prediction=prediction,
                performanceEvaluation=performance,
                #covariateSummary=covSummary,
                analysisRef=list(analysisId=analysisId,
                                 analysisName = c("DemographicGender", 'LDA'),
                                 analysisSettings = NULL))
class(results) <- c('runPlp')

