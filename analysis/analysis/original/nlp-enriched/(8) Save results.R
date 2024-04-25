#PatientLevelPrediction 에서 SaveLoadPlp 돌리는 게 나음

# save the plots?
if(savePlpPlots & !is.null(performance)){
  plotPlp(result = results, filename = file.path(analysisPath))
}


# save the results
source('~/R/x86_64-pc-linux-gnu-library/4.1/PatientLevelPrediction/R/SaveLoadPlp.R', echo=TRUE)

if(savePlpResult){
  ParallelLogger::logInfo(paste0('Saving PlpResult'))
  tryCatch(PatientLevelPrediction::savePlpResult(results, file.path(analysisPath,'plpResult')),
           finally= ParallelLogger::logTrace('Done.'))
  ParallelLogger::logInfo(paste0('plpResult saved to ..\\', analysisPath ,'\\plpResult'))
  
  # update from temp location to saved location
  results$model <- updateModelLocation(results$model, file.path(analysisPath,'plpResult'))
}


ParallelLogger::logInfo(paste0('Log saved to ',logFileName))  
ParallelLogger::logInfo("Run finished successfully.")

# stop logger
ParallelLogger::clearLoggers()
logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                       threshold = "INFO",
                                       appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
ParallelLogger::registerLogger(logger)

saveRDS(plpModel$varImp, file = file.path(analysisPath,  "varImp.rds"))

return(results)

savePlpModel <- function(plpModel, dirPath){
  if (missing(plpModel))
    stop("Must specify plpModel")
  if (missing(dirPath))
    stop("Must specify directory path")
  if (class(plpModel) != "plpModel")
    stop("Not a plpModel")
  
  if(!dir.exists(dirPath)) dir.create(dirPath)
  
  #============================================================
  
  # if deep (keras) then save hdfs
  dirPath <- analysisPath
  saveRDS(plpModel$model, file = file.path(dirPath, "model.rds"))
  
  #saveRDS(plpModel$predict, file = file.path(dirPath, "transform.rds"))
  saveRDS(NULL, file = file.path(dirPath, "transform.rds"))
  saveRDS(plpModel$index, file = file.path(dirPath, "index.rds"))
  saveRDS(plpModel$trainCVAuc, file = file.path(dirPath, "trainCVAuc.rds"))
  saveRDS(plpModel$hyperParamSearch, file = file.path(dirPath, "hyperParamSearch.rds"))
  saveRDS(plpModel$modelSettings, file = file.path(dirPath,  "modelSettings.rds"))
  saveRDS(plpModel$metaData, file = file.path(dirPath, "metaData.rds"))
  saveRDS(plpModel$populationSettings, file = file.path(dirPath, "populationSettings.rds"))
  saveRDS(plpModel$trainingTime, file = file.path(dirPath,  "trainingTime.rds"))
  saveRDS(plpModel$varImp, file = file.path(dirPath,  "varImp.rds"))
  saveRDS(plpModel$dense, file = file.path(dirPath,  "dense.rds"))
  saveRDS(plpModel$cohortId, file = file.path(dirPath,  "cohortId.rds"))
  saveRDS(plpModel$outcomeId, file = file.path(dirPath,  "outcomeId.rds"))
  saveRDS(plpModel$analysisId, file = file.path(dirPath,  "analysisId.rds"))
  #if(!is.null(plpModel$covariateMap))
  saveRDS(plpModel$covariateMap, file = file.path(dirPath,  "covariateMap.rds"))
  
  attributes <- list(type=attr(plpModel, 'type'), predictionType=attr(plpModel, 'predictionType') )
  saveRDS(attributes, file = file.path(dirPath,  "attributes.rds"))
  
  
}


library(pROC)
nlp_prediction <- roc(prediction$outcomeCount, prediction$value, ci = TRUE, smooth = TRUE, boot.n = 1000, 
                      boot.stratified = TRUE)
