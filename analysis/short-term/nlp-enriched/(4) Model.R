################################## train the model ##################################################################
#PatientLevelPrediction	MapCovariates	finished MapCovariates#

tempmeta <- attr(population, "metaData")
population <- merge(population, indexes, by = 'rowId')

colnames(population)[colnames(population)=='index'] <- 'indexes'  #change index to indexes 
attr(population, "metaData") <- tempmeta

settings <- list(data=plpData, minCovariateFraction=minCovariateFraction,
                 normalizeData = normalizeData,
                 modelSettings = modelSettings,
                 population=population,
                 cohortId=cohortId,
                 outcomeId=outcomeId)

##save  data formatted sparse matrix? -- Chungsoo Kim
## saveData <- readline(prompt = paste("Do you want to save csv format data used for",analysisId,"training? (Y/N)"))
saveData <- 'Y'
if(tolower(saveData) %in% c('y', 'yes')){saveRDS(PatientLevelPrediction:::toSparseM(plpData, population), file = file.path(analysisPath, "CovariateMatrixData.rds"))}
if(tolower(saveData) %in% c('y', 'yes')){saveRDS(population, file = file.path(analysisPath, "population.rds"))}

ParallelLogger::logInfo(sprintf('Training %s model',settings$modelSettings$name))  
# the call is sinked because of the external calls (Python etc)
if (sink.number()>0){
  ParallelLogger::logWarn(paste0('sink had ',sink.number(),' connections open!'))
}
#sink(logFileName, append = TRUE, split = TRUE)

