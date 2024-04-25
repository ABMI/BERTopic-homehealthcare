# removeRedundancy <- ifelse("timeId" %in%colnames(plpData$covariateData$covariates), F, T)
plpData$covariateData <- tryCatch({
  suppressWarnings(FeatureExtraction::tidyCovariateData(covariateData=plpData$covariateData, 
                                                        minFraction = minCovariateFraction,
                                                        normalize = normalizeData,
                                                        removeRedundancy = removeRedundancy))
})
population2 = population %>% filter(rowId %in% as.data.frame(plpData$covariateData$covariates)$rowId)

plpData2 <- plpData
##################################################################
#2022-09-23 15:22:41	[Main thread]	INFO	PatientLevelPrediction	limitCovariatesToPopulation	Starting to limit covariate data to population...
# Now apply the classifier:
# fun <- modelSettings$model
fun = PatientLevelPrediction:::fitLassoLogisticRegression

args <- list(plpData =plpData2,param =modelSettings$param, 
             population=population2, cohortId=cohortId, outcomeId=outcomeId)
plpModel <- do.call(fun, args)

count(plpModel$predictionTrain, value)
plpModel$varImp #covariateValue 는 seed 고정 안해서 계속 바뀜. 완성 후 고정하기  

