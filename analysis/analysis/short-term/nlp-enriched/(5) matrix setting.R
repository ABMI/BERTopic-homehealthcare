# population replace

population2 <- population

matrix <- readRDS("~/storage/14/study/2024/homehealthcare/analysis/original/variable-matrix/base-nlp-matrix.rds")

matrix <- matrix %>% select(person_id, male, drinker, smoker, crp, hct, bun, abga, bleeding, op, tramadol, nausea, dm, heart, secondary, primary, Topic1, Topic2, Topic3, Topic4, Topic5)
nlp_matrix <- matrix%>% select(-person_id)
tfmatrix_nlp <- Matrix(as.matrix(nlp_matrix),sparse=TRUE)
plpData$CovariateMatrixData$data <- tfmatrix_nlp
str(plpData$CovariateMatrixData$data)

plpData$CovariateMatrixData[["data"]]@Dimnames[[2]] <- c('male', 'drinker', 'smoker', 'crp', 'hct', 'bun', 'abga', 'bleeding', 'op', 'tramadol', 'nausea', 'dm', 'heart', 'secondary', 'primary',
                                                         'Topic1', 'Topic2', 'Topic3', 'Topic4', 'Topic5')

# normalise the data:
removeRedundancy <- ifelse("timeId" %in%colnames(plpData$covariateData$covariates), F, T)
plpData$covariateData <- tryCatch({
  suppressWarnings(FeatureExtraction::tidyCovariateData(covariateData=plpData$covariateData, 
                                                        minFraction = minCovariateFraction,
                                                        normalize = normalizeData,
                                                        removeRedundancy = removeRedundancy))
})

#tidyCovariateData	Tidying covariates
Andromeda::createIndex(plpData$covariateData$covariates, c('rowId'),
                       indexName = 'restrict_pop_rowId') # is this needed now?


plpData$covariateData$analysisRef = data.frame(analysisId = c(1,2), analysisName = c("DemographicGender", 'LDA'), domainId = c("Demographics", "Custom"), startDay = c(NA, NA), endDay = c(NA, NA), isBinary = c('Y', 'Y'), missingMeansZero = c(NA, NA))
plpData$covariateData$covariateRef = data.frame(covariateId = c(8507001, 4074035802,4298794802,30204602712,30233142710,30136823712,30165021710,21600960412,4296529504,
                                                                1103314410,21600490412,201820210,316866212,432851212,439392210,
                                                                1002, 2002, 3002, 4002, 5002), 
                                                covariateName = c('male', 'drinker', 'smoker', 'crp', 'hct', 'bun', 'abga', 'bleeding', 'op', 'tramadol', 'nausea', 'dm', 'heart', 'secondary', 'primary',
                                                                  'Topic1', 'Topic2', 'Topic3', 'Topic4', 'Topic5'), 
                                                analysisId = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2), conceptId = c(8507, 4074035,4298794,30204602,30233142,30136823, 30165021, 21600960, 4296529,
                                                                                                             1103314,21600490,201820,316866,432851,439392,
                                                                                                             100, 200, 300, 400, 500))
nlp_ref = plpData$covariateData$covariateRef
CovariateMatrixData <- plpData$CovariateMatrixData

head(CovariateMatrixData$data)
covariateDataFrame  = as.data.frame(as.matrix(CovariateMatrixData$data))

covariateDataFrame$rowId = seq(1:1819)
colnames(covariateDataFrame) = c('male', 'drinker', 'smoker', 'crp', 'hct', 'bun', 'abga', 'bleeding', 'op', 'tramadol', 'nausea', 'dm', 'heart', 'secondary', 'primary',
                                 'Topic1', 'Topic2', 'Topic3', 'Topic4', 'Topic5','rowId')
meltData = melt(covariateDataFrame, id.vars = c("rowId"), measure.vars = c('male', 'drinker', 'smoker', 'crp', 'hct', 'bun', 'abga', 'bleeding', 'op', 'tramadol', 'nausea', 'dm', 'heart', 'secondary', 'primary',
                                                                           'Topic1', 'Topic2', 'Topic3', 'Topic4', 'Topic5'))
meltData = meltData %>% mutate(covariateId = ifelse(variable == 'male', 8507001,
                                                    ifelse(variable == 'drinker',4074035802,
                                                           ifelse(variable == 'smoker', 4298794802,
                                                                  ifelse(variable == 'crp', 30204602712,
                                                                         ifelse(variable == 'hct', 30233142710,
                                                                                ifelse(variable == 'bun', 30136823712,
                                                                                       ifelse(variable == 'abga', 30165021710,
                                                                                              ifelse(variable == 'bleeding', 21600960412,
                                                                                                     ifelse(variable == 'op', 4296529504,
                                                                                                            ifelse(variable == 'tramadol', 1103314410,
                                                                                                                   ifelse(variable == 'nausea', 21600490412,
                                                                                                                          ifelse(variable == 'dm',201820210,
                                                                                                                                 ifelse(variable == 'heart', 316866212,
                                                                                                                                        ifelse(variable == 'secondary', 432851212,
                                                                                                                                               ifelse(variable == 'primary', 439392210,
                                                                                                                                                ifelse(variable == 'Topic1', 1002,
                                                                                                                                                      ifelse(variable == 'Topic2', 2002,
                                                                                                                                                             ifelse(variable == 'Topic3', 3002,
                                                                                                                                                                    ifelse(variable == 'Topic4', 4002,5002))))))))))))))))))))


colnames(meltData) = c("rowId", "variable", "covariateValue","covariateId")
meltData = meltData %>% select (-variable)
meltData = meltData %>% filter(!is.na(covariateValue))
plpData$covariateData$covariates = meltData

