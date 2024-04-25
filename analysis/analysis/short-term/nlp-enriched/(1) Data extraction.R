# packages to load 
library(Matrix)
library(dplyr)
library(reshape2)

# The folder where the study intermediate and result files will be written:

outputFolder <- "/home/gansujin/storage/14/output/2024/homehealthcare/short-term/nlp-enriched/T04"

# Specify where the temporary files (used by the ff package) will be created:
options(andromedaTempFolder="~/temp")

# Details for connecting to the server:
Sys.setenv(DATABASECONNECTOR_JAR_FOLDER = './Users/new_path')

DATABASECONNECTOR_JAR_FOLDER <- './'
# 535 version 
connectionDetails<-DatabaseConnector::createConnectionDetails (dbms="sql server",
                                                               server="10.5.99.50",
                                                               user="gansujin",
                                                               password="sujin30401@",
                                                               port="1433",
                                                               pathToDriver=DATABASECONNECTOR_JAR_FOLDER)
# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- "CDMPv535_ABMI.dbo"

# Add a sharebale name for the database containing the OMOP CDM data
cdmDatabaseName <- "AUSOM"
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- "cohortDb.dbo"
oracleTempSchema <- NULL
tempEmulationSchema <- NULL

# table name where the cohorts will be generated

cohortTable <- 'sooj_hhc_231229_original_copy_T002'

#=======================
verbosity = "INFO"
cdmVersion = 5
#========================================== Data extraction =====================================================
ParallelLogger::logInfo("Running predictions")
predictionAnalysisListFile <- system.file("settings",
                                          "predictionAnalysisList.json",
                                          package = "readmission")

predictionAnalysisList <- PatientLevelPrediction::loadPredictionAnalysisList(predictionAnalysisListFile)
predictionAnalysisList$connectionDetails = connectionDetails
predictionAnalysisList$cdmDatabaseSchema = cdmDatabaseSchema
predictionAnalysisList$cdmDatabaseName = cdmDatabaseName
predictionAnalysisList$oracleTempSchema = oracleTempSchema
predictionAnalysisList$cohortDatabaseSchema = cohortDatabaseSchema
predictionAnalysisList$cohortTable = cohortTable
predictionAnalysisList$outcomeDatabaseSchema = cohortDatabaseSchema
predictionAnalysisList$outcomeTable = cohortTable
predictionAnalysisList$cdmVersion = cdmVersion
predictionAnalysisList$outputFolder = outputFolder
predictionAnalysisList$verbosity = verbosity

# result <- do.call(PatientLevelPrediction::runPlpAnalyses, predictionAnalysisList)

outcomeDatabaseSchema = predictionAnalysisList$outcomeDatabaseSchema
outcomeTable = predictionAnalysisList$outcomeTable
onlyFetchData = FALSE
modelAnalysisList = predictionAnalysisList$modelAnalysisList
cohortIds = predictionAnalysisList$cohortIds
cohortNames = predictionAnalysisList$cohortNames
outcomeIds = predictionAnalysisList$outcomeIds
outcomeNames = predictionAnalysisList$outcomeNames
washoutPeriod = predictionAnalysisList$washoutPeriod
maxSampleSize = predictionAnalysisList$maxSampleSize
minCovariateFraction = predictionAnalysisList$minCovariateFraction
normalizeData = predictionAnalysisList$normalizeData
testSplit = predictionAnalysisList$testSplit
testFraction = predictionAnalysisList$testFraction
splitSeed = predictionAnalysisList$splitSeed
#splitSeed = -3470507
nfold = predictionAnalysisList$nfold
settings = NULL

