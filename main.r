# Install rdhs package for management for DHS data
install.packages("rdhs")

# Install devtools package for easily installation and management of other packages from various repositories.
install.packages("devtools")

# Child command to detach current package to another child command.
devtools::install_github("ropensci/rdhs")

# Install this package for performance and access fruitful connection between the repositories
install.packages("microbenchmark")

#Load the package
library(rdhs)

# Thousands of indicators are available from each survey. This component provides you a list of these indicators. 
# Find here: https://api.dhsprogram.com/rest/dhs/indicators?f=html

dhs_indicators(indicatorIds = "CN_NUTS_C_HA2", returnFields=c("IndicatorId", "ShortName"))


# Defined RDHS config file for authentication.
set_rdhs_config()

# Access the parameters. 
dhs_data(countryIds = c("IA"), indicatorIds = "CN_NUTS_C_HA2", surveyYearStart = 1993,
         returnFields=c("Indicator", "SurveyId", "Value", "SurveyYearLabel", "CountryName"))

## call with no arguments to return all characteristics
sc <- dhs_survey_characteristics()

# Grep this parameters
sc[grepl("Child Stunted", sc$SurveyCharacteristicName), ]

## what are the countryIds - we can find that using this API request
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))

## find all the surveys that match the search criteria
survs <- dhs_surveys(surveyCharacteristicIds = 1, countryIds = c("IA"), surveyYearStart = 2020)

## Making an API Request from server
survs <- dhs_surveys(surveyCharacteristicIds = 1,
                     countryIds = c("IA"),
                     surveyType = "DHS",
                     surveyYearStart = 2020)

## Store the fetched data into the datasets
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")

# express the Data table and data frames
str(datasets)

## set up your credentials
set_rdhs_config(email = "<Add mail ID of your DHS portal>",
                project = "<Add your project name>",
                config_path = "~/.rdhs.json",
                data_frame = "data.table::as.data.table",
                cache_path = "~/",
                timeout = 10000,
                global = TRUE)


# the first time this will take a few seconds 
microbenchmark::microbenchmark(dhs_datasets(surveyYearStart = 2020),times = 1)

# after caching, results will be available instantly
microbenchmark::microbenchmark(dhs_datasets(surveyYearStart = 2020),times = 1)


# download datasets and store it in final variable
downloads <- get_datasets(datasets$FileName)

# Connect with the DHS server with login credentials
Sys.getenv("<Add mail ID of your DHS portal>")
Sys.getenv("<Add your project name>")


str(downloads)

# read in first dataset
cdpr <- readRDS(downloads$IAPR42FL)


# View and present the data and store the data at local storage
head(downloads$IAPR42FL)


#################################################################################################################################################
## Reading the data on R Studio

# # read in our dataset
cdpr <- readRDS(downloads$IAPR42FL)
# 
# # 
head(cdpr$hv024)
# # 
# # 
# # # and then the dataset
class(cdpr$hv024)
# 
# 
# # let's look at the variable_names
head(get_variable_labels(cdpr))

# 
# 
# # Children stunted
questions <- search_variable_labels(datasets$FileName, search_terms = "weight")
# 
table(questions$dataset_filename)
# 
# 
# # let's just use the PR files thus
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")
downloads <- get_datasets(datasets$FileName)
# 
# # and grab the questions from this again along with also questions detailing the province
questions <- search_variable_labels(datasets$FileName, search_terms = c("weight"))
# 
# # and now extract the data
extract <- extract_dhs(questions, add_geo = FALSE)
# 
# # what does our extract look like
str(extract)
# 
# 
# 
# # and grab the questions from this now utilising the survey variables
questions <- search_variables(datasets$FileName, variables = c("hv024","hml35"))
# 
# # and now extract the data
extract2 <- extract_dhs(questions, add_geo = FALSE)
# 
# # quick check 
head(extract2$IAPR42FL)
# 
# 
# 
# # and just to prove that hml35 did actually read in okay (there are just lots of NA)
table(extract2$CDPR61FL$hml35,useNA = "always")
# 
# 
# # first let's bind our first extraction, without the hv024
extract_bound <- rbind_labelled(extract)
# 
head(extract_bound)
# 
# 
# # now let's try our second extraction
extract2_bound <- rbind_labelled(extract2)
# 
# 
# # lets try concatenating the hv024
better_bound <- rbind_labelled(extract2, labels = list("hv024"="concatenate"))
# 
head(better_bound$hv024)
# 
# 
# # lets try concatenating the hv024 and providing new labels
better_bound <- rbind_labelled(extract2, 
                               labels = list("hv024"="concatenate"))

# # and our new label
head(better_bound$hv024)
# 
# 
# 
# # download the datasets with the reformat arguments
downloads <- get_datasets(datasets$FileName, reformat=TRUE)
# 
# # grab the questions but specifying the reformat argument
questions <- search_variables(datasets$FileName, variables = c("hv024", "hml35"),
                              reformat=TRUE)
# 
# # and now extract the data
extract3 <- extract_dhs(questions, add_geo = FALSE)
# 
# # group our results
bound_no_labels <- rbind_labelled(extract3)
# 
# # what does our hv024 look like now
class(bound_no_labels$hv024[1])
# 
# 
# 
# # grab the additional variable hv023 and hv024 which have the strata and weights respectively, and hc1 which is the age
questions <- search_variables(datasets$FileName,variables = c("hv005","hv021","hv022","hv023","hv024",
                                                              "hv025","hv214","hml20", "hc1","hml35"))
extraction <- extract_dhs(questions,TRUE)

# # now concatenate the provinces as before and remove missing responses
dat <- rbind_labelled(extraction,labels=list("hv024"="concatenate"))
dat <- dat[-which(dat$hml35==9),] 
# remove missing responses
# 
# # and we are going to compare our extract to the API malaria prevalence by RDT, which is for those between 6 and 59 months
dat <- dat[-which(!dat$hc1>6 & dat$hc1<=60),]
# 
# # create a denominator response for hml35
dat$hml35denom <- as.integer(!is.na(dat$hml35))
dat$bricks <- dat$hv214 %in% c(8,18,5,9,10)
dat$net <- as.logical(dat$hml20)

# # specify the strata and sample weights
dat$strata <- paste0(dat$hv023,dat$DATASET)
dat$hv005 <- dat$hv005/1e6

# # construct a survey design using the survey pacakge
install.packages("survey")
library(survey)

# # construct the sample design and calculate the mean and totals 
des <-  survey::svydesign(~CLUSTER+DATASET, data=dat, weight=~hv005)
results <- cbind(survey::svyby(~hv005,by=~DHSREGNA+DATASET, des, survey::svyciprop,na.rm=TRUE),
                 survey::svyby(~hv024,by=~DHSREGNA+DATASET, des, survey::svytotal,na.rm=TRUE))


results <- results[order(results$DATASET),]

# # grab the same data from the API 
dhs_api_data <- dhs_data(countryIds = c("IA"),indicatorIds = "CN_NUTS_C_HA2",breakdown = "subnational",surveyYearStart = 2013, surveyYearEnd = 2016)
dhs_api_data <- cbind(dhs_api_data$Value,dhs_api_data$DenominatorWeighted,dhs_api_data$CharacteristicLabel, dhs_api_data$SurveyId)
api <- dhs_api_data[!grepl("\\.\\.",dhs_api_data[,3]),] 

# remove subregions included in India
api <- api[order(apply(api[,4:3],1,paste,collapse="")),]

# # bind the results and remove duplicate Region Columns
comparison <- cbind(results[,c(1,3,7)],api[])
names(comparison) <- c("Region","Survey_RDT_Prev","Survey_RDT_Denom","API_RDT_Prev","API_RDT_Denom","API_Regions","SurveyID")
head(comparison[,c(1,2,4,3,5,7)])

# # contsruct our glm using svyglm and specify quasibinomial to handle the na in hml35
summary(svyglm(hml35 ~ DATASET + hv025 + net + bricks, des, family="quasibinomial"))

