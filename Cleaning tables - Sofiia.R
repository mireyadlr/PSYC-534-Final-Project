#####=================== CLEANING DATA =====================#####
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(psych)

# This is the code for cleaning data and combining variables of interest in one dataset
happy2015 <- read.csv('Data/2015.csv', header = TRUE)
happy2016 <- read.csv('Data/2016.csv', header = TRUE)
happy2017 <- read.csv('Data/2017.csv', header = TRUE)
happy2018 <- read.csv('Data/2018.csv', header = TRUE)
happy2019 <- read.csv('Data/2019.csv', header = TRUE)
happy2020 <- read.csv('Data/2020.csv', header = TRUE)
happy2021 <- read.csv('Data/2021.csv', header = TRUE)
happy2022 <- read.csv('Data/2022.csv', header = TRUE)

#####============== Cleaning table for 2015 =================#####
happy2015 <- rename(happy2015,
                    'Score_2015' = 'Happiness.Score',
                    'GDP_2015' = 'Economy..GDP.per.Capita.',
                    'Social_Support_2015' = 'Family',
                    'Corruption_2015' = 'Trust..Government.Corruption.',
                    'Dystopia_Residual_2015' = 'Dystopia.Residual',
                    'Life_Expectancy_2015' = 'Health..Life.Expectancy.',
                    'Freedom_2015' = 'Freedom',
                    'Generosity_2015' = 'Generosity')
happy2015$Standard.Error <- NULL
happy2015$Happiness.Rank <- NULL

#####============== Cleaning table for 2016 =================#####
happy2016 <- rename(happy2016, 
                    'Score_2016' = 'Happiness.Score',
                    'GDP_2016' = 'Economy..GDP.per.Capita.',
                    'Social_Support_2016' = 'Family',
                    'Corruption_2016' = 'Trust..Government.Corruption.',
                    'Dystopia_Residual_2016' = 'Dystopia.Residual',
                    'Life_Expectancy_2016' = 'Health..Life.Expectancy.',
                    'Freedom_2016' = 'Freedom',
                    'Generosity_2016' = 'Generosity')
happy2016$Lower.Confidence.Interval <- NULL
happy2016$Upper.Confidence.Interval <- NULL
happy2016$Region <- NULL
happy2016$Happiness.Rank <- NULL

#####============== Cleaning table for 2017 =================#####
happy2017 <- rename(happy2017,
                    'Score_2017' = 'Happiness.Score',
                    'GDP_2017' = 'Economy..GDP.per.Capita.',
                    'Social_Support_2017' = 'Family',
                    'Corruption_2017' = 'Trust..Government.Corruption.',
                    'Dystopia_Residual_2017' = 'Dystopia.Residual',
                    'Life_Expectancy_2017' = 'Health..Life.Expectancy.',
                    'Freedom_2017' = 'Freedom',
                    'Generosity_2017' = 'Generosity')
happy2017$Whisker.high <- NULL
happy2017$Whisker.low <- NULL
happy2017$Happiness.Rank <- NULL


#####============== Cleaning table for 2018 =================#####
happy2018 <- rename(happy2018,
                    'Country' = 'Country.or.region',
                    'Score_2018' = 'Score',
                    'GDP_2018' = 'GDP.per.capita',
                    'Social_Support_2018' = 'Social.support',
                    'Life_Expectancy_2018' = 'Healthy.life.expectancy',
                    'Freedom_2018' = 'Freedom.to.make.life.choices',
                    'Generosity_2018' = 'Generosity',
                    'Corruption_2018' = 'Perceptions.of.corruption'
                    )

happy2018$Overall.rank <- NULL


#####============== Cleaning table for 2019 =================#####
happy2019 <- rename(happy2019,
                    'Country' = 'Country.or.region',
                    'Score_2019' = 'Score',
                    'GDP_2019' = 'GDP.per.capita',
                    'Social_Support_2019' = 'Social.support',
                    'Life_Expectancy_2019' = 'Healthy.life.expectancy',
                    'Freedom_2019' = 'Freedom.to.make.life.choices',
                    'Generosity_2019' = 'Generosity',
                    'Corruption_2019' = 'Perceptions.of.corruption'
)

happy2019$Overall.rank <- NULL

#####============== Cleaning table for 2020 =================#####
happy2020$Logged.GDP.per.capita <-NULL
happy2020$Social.support <-NULL
happy2020$Healthy.life.expectancy <-NULL
happy2020$Freedom.to.make.life.choices <-NULL
happy2020$Generosity <-NULL
happy2020$Perceptions.of.corruption <-NULL
happy2020$Ladder.score.in.Dystopia <-NULL
happy2020$Standard.error.of.ladder.score <-NULL
happy2020$upperwhisker <-NULL
happy2020$lowerwhisker <-NULL
happy2020$Regional.indicator <- NULL


happy2020 <- rename(happy2020,
                    'Country' = 'Country.name',
                    'Score_2020' = 'Ladder.score',
                    'GDP_2020' = 'Explained.by..Log.GDP.per.capita',
                    'Social_Support_2020' = 'Explained.by..Social.support',
                    'Life_Expectancy_2020' = 'Explained.by..Healthy.life.expectancy',
                    'Freedom_2020' = 'Explained.by..Freedom.to.make.life.choices',
                    'Generosity_2020' = 'Explained.by..Generosity',
                    'Corruption_2020' = 'Explained.by..Perceptions.of.corruption',
                    'Corruption_2020' = 'Explained.by..Perceptions.of.corruption',
                    'Dystopia_Residual_2020' = 'Dystopia...residual'
)

#####============== Cleaning table for 2021 =================#####
happy2021$Logged.GDP.per.capita <-NULL
happy2021$Social.support <-NULL
happy2021$Healthy.life.expectancy <-NULL
happy2021$Freedom.to.make.life.choices <-NULL
happy2021$Generosity <-NULL
happy2021$Perceptions.of.corruption <-NULL
happy2021$Ladder.score.in.Dystopia <-NULL
happy2021$Standard.error.of.ladder.score <-NULL
happy2021$upperwhisker <-NULL
happy2021$lowerwhisker <-NULL
happy2021$Regional.indicator <- NULL

happy2021 <- rename(happy2021,
                    'Country' = 'Country.name',
                    'Score_2021' = 'Ladder.score',
                    'GDP_2021' = 'Explained.by..Log.GDP.per.capita',
                    'Social_Support_2021' = 'Explained.by..Social.support',
                    'Life_Expectancy_2021' = 'Explained.by..Healthy.life.expectancy',
                    'Freedom_2021' = 'Explained.by..Freedom.to.make.life.choices',
                    'Generosity_2021' = 'Explained.by..Generosity',
                    'Corruption_2021' = 'Explained.by..Perceptions.of.corruption',
                    'Dystopia_Residual_2021' = 'Dystopia...residual',
)



#####============== Cleaning table for 2022 =================#####
happy2022$RANK <-NULL
happy2022$Whisker.high <-NULL
happy2022$Whisker.low <-NULL

happy2022 <- rename(happy2022,
                    'Score_2022' = 'Happiness.score',
                    'GDP_2022' = 'Explained.by..GDP.per.capita',
                    'Social_Support_2022' = 'Explained.by..Social.support',
                    'Life_Expectancy_2022' = 'Explained.by..Healthy.life.expectancy',
                    'Freedom_2022' = 'Explained.by..Freedom.to.make.life.choices',
                    'Generosity_2022' = 'Explained.by..Generosity',
                    'Corruption_2022' = 'Explained.by..Perceptions.of.corruption',
                    'Dystopia_Residual_2022' = 'Dystopia..1.83....residual',
)

#####============== Ensuring country names are the same for all datasets =================#####
# To check country names spelling
# setdiff(unique(happy2015$Country), unique(happy2016$Country))
# unique(happy2017$Country[grepl("(?=.*o)(?=.*m)(?=.*a)(?=.*l)(?=.*i)", happy2017$Country, ignore.case = TRUE, perl = TRUE)])

happy2015$Country[happy2015$Country == 'Somaliland region'] <- 'Somaliland Region'
happy2015$Country[happy2015$Country == 'Macedonia'] <- 'North Macedonia'
happy2016$Country[happy2016$Country == 'Macedonia'] <- 'North Macedonia'
happy2017$Country[happy2017$Country == 'Macedonia'] <- 'North Macedonia'
happy2018$Country[happy2018$Country == 'Macedonia'] <- 'North Macedonia'
happy2020$Country[happy2020$Country == 'Macedonia'] <- 'North Macedonia'
happy2022$Country[happy2022$Country == 'Congo'] <- "Congo (Brazzaville)"

happy2022$Country[happy2022$Country == 'Lesotho*'] <- "Lesotho"
happy2022$Country[happy2022$Country == 'Palestinian Territories*'] <- "Palestinian Territories"
happy2022$Country[happy2022$Country == 'Yemen*'] <- "Yemen"
happy2022$Country[happy2022$Country == 'Turkmenistan*'] <- "Turkmenistan"
happy2022$Country[happy2022$Country == 'Rwanda*'] <- "Rwanda"
happy2022$Country[happy2022$Country == 'North Cyprus*'] <- "North Cyprus"
happy2022$Country[happy2022$Country == 'Niger*'] <- "Niger"
happy2022$Country[happy2022$Country == 'Mauritania*'] <- "Mauritania"
happy2022$Country[happy2022$Country == 'Guatemala*'] <- "Guatemala"
happy2022$Country[happy2022$Country == 'Gambia*'] <- "Gambia"
happy2022$Country[happy2022$Country == 'Comoros*'] <- "Comoros"
happy2022$Country[happy2022$Country == 'Chad*'] <- "Chad"
happy2022$Country[happy2022$Country == 'Botswana*'] <- "Botswana"
happy2022$Country[happy2022$Country == 'Belarus*'] <- "Belarus"
happy2022$Country[happy2022$Country == 'Azerbaijan*'] <- "Azerbaijan"
happy2022$Country[happy2022$Country == 'Eswatini, Kingdom of*'] <- "Swaziland"
happy2022$Country[happy2022$Country == 'Czechia'] <- "Czech Republic"

happy2017$Country[happy2017$Country == 'Hong Kong S.A.R., China'] <- "Hong Kong"
happy2020$Country[happy2020$Country == 'Hong Kong S.A.R. of China'] <- "Hong Kong"
happy2021$Country[happy2021$Country == 'Hong Kong S.A.R. of China'] <- "Hong Kong"
happy2022$Country[happy2022$Country == 'Hong Kong S.A.R. of China'] <- "Hong Kong"

happy2022$Country[happy2022$Country == 'Madagascar*'] <- "Madagascar"
happy2022$Country[happy2022$Country == 'Luxembourg*'] <- "Luxembourg"
happy2022$Country[happy2022$Country == 'Libya*'] <- "Libya"
happy2022$Country[happy2022$Country == 'Liberia*'] <- "Liberia"
happy2022$Country[happy2022$Country == 'Kuwait*'] <- "Kuwait"
happy2022$Country[happy2022$Country == 'Mauritania*'] <- "Mauritania"

happy2017$Country[happy2017$Country == 'Taiwan Province of China'] <- "Taiwan"
happy2020$Country[happy2020$Country == 'Taiwan Province of China'] <- "Taiwan"
happy2021$Country[happy2021$Country == 'Taiwan Province of China'] <- "Taiwan"
happy2022$Country[happy2022$Country == 'Taiwan Province of China'] <- "Taiwan"

happy2019$Country[happy2019$Country == 'Northern Cyprus'] <- "North Cyprus"
happy2018$Country[happy2018$Country == 'Northern Cyprus'] <- "North Cyprus"
happy2022$Country[happy2022$Country == 'Congo'] <- "Congo (Brazzaville)"

happy2019$Country[happy2019$Country == 'Trinidad & Tobago'] <- "Trinidad and Tobago"
happy2018$Country[happy2018$Country == 'Trinidad & Tobago'] <- "Trinidad and Tobago"

happy2015$Dystopia_Residual_2015 <- NULL
happy2016$Dystopia_Residual_2016 <- NULL
happy2017$Dystopia_Residual_2017 <- NULL
happy2018$Dystopia_Residual_2018 <- NULL
happy2019$Dystopia_Residual_2019 <- NULL
happy2020$Dystopia_Residual_2020 <- NULL
happy2021$Dystopia_Residual_2021 <- NULL
happy2022$Dystopia_Residual_2022 <- NULL




#####============== Merging datasets =================#####
merged_15_16 <- merge(happy2015, happy2016, by = "Country", all = TRUE)
merged_15_17 <- merge(merged_15_16, happy2017, by = "Country", all = TRUE)
merged_15_18 <- merge(merged_15_17, happy2018, by = "Country", all = TRUE)
merged_15_19 <- merge(merged_15_18, happy2019, by = "Country", all = TRUE)
merged_15_20 <- merge(merged_15_19, happy2020, by = "Country", all = TRUE)
merged_15_21 <- merge(merged_15_20, happy2021, by = "Country", all = TRUE)
happy_15to22 <- merge(merged_15_21, happy2022, by = "Country", all = TRUE)
happy_15to22 <- happy_15to22[happy_15to22$Country != "xx", ] # deleted an invalid xx entry
happy_15to22$Country[happy_15to22$Country == 'Swaziland'] <- 'Eswatini'
happy_15to22$Region[c(50, 101, 132, 136)] <- "Sub-Saharan Africa"
happy_15to22$Region[89] <- "Southern Asia"
happy_15to22$Region[c(14, 120)] <- "Latin America and Caribbean" # manually assigning region to countries with NA for Region

# ensuring scores are num and not chr
happy_15to22$Generosity_2022 <- as.numeric(sub(",", ".", happy_15to22$Generosity_2022))
happy_15to22$GDP_2022 <- as.numeric(sub(",", ".", happy_15to22$GDP_2022))
happy_15to22$Social_Support_2022 <- as.numeric(sub(",", ".", happy_15to22$Social_Support_2022))
happy_15to22$Corruption_2018 <- as.numeric(happy_15to22$Corruption_2018)
happy_15to22$Corruption_2022 <- as.numeric(sub(",", ".", happy_15to22$Corruption_2022))
happy_15to22$Life_Expectancy_2022 <- as.numeric(sub(",", ".", happy_15to22$Life_Expectancy_2022))
happy_15to22$Freedom_2022 <- as.numeric(sub(",", ".", happy_15to22$Freedom_2022))

#####-------------- Creating columns of average scores --------------#####
happy_15to22$meanGDP <- rowMeans(happy_15to22[, c("GDP_2015","GDP_2016","GDP_2017","GDP_2018","GDP_2019","GDP_2020","GDP_2021","GDP_2022")], 
                                           na.rm = TRUE)
happy_15to22$meanSocialSupport <- rowMeans(happy_15to22[, c("Social_Support_2015","Social_Support_2016","Social_Support_2017","Social_Support_2018","Social_Support_2019","Social_Support_2020","Social_Support_2021","Social_Support_2022")], 
                                           na.rm = TRUE)
happy_15to22$meanGenerosity <- rowMeans(happy_15to22[, c("Generosity_2015","Generosity_2016","Generosity_2017","Generosity_2018","Generosity_2019","Generosity_2020","Generosity_2021","Generosity_2022")], 
                                           na.rm = TRUE)
happy_15to22$meanCorruption <- rowMeans(happy_15to22[, c("Corruption_2015","Corruption_2016","Corruption_2017","Corruption_2018","Corruption_2019","Corruption_2020","Corruption_2021","Corruption_2022")], 
                                           na.rm = TRUE)
happy_15to22$meanLifeExpectancy <- rowMeans(happy_15to22[, c("Life_Expectancy_2015","Life_Expectancy_2016","Life_Expectancy_2017","Life_Expectancy_2018","Life_Expectancy_2019","Life_Expectancy_2020","Life_Expectancy_2021","Life_Expectancy_2022")], 
                                           na.rm = TRUE)
happy_15to22$meanFreedom <- rowMeans(happy_15to22[, c("Freedom_2015","Freedom_2016","Freedom_2017","Freedom_2018","Freedom_2019","Freedom_2020","Freedom_2021","Freedom_2022")], 
                                           na.rm = TRUE)


happy_avg <- happy_15to22[, c("Country", "Region", "meanGDP", "meanSocialSupport", "meanGenerosity", "meanCorruption", "meanLifeExpectancy", "meanFreedom")]
#####=============== END OF CLEANING SECTION ===========#####
# USE final happy_avg Dataset for all analysis
# write.csv(happy_avg, "happy_avg.csv", row.names = FALSE)


