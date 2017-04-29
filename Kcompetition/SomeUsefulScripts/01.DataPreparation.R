# Data importing with NA fields
train <- read.csv("train2016.csv", na.strings = c("", "NA"))
test <- read.csv("test2016.csv", na.strings = c("", "NA"))

# Baseline training set accuracy: 0.5299928
table(train_label)
(2951)/length(train_label)

# Looking at YOBs
mean(train$YOB, na.rm=TRUE)
mean(test$YOB, na.rm=TRUE)
median(train$YOB, na.rm=TRUE)
median(test$YOB, na.rm=TRUE)
train$YOB[train$YOB>2004 & !is.na(train$YOB)]
train$YOB[train$YOB<1901 & !is.na(train$YOB)]
test$YOB[test$YOB>2004 & !is.na(test$YOB)]
test$YOB[test$YOB<1901 & !is.na(test$YOB)]
# Looking at high-outliers for train$YOB
train[train$YOB>2004 & !is.na(train$YOB),]
# Looking at high-outliers for test$YOB: NONE
test[test$YOB>2004 & !is.na(test$YOB),]
# Will remove 4 high-outliers in train$YOB
train <- subset(train, train$YOB <= 2004 | is.na(train$YOB))
# Looking at low-outliers for train$YOB
train[train$YOB<1901 & !is.na(train$YOB),] 
# Looking at low-outliers for test$YOB
test[test$YOB<1901 & !is.na(test$YOB),]
# For train$YOB of < 1900 will be added 100
train$YOB[train$YOB < 1900 & !is.na(train$YOB)] <-
  train$YOB[train$YOB < 1900 & !is.na(train$YOB)] + 100
# for both train$YOB and test$YOB = 1900 will be assigned median values
median(subset(train$YOB, train$YOB != 1900), na.rm=TRUE) #1983
median(subset(test$YOB, test$YOB != 1900), na.rm=TRUE) # 1984
# will assign 1984 for both sets
train$YOB[train$YOB == 1900 & !is.na(train$YOB)] <- 1984
test$YOB[test$YOB == 1900 & !is.na(test$YOB)] <- 1984
# turn out there is one 1901 for train$YOB...  will assign median value again.
train$YOB[train$YOB == 1901 & !is.na(train$YOB)] <- 1984
# Checking distribution 
plot(train$YOB)
plot(test$YOB)
# Distribution looks satisfactory.
train_label <- train$Party
train$Party <- NULL
# This will be saved as "YOB-Outliers-removed-with-NA.RData".

# I will now create a factor variable AgeGroup for both data sets.
# combining both data: train(5564) & test(1392) total(6956)
data <- rbind(train, test)
# Creating AgeGroup
AgeGroup <- vector(mode="character", length=6956)
for(i in 1:6956){
  age <- 2016 - data$YOB[i]
  if(is.na(age)){
    AgeGroup[i] <- NA
  }
  else if(age < 18){
    AgeGroup[i] <- "Under18"
  }
  else if(age >= 18 & age < 25){
    AgeGroup[i] <- "18-24"
  }
  else if(age >= 25 & age < 35){
    AgeGroup[i] <- "25-34"
  }
  else if(age >= 35 & age < 45){
    AgeGroup[i] <- "35-44"
  }
  else if(age >= 45 & age < 55){
    AgeGroup[i] <- "45-54"
  }
  else if(age >= 55 & age < 65){
    AgeGroup[i] <- "55-64"
  }
  else if(age >= 65){
    AgeGroup[i] <- "Over65"
  }
}
rm(i, age)

# converting AgeGroup into an ordered factor variable
AgeGroup <- factor(AgeGroup, c("Under18","18-24","25-34","35-44","45-54",
                               "55-64","Over65"), ordered = TRUE)

# I will now reorder Income and EducationLevel ordred factor variables.
data$Income <- factor(data$Income, levels=c("under $25,000",
                                            "$25,001 - $50,000",
                                            "$50,000 - $74,999",
                                            "$75,000 - $100,000",
                                            "$100,001 - $150,000",
                                            "over $150,000"),
                      ordered = TRUE)

data$EducationLevel <- factor(data$EducationLevel, 
                              levels=c("Current K-12","High School Diploma",
                                       "Associate's Degree",
                                       "Current Undergraduate",
                                       "Bachelor's Degree",
                                       "Master's Degree","Doctoral Degree"),
                              ordered = TRUE)

# Let's remove YOB and replace it with AgeGroup.
data$YOB <- NULL
USER_ID <- data$USER_ID
data <- cbind(USER_ID, AgeGroup, data[,2:106])
rm(USER_ID, AgeGroup)

# Let's split data into train and test again
train <- data[1:5564,]
test <- data[5565:6956,]
rm(data)

# Will save the data at this point as "FullyFactorized-YOB-removed-with-NA.RData"

# Preparing data for imputation
data0 <- rbind(train, test)
USER_ID <- data0[,1]

# giving the missing data a look with Amelia package
# library(Amelia)
# missmap(data[2:107])
# There is inherent similarity in raggedness of both train and test data sets.
# What could this mean??? Image saved as: "DataMissingness.png"

# Well, I tried shuffling data and reverting back in "DataShuffling.R"
# Hopefully, this will help in Multiple Imputation process.
data <- data0
set.seed(666)
shuffled_USER_ID <- sample(USER_ID)
data <- data0[match(shuffled_USER_ID, data0$USER_ID),]
data <- data[2:107]

# Well, might as well give imputation another shot.
library(mice)
set.seed(666)
tempData <- mice(data, m=100, maxit=10)

# Creating all 100 MICE imputed datasets (This part is quick so won't be saved.)
# data_001 <- complete(tempData, 1)
# data_002 <- complete(tempData, 2)
# data_003 <- complete(tempData, 3)
# data_004 <- complete(tempData, 4)
# data_005 <- complete(tempData, 5)
# data_006 <- complete(tempData, 6)
# data_007 <- complete(tempData, 7)
# data_008 <- complete(tempData, 8)
# data_009 <- complete(tempData, 9)
# data_010 <- complete(tempData, 10)
# data_011 <- complete(tempData, 11)
# data_012 <- complete(tempData, 12)
# data_013 <- complete(tempData, 13)
# data_014 <- complete(tempData, 14)
# data_015 <- complete(tempData, 15)
# data_016 <- complete(tempData, 16)
# data_017 <- complete(tempData, 17)
# data_018 <- complete(tempData, 18)
# data_019 <- complete(tempData, 19)
# data_020 <- complete(tempData, 20)
# data_021 <- complete(tempData, 21)
# data_022 <- complete(tempData, 22)
# data_023 <- complete(tempData, 23)
# data_024 <- complete(tempData, 24)
# data_025 <- complete(tempData, 25)
# data_026 <- complete(tempData, 26)
# data_027 <- complete(tempData, 27)
# data_028 <- complete(tempData, 28)
# data_029 <- complete(tempData, 29)
# data_030 <- complete(tempData, 30)
# data_031 <- complete(tempData, 31)
# data_032 <- complete(tempData, 32)
# data_033 <- complete(tempData, 33)
# data_034 <- complete(tempData, 34)
# data_035 <- complete(tempData, 35)
# data_036 <- complete(tempData, 36)
# data_037 <- complete(tempData, 37)
# data_038 <- complete(tempData, 38)
# data_039 <- complete(tempData, 39)
# data_040 <- complete(tempData, 40)
# data_041 <- complete(tempData, 41)
# data_042 <- complete(tempData, 42)
# data_043 <- complete(tempData, 43)
# data_044 <- complete(tempData, 44)
# data_045 <- complete(tempData, 45)
# data_046 <- complete(tempData, 46)
# data_047 <- complete(tempData, 47)
# data_048 <- complete(tempData, 48)
# data_049 <- complete(tempData, 49)
# data_050 <- complete(tempData, 50)
# data_051 <- complete(tempData, 51)
# data_052 <- complete(tempData, 52)
# data_053 <- complete(tempData, 53)
# data_054 <- complete(tempData, 54)
# data_055 <- complete(tempData, 55)
# data_056 <- complete(tempData, 56)
# data_057 <- complete(tempData, 57)
# data_058 <- complete(tempData, 58)
# data_059 <- complete(tempData, 59)
# data_060 <- complete(tempData, 60)
# data_061 <- complete(tempData, 61)
# data_062 <- complete(tempData, 62)
# data_063 <- complete(tempData, 63)
# data_064 <- complete(tempData, 64)
# data_065 <- complete(tempData, 65)
# data_066 <- complete(tempData, 66)
# data_067 <- complete(tempData, 67)
# data_068 <- complete(tempData, 68)
# data_069 <- complete(tempData, 69)
# data_070 <- complete(tempData, 70)
# data_071 <- complete(tempData, 71)
# data_072 <- complete(tempData, 72)
# data_073 <- complete(tempData, 73)
# data_074 <- complete(tempData, 74)
# data_075 <- complete(tempData, 75)
# data_076 <- complete(tempData, 76)
# data_077 <- complete(tempData, 77)
# data_078 <- complete(tempData, 78)
# data_079 <- complete(tempData, 79)
# data_080 <- complete(tempData, 80)
# data_081 <- complete(tempData, 81)
# data_082 <- complete(tempData, 82)
# data_083 <- complete(tempData, 83)
# data_084 <- complete(tempData, 84)
# data_085 <- complete(tempData, 85)
# data_086 <- complete(tempData, 86)
# data_087 <- complete(tempData, 87)
# data_088 <- complete(tempData, 88)
# data_089 <- complete(tempData, 89)
# data_090 <- complete(tempData, 90)
# data_091 <- complete(tempData, 91)
# data_092 <- complete(tempData, 92)
# data_093 <- complete(tempData, 93)
# data_094 <- complete(tempData, 94)
# data_095 <- complete(tempData, 95)
# data_096 <- complete(tempData, 96)
# data_097 <- complete(tempData, 97)
# data_098 <- complete(tempData, 98)
# data_099 <- complete(tempData, 99)
# data_100 <- complete(tempData, 100)

# Now I will consolidate 100 imputed data using the routine from
# "MergingImputedData.R" This routine requires nnet package for which.is.max
# used for tie-breaking. We will start by first making a copy of data, which
# is a copy of data0(unshuffled data) that has been shuffled according to
# shuffled_USER_ID.  This will have to be unshuffled after the following process.
library(nnet)
dataImputed <- data
# Below routine will loop through all of the unimputed data and fill in the NAs
# with majority vote result of all the imputed data to reduce variance.
features <- colnames(data)
for(i in features){
  for(j in 1:nrow(data)){
    featureLevels <- levels(data[i][,1])
    featureCount <- numeric(length=length(featureLevels))
    # cat(featureLevels, featureCount, "\n")
    if(is.na(data[i][j,1])){
      for(k in 1:length(featureLevels)){
        if(data_001[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_002[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_003[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_004[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_005[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_006[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_007[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_008[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_009[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_010[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_011[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_012[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_013[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_014[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_015[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_016[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_017[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_018[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_019[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_020[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_021[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_022[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_023[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_024[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_025[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_026[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_027[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_028[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_029[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_030[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_031[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_032[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_033[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_034[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_035[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_036[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_037[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_038[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_039[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_040[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_041[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_042[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_043[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_044[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_045[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_046[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_047[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_048[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_049[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_050[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_051[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_052[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_053[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_054[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_055[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_056[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_057[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_058[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_059[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_060[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_061[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_062[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_063[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_064[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_065[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_066[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_067[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_068[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_069[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_070[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_071[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_072[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_073[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_074[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_075[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_076[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_077[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_078[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_079[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_080[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_081[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_082[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_083[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_084[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_085[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_086[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_087[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_088[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_089[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_090[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_091[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_092[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_093[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_094[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_095[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_096[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_097[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_098[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_099[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
        if(data_100[i][j,1] == featureLevels[k]){featureCount[k] <- 
          featureCount[k] + 1}
      }
      dataImputed[i][j,1] <- featureLevels[which.is.max(featureCount)]
      cat("Feature, ", i, " row, ", j, "has been assigned: ", 
          featureLevels[which.is.max(featureCount)], " by vote: ", 
          featureCount, "\n")
    }
  }
}

# Let's clean it up some.

# Quick way to delete 100 imputed datasets
for(i in 1:100){
  paddedInt <- sprintf("%03d", i)
  outString <- paste("data_", paddedInt, sep="")
  rm(list = outString)
}

# rest of useless variables
rm(features, featureLevels, featureCount, i, j, k, outString, paddedInt)

# Now, it's time to unshuffle the data.
data <- cbind(shuffled_USER_ID, dataImputed)
data <- data[match(USER_ID, data$shuffled_USER_ID),]
data <- data[,2:107]
data <- cbind(USER_ID, data)

# Let's save this data as: "FullData-Imputation-YOB.RData"

# Ok, everything is in order, let's create a dataset for use with analysis.
train <- data[1:5564,]
test <- data[5565:6956,]

# cleaning up the rest of stuff.
rm(data, data0, dataImputed, shuffled_USER_ID, tempData, USER_ID)

# Ok, so it looks like there is a benefit to turning the data into several
# different formats for use later.
data <- rbind(train, test)
USER_ID <- data$USER_ID
data$USER_ID <- NULL
demographics <- data[,1:5]
questionaire <- data[,6:106]

# First thing I am going to do is to turn it into a matrix.
data[,2] <- as.integer(as.integer(data[,2]) - 1)
for(i in 6:106){
  data[,i] <- as.integer(as.integer(data[,i]) - 1)
}
dataM <- data.matrix(data)
trainM <- dataM[1:5564,]
testM <- dataM[5565:6956,]

# Ok, now Let me dummify the data so to speak... by expressing factor levels as
# new features.
library(caret)
demographics$AgeGroup <- factor(demographics$AgeGroup, ordered=FALSE)
demographics$Income <- factor(demographics$Income, ordered=FALSE)
demographics$EducationLevel <- factor(demographics$EducationLevel, ordered=FALSE)
dmy <- dummyVars("~.", data=demographics, fullRank=FALSE)
demoD <- as.data.frame(predict(dmy, demographics))
demoD$Gender.Female <- NULL
dmy <- dummyVars("~.",data=questionaire, fullRank=TRUE)
questD <- as.data.frame(predict(dmy,questionaire))
dataD <- cbind(demoD, questD)
for(i in 1:128){
  dataD[,i] <- as.integer(dataD[,i])
}
trainDF <- dataD[1:5564,]
testDF <- dataD[5565:6956,]

# Well, back to cleaning up again...
rm(data, dataM, dataD, demoD, demographics, questD, questionaire, dmy, i)

# Great! Now I have data ready for anaysis: "Data-for-Analysis.RData"

