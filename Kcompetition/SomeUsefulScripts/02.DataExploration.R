# Let's explore the data in more detail.
# I am importing the data file: "Data-for-Analysis.RData"

# loading necessary packages:
library(caret)
library(psych) # for pairs.panels function later on

# I will try to explore variables highly correlated with Voting Preference.
# I will look at the train data set - USER_ID + Party binarized (1=Republican)
# bin_label <- ifelse(train_label == "Republican", 1, 0)
# bin_label <- as.integer(bin_label)
# dmy <- dummyVars("~.", data = cbind(train[2:107], bin_label))
# bin_train <- data.frame(predict(dmy, newdata = cbind(train[2:107], bin_label)))

#A couple of useful functions: # Correlation matrix with p-values. See
#http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) 
    stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) 
    stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

# Let's take a look:
corMasterList <- flattenSquareMatrix(cor.prob(cbind(trainM, bin_label)))
print(head(corMasterList, 30))
#               i               j          cor            p
# 1         AgeGroup          Gender -0.003617982 7.873037e-01
# 2         AgeGroup          Income  0.328250082 0.000000e+00
# 3           Gender          Income  0.041823633 1.806166e-03
# 4         AgeGroup HouseholdStatus -0.338101837 0.000000e+00
# 5           Gender HouseholdStatus  0.013086894 3.290630e-01
# 6           Income HouseholdStatus -0.232825224 0.000000e+00
# 7         AgeGroup  EducationLevel  0.412003437 0.000000e+00
# 8           Gender  EducationLevel -0.009656415 4.714336e-01
# 9           Income  EducationLevel  0.184324397 0.000000e+00
# 10 HouseholdStatus  EducationLevel -0.210386767 0.000000e+00
# 11        AgeGroup         Q124742 -0.191079764 0.000000e+00
# 12          Gender         Q124742 -0.041186172 2.120736e-03
# 13          Income         Q124742 -0.075219759 1.937315e-08
# 14 HouseholdStatus         Q124742  0.078475451 4.590255e-09
# 15  EducationLevel         Q124742 -0.202633146 0.000000e+00
# 16        AgeGroup         Q124122 -0.198399690 0.000000e+00
# 17          Gender         Q124122 -0.040895174 2.280404e-03
# 18          Income         Q124122 -0.063801247 1.908679e-06
# 19 HouseholdStatus         Q124122  0.066101572 8.017000e-07
# 20  EducationLevel         Q124122 -0.065400591 1.047455e-06
# 21         Q124742         Q124122  0.104629365 5.107026e-15
# 22        AgeGroup         Q123464 -0.112642413 0.000000e+00
# 23          Gender         Q123464 -0.018759797 1.617699e-01
# 24          Income         Q123464 -0.120886896 0.000000e+00
# 25 HouseholdStatus         Q123464  0.084087031 3.343374e-10
# 26  EducationLevel         Q123464 -0.065275384 1.098385e-06
# 27         Q124742         Q123464  0.063480396 2.149241e-06
# 28         Q124122         Q123464 -0.012897449 3.361133e-01
# 29        AgeGroup         Q123621  0.383676478 0.000000e+00
# 30          Gender         Q123621  0.113635296 0.000000e+00

# Let's look at it with respect to bin_preditor.
corList <- corMasterList[order(corMasterList$cor),]
selectedSub <- subset(corList, (abs(cor) > 0.1 & j == "bin_label"))
bestSub <-  sapply(strsplit(as.character(selectedSub$i),'[.]'), "[", 1)
bestSub <- unique(bestSub)
#The results are: "Q109244:feminist" "Q115611:gun" "Q113181:meditate" 
#"Q98197:pray" "Gender" "Q105840:retailTherapy" "Q98869:LifePurpose" 
# -> Q98197 & Q113181 are basically SAME???

# # Let's try lowering the threshold to 0.05?
# selectedSub <- subset(corList, (abs(cor) > 0.05 & j == "bin_label"))
# bestSub <-  sapply(strsplit(as.character(selectedSub$i),'[.]'), "[", 1)
# bestSub <- unique(bestSub)
# # "Q109244:feminist" "Q115611:gun" "Q113181:meditate" "Q98197:pray" "Gender"
# # "Q105840:retailtherapy" "Q98869:LifePurpose" "Q101163:optimist/pessimist" 
# # "Q106272:powertool" "Q115899:hardship" "Q99480:spanked" "HouseholdStatus"
# # "Q120472:Science/Art" "Q100680:cried" "Q102089:rent/own" "Q116881:happy"
# # "Q106042:meds" "Q118892:badEye" "Q101596:treehouse" "Q119851:reading"
# # "Q120379:higheducation" "Q98078:hobby?" "Q120012:weatherMood"
# # "Q121699:drink?" "Q99716:liveAlone?"

# pairwise correlation plot from psych package(let's do it at 0.1 level...)
train_with_pred <- cbind(train[2:107], bin_label)
pairs.panels(train_with_pred[c(bestSub, "bin_label")])
# plot saved as "ImportantCorrelations.png"

# So Q98197 & Q113181 need to be consolidated.
nrow(subset(train, train$Q98197=="")) # 0
nrow(subset(train, train$Q98197=="No")) # 3460
nrow(subset(train, train$Q98197=="Yes")) # 2104
nrow(subset(train, train$Q113181=="")) # 0
nrow(subset(train, train$Q113181=="No")) # 3416
nrow(subset(train, train$Q113181=="Yes")) # 2148
sum(train$Q98197 == train$Q113181) # 5222 records match...
idx_unmatched <- which(train$Q98197 != train$Q113181) # 342 records unmatched...
train$Q98197[idx_unmatched]
train$Q113181[idx_unmatched]
# Hmm... What to do...  Results look much better with newly imputed dataset!!!