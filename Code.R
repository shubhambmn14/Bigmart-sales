#######*************BIG MART DATA ANALYSIS - sales prediction*****************############
setwd("D://Sunstone//kaggle//BigMart")
train <- read.csv("train.csv", stringsAsFactors = TRUE)
test <- read.csv("test.csv", stringsAsFactors = TRUE)
str(train)
summary(train)
hist(train$Item_Outlet_Sales, breaks = 80)
#looks highly right skewed
hist(log(train$Item_Outlet_Sales), breaks= 80)
test$Item_Outlet_Sales <- NA
combi <- rbind(train,test)
str(combi)
summary(combi)
#*****Assuming that LF, low fat as Low Fat and reg as Regular******#
combi$Item_Fat_Content <- as.character(combi$Item_Fat_Content)
combinedfull$Fat_Content[combinedfull$Fat_Content == 'low fat'] <- 'Low Fat'
combinedfull$Fat_Content[combinedfull$Fat_Content == 'reg'] <- 'Regular'
combinedfull$Fat_Content <- factor(combinedfull$Fat_Content) #coverting back to Facotr
summary(combi)
#still some problem with the data
sub(' ','',combi$Item_Fat_Content)
combi$Item_Fat_Content[combi$Item_Fat_Content == 'low fat'] <- 'Low Fat'
combi$Item_Fat_Content <- factor(combi$Item_Fat_Content)
#Now we have only two factors for Fat content = Low Fat and Regular
#*************There are lot of missing values in ITEM WEIGHT***************
write.csv(combi, file = "combined.csv", row.names = FALSE)
boxplot(combi$Item_Outlet_Sales)
table(combi$Item_Identifier)
#Identifying the missing weight variable
missing <- combi[is.na(combi$Item_Weight),]
missing1 <- train[!(is.na(train$Item_Weight)),]
plot(missing1$Item_Weight, missing1$Item_Outlet_Sales)
hist(missing1$Item_Weight, breaks = 80) #the histogram is no where close to normal
#Using box-cox transformations
install.packages("Rcpp")
library(car)
power <- coef(powerTransform(missing1$Item_Weight))
hist(bcPower(missing1$Item_Weight, power), breaks = 80)
#plotting the pair-wise scatterplot
pairs(missing1[,c(2:12)])
plot(missing1$Item_Visibility, missing1$Item_Outlet_Sales)
hist(sqrt(missing1$Item_Visibility), breaks = 40)
hist(sqrt(train$Item_Outlet_Sales), breaks = 80)
cor.test(sqrt(missing1$Item_Outlet_Sales),sqrt(missing1$Item_Visibility))
#Correlation is significant with value around -0.085#
cor.test(missing1$Item_Outlet_Sales,missing1$Item_Visibility)
cor.test(missing1$Item_Weight, missing1$Item_Outlet_Sales)
#p value is more than 0.05, with correlation is 0.0141 which is almost equal to 0. We can discard this variable.
summary(combi$Outlet_Size)
#Imputing the missing values of weight.
#Used VLOOKUP function in Excel for this.
combi <- read.csv("combined.csv")
sum(is.na(combi$Item_Weight)) #Shows there are no more NA values in weight.
summary(combi)
#Outlet size seems to be missing. 4016 values for Out10, Out17 and Out45
#Using KNN Imputations
install.packages("VIM")
library(VIM)
summary(combi)
?kNN()
combi2 <- kNN(combi, variable = "Outlet_Size", k = 5)
summary(combi2)
combi2 <- subset(combi2, select = Item_Identifier:Item_Outlet_Sales)
combi <- combi2
#Since there are lot of 0 for Item_visibility which doesnt make sense.
combi$Item_Visibility[combi$Item_Visibility == 0] <- NA
combi2 <- kNN(combi, variable = "Item_Visibility", k = 5)
combi2 <- subset(combi2, select = Item_Identifier:Item_Outlet_Sales)
combi <- combi2
combi$Low_Fat <- ifelse(combi$Item_Fat_Content == 'Low Fat', 1, 0)
combi <- combi[,-3]
#Manipulating the item type to lesser factors
str(combi$Item_Type)
combi$Item_Type <- as.character(combi$Item_Type)
combi$Item_Type[combi$Item_Type %in% c('Hard Drinks', 'Dairy', 'Soft Drinks')] <- 'Drinks'
combi$Item_Type[combi$Item_Type %in% c('Household', 'Others', 'Health and Hygiene')] <- 'Non-Consumable'
combi$Item_Type[combi$Item_Type %in% c('Baking Goods', 'Breads', 'Breakfast','Canned', 'Frozen Foods', 'Fruits and Vegetables', 'Meat', 'Seafood','Snack Foods', 'Starchy Foods')] <- 'Food'
combi$Item_Type <- factor(combi$Item_Type)
#Converting the Year of establishment into age of the store.
str(combi$Outlet_Establishment_Year)
combi$Age_of_Store <- (2013 - combi$Outlet_Establishment_Year)
combi <- combi[,-7]
write.csv(combi, file = "combined.csv", row.names = FALSE)
#Since we have few NC category which are termed as LOW FAT AND REGULAR..adding one more
#variable into the Non-edible
setwd("D://Sunstone//kaggle//BigMart")
combined1 <- read.csv("combined.csv")
combined <- cbind(combi[,-13], combined1[,11])
rm(combined1)
rm(combi)
Some of the non-consumables have also been termed as Low fat, we would covert their fat content to non-edible
combined$Fat_content <- combined$`combined1[, 11]`
combined <- combined[,-11]
combined$Item_Type <- as.character(combined$Item_Type)
combined$Fat_content <- as.character(combined$Fat_content)
combined$Fat_content[combined$Item_Type == 'Non-Consumable'] <- 'Non-Edible'
combined$Item_Type <- factor(combined$Item_Type)
combined$Fat_content <- factor(combined$Fat_content)
str(combined)
write.csv(combined, file = "combined.csv", row.names = FALSE) # writing it back to the system

#Creating dummy variable for the FatContent variable.
combinedfull$Low_Fat <- ifelse(combinedfull$Fat_Content == 'Low Fat',1,0)
combinedfull$Regular <- ifelse(combinedfull$Fat_Content == 'Regular',1,0)
combined <- combined[,-12]#removing the actual variable

#Creating dummy variables for Outlet_Location_Type - there are 3 levels - creating 2 dummy.

combined$Tier2 <- ifelse(combined$Outlet_Location_Type == 'Tier 2',1,0)
combined$Tier3 <- ifelse(combined$Outlet_Location_Type == 'Tier 3',1,0)
combined <- combined[,-8]

#Creating Dummy variable for Item_type - there are 3 levels - creating 2 dummies

combined$Drinks <- ifelse(combined$Item_Type == 'Drinks',1,0)
combined$Food <- ifelse(combined$Item_Type == 'Food',1,0)
combined <- combined[,-4]

#Creating dummy for outlet size:
levels(combined$Outlet_Size)
combined$Size_High <- ifelse(combined$Outlet_Size == 'High',1,0)
combined$Size_Medium <- ifelse(combined$Outlet_Size == 'Medium',1,0)
combined <- combined[,-6]

#CReating Dummy for Outlet_Type

levels(combined$Outlet_Type)
combined$SuperMarket1 <- ifelse(combined$Outlet_Type == 'Supermarket Type1',1,0)
combined$SuperMarket2 <- ifelse(combined$Outlet_Type == 'Supermarket Type2',1,0)
combined$SuperMarket3 <- ifelse(combined$Outlet_Type == 'Supermarket Type3',1,0)
combined <- combined[,-6]

#Creating Dummy variables for Outlet Identifier

levels(combined$Outlet_Identifier)
combined$Outlet13 <- ifelse(combined$Outlet_Identifier == 'OUT013',1,0)
combined$Outlet17 <- ifelse(combined$Outlet_Identifier == 'OUT017',1,0)
combined$Outlet18 <- ifelse(combined$Outlet_Identifier == 'OUT018',1,0)
combined$Outlet19 <- ifelse(combined$Outlet_Identifier == 'OUT019',1,0)
combined$Outlet27 <- ifelse(combined$Outlet_Identifier == 'OUT027',1,0)
combined$Outlet35 <- ifelse(combined$Outlet_Identifier == 'OUT035',1,0)
combined$Outlet45 <- ifelse(combined$Outlet_Identifier == 'OUT045',1,0)
combined$Outlet46 <- ifelse(combined$Outlet_Identifier == 'OUT046',1,0)
combined$Outlet49 <- ifelse(combined$Outlet_Identifier == 'OUT049',1,0)
combined <- combined[,-7]

#Running a linear Regression model.
?lm()
install.packages("gpairs")
library(gpairs)
plot(combined$Item_MRP, combined$Item_Outlet_Sales)
fit <- lm(Item_Outlet_Sales ~ )
hist(log(combinedfull$Item_Outlet_Sales), breaks = 40)
trainBig <- combinedfull[1:8523,]
testBig <- combinedfull[8524:14204,]
library(psych)
library(car)
#Visualizing the data
#Subset the variables
names(combinedfull)
subsetcombi <- subset(combinedfull, select = c(Size_Medium, Size_High, SuperMarket1, SuperMarket2, SuperMarket3, Item_Outlet_Sales))
subsetcombi1 <- subset(combinedfull, select = c(Outlet49, Outlet46, Outlet45, Outlet35, Outlet27, Outlet19, Outlet18, Outlet17, Outlet13, Item_Outlet_Sales))
pairs.panels(subsetcombi,col="blue")
pairs.panels(subsetcombi1,col="red")
combinedfull$Item_Visibility <- log10(combinedfull$Item_Visibility)
combinedfull$Item_Outlet_Sales <- log10(combinedfull$Item_Outlet_Sales)
combinedfull$Item_MRP <- log10(combinedfull$Item_MRP)
combinedfull$Item_Weight <- log10(combinedfull$Item_Weight)

pairs.panels(subsetcombi,col="blue")
#We will divide our training data into two parts, training and validation data
set.seed(2017)
train.size <- 0.8
train.index <- sample.int(length(trainBig$Item_Outlet_Sales), round(length(trainBig$Item_Outlet_Sales) * train.size))
train.sample <- trainBig[train.index,]
valid.sample <- trainBig[-train.index,]
#Building a backward elimination model
fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Outlet35+Outlet27+Outlet19+Outlet18+Outlet17+Outlet13+Low_Fat+Regular, data = train.sample)
summary(fit)

#there are lot of NA for the coefficients for variable: Outlet35,27,19,18,17,13, Regular
#Removing one variable at a time.Removing Regular categorical variable
fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Outlet35+Outlet27+Outlet19+Outlet18+Outlet17+Outlet13+Low_Fat, data = train.sample)
summary(fit)
#Removing Outlet variables as well

fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)


#We can see lot of extreme values which need to be treated.
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
#from the plots we can see that row names (8518,225,8523) are clear outliers and which are also affecting our linear model
#Removing the rows:

fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)
train.sample <- train.sample[-which(rownames(train.sample) %in% c("697","67","920")),]
plot(fit)
#Again run the model after the outlier elimination - Iteration 1.
fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)
#Again check and eliminate, if exteremes still exist.
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
#We see that rows 1444, 62 and 656 are still extreme values which have emerged after the last iteration
train.sample <- train.sample[-which(rownames(train.sample) %in% c("656","62","1444")),]
fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)
plot(fit)
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
#Again running the iteration 
train.sample <- train.sample[-which(rownames(train.sample) %in% c("1304","1324","1208")),]
fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)
plot(fit)

#Again running the iteration.
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)

train.sample <- train.sample[-which(rownames(train.sample) %in% c("1314","257","76")),]

fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)
# We see the Rsq and Adj Rsq. improving

#Again checking if the outliers are still present. Running the Cook's distance fn
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
#Removing the rows which are affecting the model.
train.sample <- train.sample[-which(rownames(train.sample) %in% c("1446","867","444")),]
#Running the model again
fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier2+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)

#The Rsq and Adj Rsq has improved again.
#Again checking for the outliers
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
plot(fit)
#We will leave the data as it is now. Assuming we have treated all the outliers

#Now we will check for Multiple collinearity in the model.

#VIF - Variance Inflation factors
#If VIF = 1 None are correlated, 1<VIF<5 moderately, 5<VIF<10 Highly
vif(fit)

#Item_Weight Item_Visibility        Item_MRP    Age_of_Store           Tier2 
#1.005633        1.057559        1.002438      192.712519      788.203449 
#Tier3          Drinks            Food     Size_Medium       Size_High 
#108.959391        1.924181        1.892924        3.718295       83.448367 
#SuperMarket3    SuperMarket2    SuperMarket1        Outlet49        Outlet46 
#51.947734       31.794958      273.210147      246.049212      234.724876 
#Outlet45         Low_Fat 
#5.130100        1.297430 

#There are lot of variables with more than 10 which are critical on the correlation perspective.
#Removing the Tier2 variable which has the largest VIF factor

fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+SuperMarket1+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)

#Again running the VIF on the remaining model

vif(fit)
#Again there are many variables with greater than 10. Removing the highest one
#Removing Supermarket1

fit <- lm(Item_Outlet_Sales ~ Item_Weight+Item_Visibility+Item_MRP+Age_of_Store+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)

#Rsq and Adj Rsq values are coming down :(
#Again running VIF
vif(fit)
#Now the VIF factors are all less than 5.Our model is treated for correlation

#Now looking at the p-value. 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      1.3774261  0.0393085  35.041  < 2e-16 ***
#  Item_Weight      0.0148199  0.0182205   0.813  0.41604    
#Item_Visibility -0.0304149  0.0093379  -3.257  0.00113 ** 
#  Item_MRP         1.0233559  0.0136456  74.995  < 2e-16 ***
#  Age_of_Store    -0.0463477  0.0007068 -65.574  < 2e-16 ***
#  Drinks           0.0100633  0.0119543   0.842  0.39992    
#Food             0.0134140  0.0090101   1.489  0.13660    
#Size_Medium     -0.1357389  0.0102406 -13.255  < 2e-16 ***
#  Size_High        0.8566493  0.0142058  60.303  < 2e-16 ***
#  SuperMarket3     1.3390554  0.0182027  73.563  < 2e-16 ***
#  SuperMarket2    -0.0763564  0.0134476  -5.678 1.42e-08 ***
#  Outlet49         0.4707197  0.0131067  35.914  < 2e-16 ***
#  Outlet46         0.4147556  0.0113141  36.658  < 2e-16 ***
#  Outlet45         0.2280740  0.0111461  20.462  < 2e-16 ***
#  Low_Fat         -0.0051492  0.0071127  -0.724  0.46913 

#The above shows that lot of variables are not significant with p values > 0.05
#Low_Fat, Food, Drinks, Item_weight

#Removing Item_Weight from the model

fit <- lm(Item_Outlet_Sales ~ Item_Visibility+Item_MRP+Age_of_Store+Tier3+Drinks+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)

#Removing Drinks now. P-value is 0.986
fit <- lm(Item_Outlet_Sales ~ Item_Visibility+Item_MRP+Age_of_Store+Tier3+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)

#removing Item_visibility, p value is 0.726
fit <- lm(Item_Outlet_Sales ~ Item_MRP+Age_of_Store+Tier3+Food+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45+Low_Fat, data = train.sample)
summary(fit)

#Removing low fat and Food as well
fit <- lm(Item_Outlet_Sales ~ Item_MRP+Age_of_Store+Tier3+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45, data = train.sample)
summary(fit)

######    Now all variables are significant. Rsq Adjusted is 73.51%
plot(fit)

#Again if we see the residual plot, we can see that there are lot of outliers. we can easily identify rows: 501,787,6995

#removing them and again running the model to see if Rsq increases or not.
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
#Keeping a copy of train.sample just in case model doesnt improve
train.samplecopy <- train.sample
train.sample <- train.sample[-which(rownames(train.sample) %in% c("6995","4142","1462")),]

fit <- lm(Item_Outlet_Sales ~ Item_MRP+Age_of_Store+Tier3+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45, data = train.sample)
summary(fit)
plot(fit)
cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)

#removing row 4072
train.sample <- train.sample[-which(rownames(train.sample) %in% c("4072")),]
fit <- lm(Item_Outlet_Sales ~ Item_MRP+Age_of_Store+Tier3+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45, data = train.sample)
summary(fit)

cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2) #Using cook's distance formula
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
plot(fit)

train.sample <- train.sample[-which(rownames(train.sample) %in% c("501","787","225")),]
fit <- lm(Item_Outlet_Sales ~ Item_MRP+Age_of_Store+Tier3+Size_Medium+Size_High+SuperMarket3+SuperMarket2+Outlet49+Outlet46+Outlet45, data = train.sample)
summary(fit)

cutoff <- 4/(nrow(train.sample)-length(fit$coefficients)-2)
plot(fit, which = 4, cook.levels = cutoff)
plot(fit, which = 5, cook.levels = cutoff)
plot(fit)

#So now leaving the model as it is. Now testing it on the test.sample and valid.sample data ad
train.sample$Pred_Sales <- predict(fit, newdata = subset(train.sample, select = c(Item_MRP, Age_of_Store, Tier3, Size_Medium, Size_High, SuperMarket3, SuperMarket2, Outlet49, Outlet46, Outlet45)))
train.corr <- round(cor(train.sample$Pred_Sales, train.sample$Item_Outlet_Sales),2)
train.RMSE <- sqrt(mean((10^train.sample$Item_Outlet_Sales- 10^train.sample$Pred_Sales)^2))
train.MAE <- round(mean(abs(10^train.sample$Item_Outlet_Sales- 10^train.sample$Pred_Sales)))
#Checking the model again

subsetcombi2 <- subset(train.sample, select = c(Item_Outlet_Sales, Item_MRP, Age_of_Store, Tier3, Size_Medium, Size_High, SuperMarket3, SuperMarket2, Outlet49, Outlet46, Outlet45))
pairs.panels(subsetcombi2,col="blue")

valid.sample$Pred_Sales <- predict(fit, newdata = subset(valid.sample, select = c(Item_MRP, Age_of_Store, Tier3, Size_Medium, Size_High, SuperMarket3, SuperMarket2, Outlet49, Outlet46, Outlet45)))
valid.corr <- cor(valid.sample$Pred_Sales, valid.sample$Item_Outlet_Sales)
valid.RMSE <- sqrt(mean((10^valid.sample$Item_Outlet_Sales- 10^valid.sample$Pred_Sales)^2))
valid.MAE <- round(mean(abs(10^valid.sample$Item_Outlet_Sales- 10^valid.sample$Pred_Sales)))
c(valid.corr,valid.RMSE,valid.MAE)

#.8486711 1590.0033068  767.0000000
#checking the output for test data
testBig$Item_Outlet_Sales <- predict(fit, newdata = subset(testBig, select = c(Item_MRP, Age_of_Store, Tier3, Size_Medium, Size_High, SuperMarket3, SuperMarket2, Outlet49, Outlet46, Outlet45)))
testBig$Item_Outlet_Sales <- 10^testBig$Item_Outlet_Sales
write.csv(testBig[,c(1,5)], file = "predicted.csv", row.names = FALSE)
