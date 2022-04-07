# clearing the environment 
rm(list = ls())
 
# LOADING LIBRARIES #### 
library(dplyr) #for data manipulation 
library(car) #for vif - multi colinearity  
library(caTools) #for stratified train test split 
library(fastDummies) # for creating dummy variables from categorical variables   



# Modelling 
library(rpart) # for regression tree 
library(rpart.plot) # plotting tree results 

library(gbm) #for gradient boosting 
library(mlr) # for extreme gradient boosting 

library(ggplot2) # visualization  



# setting working directory 
setwd("")

# setting seed for result duplication 
set.seed(1234)


# READING DATA #### 

ads <- read.csv("adsV2.csv") #33165
ads <- ads %>% filter( monetary < 1e+06) #33110 
ads <- ads %>% filter( monetary < 2e+05) #32570 
ads <- ads %>% filter( monetary <= 50000) #31122 
31122/33730 


ads2 <- read.csv("cust_total_sales.csv")

t1 = ads2 %>% group_by(industry, rfm_score) %>% summarise( customer_count = n())

p <- ggplot(aes(x=industry, y=customer_count,), data=t1)
p + geom_point(color = "#FC4E07")+  ggtitle("Industry") + facet_wrap( "rfm_score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



# train test split 
library(caret)
train.index <- createDataPartition(ads$monetary, p = .9, list = FALSE)
train <- ads[ train.index,]
test  <- ads[-train.index,]


train_feature <- train %>% select(  -monetary, -log_monetary, -branch, -description, -rfm_score)

train_salaries <- train[,c("id","log_monetary")]


# test
test_feature <- test %>% select( -monetary, -log_monetary, -branch, -description, -rfm_score)
test_label <- test[,c("id","log_monetary")]

# DATA WRANGLING ####  

# checking for missing values in each column 
sapply(train_feature, function(x){sum(is.null(x))}) 
# THERE IS NO MISSING VALUE IN ANY OF THE ROWS IN ALL THE COLUMNS 


# BOX PLOT FOR OUTLIERS 
# plotting frequency
boxplot(train_feature$frequency, main = "")
# AS THE MEDIAN IS ROUGHLY IN THE MIDDLE DIVING THE DATA BY 50%, SHOWS THAT THE DISTRIBUTION IS NOT SKEWED  
hist(train_feature$frequency, main = "")

# BOX PLOT FOR OUTLIERS 
# plotting milesFromMetropolis
boxplot(train_feature$f_nf_diff, main = "miles from metropolis")

# distribution of salary
hist(ads$monetary)




# EDA #### 

# variation of salary across different job types   
p <- ggplot(aes(x=frequency, y=monetary), data=ads)
p + geom_point(color = "orange")+  ggtitle("Frequency vs sales with freight") + facet_wrap( "rfm_score")


# variation of salary across different industries 
p <- ggplot(aes(x=recency, y=monetary, color = "recency"), data=ads)
p + geom_point(color = "steelblue")+  ggtitle("Recency vs sales with freight") + facet_wrap( "rfm_score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ads %>% group_by(industry) %>% summarise( t = mean(salary)) %>% arrange(t)

# Finance and oil have the almost exact variation in salary 
# Followed by Heatlh and Web and lastly auto, service and education
# Instead of having 

# variation of salary across different majors - hypothesis being tech gets paid more  
p <- ggplot(aes(x=salary, colour=major), data=ads)
p + geom_density(aes(y = ..count..)) + ggtitle("Major vs salary") + 
                                                          coord_cartesian(ylim=c(0, 700))
ads %>% group_by(major) %>% summarise( t = mean(salary)) %>% arrange(t)



# Changing all the character columns to factors to avoid dummy-fying the variables 
ads1 <- as.data.frame(unclass(ads),stringsAsFactors=TRUE) 


# 
# # Train Test split ####
# 
# split <- sample.split(ads1$salary, 0.75)
# train <- subset(ads1, split == T)
# test <- subset(ads1, split == F)

# MERGING #### 
# DOING A LEFT JOIN BY KEEP SALARIES TABLE AS BASE BECAUSE CAN'T REALLY BUILD A MODEL WITHOUT DEPENDANT VARIABLE 
model_ads <- merge(train_salaries, train_feature, by = "id",all.x = T)
test_ads<- merge(test_label, test_feature, by = "id",all.x = T)


# BUILDING THE MODEL ####
# LINEAR REGRESSION MODEL TO GET MOST INTERPRETABILITY - base model with variables ####
# LOG TRANSFORMATION OF SALARY TO YEILD MORE LINEAR RELATIONSHIP #### 

mod1 <- lm(log_monetary ~., data = model_ads %>% select(-id))
summary(mod1)
# Adjusted R2 = 44.68% 
# RMSE = 1.685 



# checking for heteroscedasticity  
plot(mod1)
# heteroscedasticity is present, linear fit is not the most appropriate 


# calculating test RMSE for linear regression 
predicted_salary_lm <- predict(mod1, newdata = test_ads %>% select(-id,-log_monetary))


# TRANSFORMING IT BACK TO SALARY 
predicted_salary <- exp(predicted_salary_lm)

RMSE_test_lm <- sqrt(mean((predicted_salary - exp(test_ads$log_monetary))^2)) 
#1.608 - almost equal to train RMSE  # 279,214 





# few observations are influencing the model (where salary is 0 - based on cook's distance) 
plot1 <- ggplot(data = model_ads, aes(x = f_nf_diff, y = log_monetary)) +
  geom_point() + 
  geom_smooth(method = lm) +
  
  ggtitle("With Outliers")

plot1










# REGRESSION TREE MODEL #### 

model_ads <- model_ads %>% filter( id != '133FDIIB')


rtree <- rpart(log_monetary ~., data = model_ads %>% select(-id), method = "anova")

# PLOTTING THE TREE 
prp(rtree,  type = 4)

# GETTING R2 
tmp <- printcp(rtree)
rsq.val <- 1-tmp[,c(3,4)]  
rsq.val[nrow(rsq.val),] # 65.24%

# CALCULATING RMSE 
predictedSalary_tree <- predict(rtree, model_ads)
sqrt(mean( (exp(predictedSalary_tree) - exp(model_ads$log_monetary))^2)) #27.02   
# not better than simple linear regression not testing 



# BOOSTING - ran for 35 mins ####
library(gbm)

model_ads$recency <- as.factor(model_ads$recency)

boost_salary <- gbm(log_monetary ~., data = model_ads %>% select(-id) ,distribution="gaussian",
                    n.trees=100,interaction.depth=4, cv.folds = 5)

print(Sys.time()) 

summary(boost_salary, las = 1.05, order=TRUE, cBars = 10)

# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(boost_salary, method = "cv")
print(best.iter)
summary(boost_salary, n.trees = best.iter)

sqrt(boost_salary$train.error[best.iter])




yhat_boost <- predict(boost_salary, newdata = test_ads %>% select(-log_monetary, -id) , n.trees=99)
sqrt(mean((exp(yhat_boost)-exp(test_ads$log_monetary))^2))
# 5382.7

