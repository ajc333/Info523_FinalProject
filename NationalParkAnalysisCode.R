#load all libraries
library(tidyverse)
library(maps)
library(mapproj)
library(stringr)
library(tidyr)
library(dplyr)
library(stringr)
library(plyr)
library(rjson)

#########################################################
##Data set with park characteristics
json_file <- "https://query.data.world/s/35owtjlnromxcesiodsyffd6nnmjha"
park_data <- fromJSON(paste(readLines(json_file), collapse=""))
park_data <- lapply(park_data, data.frame, stringsAsFactors = FALSE)
#make list of lists into dataframe
park_data <-rbind.fill(park_data)
#reorder columns
park_data<- park_data[,c(14,15,13,12,19,21,16,1,2,3,4,5,6,7,8,9,10,11)]
#make columns numeric
park_data$visitors <- as.numeric(gsub(",", "", park_data$visitors ))
park_data$area.acres <- as.numeric(gsub(",", "", park_data$area.acres ))
park_data$area.square_km <- as.numeric(gsub(",", "", park_data$area.square_km ))
park_data$size_ranking <-  NA
park_data$size_ranking [order(park_data$area.acres)] <- nrow(park_data):1
#add world heritage site data
whs = read.table('/Users/alexchristensen/Desktop/nps-annual-park-ranking-recreation-visits/world_heritage_site.txt',
                 header = TRUE)
park_data$world_heritage_site = whs$world_heritage_site

#make df with just features of interest, then add elevation and year established
park_data_clean = park_data[,c(1,3,7,8,12, 19,20)]
park_data_clean_edits = separate(park_data_clean, date_established_readable, 
                                 c("day", "month", "year"))
park_data_clean$year_established = as.numeric(park_data_clean_edits$year)
elevation =read.csv('/Users/alexchristensen/Desktop/nps-annual-park-ranking-recreation-visits/elevation.csv')
colnames(elevation)[1] = 'title'
park_data_clean <- merge(x=park_data_clean,y=elevation[,c(1,2)],by='title')
park_data_clean_edits = separate(park_data_clean,Elevation, 
                                 c("elevation", "pt2", "rest"))
park_data_clean_edits$pt2 = gsub("feet", "", park_data_clean_edits$pt2)
park_data_clean$Elevation = as.numeric(str_c(park_data_clean_edits$elevation, "", park_data_clean_edits$pt2))

#convert to all numeric values
park_data_clean$world_heritage_site = as.integer(as.logical(park_data_clean$world_heritage_site))
park_data_clean$StateNumeric = as.numeric(as.factor(park_data_clean$states.title))

#run lasso on park_data_clean

#shuffle data
park_data_clean = park_data_clean[sample(1:nrow(park_data_clean)), ]
#random division, test and train data
set.seed(33)
random_indexes = sort(sample(nrow(park_data_clean),nrow(park_data_clean)*.7))
train = park_data_clean[random_indexes,]
y_train = train$visitors
test = park_data_clean[-random_indexes,]
y_test = test$visitors

#remove true value column and park name and nonnumeric values
train = subset(train, select = -c(visitors, title, states.title,date_established_readable))
test = subset(test, select = -c(visitors,title, states.title,date_established_readable))

#LASSO
library(lars)
fit.lasso <- lars(as.matrix(train),y_train)
set.seed(2016)  # the results depend on the seed for cross validation
lasso.cv<-cv.lars(as.matrix(train),y_train,K=10,type="lasso", mode="fraction",normalize=T)

### minimum CV rule
lasso.mcv<-which.min(lasso.cv$cv)
bests1<-lasso.cv$index[lasso.mcv]
print(bests1)
lasso.coef1 <- predict(fit.lasso, s=bests1, type="coef", mode="frac")
print(lasso.coef1$coef)
keep_cols1 = subset(data.frame(lasso.coef1$coef), lasso.coef1.coef != 0.00)

#train and test
lasso.train1 <- predict(fit.lasso, newx = train, s=bests1, type="fit", mode="frac")
lasso.test1 <- predict(fit.lasso, newx = test, s=bests1, type="fit", mode="frac")
lasso.trainerr1 <- mean((y_train-lasso.train1$fit)^2)
lasso.testerr1 <- mean((y_test-lasso.test1$fit)^2)
cat("LASSO+min CV: train error = " , lasso.trainerr1,", test error = ", lasso.testerr1, "\n")

### one standard devaition rule
bound<-lasso.cv$cv[lasso.mcv]+lasso.cv$cv.error[lasso.mcv]
bests2<-lasso.cv$index[min(which(lasso.cv$cv<bound))]
print(bests2)

lasso.coef2 <- coef(fit.lasso, s=bests2, mode="frac")
print(lasso.coef2) 
keep_cols2 = subset(data.frame(lasso.coef2), lasso.coef2 != 0.00)

lasso.train2 <- predict(fit.lasso, newx = train, s=bests2, type="fit", mode="frac")
lasso.test2 <- predict(fit.lasso, newx = test, s=bests2, type="fit", mode="frac")
lasso.trainerr2 <- mean((y_train-lasso.train2$fit)^2)
lasso.testerr2 <- mean((y_test-lasso.test2$fit)^2)
cat("LASSO+min CV: train error = " , lasso.trainerr2,", test error = ", lasso.testerr2, "\n")

#refines based on the columns selected by LASSO
refined1 = train[ , (names(train) %in% row.names(keep_cols1))]
refined2 = train[ , (names(train) %in% row.names(keep_cols2))] #empty!
lm_refined_model1 = lm(y_train ~. ,data = refined1)
summary(lm_refined_model1)
prediction_modlel1 = predict(lm_refined_model1, test)
print(prediction_modlel1)