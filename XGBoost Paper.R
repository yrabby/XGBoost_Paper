##Landslide susceptibility Mapping of three Upazilas of Rangamati District using XGBoost, KNN and Random forest Models
## I have used the code of Dr. Omar AlThuwaynee and Modified it According to my Need
##Uploading the Data
Data=read.csv("Dis2.csv")
Data=data.frame(Data)  # to remove the unwelcomed attributes

# Install packages
install.packages("xgboost")
install.packages("rlang")
install.packages("doSNOW")
install.packages("RStoolbox") 
install.packages("doParallel")
install.packages("Matrix")
install.packages("e1071")

library(xgboost)
library(rgdal)        # spatial data processing
library(raster)       # raster processing
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # plotting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # plotting
library(sp)           # spatial data
library(caret)        # machine laerning
library(doParallel)   # Parallel processing
library(doSNOW)
library(e1071)




##Conversion of Value to YES and NO
Data$Y=ifelse(Data$Y == 1, "yes","no")


## XGBoost function ------------------------------------------------

#Tunning prameters
myControl <- trainControl(method="repeatedcv", 
                          number=10, 
                          repeats=5,
                          returnResamp='all', 
                          allowParallel=TRUE)



tune_grid <- expand.grid(nrounds = 200,           # the max number of iterations INCREASE THE PROCESSING TIME COST
                         max_depth = 6,            # depth of a tree EFFECTIVE OPTIMIZATION
                         eta = 0.3,               # control the learning rate
                         gamma = 0,             # minimum loss reduction required
                         colsample_bytree = 1,  # subsample ratio of columns when constructing each tree
                         min_child_weight = 1,     # minimum sum of instance weight (hessian) needed in a child 
                         subsample = 1)          # subsample ratio of the training instance


# Step 5 modeling
set.seed(849)
names(Data)
fit.xgb_Landslides<- train(Y~., 
                      data=Data,
                      method = "xgbTree",
                      metric= "Accuracy",
                      preProc = c("center", "scale"), 
                      trControl = myControl,
                      tuneGrid = tune_grid,
                      tuneLength = 10)
fit.xgb_Landslides$results

fit.xgb_Landslides$resample$Accuracy
X.xgb = varImp(fit.xgb_Landslides)
plot(X.xgb)
#Confusion Matrix - train data
p1<-predict(fit.xgb_Landslides, Data[,c(-1)], type = "raw")
confusionMatrix(p1, as.factor(Data$Y))  # using more deep tree, the accuracy linearly increases! 


######## Hyperparameter----

tune_grid2 <- expand.grid(nrounds = c(200,210),           # the max number of iterations INCREASE THE PROCESSING TIME COST
                          max_depth = c(6,18,22),            # depth of a tree EFFECTIVE OPTIMIZATION
                          eta = c(0.05,0.3,1),               # control the learning rate
                          gamma = c(0,0.01,0.1),             # minimum loss reduction required
                          colsample_bytree = c(0.75,1),  # subsample ratio of columns when constructing each tree
                          min_child_weight = c(0,1,2),     # minimum sum of instance weight (hessian) needed in a child 
                          subsample = c(0.5,1))            # subsample ratio of the training instance
set.seed(849)
fit.xgb_Landslides2<- train(Y~., 
                            data=Data,
                            method = "xgbTree",
                            metric= "Accuracy",
                            preProc = c("center", "scale"), 
                            trControl = myControl,
                            tuneGrid = tune_grid2,
                            tuneLength = 10,
                            importance = TRUE)

summaryRes=fit.xgb_Landslides2$results 
head(summaryRes)
best(summaryRes)
summary(summaryRes)
head(summaryRes[order(summaryRes$Accuracy, decreasing = TRUE),],n=2)  
# Plot
pairs(summaryRes[,c(-9:-11)])
# Save it
write.csv(fit.xgb_Landslides2$results,file = "fit.xgb_train_hyper.csv")#, sep = "",row.names = T)

# Re-run using recommended settings of expand.grid
tune_grid3 <- expand.grid(nrounds = c(210),           # the max number of iterations INCREASE THE PROCESSING TIME COST
                          max_depth = c(18),            # depth of a tree EFFECTIVE OPTIMIZATION
                          eta = c(0.3),               # control the learning rate
                          gamma = c(0),             # minimum loss reduction required
                          colsample_bytree = c(0.75),  # subsample ratio of columns when constructing each tree
                          min_child_weight = c(0),     # minimum sum of instance weight (hessian) needed in a child 
                          subsample = c(.5))
set.seed(849)
fit.xgb_Landslides3<- train(Y~., 
                        data=Data,
                        method = "xgbTree",
                        metric= "Accuracy",
                        preProc = c("center", "scale"), 
                        trControl = myControl,
                        tuneGrid = tune_grid3,
                        tuneLength = 10,
                        importance = TRUE)


fit.xgb_Landslides3$results

X.xgb =  (fit.xgb_Landslides3)
plot(X.xgb)

#Confusion Matrix - train data
p2_xgb_Landslides3<-predict(fit.xgb_Landslides3, Data[,c(-1)], type = "raw")
confusionMatrix(p2_xgb_Landslides3, as.factor(Data$Y))  # using more deep tree, the accuracy linearly increases! 
#while increase the iterations to 220 double the processing time with slight accuracy improvment!




#Produce LSM map using Training model results and Raster layers data

# Import Raster
install.packages("raster")
install.packages("rgdal")
library(raster)
library(rgdal)


# load all the data

# Load the Raster data
ELEVATION = raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Elevation.tif")  
SLOPE= raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Slope1.tif") 
PLAN= raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/FR_Plan.tif") 
PROFILE= raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Profile.tif") 
TWI= raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_TWI.tif")  
SPI=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_SPI.tif") 
ASPECT=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Aspect.tif")
LANDUSE=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_landuse.tif")
DRAINAGE=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Drainage.tif")
CHANGE=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_change.tif")
NDVI=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_NDVI1.tif")
RAINFALL=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Rainfall.tif")
ROAD=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Road.tif")
GEOLOGY=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Geology.tif")
FAULTLINES=raster("C:/Rangamati/Corrected Rangamati/Xgboost/Excel files/Fr_Faultlines.tif")

# check attributes and projection and extent
extent(ELEVATION)
extent(SLOPE)
extent(TWI)
extent(SPI)
extent(ASPECT)
extent(PLAN)
extent(LANDUSE)
extent(ROAD)
extent(NDVI)
extent(GEOLOGY)
extent(PROFILE)
extent(FAULTLINES)
extent(DRAINAGE)
extent(RAINFALL)
extent(FAULTLINES)


# if you have diffrent extent, then try to Resample them using the smallest area
ELEVATION_r=resample(ELEVATION,ROAD, resample='bilinear')
SLOPE_r=resample(SLOPE,ROAD, resample='bilinear') 
ASPECT_r=resample(ASPECT,ROAD, resample='bilinear') 
TWI_r=resample(TWI,ROAD, resample='bilinear') 
SPI_r=resample(SPI,ROAD, resample='bilinear') 
DRAINAGE_r=resample(DRAINAGE,ROAD, resample='bilinear') 
PROFILE_r=resample(PROFILE,ROAD, resample='bilinear') 
PLAN_r=resample(PLAN,ROAD, resample='bilinear') 
CHANGE_r=resample(CHANGE,ROAD, resample='bilinear') 
NDVI_r=resample(NDVI,ROAD, resample='bilinear') 
RAINFALL_r=resample(RAINFALL,ROAD, resample='bilinear') 
FAULTLINES_r=resample(ELEVATION,ROAD, resample='bilinear') 
ROAD_r=resample(ROAD,ROAD, resample='bilinear') 
GEOLOGY_r=resample(GEOLOGY,ROAD, resample='bilinear') 
LANDUSE_r=resample(LANDUSE,ROAD, resample='bilinear') 

# write to a new geotiff file
# Create new folder in WD using manually or in R studio (lower right pan)
writeRaster(ASPECT_r,filename="resampled/Aspect.tif", format="GTiff", overwrite=TRUE) 
writeRaster(SPI_r,filename="resampled/SPI.tif", format="GTiff", overwrite=TRUE)
writeRaster(PLAN_r,filename="resampled/Plan.tif", format="GTiff", overwrite=TRUE)
writeRaster(TWI_r,filename="resampled/TWI.tif", format="GTiff", overwrite=TRUE)
writeRaster(ELEVATION_r,filename="resampled/Elevation.tif", format="GTiff", overwrite=TRUE)
writeRaster(SLOPE_r,filename="resampled/Slope.tif", format="GTiff", overwrite=TRUE)
writeRaster(LANDUSE_r,filename="resampled/Landuse.tif", format="GTiff", overwrite=TRUE)
writeRaster(ROAD_r,filename="resampled/Road.tif", format="GTiff", overwrite=TRUE)
writeRaster(GEOLOGY_r,filename="resampled/Geology.tif", format="GTiff", overwrite=TRUE)
writeRaster(NDVI_r,filename="resampled/NDVI.tif", format="GTiff", overwrite=TRUE)
writeRaster(RAINFALL_r,filename="resampled/Rainfall.tif", format="GTiff", overwrite=TRUE)
writeRaster(FAULTLINES_r,filename="resampled/Faultlines.tif", format="GTiff", overwrite=TRUE)
writeRaster(DRAINAGE_r,filename="resampled/Drainage.tif", format="GTiff", overwrite=TRUE)
writeRaster(CHANGE_r,filename="resampled/Change.tif", format="GTiff", overwrite=TRUE)
writeRaster(PROFILE_r,filename="resampled/Prifile.tif", format="GTiff", overwrite=TRUE)

#Stack_List= stack(ASPECT_r,LS_r)#,pattern = "tif$", full.names = TRUE)
#names(Stack_List)
#Stack_List.df = as.data.frame(Stack_List, xy = TRUE, na.rm = TRUE)
#head(Stack_List.df,1)


## stack multiple raster files
Stack_List= list.files(path = "resampled/",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)

names(Rasters)

# 6-1-1 Convert rasters to dataframe with Long-Lat -----------------------
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)

# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


##PRODUCE PROBABILITY MAP
p3<-as.data.frame(predict(fit.xgb_Landslides3, Rasters.df_N, type = "prob"))
summary(p3)
Rasters.df$Levels_yes<-p3$yes
Rasters.df$Levels_no<-p3$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(ELEVATION))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(ELEVATION))

spplot(r_ave_yes, main="Landslides SM using XGB")
writeRaster(r_ave_yes,filename="Prediction_XGBoostTunned_Landslides SM.tif", format="GTiff", overwrite=TRUE) 

spplot(r_ave_no, main="Non Slide XGB")
writeRaster(r_ave_no,filename="Prediction_XGBoostTunned_Non Slide.tif", format="GTiff", overwrite=TRUE) 

# PRODUCE CLASSIFICATION MAP
#Prediction at grid location
p3<-as.data.frame(predict(fit.xgb_Landslides3, Rasters.df_N  , type = "raw"))
summary(p3)
# Extract predicted levels class
head(Rasters.df, n=2)
Rasters.df$Levels_Slide_No_slide<-p3$`predict(fit.xgb_Landslides3, Rasters.df_N, type = "raw")`
head(Rasters.df, n=2)

# Import levels ID file 
ID=read.csv("./Levels_key.csv", header = T)

# Join landuse ID
grid.new<-join(Rasters.df, ID, by="Levels_Slide_No_slide", type="inner") 
# Omit missing values
grid.new.na<-na.omit(grid.new)    
head(grid.new.na, n=2)

#Convert to raster
x<-SpatialPointsDataFrame(as.data.frame(grid.new.na)[, c("x", "y")], data = grid.new.na)
r_ave_Slide_No_slide <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Level_ID")])

# coord. ref. : NA 
# Add coord. ref. system by using the original data info (Copy n Paste).
# borrow the projection from Raster data
proj4string(r_ave_Slide_No_slide)=CRS(projection(ELEVATION)) # set it to lat-long

# Export final prediction map as raster TIF ---------------------------
# write to a new geotiff file
writeRaster(r_ave_Slide_No_slide,filename="Classification_Map XGBoost Tunned SLIDE_NO SLIDE.tif", format="GTiff", overwrite=TRUE) 



#####KNNN

#default search#####

control <- trainControl(method=5 ,'repeatedcv', 
                        number=100, 
                        repeats=3)


set.seed(1)
Data$Y=factor(Data$Y)
knn_grid1 = train(Y~., 
                  data=Data,
                  method = "knn",
                  trControl = control,
                  tuneGrid = expand.grid(k = seq(1, 100, by = 1))
)
plot(knn_grid1, main="KNN with different K values")
plot(varImp(knn_grid1))

# Evaluate the model
p1_knn_grid<-predict(knn_grid1, Data[,c(-1)], type = "raw")
confusionMatrix(p1_knn_grid, as.factor(Data$Y)) 

set.seed(1)
knn_default = train(Y~., 
                    data=Data,
                    method = "knn",
                    trControl = control)

knn_default
plot(knn_default)
plot(varImp(knn_default), main="KNN DEFAULT")

# Evaluate the model
p1_knn_default<-predict(knn_default, Data[,c(-1)], type = "raw")


## stack multiple raster files
Stack_List= list.files(path = "resampled/",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)

names(Rasters)


# 6-1-1 Convert rasters to dataframe with Long-Lat -----------------------
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)


# Now:Prediction using imported Rasters

# check the varaibles names to match with training data
#colnames(Rasters.df)[4] <- "ElEVATION"   # change columns names 
#colnames(Rasters.df)[6] <- "SlOPE"   # change columns names 

#head(Rasters.df[,c(-9,-10)],1)
#head(nn.ce$covariate,1)

Rasters.df_KNN <- Rasters.df[,c(-1,-2)] # remove x, y

# PRODUCE PROBABILITY MAP
p4<-as.data.frame(predict(knn_grid1 , Rasters.df_KNN, type = "prob"))
summary(p4)
Rasters.df$Levels_yes<-p4$yes
Rasters.df$Levels_no<-p4$no
#Rasters.df$Levels_uheal<-p3$UHeal

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(ELEVATION))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(ELEVATION))


# Plot Maps
spplot(r_ave_yes, main="Landslides SM using KNN")
writeRaster(r_ave_yes,filename="Prediction_KNN Tunned_Landslides SM.tif", format="GTiff", overwrite=TRUE) 

spplot(r_ave_no, main="Non Slide KNN")
writeRaster(r_ave_no,filename="Prediction_KNN Tunned_Non Slide.tif", format="GTiff", overwrite=TRUE) 


# PRODUCE CLASSIFICATION MAP
#Prediction at grid location
p4<-as.data.frame(predict(knn_grid1, Rasters.df_KNN, type = "raw"))
summary(p4)
# Extract predicted levels class
head(Rasters.df_KNN, n=2)
Rasters.df$Levels_Slide_No_slide<-p4$`predict(knn_grid1, Rasters.df_KNN, type = "raw")`
head(Rasters.df, n=2)

# Import levels ID file 
ID<-read.csv("./Levels_key.csv", header = T)

# Join landuse ID
grid.new<-join(Rasters.df_KNN, ID, by="Levels_Slide_No_slide", type="inner") 
# Omit missing values
grid.new.na<-na.omit(grid.new)    
head(grid.new.na, n=2)

#Convert to raster
x<-SpatialPointsDataFrame(as.data.frame(grid.new.na)[, c("x", "y")], data = grid.new.na)
r_ave_Slide_No_slide <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Level_ID")])

# coord. ref. : NA 
# Add coord. ref. system by using the original data info (Copy n Paste).
# borrow the projection from Raster data
proj4string(r_ave_Slide_No_slide)=CRS(projection(ELEVATION)) # set it to lat-long

# Export final prediction map as raster TIF ---------------------------
# write to a new geotiff file
writeRaster(r_ave_Slide_No_slide,filename="Classification_Map KNN Tunned SLIDE_NO SLIDE.tif", format="GTiff", overwrite=TRUE) 





##Random Forest
# Install packages
install.packages("RStoolbox")    # Image analysis & plotting spatial data 
library(rgdal)        # spatial data processing
library(raster)       # raster processing
library(plyr)         # data manipulation 
library(dplyr)        # data manipulation 
library(RStoolbox)    # Image analysis & plotting spatial data 
library(RColorBrewer) # color
library(ggplot2)      # plotting
library(sp)           # spatial data
library(caret)        # machine laerning
library(doParallel)   # Parallel processing
library(e1071)        # Naive Bayes


#Creating seperate dataframe for '"LevelsAve" features which is our target.
number.perfect.splits <- apply(X=Data, MARGIN = 2, FUN = function(col){
  t <- table(Data$Y,col)
  sum(t == 0)})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,main="Number of perfect splits vs feature",xlab="",ylab="Feature",las=3,col="wheat") # Slope and SPI are the best classifiers


# Step 1) Default settings

# Define the control
trControl <- trainControl(method='repeatedcv', 
                          repeats=3,
                          number = 10,
                          search = "grid")

#Let's try the build the model with the default values.

set.seed(1234)
# Run the model
rf_defaultN <- train(Y~., 
                     data=Data,
                     method = "rf",
                     metric = "Accuracy",
                     trControl = trControl)
# Print the results
print(rf_defaultN)     
plot(rf_defaultN)
rf_defaultN$finalModel         # Results mtry=8 Number of trees: 500
rf_defaultN$results 

# Step 2) Search best mtry

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 22))
rf_mtry <- train(Y~., 
                 data=Data,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 4,
                 ntree = 500)
print(rf_mtry)
rf_mtry$bestTune$mtry
#You can store it and use it when you need to tune the other parameters.
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 
best_mtry


# Step 3) Search the best maxnodes SKIP

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 30)) {
  set.seed(1234)
  rf_maxnode <- train(Y~., 
                      data=Data,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 4,
                      maxnodes = maxnodes,
                      ntree = 500)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)




fit_rf_final <- train(Y~., 
                      data=Data,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE
)

fit_rf_final
print(fit_rf_final)
varImp(fit_rf_final)
plot(varImp(fit_rf_final), main="RF tuned model")


# Evaluate the model
p1_random<-predict(rf_random, Data[,c(-1)], type = "raw")
confusionMatrix(p1_random, as.factor(Data$Y))  # using more deep tree, the accuracy linearly increases! 







# 6  Produce prediction map using Raster data ---------------------------


# 6-1 Import and process thematic maps ------------------------------------


#Produce LSM map using Training model results and Raster layers data

# Import Raster
install.packages("raster")
install.packages("rgdal")
library(raster)
library(rgdal)


## stack multiple raster files
Stack_List= list.files(path = "resampled/",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)
rm(SPI)
# 6-1-1 Convert rasters to dataframe with Long-Lat -----------------------
#Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y

# PRODUCE PROBABILITY MAP
p6<-as.data.frame(predict(fit_rf_final, Rasters.df_N, type = "prob"))
summary(p6)
Rasters.df$Levels_yes<-p6$yes
Rasters.df$Levels_no<-p6$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_yes")])
proj4string(r_ave_yes)=CRS(projection(ELEVATION))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "Levels_no")])
proj4string(r_ave_no)=CRS(projection(ELEVATION))


# Plot Maps
spplot(r_ave_yes, main="Landslides SM using RF")
writeRaster(r_ave_yes,filename="Prediction_RF Tunned_Landslides SM.tif", format="GTiff", overwrite=TRUE) 

spplot(r_ave_no, main="Non Slide RF")
writeRaster(r_ave_no,filename="Prediction_RF Tunned_Non Slide.tif", format="GTiff", overwrite=TRUE) 


