library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
#library(mapedit)
library(mapview)
library(caret)
library(forcats)

sentinel<-stack('data/sentinel.grd')
training<- read_sf('data/training.shp')

#printing an dunderstanding the data
print(sentinel)
print(training)

#lest's plot the sentinel where we see the all bands
plot(sentinel) #this plots all the bdands in this ratsertsack

#then we plot the layerstack as an rgb using the necessary band combinition
#then adding abasemap of type esriworld imagery
#then adding mapview of training data
viewRGB(sentinel,r=3,g=2,b=1,map.types = "Esri.worldImagery")+
  mapview(training)

#lets extract raster information to prepare the data for modeling
#use the extract function to extract pixel values in the polygon training
#and then generates a table por pixel polygons
extr<- extract(sentinel,training,df=TRUE)


#fixing land cover type by merging the extracted pixels
#and the training data by use of ids
extr<- merge(extr,training, by.x="ID",by.y="id")

#lest explore the data
head(extr)
nrow(extr)#getting the total number of rows.

#then lets split the dataset for samples to use for model
#training and data for model validation
set.seed(100)

#using createdatapartisioin from caret to split the dataset
#p=0,3 indicates 30% of the data will be used as the training data
#extr$class enable equal distribution of clases in this partition
trainids<-createDataPartition(extr$class,list=FALSE,p=0.3)

#we get the idsof row numbers of the data that 
#have been selected to specific partitions
head(trainids)

#lets now split the dataset based on the ids above
trainDat<- extr[trainids,]

#using the - to indicate the rest of data to be used as
#the test data
testDat<-extr[-trainids,]
nrow(trainDat)
nrow(testDat)

#visualizing relationship
#eg plotting a box plot relating yellowness and each of the land cover type.
#we can now see how different land cover types differ interms of yellowness
#
boxplot(trainDat$yellowness~trainDat$class,las=2)
boxplot(trainDat$redband~trainDat$class,las=2)#using a different atribute

#we can also get an imperension about the separability of the classes in the scatter plot of the sentinel channels
featurePlot(x=trainDat[,c("B03","B04","B08","yellowness")],
            y=factor(trainDat$class),
            plot = "pairs",
            auto.key=list(columns=4))

#actual model trainig
#define the predictor variables we want to use
#this is done if the satellite data is no actually continous
predictors<- c("B01","B02","B03","B04","B05","B06","B07",
               "B08","BBA","yellowness","NDVI")
response<- "class" #this is the landcover column name


#ransom forest modeling
set.seed(100)
model<- train(trainDat[,predictors],trainDat[,response],method="rf",
              trControl = trainControl(method = "Cv"),importance=TRUE)

print(model)
plot(model)

#now we can plot the model importance
plot(varImp(model))

#model prediction
#we then use the model to make spatial predictions
#hence for classifying the entire sentinel scene.
#therefore the model is apploed on the full raster stack using the predict function from the raster package

prediction<- predict(sentinel,model)

#we can now make a map from the prediction and specify variouse of the predicted land cover types
spplot(prediction,col.regions=c("brown","darkgreen","black","yellow",
                                "green","white","red","blue"))


#Model validation
#After we  we classify the scene we want to know the performance of
#the model or the accuracy of the land cover map 
#therefore we use the model to make predictions for the test data(the data pixels that have not been used for training)
#then we compare these predictions with the true values in the contogemcy table.

pred_valid<- predict(model,testDat)
table(testDat$class,pred_valid)
