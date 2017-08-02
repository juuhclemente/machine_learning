Intro
=====

Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

Data Source
-----------

The training data for this project are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source: <http://groupware.les.inf.puc-rio.br/har>

Data processing
===============

Libraries

Import training data
--------------------

``` r
# Read the training dataset and set "#DIV/0!" as NA
train <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
                  , na.strings=c("NA","#DIV/0!",""), stringsAsFactors = FALSE) 

#Variável de Interesse: Classe = 'A'
#summary(train)
```

Eliminate first six rows related to data identification variables (id, timestamps...)

``` r
train <- train[, -(1:6)]
```

Partition Training Set
----------------------

Partioning Training data set into two data sets, 60% for training, 40% for testing, to allow for cross validation

``` r
inTrain <- createDataPartition(y=train$classe, p=0.6, list=FALSE)
training <- train[inTrain, ]
dim(training)
```

    ## [1] 11776   154

``` r
testing <-  train[-inTrain, ]
dim(testing)
```

    ## [1] 7846  154

Cleaning Data set
-----------------

Eliminate variables with near zero variance

``` r
nearZero <- nearZeroVar(training, saveMetrics=TRUE)

training <- training[, !nearZero$nzv]

# Apply the same to the testing set
testing <-  testing[, !nearZero$nzv]
```

Eliminate variables with too many NAs (consider too many as more than 70% of NAs)

``` r
treshold <- nrow(training) * 0.7
#Remove columns with more than 70% of NA or "" values
goodColumns <- !apply(training, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)

training <- training[, goodColumns]

# Apply the same to the testing set
testing <-  testing[, goodColumns]

training$classe <- as.factor(training$classe)
```

Prediction with Random Forest Model
===================================

Construct model
---------------

``` r
forestPrediction <- randomForest(classe ~., data=training, type="class")
```

Check error estimates

``` r
trainResults <- predict(forestPrediction,newdata=training)

## Use a confusion matrix to get the insample error
confusionMatrix(trainResults,training$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 3348    0    0    0    0
    ##          B    0 2279    0    0    0
    ##          C    0    0 2054    0    0
    ##          D    0    0    0 1930    0
    ##          E    0    0    0    0 2165
    ## 
    ## Overall Statistics
    ##                                      
    ##                Accuracy : 1          
    ##                  95% CI : (0.9997, 1)
    ##     No Information Rate : 0.2843     
    ##     P-Value [Acc > NIR] : < 2.2e-16  
    ##                                      
    ##                   Kappa : 1          
    ##  Mcnemar's Test P-Value : NA         
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
    ## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
    ## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
    ## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
    ## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

Validation of the model
-----------------------

Now, lets test our model performance applying it to the test set

``` r
testingResults <- predict(forestPrediction, testing)
print(confusionMatrix(testingResults, testing$classe))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2231    6    0    0    0
    ##          B    0 1511    3    0    0
    ##          C    0    1 1361   16    0
    ##          D    0    0    4 1269    6
    ##          E    1    0    0    1 1436
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9952          
    ##                  95% CI : (0.9934, 0.9966)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9939          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9996   0.9954   0.9949   0.9868   0.9958
    ## Specificity            0.9989   0.9995   0.9974   0.9985   0.9997
    ## Pos Pred Value         0.9973   0.9980   0.9877   0.9922   0.9986
    ## Neg Pred Value         0.9998   0.9989   0.9989   0.9974   0.9991
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2843   0.1926   0.1735   0.1617   0.1830
    ## Detection Prevalence   0.2851   0.1930   0.1756   0.1630   0.1833
    ## Balanced Accuracy      0.9992   0.9975   0.9961   0.9926   0.9978

The cross validation accuracy is about 99.5% with high levels of sensitivity by class, which seems very good.
