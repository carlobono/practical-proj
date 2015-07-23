---

# Practical Machine Learning Project

## Description
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: [http://groupware.les.inf.puc-rio.br/har] (see the section on the Weight Lifting Exercise Dataset)


---

## Data description
The training data for this project are available here: 
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv]

The test data are available here:
[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv]

We have a look at the number of instances and features: 


```r
# After downloading the files
training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')

dim(training)
```

```
## [1] 19622   160
```

---

## Data preprocessing and selection

We first remove near zero variance columns:


```r
library(caret)
rem <- nearZeroVar(training)
length(rem)
```

```
## [1] 60
```

```r
training <- training[-rem]
```

Then we keep scalar columns only, and replace missing with K-nearest neighbour:


```r
ids <- which(lapply(training,class) %in% c('numeric'))
pre <- preProcess(training[,ids], method=c('knnImpute'))
trainingknn <- predict(pre, training[,ids])
trainingknn$classe <- training$classe
```

We keep near-zero variance identifiers (rem) and the preprocessing configuration (ids, pre) for later use (testing).

Then we split in a training and a validation set:


```r
subset <- createDataPartition(trainingknn$classe, p = 0.8, list = FALSE)
train <- trainingknn[subset,]
validate <- trainingknn[-subset,]
```

---

## Model

We choose Random Forest for modeling since it's a robust ML algorithm.
Moreover, cross-validation isn't really necessary in the case of RF.


```r
library(randomForest)
mod <- randomForest(classe~., data=train, importance=TRUE, ntrees=5)
```

---

## Accuracy

In-sample accuracy:


```r
res <- predict(mod,train)
confusionMatrix(res,train$classe)$overall[1]
```

```
## Accuracy 
##        1
```

Validation set accuracy:


```r
resval <- predict(mod,validate)
confusionMatrix(resval,validate$classe)$overall[1]
```

```
##  Accuracy 
## 0.9872547
```


---

## Testset

We must perform the same transformations for the test set (only numeric columns, KNN replacement for missing).


```r
testing<- testing[-rem]
restest <- predict(pre, testing[,ids])
resfinal <- predict(mod,restest)
resfinal
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

And finally we write output as described


```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(as.character(resfinal))
```


---
