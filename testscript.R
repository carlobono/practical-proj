#require(knitr) # required for md to html
#require(markdown) # required for md to html
#knit('practical-proj.Rmd', 'practical-proj.md') # creates md file
#markdownToHTML('practical-proj.md', 'practical-proj.html')

# Data description
training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')

dim(training)

# Data preprocessing and selection

library(caret)
rem <- nearZeroVar(training)
length(rem)
training <- training[-rem]

# Replace missing with KNN
ids <- which(lapply(training,class) %in% c('numeric'))
pre <- preProcess(training[,ids], method=c('knnImpute'))
trainingknn <- predict(pre, training[,ids])
trainingknn$classe <- training$classe


set.seed(111)
subset <- createDataPartition(trainingknn$classe, p = 0.8, list = FALSE)
train <- trainingknn[subset,]
validate <- trainingknn[-subset,]

# Modeling
library(randomForest)
mod <- randomForest(classe~., data=train, importance=TRUE, ntrees=5)

# Validation
res <- predict(mod,train)
confusionMatrix(res,train$classe)$overall[1]

resval <- predict(mod,validate)
confusionMatrix(resval,validate$classe)$overall[1]

# Testset
testing<- testing[-rem]
restest <- predict(pre, testing[,ids])
resfinal <- predict(mod,restest)
resfinal

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(as.character(resfinal))
