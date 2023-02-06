# clss_ny.R

library(RTextTools)
data(NYTimes)
data <- NYTimes[sample(1:3100,size=100,replace=FALSE),]
matrix <- create_matrix(cbind(data["Title"],data["Subject"]), language="english",removeNumbers=TRUE,
                        stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix,data$Topic.Code,trainSize=1:75, testSize=76:100,virgin=FALSE)

# train model and classify
svm_model <- train_model(container,"SVM")
svm_results <- classify_model(container,svm_model)

# analytics
analytics <- create_analytics(container, svm_results)
