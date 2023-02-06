# clss_news.R

# init ----
library(RTextTools)
library(dplyr)

# read data files ----
mypath <- paste0(getwd(),"/zip")
filenames <- list.files(path=mypath, pattern="*.zip", full.names=TRUE)
df_lst <- lapply(filenames[], function(x) readr::read_delim(x,"|"))

# prep df container ----
# news <- data.table::rbindlist(df_lst, fill = TRUE)
# data <- news[nchar(news$article) > 35 & !is.na(news$headlines) & !is.na(news$kategori)]
# data <- data[sample(1:3200,size=100,replace=FALSE),]
data <- news[sample(1:2483,size=1000,replace=FALSE),]
matrix <- create_matrix(cbind(data$article), removeNumbers=TRUE,
                        stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix,data$kategori,trainSize=1:750, testSize=751:1000,virgin=FALSE)

# train model -----
model <- train_model(container,"SVM")
result <- classify_model(container,model)

# save data ----
save.image("news_model.RData")
save(list = c("model","matrix"),file = "~/R/Media/news_model.RData")

# predict ----
# analytics <- create_analytics(container, result)
new_data <- data[1:25,]
pred_mat <- create_matrix(cbind(new_data$article), originalMatrix = matrix, removeNumbers=TRUE,
                        stemWords=FALSE, weighting=tm::weightTfIdf)
pred_cont <- create_container(pred_mat,labels = rep("",24), testSize = 1:24, virgin=FALSE)
pred_df <- classify_model(pred_cont,model)

# confusion matrix ----
conf_mat <- caret::confusionMatrix(pred_df$SVM_LABEL,new_data$kategori)


