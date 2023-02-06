# caret_news.R

# init ----
library(RTextTools)
library(dplyr)

# read data files ----
mypath <- paste0(getwd(),"/zip")
filenames <- list.files(path=mypath, pattern="*.zip", full.names=TRUE)
df_lst <- lapply(filenames[], function(x) readr::read_delim(x,"|"))

# Create the training and test datasets ----
news <- data.table::rbindlist(df_lst, fill = TRUE)
news <- news %>% 
  filter(!is.na(article)) %>% 
  filter(nchar(article)>25) %>% 
  filter(kategori %in% c("Politik","Sosio-Ekonomi","Bencana","Jenayah","Imigran","Keselamatan","Ketenteraan"))
set.seed(100)

# Step 1: Get row numbers for the training data
# data <- news[sample(1:3100,size=100,replace=FALSE),]
trainRowNumbers <- caret::createDataPartition(news$kategori, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
data <- news[trainRowNumbers,]

# train model -----
matrix <- create_matrix(data$article, removeNumbers = TRUE, weighting=tm::weightTfIdf, removeStopwords = TRUE)
container <- create_container(matrix, data$kategori,trainSize=1:1862, testSize=1863:2483,virgin = FALSE)
model <- train_model(container,"SVM")
result <- classify_model(container,model)

# save data ----
save.image("news_model.RData")
save(list = c("model","matrix"),file = "~/R/Media/news_model.RData")

# predict ----
# analytics <- create_analytics(container, result)
matrix <- create_matrix(data$article, removeNumbers = TRUE, weighting=tm::weightTfIdf, 
                        removeStopwords = TRUE)
container <- create_container(matrix, data$kategori,trainSize=1:1862, testSize=1863:2483,virgin = FALSE)
model <- train_model(container,"SVM")
new_data <- data[sample(1:2400,size=25, replace=FALSE)]
pred_mat <- create_matrix(new_data$article, originalMatrix = matrix, removeNumbers=TRUE, removeStopwords = TRUE,
                        stemWords=FALSE, weighting=tm::weightTfIdf)
pred_cont <- create_container(pred_mat,labels = rep("",25), testSize = 1:25, virgin=FALSE)
pred_df <- classify_model(pred_cont,model)

# confusion matrix ----
conf_mat <- caret::confusionMatrix(table(pred_df$SVM_LABEL,new_data$kategori))


