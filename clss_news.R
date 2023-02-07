# clss_news.R

# init ----
library(RTextTools)
library(dplyr)

# read data files ----
con <- RMariaDB::dbConnect(RMariaDB::MariaDB(), group = "pve")
res <- dbSendQuery(con, "SELECT * FROM text limit 5000")
news <- RMariaDB::dbFetch(res)
RMariaDB::dbClearResult(res)
RMariaDB::dbDisconnect(con)
news <- news %>% 
  filter(!is.na(article)) %>% 
  filter(!is.na(kategori))

# Run query to get results as dataframe
data <- news[sample(1:nrow(news),size=1500,replace=FALSE),]
data %>%
  group_by(kategori) %>%
  tally()

# train model -----
matrix <- create_matrix(cbind(data$article), removeNumbers=TRUE,
                        stemWords=FALSE, weighting=tm::weightTfIdf)
container <- create_container(matrix,data$kategori,trainSize=1:1124, testSize=1125:1500,virgin=FALSE)
model <- train_model(container,"SVM")
result <- classify_model(container,model)

# save data ----
save.image("news_model.RData")
save(list = c("model","matrix"),file = "~/media/news_model.RData")

# predict ----
# analytics <- create_analytics(container, result)
new_data <- data[1:100,]
pred_mat <- create_matrix(cbind(new_data$article), originalMatrix = matrix, removeNumbers=TRUE,
                        stemWords=FALSE, weighting=tm::weightTfIdf)
pred_cont <- create_container(pred_mat,labels = rep("",100), testSize = 1:100, virgin=FALSE)
pred_df <- classify_model(pred_cont,model)

# confusion matrix ----
lev <- c("Bencana","Imigran","Jenayah","Keselamatan","Ketenteraan","Kewangan","Politik","Sosio-Ekonomi")
levels(pred_df$SVM_LABEL) <- lev
new_data$kategori <- as.factor(new_data$kategori)
levels(new_data$kategori) <- lev
caret::confusionMatrix(table(pred_df$SVM_LABEL,new_data$kategori))

