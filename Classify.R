
# 1. Loading packages ----
# reference: https://journal.r-project.org/archive/2013/RJ-2013-001/RJ-2013-001.pdf
library(RTextTools)

# 2. Loading data ----
generality <- read.csv("generality_consensus_coding_clean.csv", stringsAsFactors=FALSE)
generality$new_code[generality$new_code == "nc"] <- NA # replacing "nc" with NA
generality <- na.omit(generality) # removing observations with NA
table(generality$new_code) # number of instances of codes

# 3. Creating a matrix ----
doc_matrix <- create_matrix(generality$Why, removeSparseTerms = .999) # can modify removeSparseTerms (higher number = more terms)
str(doc_matrix)

# 4. Analysis ----
nclassifiers <- 5
nfolds <- 10
classifiers <- c("RF", "TREE", "SVM", "MAXENT", "NNET")

for (i in seq(nclassifiers)){
  
  container <- list()
  train_model <- list()
  classify_model <- list()
  analytics <- list()
  precision <- list()
  
  for (j in seq(nfolds)){
    
    test_samp <- sample(seq(nrow(generality)), floor(nrow(generality) / nfolds))
    train_samp <- setdiff(seq(nrow((generality))), test_samp)
    container[[j]] <- create_container(doc_matrix, generality$new_code, trainSize = train_samp, testSize = test_samp, virgin = F)
    train_model[[j]] <- train_model(container[[j]], classifiers[i])
    classify_model[[j]] <- classify_model(container[[j]], train_model[[j]])
    analytics[[j]] <- create_analytics(container[[j]], classify_model[[j]])
    precision[[j]] <- (1 - (sum(analytics[[j]]@document_summary[, 6]) / length(analytics[[j]]@document_summary[, 6])))
    precision_mat <- unlist(precision)
    
  }
  
  print(paste("The mean precision with ", nfolds, " folds for ", classifiers[i], " is ", round(mean(precision_mat), digits = 2), sep =""))
  
}
