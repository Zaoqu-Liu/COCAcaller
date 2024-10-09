#' @title Identify COCA Subtype from miRNA Expression in Chronic Pancreatitis
#' @description This function identifies the COCA subtype from miRNA expression data in chronic pancreatitis patients. It uses a pre-trained multi-layer perceptron classifier (mlpcla) to predict the probability of each sample belonging to one of the COCA subtypes (COCA1, COCA2, COCA3) or being normal.
#' @author Zaoqu Liu; Email: liuzaoqu@163.com
#' @param data A miRNA expression matrix or dataframe with sample rows and gene columns. Each row represents a sample and each column represents a gene.
#' @return A dataframe with the predicted probabilities for each COCA subtype and the normal category. Additionally, it includes a column indicating the predicted group (Normal, COCA1, COCA2, COCA3) with the highest probability for each sample.
#' @details This function takes a miRNA expression dataset as input and applies a pre-trained multi-layer perceptron classifier (mlpcla) to predict the subtype of chronic pancreatitis for each sample. The function outputs a dataframe with the probabilities of each sample being classified as Normal, COCA1, COCA2, or COCA3, along with the predicted group with the highest probability. The classifier should be trained prior to using this function.
#' @export
COCAcaller <- function(data) {
  tmp <- as.data.frame(predict(mlpcla, data))
  colnames(tmp) <- c("Normal", "COCA1", "COCA2", "COCA3")
  tmp$Group <- apply(tmp, 1, \(x) {
    colnames(tmp)[which.max(x)]
  })
  colnames(tmp)[1:4] <- paste0(colnames(tmp)[1:4], "_prob")
  return(tmp)
}
