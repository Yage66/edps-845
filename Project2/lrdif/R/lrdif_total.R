#' @title Using logistic Regression Method to Detect DIF for
#' all the items in the dataset
#'
#' @description This function use logistic regression method to detect DIF for
#' all the items in the dataset.
#'
#' @details The logistic regression is a commonly used method to
#' dectect DIF.
#'
#' @param filename filename is the name of the dataset you want to test for DIF.
#'
#' @export

#Testing DIF for all items in the dataset

lrdif_total <- function(filename){

  filedata <- read.table(filename)

  ncol = dim(filedata)[2] - 1

  for(i in 1:ncol){
    lrdif(filedata,i,1,0.05)
  }

}
