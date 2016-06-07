#' @title Using logistic Regression Method to Detect DIF
#'
#' @description This function use logistic regression to detect
#' dichotomous differential item functioning.
#'
#' @details The logistic regression is a commonly used method to
#' dectect DIF.
#'
#' @param Data   Data is the matrix or data.frame of scored item responses, except for
#' the first column which corresponds to group membership (represented by 0s and 1s).
#'
#' @param itemn  Itemn is the specific item that you want to test for the presence of
#' DIF.Because the first column of the data matrix is the group column, so the
#' column  itemn + 1 will be read for item.
#'
#' @param groupcolumn  groupcolumn is the group membership (represented by 0s and 1s).
#' This is set to 1 by default, which represents the first column in the dataset.
#'
#' @param alpha  alpha is used to specificy a confidence level. This is set to .05
#' (a confidence level of .95) by default.
#'
#' @param filename filename is the name of the dataset you want to test for DIF.
#'
#'
#' @export


lrdif <- function(Data, itemn, groupcolumn = 1, alpha = .05){
  total.score <- apply(Data[,-1], 1, sum)
  Data  <- cbind(Data, total.score)
  item  <- Data[, itemn+1]
  group <- Data[, groupcolumn]

  # Run this analysis for item 1 first
  # Model P(item1 = 1 | total.score and group.membership and total.score*group.membership)
  # total.score = x (a continuous variable, the ability estimate) and group.membership = g
  # P (item1 = 1 | x,g,x*g) = e^(z) / (1 + e^z), where z = beta_0 + beta_1 * x + beta_2 * g + beta_3 * x*g

  # Here are three models:
  # model1, P(item1 = 1 | x)
  model1 <- glm(formula=item ~ total.score, family=binomial(link="logit"))

  # model2, P(item1 = 1 | x,g)
  model2 <- glm(formula=item ~ total.score + group, family=binomial(link="logit"))

  # model3, P(item1 = 1 | x,g,x*g)
  model3 <- glm(formula=item ~ total.score + group + total.score * group, family=binomial(link="logit"))

  #Extract the log likelihoods
  lik1 <- logLik(model1)
  lik2 <- logLik(model2)
  lik3 <- logLik(model3)

  #Calculate Chi^2 = 2 [(ln L(model3)) - ln(L(model2))]
  chi_32 <- 2 * (lik3 - lik2)
  chi_21 <- 2 * (lik2 - lik1)
  chi_31 <- 2 * (lik3 - lik1)

  #Comparing these chi-values to the relevant values
  alpha.adj <- alpha / 2
  conf.level <- 1 - alpha.adj
  chisig1 <- qchisq(conf.level,1)
  chisig2 <- qchisq(conf.level,2)

  if(chi_32 > chisig1){
    p <- 2* pchisq(chi_32[1], 1, lower.tail=F)
    cat("There is significant nonuniform DIF, p =", format(p, digits=4),
        "and chi-square =", format(chi_32[1], digits=4), "for model 3 versus model 2.")
  }
  if(chi_21 > chisig1){
    p <- 2* pchisq(chi_21[1], 1, lower.tail=F)
    cat("There is significant uniform DIF, p =", format(p, digits=4),
        "and chi-square =", format(chi_21[1], digits=4), "for model 2 versus model 1.","\n")
  }
  if(chi_31 > chisig2 && chi_21 < chisig1 && chi_31 < chisig2){
    print("There is some form of DIF present","\n")
  }
  if(chi_32 < chisig1 && chi_21 < chisig1 && chi_31 < chisig2 )
  {
    p <- pchisq(chi_31[1], 2, lower.tail=F)
    cat("There is no significant DIF, p =", format(p, digits=4),
        "and chi-square =", format(chi_31[1], digits=4), "for model 3 versus model 1.","\n")
  }
}


 #Testing DIF for all items in the dataset

lrdif_total <- function(filename){

  filedata <- read.table(filename)

  ncol = dim(filedata)[2] - 1

  for(i in 1:ncol){
    lrdif(filedata,i,1,0.05)
  }

}

