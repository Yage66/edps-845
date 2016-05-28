#' @title Using logistic Regression Method to Detect DIF
#'
#' @description This function use logistic regression to detect
#' dichotomous differential item functioning.
#'
#' @details The logistic regression is a commonly used method to
#' dectect DIF.
#'
#' @param Data   matrix or data.frame of scored item responses.
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

  #model2, P(item1 = 1 | x,g)
  model2 <- glm(formula=item ~ total.score + group, family=binomial(link="logit"))

  #model3, P(item1 = 1 | x,g,x*g)
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
    cat("There is significant nonuniform DIF, p =", "and chi-square =")
  }
  if(chi_21 > chisig1){
    p <- 2* pchisq(chi_21[1], 1, lower.tail=F)
    cat("There is significant uniform DIF, p =", "and chi-square =")
  }
  if(chi_31 > chisig2 && chi_21 < chisig1 && chi_31 < chisig2){
    print("There is some form of DIF present")
  }
  if(chi_32 < chisig1 && chi_21 < chisig1 && chi_31 < chisig2 )
  {
    p <- pchisq(chi_31[1], 2, lower.tail=F)
    cat("There is no significant DIF, p =", "and chi-square =")
  }
}


