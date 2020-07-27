library(kernlab)
data(spam)
set.seed(333)

smallSpam <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (smallSpam$type=="spam") + 1; spamLabel

plot(smallSpam$capitalAve, col = spamLabel)

rule1 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x < 2.4] <- "nonspam"
  prediction[x>=2.4 & x <=2.45] <- "spam" #overfitting
  prediction[x>2.45 & x <=2.7] <- "nonspam"
  return(prediction)
}

t1_train <- table(rule1(smallSpam$capitalAve), smallSpam$type)
t1_train

rule2 <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 2.8] <- "spam"
  prediction[x <= 2.8] <- "nonspam"
  return(prediction)
}

t2_train <- table(rule2(smallSpam$capitalAve), smallSpam$type)
t2_train

t1 <- table(rule1(spam$capitalAve),spam$type)
t2 <- table(rule2(spam$capitalAve),spam$type)

t1[1,1] + t1[2,2]
t2[1,1] + t2[2,2]