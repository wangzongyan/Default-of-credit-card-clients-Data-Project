# This file make use of the training set and build a neural network model
n <- names(train)
f <- as.formula(paste("default ~", paste(n[!n %in% "default"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)