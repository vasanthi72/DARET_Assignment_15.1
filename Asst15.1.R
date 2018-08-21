Features_Variant_1 <- read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Training/Features_Variant_1.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_Variant_1)
FV1 <- Features_Variant_1
nrow(FV1)
ncol(FV1)
sum(is.na(FV1))
Features_Variant_2 <- read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Training/Features_Variant_2.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_Variant_2)
FV2 <- Features_Variant_2
nrow(FV2)
ncol(FV2)
sum(is.na(FV2))
Features_TestSet <- read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Testing/Features_TestSet.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_TestSet)
test <- Features_TestSet
nrow(test)
ncol(test)
sum(is.na(test))
colnames(FV1)[c(1:4)] <- c("PPop","PChek","PTalk","PCat")
colnames(FV1)
colnames(FV1)[c(30:54)] <- c("tcbbt","cl24bbt","c4824bbt","cf24ap","diff4824","bt","Polen","PoShare","PoPromo","H","Psun","Pmon","Ptue","Pwed","Pthu","Pfri","Psat","Bsun","Bmon","Btue","Bwed","Bthu","Bfri","Bsat","ncH")
colnames(FV1)
basemodel <- lm(ncH~.,data = FV1)
summary(basemodel)
fitmodel <- step(basemodel)
coefficients(basemodel)
coefficients(fitmodel)
AIC(basemodel)
AIC(fitmodel)
predFV1 <- predict(fitmodel,FV1)
msemodel1 <- mean((FV1$ncH - predFV1)^2)
msemodel1
colnames(test)[c(1:4)] <- c("PPop","PChek","PTalk","PCat")
colnames(test)[c(30:54)] <- c("tcbbt","cl24bbt","c4824bbt","cf24ap","diff4824","bt","Polen","PoShare","PoPromo","H","Psun","Pmon","Ptue","Pwed","Pthu","Pfri","Psat","Bsun","Bmon","Btue","Bwed","Bthu","Bfri","Bsat","ncH")
colnames(test)
testpred <- predict(fitmodel, test)
msetest <- mean((test$ncH - testpred)^2)
msetest
basemodel
ls(basemodel)
cor(FV1$ncH,predFV1)
cor(test$ncH,testpred)
AIC(testpred)
minmaxFV1 <- min_max_accuracy(FV1$ncH,predFV1)
minmaxFV1 <- mean(min(FV1$ncH,predFV1)/max(FV1$ncH,predFV1))
minmaxFV1
minmaxFV1 <- mean(min(test$ncH,testpred)/max(test$ncH,testpred))
minmaxFV1 <- mean(min(FV1$ncH,predFV1)/max(FV1$ncH,predFV1))
minmaxtest <- mean(min(test$ncH,testpred)/max(test$ncH,testpred))
minmaxtest
mapeFV1 <- mean(abs(predFV1-FV1$ncH)/(FV1$ncH))
mapeFV1
mapetest <- mean(abs(testpred - test$ncH)/(test$ncH))
mapetest
FV1$dev <- (predFV1-FV1$ncH)
summary(FV1$dev)
test$dev <- (testpred - test$ncH)
summary(test$dev)
Features_Variant_3 <- read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Training/Features_Variant_3.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_Variant_3)
FV3 <- Features_Variant_3
Features_Variant_4 <- read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Training/Features_Variant_4.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_Variant_4)
FV4 <- Features_Variant_4
Features_Variant_5 <- read.csv("E:/kamagyana/Computing/DARET/Assignments/Dataset/Training/Features_Variant_5.csv", header=FALSE, stringsAsFactors=FALSE)
View(Features_Variant_5)
FV5 <- Features_Variant_5
colnames(FV2)[c(1:4)] <- c("PPop","PChek","PTalk","PCat");colnames(FV2)[c(30:54)] <- c("tcbbt","cl24bbt","c4824bbt","cf24ap","diff4824","bt","Polen","PoShare","PoPromo","H","Psun","Pmon","Ptue","Pwed","Pthu","Pfri","Psat","Bsun","Bmon","Btue","Bwed","Bthu","Bfri","Bsat","ncH")
colnames(FV3)[c(1:4)] <- c("PPop","PChek","PTalk","PCat");colnames(FV3)[c(30:54)] <- c("tcbbt","cl24bbt","c4824bbt","cf24ap","diff4824","bt","Polen","PoShare","PoPromo","H","Psun","Pmon","Ptue","Pwed","Pthu","Pfri","Psat","Bsun","Bmon","Btue","Bwed","Bthu","Bfri","Bsat","ncH")
colnames(FV4)[c(1:4)] <- c("PPop","PChek","PTalk","PCat");colnames(FV4)[c(30:54)] <- c("tcbbt","cl24bbt","c4824bbt","cf24ap","diff4824","bt","Polen","PoShare","PoPromo","H","Psun","Pmon","Ptue","Pwed","Pthu","Pfri","Psat","Bsun","Bmon","Btue","Bwed","Bthu","Bfri","Bsat","ncH")
colnames(FV5)[c(1:4)] <- c("PPop","PChek","PTalk","PCat");colnames(FV5)[c(30:54)] <- c("tcbbt","cl24bbt","c4824bbt","cf24ap","diff4824","bt","Polen","PoShare","PoPromo","H","Psun","Pmon","Ptue","Pwed","Pthu","Pfri","Psat","Bsun","Bmon","Btue","Bwed","Bthu","Bfri","Bsat","ncH")
basemodel2 <- lm(ncH~.,data = FV2)
basemodel3 <- lm(ncH~.,data = FV3)
basemodel4 <- lm(ncH~.,data = FV4)
basemodel5 <- lm(ncH~.,data = FV5)
fitmodel2 <- step(basemodel2)
fitmodel3 <- step(basemodel3)
fitmodel4 <- step(basemodel4)
fitmodel5 <- step(basemodel5)
predFV2 <- predict(fitmodel2,FV2)
msemodel2 <- mean((FV2$ncH - predFV2)^2)
msemodel2
predFV3 <- predict(fitmodel3,FV3)
msemodel3 <- mean((FV2$ncH - predFV3)^2)
msemodel3 <- mean((FV3$ncH - predFV3)^2)
msemodel3
predFV4 <- predict(fitmodel4,FV4)
msemodel4 <- mean((FV4$ncH - predFV4)^2)
msemodel4
predFV5 <- predict(fitmodel5,FV5)
msemodel5 <- mean((FV5$ncH - predFV5)^2)
msemodel5
msemodel1
testpred3 <- predict(fitmodel3,test)
msetest3 <- mean((test$ncH - testpred3)^2)
msetest3
testpred2 <- predict(fitmodel2,test);msetest2 <- mean((test$ncH - testpred2)^2); msetest2
testpred4 <- predict(fitmodel4,test);msetest4 <- mean((test$ncH - testpred4)^2); msetest4
testpred5 <- predict(fitmodel5,test);msetest5 <- mean((test$ncH - testpred5)^2); msetest5
mapeFV2 <- mean(abs(predFV2-FV2$ncH)/(FV2$ncH));mapeFV2
mapeFV3 <- mean(abs(predFV3-FV2$ncH)/(FV3$ncH));mapeFV3
mapeFV3 <- mean(abs(predFV3-FV3$ncH)/(FV3$ncH));mapeFV3
mapeFV4 <- mean(abs(predFV4-FV4$ncH)/(FV4$ncH));mapeFV4
mapeFV5 <- mean(abs(predFV5-FV5$ncH)/(FV5$ncH));mapeFV5