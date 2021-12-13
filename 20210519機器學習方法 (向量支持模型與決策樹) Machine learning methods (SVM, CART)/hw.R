## hw1
# 請讀入partA_cv.csv
partA_cv <- read.csv(file.choose())

# 
random_index <- sort(sample(100, size=50, replace=F))
A <- partA_cv[random_index, ]
B <- partA_cv[- random_index, ]

# A=train B=test
train <- A
test <- B
fit <- lm(Dose~G1, data=train)
pre_fit <- predict(fit, test)

accuracy <- c(abs(pre_fit - test$Dose) < 0.02)

# A=test B=train
train <- B
test <- A
fit <- lm(Dose~G1, data=train)
pre_fit <- predict(fit, test)

accuracy <- c(accuracy, abs(pre_fit - test$Dose) < 0.02)

# calculate probability of accuracy
prob_accuracy <- sum(accuracy) / length(accuracy)
prob_accuracy



## hw2
choose(30, 4) * choose(19970, 96) / choose(20000, 100)

# import data
partB_cv <- read.csv(file.choose())

# data cleaning
geneset_A <- partB_cv$Geneset.A[!is.na(partB_cv$Geneset.A)]
all_gene <- partB_cv$Identified.genes

# select

match_gene <- c()

for(i in 1:100000){
    samples <- sample(c(1:20000), size = 100)
    match_gene <- c(match_gene, length(intersect(samples, geneset_A)))
}

count <- table(match_gene)

# 計算超幾何機率
prob_hypergeom <- sapply(c(0:(length(count) - 1)), function(x) choose(30, x) * choose(19970, 100 - x) / choose(20000, 100))

result <- cbind(count, hyper_geom_prob)
result

# p < 0.05
sum(count[3:length(count)])


## hw3
library(e1071)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

### import data_file
data <- read.csv(file.choose())
# 取出probe名字
probename <- data$Type
# 轉置
data <- as.data.frame(t(data[, - 1]))
names(data) <- probename
phenotype <- as.factor(rep(c("cancer","normal"), each=60))
data <- cbind(phenotype, data)

### 挑probe
# 儲存顯著的p-value和位點 (注意這邊的是於data的行數)
probe <- c()
for(i in 2:54676){
    p <- t.test(data[1:60, i], data[61:120, i])$p.value
    probe <- rbind(probe, c(i, p))
}
probe <- as.data.frame(probe)
# 大約挑個900個p較小的probe (注意這邊的是於data的行數)
# 這裡隨意改
probe_select <- subset(probe, probe$V2 < 10 ^ (- 15))
probe_input <- probe_select$V1
# 把data只取想要的probe 之後才好放入model
data_pro <- data[, c(1, probe_input)]

### cross validation分2組
C <- sample(c(1:60), size=30)
N <- sample(c(61:120), size=30)
data_A <- data_pro[c(C, N), ]
data_B <- data_pro[- c(C, N), ]

### SVM
# 1
SVM_model1 <- svm(phenotype ~ ., data = data_A, kernel = "linear")
SVM_output1 <- predict(SVM_model1, data_B)
SVM_result1 <- table(SVM_output1, data_B$phenotype, dnn = c("predict", "true"))
# 2
SVM_model2 <- svm(phenotype ~ ., data = data_B, kernel = "linear")
SVM_output2 <- predict(SVM_model2, data_A)
SVM_result2 <- table(SVM_output2, data_A$phenotype, dnn = c("predict", "true"))
# result & acc
SVM_result <- SVM_result1 + SVM_result2
SVM_accuracy <- sum(diag(SVM_result)) / sum(SVM_result)
SVM_accuracy

### CART
# 1
CART_model1 <- rpart(phenotype ~ ., data = data_A, minsplit = 5)
CART_output1 <- predict(CART_model1, data_B, type = "class")
CART_result1 <- table(CART_output1, data_B$phenotype, dnn = c("predict", "true"))
rpart.plot(CART_model1)
# 2
CART_model2 <- rpart(phenotype ~ ., data = data_B, minsplit = 5)
CART_output2 <- predict(CART_model2, data_A, type = "class")
CART_result2 <- table(CART_output2, data_A$phenotype, dnn = c("predict", "true"))
rpart.plot(CART_model2)
# result & acc
CART_result <- CART_result1 + CART_result2
CART_accuracy <- sum(diag(CART_result)) / sum(CART_result)
CART_accuracy