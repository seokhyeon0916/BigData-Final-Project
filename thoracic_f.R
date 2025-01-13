# 필요한 패키지 설치
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
if (!requireNamespace("ROSE", quietly = TRUE)) install.packages("ROSE")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# 라이브러리 로드
library(caret)
library(randomForest)
library(e1071)
library(ROSE)
library(ggplot2)
library(dplyr)

# 데이터 로드
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00277/ThoraricSurgery.arff"
data <- foreign::read.arff(url)

binary_vars <- c("PRE7", "PRE8", "PRE9", "PRE10", "PRE11", 
                 "PRE17", "PRE19", "PRE25", "PRE30", "PRE32", "Risk1Yr")
data[binary_vars] <- lapply(data[binary_vars], function(x) ifelse(x == "T", 1, 0))

library(caret)
dummies <- dummyVars(~ . - Risk1Yr, data = data)  # Risk1Yr 제외
data_transformed <- data.frame(predict(dummies, newdata = data))
data_transformed$Risk1Yr <- data$Risk1Yr  # 팩터로 복원

data$PRE4 <- (data$PRE4 - min(data$PRE4)) / (max(data$PRE4) - min(data$PRE4))  # Min-Max 정규화
data$PRE5 <- scale(data$PRE5)  # 표준화
data$Risk1Yr <- as.factor(data$Risk1Yr)

str(data_transformed)
summary(data)


# 생존 여부 막대 그래프
ggplot(data, aes(x = factor(Risk1Yr), fill = factor(Risk1Yr))) +
  geom_bar() +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  labs(title = "Survival After Surgery", x = "Survival (1 Year)", y = "Count") +
  theme_minimal()
  
  # ROSE로 데이터 불균형 해결
set.seed(42)
rose_data <- ROSE::ovun.sample(
  Risk1Yr ~ ., 
  data = data, 
  method = "both", 
  N = nrow(data) * 2 
)$data

table(rose_data$Risk1Yr)
  
  # 데이터 분할
set.seed(42)
train_index <- createDataPartition(rose_data$Risk1Yr, p = 0.7, list = FALSE)
train_data <- rose_data[train_index, ]
test_data <- rose_data[-train_index, ]

# 랜덤 포레스트 모델 학습
rf_model <- randomForest(Risk1Yr ~ ., data = train_data, ntree = 100, importance = TRUE)

rf_predictions <- predict(rf_model, newdata = test_data)

rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$Risk1Yr)
print(rf_conf_matrix)
varImpPlot(rf_model)

# SVM 모델 학습
svm_model <- svm(Risk1Yr ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1)
svm_predictions <- predict(svm_model, newdata = test_data)
svm_conf_matrix <- confusionMatrix(svm_predictions, test_data$Risk1Yr)
print(svm_conf_matrix)

# 로지스틱 회귀 모델 학습
log_model <- glm(Risk1Yr ~ ., data = train_data, family = binomial)
log_probabilities <- predict(log_model, newdata = test_data, type = "response")
log_predictions <- ifelse(log_probabilities > 0.5, 1, 0)
log_conf_matrix <- confusionMatrix(factor(log_predictions, levels = c(0, 1)), test_data$Risk1Yr)
print(log_conf_matrix)






# SVM 하이퍼파라미터 튜닝
svm_grid <- expand.grid(
  C = c(0.1, 1, 10, 100),  
  sigma = c(0.01, 0.1, 1)  
)

set.seed(42)
svm_control <- trainControl(method = "cv", number = 5)
svm_tuned <- train(
  Risk1Yr ~ ., 
  data = train_data, 
  method = "svmRadial", 
  trControl = svm_control, 
  tuneGrid = svm_grid
)
print(svm_tuned$bestTune)

svm_predictions <- predict(svm_tuned, newdata = test_data)
svm_conf_matrix <- confusionMatrix(svm_predictions, test_data$Risk1Yr)
print(svm_conf_matrix)


#로지스틱 튜닝
log_grid <- expand.grid(
  alpha = c(0, 0.5, 1),       
  lambda = c(0.01, 0.1, 1, 10, 100)  
)

log_control <- trainControl(method = "cv", number = 5)

set.seed(42)
log_tuned <- train(
  Risk1Yr ~ ., 
  data = train_data, 
  method = "glmnet", 
  trControl = log_control, 
  tuneGrid = log_grid
)

print(log_tuned$bestTune)

log_predictions <- predict(log_tuned, newdata = test_data)

log_conf_matrix <- confusionMatrix(log_predictions, test_data$Risk1Yr)
print(log_conf_matrix)





#변수 선택 및 학습 ##############

# 필요한 패키지 설치 및 로드
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
if (!requireNamespace("ROSE", quietly = TRUE)) install.packages("ROSE")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(caret)
library(randomForest)
library(e1071)
library(ROSE)
library(ggplot2)
library(dplyr)

# 데이터 로드
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00277/ThoraricSurgery.arff"
data <- foreign::read.arff(url)

binary_vars <- c("PRE7", "PRE8", "PRE9", "PRE10", "PRE11",
                 "PRE17", "PRE19", "PRE25", "PRE30", "PRE32", "Risk1Yr")
data[binary_vars] <- lapply(data[binary_vars], function(x) ifelse(x == "T", 1, 0))

library(caret)
dummies <- dummyVars(~ . - Risk1Yr, data = data)  # Risk1Yr 제외
data_transformed <- data.frame(predict(dummies, newdata = data))
data_transformed$Risk1Yr <- data$Risk1Yr  # 팩터로 복원

# PRE4와 PRE5를 정규화하고 소수점 아래 2자리로 반올림
data$PRE4 <- round((data$PRE4 - min(data$PRE4)) / (max(data$PRE4) - min(data$PRE4)), 2)
data$PRE5 <- round((data$PRE5 - min(data$PRE5)) / (max(data$PRE5) - min(data$PRE5)), 2)

data$PRE6 <- factor(data$PRE6, levels = c("PRZ0", "PRZ1", "PRZ2"), ordered = TRUE)
data$PRE6 <- as.numeric(data$PRE6) - 1

data$PRE14 <- factor(data$PRE14, levels = c("OC11", "OC12", "OC13", "OC14"), ordered = TRUE)
data$PRE14 <- as.numeric(data$PRE14) - 1

#data$PRE5 <- scale(data$PRE5)  # 표준화
data$Risk1Yr <- as.factor(data$Risk1Yr)
data <- data %>%
  mutate(AGE = case_when(
    AGE >= 20 & AGE < 30 ~ "20-29",
    AGE >= 30 & AGE < 40 ~ "30-39",
    AGE >= 40 & AGE < 50 ~ "40-49",
    AGE >= 50 & AGE < 60 ~ "50-59",
    AGE >= 60 & AGE < 70 ~ "60-69",
    AGE >= 70 & AGE < 80 ~ "70-79",
    AGE >= 80 & AGE < 90 ~ "80-89",
    TRUE ~ as.character(AGE) # 조건에 맞지 않는 경우 원래 값 유지
  )) %>%
  mutate(AGE = factor(AGE, levels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89")))

str(data_transformed)
summary(data)


# 생존 여부 막대 그래프
ggplot(data, aes(x = factor(Risk1Yr), fill = factor(Risk1Yr))) +
  geom_bar() +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  labs(title = "Survival After Surgery", x = "Survival (1 Year)", y = "Count") +
  theme_minimal()

  # ROSE로 데이터 불균형 해결
set.seed(42)
rose_data <- ROSE::ovun.sample(
  Risk1Yr ~ .,
  data = data,
  method = "both",
  N = nrow(data) * 2
)$data

table(rose_data$Risk1Yr)

# glm 모델로 상관관계 도출
glm_model <- glm(Risk1Yr ~ ., data = rose_data, family = binomial)
summary(glm_model)

# 상관관계 있는 변수 추출 (p-value < 0.05)
significant_vars <- names(which(summary(glm_model)$coefficients[, 4] < 0.05))
significant_vars <- significant_vars[significant_vars != "(Intercept)"]

# significant_vars 확인
print(significant_vars)

# 필요한 라이브러리 로드
library(caret)
library(dplyr)

# 범주형 변수(DGN, AGE)를 더미 변수로 변환
dummies <- dummyVars(~ DGN + AGE, data = rose_data)  # DGN과 AGE의 더미 변수 생성
dummy_data <- data.frame(predict(dummies, newdata = rose_data))

# 기존 데이터와 더미 변수를 결합
rose_data_expanded <- cbind(rose_data, dummy_data)

# 기존의 범주형 변수(DGN, AGE) 제거
rose_data_expanded <- rose_data_expanded %>%
  select(-DGN, -AGE)

# glm 결과에서 유의미한 변수 추출
significant_vars <- c("DGNDGN3", "DGNDGN5", "PRE4", "PRE5", "PRE7", "PRE8", 
                      "PRE9", "PRE10", "PRE11", "PRE14", "PRE17", "PRE30", 
                      "AGE40-49", "AGE50-59", "AGE60-69", "AGE70-79")

# selected_data를 구성할 수 있도록 significant_vars와 데이터셋의 교집합을 사용
selected_vars <- intersect(significant_vars, names(rose_data_expanded))

# 유의미한 변수와 Risk1Yr만 선택
selected_data <- rose_data_expanded[, c(selected_vars, "Risk1Yr")]

# 확인
str(selected_data)


# 데이터 분할 (70% 학습 데이터, 30% 테스트 데이터)
set.seed(42)
train_index <- createDataPartition(selected_data$Risk1Yr, p = 0.7, list = FALSE)
train_data <- selected_data[train_index, ]
test_data <- selected_data[-train_index, ]

# 랜덤 포레스트 모델 학습
rf_model <- randomForest(Risk1Yr ~ ., data = train_data, ntree = 100)
rf_predictions <- predict(rf_model, test_data)
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$Risk1Yr)
print(rf_conf_matrix)

# SVM 모델 학습
svm_model <- svm(Risk1Yr ~ ., data = train_data, kernel = "radial")
svm_predictions <- predict(svm_model, test_data)
svm_conf_matrix <- confusionMatrix(svm_predictions, test_data$Risk1Yr)
print(svm_conf_matrix)

# 선형 회귀 모델 학습
lm_model <- glm(Risk1Yr ~ ., data = train_data, family = binomial)
lm_predictions <- predict(lm_model, test_data, type = "response")
lm_pred_class <- ifelse(lm_predictions > 0.5, 1, 0)
lm_conf_matrix <- confusionMatrix(as.factor(lm_pred_class), test_data$Risk1Yr)
print(lm_conf_matrix)

# 랜덤 포레스트 하이퍼파라미터 튜닝
set.seed(42)

# 튜닝 가능한 하이퍼파라미터 설정
rf_grid <- expand.grid(
  mtry = seq(2, ncol(train_data) - 1, 2)  # mtry를 2에서 변수 수의 범위로 설정
)

# 튜닝 설정
rf_control <- trainControl(
  method = "cv",  # 교차 검증
  number = 5,     # 5-fold CV
  verboseIter = TRUE
)

# 랜덤 포레스트 모델 학습 및 튜닝
rf_tuned <- train(
  Risk1Yr ~ .,
  data = train_data,
  method = "rf",
  tuneGrid = rf_grid,
  trControl = rf_control,
  ntree = 100  # 트리 개수 고정
)

# 최적의 하이퍼파라미터 출력
print(rf_tuned$bestTune)

# 최적 모델 예측 및 평가
rf_best_predictions <- predict(rf_tuned, test_data)
rf_best_conf_matrix <- confusionMatrix(rf_best_predictions, test_data$Risk1Yr)
print(rf_best_conf_matrix)

# SVM 하이퍼파라미터 튜닝
set.seed(42)

# 튜닝 가능한 하이퍼파라미터 설정
svm_grid <- expand.grid(
  C = 2^(-2:2),       # C 값의 로그 스케일
  sigma = 2^(-2:2)    # sigma 값의 로그 스케일
)

# 튜닝 설정
svm_control <- trainControl(
  method = "cv",  # 교차 검증
  number = 5,     # 5-fold CV
  verboseIter = TRUE
)

# SVM 모델 학습 및 튜닝
svm_tuned <- train(
  Risk1Yr ~ .,
  data = train_data,
  method = "svmRadial",
  tuneGrid = svm_grid,
  trControl = svm_control
)

# 최적의 하이퍼파라미터 출력
print(svm_tuned$bestTune)

# 최적 모델 예측 및 평가
svm_best_predictions <- predict(svm_tuned, test_data)
svm_best_conf_matrix <- confusionMatrix(svm_best_predictions, test_data$Risk1Yr)
print(svm_best_conf_matrix)


# 릿지와 라쏘 회귀를 위한 glmnet 튜닝
if (!requireNamespace("glmnet", quietly = TRUE)) install.packages("glmnet")
library(glmnet)

# glmnet을 위한 데이터 준비 (x, y 형식)
x <- as.matrix(train_data[, -ncol(train_data)])
y <- as.factor(train_data$Risk1Yr)

# 교차 검증을 통한 최적 람다 값 찾기
cv_glmnet <- cv.glmnet(
  x = x,
  y = y,
  alpha = 1,  # 라쏘 회귀 (alpha = 0이면 릿지 회귀)
  family = "binomial"
)

# 최적 람다 값 출력
print(cv_glmnet$lambda.min)

# 최적 모델로 예측 및 평가
test_x <- as.matrix(test_data[, -ncol(test_data)])
glmnet_predictions <- predict(cv_glmnet, newx = test_x, s = "lambda.min", type = "class")
glmnet_conf_matrix <- confusionMatrix(glmnet_predictions, test_data$Risk1Yr)
print(glmnet_conf_matrix)