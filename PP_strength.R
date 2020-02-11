WIFI_training_PP3 <- WIFI_training_PP1

WIFI_training_PP3 [, 1:520 ][ WIFI_training_PP3[ , 1:520 ] < -65 ] <- -120
WIFI_training_PP3$wap_det <- rowSums(WIFI_training_PP3[,1:520] != "-120")

# Create training and testset PreProcess---------------------------------------------
# CARET PIPELINE PREDICT FLOOR
WIFI_training_id_PP3_F <- createDataPartition(y = WIFI_training_PP3$FLOOR, 
                                             p = 0.75,  
                                             list = F)
train_PP3_F <- WIFI_training_PP3[WIFI_training_id_PP3_F,]
test_PP3_F <- WIFI_training_PP3[-WIFI_training_id_PP3_F,]


# MODEL KNN PREPROCESS PREDICT FLOOR----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP3_F <- train((FLOOR) ~ .,  
                     data = train_PP3_F %>% select(starts_with("WAP"), FLOOR, -wap_det),
                     method = "knn",  
                     trControl = ctrl,
                     preProcess = c("scale")) 

knnFit_PP3_F


## 1st error check: Accuracy and kappa

## Results on train
train_results_PP3_F_knn <- predict(object = knnFit_PP3_F, newdata = train_PP3_F)
train_PP3_F$pred_PP3_F_knn <- train_results_PP3_F_knn

postResample(pred = train_PP3_F$pred_PP3_F_knn, obs = train_PP3_F$FLOOR)
knn_train_PP3_FLOOR <- postResample(pred = train_PP3_F$pred_PP3_F_knn, obs = train_PP3_F$FLOOR)

## Results on test
test_results_PP3_F_knn <- predict(object = knnFit_PP3_F, newdata = test_PP3_F)
test_PP3_F$pred_PP3_F_knn <- test_results_PP3_F_knn

postResample(pred = test_PP3_F$pred_PP3_F_knn, obs = test_PP3_F$FLOOR)
knn_test_PP3_FLOOR <- postResample(pred = test_PP3_F$pred_PP3_F_knn, obs = test_PP3_F$FLOOR)



















WIFI_training_PP3 %>% 
  as.tibble() %>% 
  pivot_longer(cols = c(starts_with("WAP"))) %>% 
  mutate(value = if_else(value > -65, 1, 0)) %>% 
  filter(value == 1) -> WIFI_training_PP3


