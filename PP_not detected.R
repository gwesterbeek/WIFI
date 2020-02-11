#Create file with GOOD predictions including filter on signal strenght

temp <- test_PP1_F %>%
  as_tibble() %>% 
  mutate(resid = as.integer(FLOOR) - as.integer(pred_PP1_F_knn)) %>% 
  filter(resid == 0) %>% 
  pivot_longer(cols = c(starts_with("WAP"))) %>% 
  mutate(value = if_else(value > -65, 1, 0)) %>% 
  filter(value == 1) 


#Create datasets for running knn model----

WIFI_training_PP2 <- WIFI_training_PP1

# Select WAP numbers that are > 20 times detected with GOOD strenght----

WIFI_training_PP2_GS <- WIFI_training_PP2 %>% 
                     select(-(1:10),(15:22), (25:26), (37:38), (49:50), (55:58), (63:64), (71:72), (77:79), 86, (89:95), 100, 
                              (105:106), (109:110), (119:120), (125:126), (129:130), (133:137), (146:148), (150:153), (157:160), 
                              (163:165), (170:171), (174:175), (182: 187), (190:202), (205:223), (226:252), (254:257), (264:273), 
                              (275:276), (279:281), 283, (285:287), (289:294), (296:312), (319:328), (330:331), 333, (335:337), 
                              339, (341:343), (345:350), (352:368), 373, (376:385), (387:388), (391:393), 395, (397:399), (401:404),
                              (406:451), (453:477), (479:480), (482:485), (487:488), (490:494), (497:500), (503:515), (518:520))
                              
WIFI_training_PP2_GS [, 1:161 ][ WIFI_training_PP2_GS[ , 1:161 ] < -65 ] <- -120
WIFI_training_PP2_GS %>%write_rds("WIFI_training_PP2_GS.rds")


WIFI_training_id_PP2_F <- createDataPartition(y = WIFI_training_PP2_GS$FLOOR, 
                                              p = 0.75,  
                                              list = F)
train_PP2_F <- WIFI_training_PP2[WIFI_training_id_PP2_F,]
test_PP2_F <- WIFI_training_PP2[-WIFI_training_id_PP2_F,]


# MODEL KNN PREPROCESS PREDICT FLOOR----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP2_F <- train((FLOOR) ~ .,  
                      data = train_PP2_F %>% select(starts_with("WAP"), FLOOR, -wap_det),
                      method = "knn",  
                      trControl = ctrl)

knnFit_PP2_F
saveRDS(knnFit_PP2_F, file = "knnFit_PP2_F.rds")

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP2_F_knn <- predict(object = knnFit_PP2_F, newdata = train_PP2_F)
train_PP2_F$pred_PP2_F_knn <- train_results_PP2_F_knn

postResample(pred = train_PP2_F$pred_PP2_F_knn, obs = train_PP2_F$FLOOR)
knn_train_PP2_FLOOR <- postResample(pred = train_PP2_F$pred_PP2_F_knn, obs = train_PP2_F$FLOOR)
knn_train_PP2_FLOOR <- write_rds("knn_train_PP2_FLOOR.rds")

## Results on test
test_results_PP2_F_knn <- predict(object = knnFit_PP2_F, newdata = test_PP2_F)
test_PP2_F$pred_PP2_F_knn <- test_results_PP2_F_knn

postResample(pred = test_PP2_F$pred_PP2_F_knn, obs = test_PP2_F$FLOOR)
knn_test_PP2_FLOOR <- postResample(pred = test_PP2_F$pred_PP2_F_knn, obs = test_PP2_F$FLOOR)
knn_test_PP2_FLOOR <-write_rds("knn_test_PP2_FLOOR.rds")


## 2nd error check: visualization of the errors in predicting FLOOR test model knn
treshold_PP1_F <- 0

test_PP2_F %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, FLOOR, pred_PP2_F_knn, PHONEID) %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP2_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP2_F_knn) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point() +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_F, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5)-> Plot_residual_test_PP2_F_knn

plotly::ggplotly(Plot_residual_test_PP2_F_knn)


# MODEL KNN PREPROCESS PREDICT LONGITUDE----

WIFI_training_id_PP2_LO <- createDataPartition(y = WIFI_training_PP2_GS$LONGITUDE, 
                                              p = 0.75,  
                                              list = F)
train_PP2_LO <- WIFI_training_PP2[WIFI_training_id_PP2_LO,]
test_PP2_LO <- WIFI_training_PP2[-WIFI_training_id_PP2_LO,]

# MODEL KNN PREPROCESS PREDICT LONGITUDE----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP2_LO <- train((LONGITUDE) ~ .,  
                      data = train_PP2_LO %>% select(starts_with("WAP"), LONGITUDE, -wap_det),
                      method = "knn",  
                      trControl = ctrl)

knnFit_PP2_LO
saveRDS(knnFit_PP2_LO, file = "knnFit_PP2_F.rds")

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP2_LO_knn <- predict(object = knnFit_PP2_LO, newdata = train_PP2_LO)
train_PP2_LO$pred_PP2_LO_knn <- train_results_PP2_LO_knn

postResample(pred = train_PP2_LO$pred_PP2_LO_knn, obs = train_PP2_LO$LONGITUDE)
knn_train_PP2_LONGITUDE <- postResample(pred = train_PP2_LO$pred_PP2_LO_knn, obs = train_PP2_LO$LONGITUDE)
knn_train_PP2_LONGITUDE %>%  write_rds("knn_train_PP2_LONGITUDE.rds")

## Results on test
test_results_PP2_LO_knn <- predict(object = knnFit_PP2_LO, newdata = test_PP2_LO)
test_PP2_LO$pred_PP2_LO_knn <- test_results_PP2_LO_knn

postResample(pred = test_PP2_LO$pred_PP2_LO_knn, obs = test_PP2_LO$LONGITUDE)
knn_test_PP2_LONGITUDE <- postResample(pred = test_PP2_LO$pred_PP2_LO_knn, obs = test_PP2_LO$LONGITUDE)
knn_test_PP2_LONGITUDE %>%  write_rds("knn_test_PP2_LONGITUDE.rds")


# MODEL KNN PREPROCESS PREDICT LATITUDE----
WIFI_training_id_PP2_LA <- createDataPartition(y = WIFI_training_PP2_GS$LATITUDE, 
                                               p = 0.75,  
                                               list = F)
train_PP2_LA <- WIFI_training_PP2[WIFI_training_id_PP2_LA,]
test_PP2_LA <- WIFI_training_PP2[-WIFI_training_id_PP2_LA,]

# MODEL KNN PREPROCESS PREDICT LATITUDE----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP2_LA <- train((LATITUDE) ~ .,  
                       data = train_PP2_LA %>% select(starts_with("WAP"), LATITUDE, -wap_det),
                       method = "knn",  
                       trControl = ctrl)

knnFit_PP2_LA
saveRDS(knnFit_PP2_LA, file = "knnFit_PP2_LA.rds")

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP2_LA_knn <- predict(object = knnFit_PP2_LA, newdata = train_PP2_LA)
train_PP2_LA$pred_PP2_LA_knn <- train_results_PP2_LA_knn

postResample(pred = train_PP2_LA$pred_PP2_LA_knn, obs = train_PP2_LA$LATITUDE)
knn_train_PP2_LATITUDE <- postResample(pred = train_PP2_LA$pred_PP2_LA_knn, obs = train_PP2_LA$LATITUDE)
knn_train_PP2_LATITUDE %>%  write_rds("knn_train_PP2_LATITUDE.rds")

## Results on test
test_results_PP2_LA_knn <- predict(object = knnFit_PP2_LA, newdata = test_PP2_LA)
test_PP2_LA$pred_PP2_LA_knn <- test_results_PP2_LA_knn

postResample(pred = test_PP2_LA$pred_PP2_LA_knn, obs = test_PP2_LA$LATITUDE)
knn_test_PP2_LATITUDE <- postResample(pred = test_PP2_LA$pred_PP2_LA_knn, obs = test_PP2_LA$LATITUDE)
knn_test_PP2_LATITUDE %>% write_rds("knn_test_PP2_LATITUDE.rds")