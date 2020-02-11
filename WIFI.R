# -------------------------------------------------------------------------
# GOAL: Select best fitted model for indoor locationing
# DESCRIPTION: WIFI Locationing
# DEVELOPER: Gerhard Westerbeek
# Thu Jan 16 13:31:31 2020 ------------------------------
--------------------------------------------------------------------
# -------------------------------------------------------------------------
library(caret)
#library(plyr)
library(tidyverse)
library(broom)
library(C50)
library(purrr)
library(randomForest)
library(class)
library(modelr)
library(scatterplot3d)
library(lubridate)


# Load dataset ------------------------------------------------------------

WIFI_training <- read.csv("trainingData.csv")
dim(WIFI_training)


# Explore the dataset -----------------------------------------------------

glimpse(WIFI_training)
summary(WIFI_training)
as.tible(WIFI_training)
as.tibble(WIFI_training)
view(WIFI_training)
sum(complete.cases(WIFI_training)) #There are no missing values detected in the dataset
is.na(WIFI_training)
table(is.na(WIFI_training))

# Convert attributes to factor --------------------------------------------
WIFI_training$BUILDINGID <- as.factor(WIFI_training$BUILDINGID)
WIFI_training$FLOOR <- as.factor(WIFI_training$FLOOR)
glimpse(WIFI_training[,523:524])

WIFI_training$SPACEID <- as.factor(WIFI_training$SPACEID)

# Create attribute LOCATIONID ---------------------------------------------
WIFI_training$LOCATIONID <- paste(WIFI_training$BUILDINGID,WIFI_training$FLOOR,WIFI_training$SPACEID, sep = "_")
WIFI_training$LOCATIONID <- as.factor(WIFI_training$LOCATIONID)
nlevels(WIFI_training$LOCATIONID)



# Create attribute of sum of WAPS detected (value != 100)--------------------------------

WIFI_training$wap_det <- rowSums(WIFI_training[,1:520] != "100")
WIFI_training$wap_det<- as.numeric(WIFI_training$wap_det)

# Explore the data --------------------------------------------------------

#- Visualize UJI Campus using ggplot

WIFI_training %>%  ggplot() + 
  geom_point(data = WIFI_training, aes(x = LONGITUDE, y= LATITUDE, group = BUILDINGID, color = BUILDINGID)) 
  
  
scatterplot3d(x = WIFI_training$LONGITUDE , 
                     y = WIFI_training$LATITUDE, 
                     z = WIFI_training$FLOOR, 
                     type = "p", 
                     pch = 12, 
                     angle = 155,
                     color = WIFI_training$FLOOR,
                     highlight.3d = TRUE, 
                     cex.symbols = 0.5, 
                     box = FALSE,
                     grid = FALSE,
                     main = "3D lay-out UJI Campus",
                     xlab = "Longitude",
                     ylab = "Latitude",
                     zlab = "Floor")


#-Plot histogram of WAP detection frequency

ggplot(WIFI_training, aes(x = wap_det)) +
  geom_histogram(fill='blue', binwidth = 2, color='black')+
  scale_x_continuous(breaks=seq(0,100,10)) +
  ggtitle('Frequency Count of WAP detection') +
  xlab('Number of Instances for WAP detection') +
  ylab('Frequency of Observed Count') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))

#-Plot histogram of instance counts at locations
ID_freq <- as.data.frame(table(WIFI_training$LOCATIONID))

gw_temp <- ID_freq %>% 
 # count("LOCATIONID") %>% 
  ggplot(aes(x = Freq)) +
  geom_histogram(fill='blue', binwidth = 2, color='black')+
  scale_x_continuous(breaks=seq(0,100,10)) +
  ggtitle('Frequency Count of Location ID Instances') +
  xlab('Number of Instances for a Location ID') +
  ylab('Frequency of Observed Instance Count') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))

temp_plot <- WIFI_training %>% 
  as_tibble() %>% 
  select(BUILDINGID, LOCATIONID) %>% 
  group_by(BUILDINGID, LOCATIONID) %>% 
  count() %>% 
  ggplot(aes(x = n)) +
  geom_bar(aes(fill = factor(BUILDINGID)), position = "dodge") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  ggtitle('Frequency Count of Location ID Instances') +
  xlab('Number of Instances for a Location ID') +
  ylab('Frequency of Observed Instance Count') +
  theme(text = element_text(size=14)) +
  theme(panel.border=element_rect(colour='black', fill=NA))
plotly::ggplotly(temp_plot)  

#-Distribution of WAP count by building- boxplot
ggplot(WIFI_training, aes(x=as.factor(BUILDINGID), y=wap_det)) + 
  geom_boxplot(fill='lightblue') +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Detected Wireless Access Points by Building') +
  labs(x="Building Number", y= 'WAP Counts' ) +
  theme(panel.border=element_rect(colour='black', fill=NA))

#Distribution of WAP count by building and floor
ggplot(WIFI_training, aes(x=wap_det, fill=as.factor(FLOOR))) + 
  geom_bar() +
  facet_grid(BUILDINGID~.) +
  theme(text = element_text(size=14)) +
  ggtitle('Distribution of Detected Wireless Access Points by Building') +
  labs(x="Number of WAP's Detected by Building", y= 'Counts by Building Floor') +
  theme(panel.border=element_rect(colour='black', fill=NA))


  
#Distribution of detection per WAP number

gw <-  WIFI_training%>% 
  select(-LONGITUDE,-LATITUDE,-FLOOR, -BUILDINGID,-SPACEID,-RELATIVEPOSITION,-USERID,-PHONEID,-TIMESTAMP,-wap_det,-LOCATIONID) %>% 
  gather(WAP001:WAP520, key = WAPNUM, value = Signal) %>% summarise(n=n())


ldply(gw, function(c) sum(c!="100"))  


                
gw1 <-  WIFI_training%>% pivot_longer(starts_with("WAP")) %>% as.tibble() %>% 
  select(name, value) %>%  
  group_by(name) %>% 
  filter(value==100) %>%  
  count() %>% 
  ddply("name",summarise,freqsum=sum(freq)) %>% 
  filter(freqsum==19937)

      

# Explore Predictive Models -----------------------------------------------

# Create dataset wit LOCATIONID and WAPS for model use.

#WIFI_training_set <- WIFI_training %>% select(-LONGITUDE,-LATITUDE,-FLOOR, -BUILDINGID,-SPACEID,-RELATIVEPOSITION,-USERID,-PHONEID,-TIMESTAMP,-wap_det)

###############


# Create training and testset ----------------------------------------------

WIFI_training_id_B <- createDataPartition(y = WIFI_training$BUILDINGID, 
                                     p = 0.75,  
                                     list = F)
train_B <- WIFI_training[WIFI_training_id_B,]
test_B <- WIFI_training[-WIFI_training_id_B,]


# CARET PIPELINE PREDICT FLOOR
WIFI_training_id_F <- createDataPartition(y = WIFI_training$FLOOR, 
                                        p = 0.75,  
                                        list = F)
train_F <- WIFI_training[WIFI_training_id_F,]
test_F <- WIFI_training[-WIFI_training_id_F,]


# CARET PIPELINE PREDICT LONGITUDE
WIFI_training_id_LO <- createDataPartition(y = WIFI_training$LONGITUDE, 
                                          p = 0.75,  
                                          list = F)
train_LO <- WIFI_training[WIFI_training_id_LO,]
test_LO <- WIFI_training[-WIFI_training_id_LO,]


# CARET PIPELINE PREDICT LATITUDE
WIFI_training_id_LA <- createDataPartition(y = WIFI_training$LATITUDE, 
                                           p = 0.75,  
                                           list = F)
train_LA <- WIFI_training[WIFI_training_id_LA,]
test_LA <- WIFI_training[-WIFI_training_id_LA,]

###############

# Fit model "rpart" -------------------------------------------------------

# MODEL RPART PREDICT BUILDING ID----

set.seed(123)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

rpartFit_B <- train((BUILDINGID) ~ .,  
                 data = train_B %>% select(starts_with("WAP"), BUILDINGID, -wap_det),
                 method = "rpart",  
                 tuneLength = 2,         
                 trControl = ctrl) 

rpartFit_B


## 1st error check: Accuracy and kappa


## Results on train
train_results_B_rpart <- predict(object = rpartFit_B, newdata = train_B)
train_B$pred_B_rpart <- train_results_B_rpart

postResample(pred = train_B$pred_B_rpart, obs = train_B$BUILDINGID)
rpart_train_BUILDING <- postResample(pred = train_B$pred_B_rpart, obs = train_B$BUILDINGID)

dotplot(rpart_train_BUILDING)


## Results on test
test_results_B_rpart <- predict(object = rpartFit_B, newdata = test_B)
test_B$pred_B_rpart <- test_results_B_rpart

postResample(pred = test_B$pred_B_rpart, obs = test_B$BUILDINGID)
rpart_test_BUILDING <- postResample(pred = test_B$pred_B_rpart, obs = test_B$BUILDINGID)

rpart_BUILDING <- cbind(rpart_train_BUILDING, rpart_test_BUILDING)
rpart_BUILDING <- as.data.frame(rpart_BUILDING)

rpart_BUILDING %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("rpart_train_BUILDING", "rpart_test_BUILDING"))) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 4)), vjust=5, size = 5) +
  ggtitle('Metrics Rpart BUILDING') +
  xlab('Model') + 
  theme(legend.position = "none")







## 1st error check: variable importance SHOULD THIS BE USED???

temp <- as.data.frame(matrix(nrow = length(varImp(rpartFit_B)$importance$Overall)))
temp$var_name <- row.names(varImp(rpartFit_B)$importance)
temp$var_importance <- varImp(rpartFit_B)$importance$Overall

plot_importance <- temp %>%
    filter(var_importance > 0) %>%
    ggplot() + geom_col(aes(x = reorder(var_name, desc(-var_importance)), 
                          y = var_importance), fill = "red", width = 0.5) + 
    coord_flip() + 
    theme_bw() + 
    labs(title = "Importance of the variables in the model",
        y = "Importance")
    
  plot_importance + 
    theme(axis.title.y = element_blank())


## Confusion matrix on train
confusionMatrix(train_B$BUILDINGID, train_B$pred_B_rpart)
round(prop.table(confusionMatrix(train_B$BUILDINGID, train_B$pred_B_rpart)$table),2)

## Confusion matrix on test
confusionMatrix(test_B$BUILDINGID, test_B$pred_B_rpart)
round(prop.table(confusionMatrix(test_B$BUILDINGID, test_B$pred_B_rpart)$table),2)


## 2nd error check: visualization of the errors in predicting BUILDING test model rpart


test_B %>% as_tibble() %>% 
    select(LATITUDE, LONGITUDE, BUILDINGID, pred_B_rpart) %>% 
    mutate_at(.vars = c("BUILDINGID", "pred_B_rpart"), as.integer) %>% 
    mutate(resid = BUILDINGID - pred_B_rpart) %>% 
    ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
    geom_point() +
    ggtitle('Errors Rpart BUILDING') +
    scale_colour_discrete("Value \n Residuals") -> Plot_residual_B_rpart

plotly::ggplotly(Plot_residual_B_rpart)


#MODEL RPART PREDICT FLOOR----

set.seed(123)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

rpartFit_F <- train((FLOOR) ~ .,  
                  data = train_F %>% select(starts_with("WAP"), FLOOR, -wap_det),
                  method = "rpart",  
                  tuneLength = 2,         
                  trControl = ctrl) 

rpartFit_F

## 1st error check: Accuracy and kappa

## Results on train
train_results_F_rpart <- predict(object = rpartFit_F, newdata = train_F)
train_F$pred_F_rpart <- train_results_F_rpart

postResample(pred = train_F$pred_F_rpart, obs = train_F$FLOOR)
rpart_train_FLOOR <- postResample(pred = train_F$pred_F_rpart, obs = train_F$FLOOR)
## Results on test
test_results_F_rpart <- predict(object = rpartFit_F, newdata = test_F)
test_F$pred_F_rpart <- test_results_F_rpart

postResample(pred = test_F$pred_F_rpart, obs = test_F$FLOOR)
rpart_test_FLOOR <- postResample(pred = test_F$pred_F_rpart, obs = test_F$FLOOR)

rpart_FLOOR <- cbind(rpart_train_FLOOR, rpart_test_FLOOR)
rpart_FLOOR <- as.data.frame(rpart_FLOOR)

rpart_FLOOR %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("rpart_train_FLOOR", "rpart_test_FLOOR"))) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 4)), vjust=2, size = 5) +
  ggtitle('Metrics Rpart Floor') +
  xlab('Model') + 
  theme(legend.position = "none")

## Confusion matrix on train
confusionMatrix(train_F$FLOOR, train_F$pred_F_rpart)
round(prop.table(confusionMatrix(train_F$FLOOR, train_F$pred_F_rpart)$table),2)

## Confusion matrix on test
confusionMatrix(test_F$FLOOR, test_F$pred_F_rpart)
round(prop.table(confusionMatrix(test_F$FLOOR, test_F$pred_F_rpart)$table),2)

## 2nd error check: visualization of the errors in predicting FLOOR test model rpart 


test_F %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, FLOOR, pred_F_rpart) %>% 
  mutate_at(.vars = c("FLOOR", "pred_F_rpart"), as.integer) %>% 
  mutate(resid = FLOOR - pred_F_rpart) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point() +
  ggtitle('Errors Rpart floor') +
  scale_colour_discrete("Value \n Residuals") -> Plot_residual_F_rpart

plotly::ggplotly(Plot_residual_F_rpart)

# Fit model "knn" -------------------------------------------------------

# MODEL KNN PREDICT BUILDING ID----

set.seed(123)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

knnFit_B <- train((BUILDINGID) ~ .,  
                 data = train_B %>% select(starts_with("WAP"), BUILDINGID, -wap_det),
                 method = "knn",  
                 trControl = ctrl) 

knnFit_B

## 1st error check: Accuracy and kappa

## Results on train
train_results_B_knn <- predict(object = knnFit_B, newdata = train_B)
train_B$pred_B_knn <- train_results_B_knn

postResample(pred = train_B$pred_B_knn, obs = train_B$BUILDINGID)
knn_train_BUILDINGID <- postResample(pred = train_B$pred_B_knn, obs = train_B$BUILDINGID)

## Results on test
test_results_B_knn <- predict(object = knnFit_B, newdata = test_B)
test_B$pred_B_knn <- test_results_B_knn

postResample(pred = test_B$pred_B_knn, obs = test_B$BUILDINGID)
knn_test_BUILDINGID <- postResample(pred = test_B$pred_B_knn, obs = test_B$BUILDINGID)

knn_BUILDING <- cbind(knn_train_BUILDINGID, knn_test_BUILDINGID)
knn_BUILDING <- as.data.frame(knn_BUILDING)

knn_BUILDING %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("knn_train_BUILDINGID", "knn_test_BUILDINGID"))) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 4)), vjust=5, size = 5) +
  ggtitle('Metrics knn BUILDING') +
  xlab('Model') + 
  theme(legend.position = "none")


## Confusion matrix on train
confusionMatrix(train_B$BUILDINGID, train_B$pred_B_knn)
round(prop.table(confusionMatrix(train_B$BUILDINGID, train_B$pred_B_knn)$table),2)

## Confusion matrix on test
confusionMatrix(test_B$BUILDINGID, test_B$pred_B_knn)
round(prop.table(confusionMatrix(test_B$BUILDINGID, test_B$pred_B_knn)$table),2)

## 2nd error check: visualization of the errors in predicting BUILDING test model knn

test_B %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, BUILDINGID, pred_B_knn) %>% 
  mutate_at(.vars = c("BUILDINGID", "pred_B_knn"), as.integer) %>% 
  mutate(resid = BUILDINGID - pred_B_knn) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point() +
  ggtitle('Errors knn BUILDING') +
  scale_colour_discrete("Value \n Residuals") -> Plot_residual_B_knn

plotly::ggplotly(Plot_residual_B_knn)

#MODEL KNN PREDICT FLOOR----

set.seed(123)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

knnFit_F <- train((FLOOR) ~ .,  
                    data = train_F %>% select(starts_with("WAP"), FLOOR, -wap_det),
                    method = "knn",  
                    tuneLength = 2,         
                    trControl = ctrl) 

knnFit_F 


## 1st error check: Accuracy and kappa

## Results on train
train_results_F_knn <- predict(object = knnFit_F, newdata = train_F)
train_F$pred_F_knn <- train_results_F_knn

postResample(pred = train_F$pred_F_knn, obs = train_F$FLOOR)
knn_train_FLOOR <- postResample(pred = train_F$pred_F_knn, obs = train_F$FLOOR)

## Results on test
test_results_F_knn <- predict(object = knnFit_F, newdata = test_F)
test_F$pred_F_knn <- test_results_F_knn

postResample(pred = test_F$pred_F_knn, obs = test_F$FLOOR)
knn_test_FLOOR <- postResample(pred = test_F$pred_F_knn, obs = test_F$FLOOR)


knn_FLOOR <- cbind(knn_train_FLOOR, knn_test_FLOOR)
knn_FLOOR <- as.data.frame(knn_FLOOR)

knn_FLOOR %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("knn_train_FLOOR", "knn_test_FLOOR"))) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 4)), vjust=2, size = 5) +
  ggtitle('Metrics knn Floor') +
  xlab('Model') + 
  theme(legend.position = "none")


## Confusion matrix on train
confusionMatrix(train_F$FLOOR, train_F$pred_F_knn)
round(prop.table(confusionMatrix(train_F$FLOOR, train_F$pred_F_knn)$table),2)

## Confusion matrix on test
confusionMatrix(test_F$FLOOR, test_F$pred_F_knn)
round(prop.table(confusionMatrix(test_F$FLOOR, test_F$pred_F_knn)$table),2)


## 2nd error check: visualization of the errors in predicting FLOOR test model knn 
treshold_F <- 0

test_F %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, FLOOR, pred_F_knn, LOCATIONID) %>% 
  mutate_at(.vars = c("FLOOR", "pred_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_F_knn) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point()   +
  ggtitle('Errors knn floor') +
  scale_colour_discrete("Value \n Residuals") -> Plot_residual_F_knn
  
  
  
  
  geom_label(aes(label = if_else(abs(resid) > treshold_F, 
                                 paste(LOCATIONID, wap_det), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) -> Plot_residual_F_knn
  
plotly::ggplotly(Plot_residual_F_knn)






# Lineair regression model to predict Longitude ---------------------------

mod_lm <- lm(formula = LONGITUDE ~ ., data = train_LO %>% select(starts_with("WAP"), LONGITUDE, -wap_det))

## Results on train

train_result_LO_lm <- predict(object = mod_lm, newdata = train_LO)
train_LO$pred_LO_lm <- train_result_LO_lm                               
                               
postResample(pred = train_LO$pred_LO_lm, obs = train_LO$LONGITUDE)

## Results on test
test_result_LO_lm <- predict(object = mod_lm, newdata = test_LO)
test_LO$pred_LO_lm <- test_result_LO_lm                               

postResample(pred = test_LO$pred_LO_lm, obs = test_LO$LONGITUDE)

# 2nd error check: visualization of the errors in predicting LONGITUDE test model knn 
test_LO <-  add_residuals(test_LO, mod_lm, var = "resid") 
#test_LO <- round(test_LO$resid,3) #CHECK THIS PART WITH JOAN 
treshold <-  40


test_LO %>% 
  ggplot(aes(x = pred_LO_knn, y = LONGITUDE)) +
  geom_point(aes(color = (treshold < abs(resid)))) + 
  geom_abline(intercept = 0, slope = 1, color = "darkgray") +
  geom_label(aes(label = if_else(abs(resid) > treshold, 
                                 paste(USERID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(title = "Error visualizations for Longitude model knn",
       x = "Predicted Longitude",
       y = "Actual Longitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
  geom_label(aes(x = -7700, y = -7350,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(knn_test_LONGITUDE[[1]],3),
                                "\n   - Rsquared: ",round(knn_test_LONGITUDE[[2]],3),
                                "\n   - MAE: ",round(knn_test_LONGITUDE[[3]],3))),
             label.size = NA,
             size = 4.5,
             colour = "red2",
             hjust = 0,
             vjust = 0.5,
             lineheight = 0.8) 

# Model knn to predict Longitude ---------------------------


set.seed(123)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

knnFit_LO <- train((LONGITUDE) ~ .,  
                  data = train_LO %>% select(starts_with("WAP"), LONGITUDE, -wap_det),
                  method = "knn",  
                  trControl = ctrl)  

knnFit_LO


## 1st error check: Accuracy and kappa

## Results on train
train_results_LO_knn <- predict(object = knnFit_LO, newdata = train_LO)
train_LO$pred_LO_knn <- train_results_LO_knn 

postResample(pred = train_LO$pred_LO_knn, obs = train_LO$LONGITUDE)
knn_train_LONGITUDE <- postResample(pred = train_LO$pred_LO_knn, obs = train_LO$LONGITUDE)

## Results on test
test_results_LO_knn <- predict(object = knnFit_LO, newdata = test_LO)
test_LO$pred_LO_knn <- test_results_LO_knn

postResample(pred = test_LO$pred_LO_knn, obs = test_LO$LONGITUDE)
knn_test_LONGITUDE <- postResample(pred = test_LO$pred_LO_knn, obs = test_LO$LONGITUDE)



## 2nd error check: visualization of the errors in predicting LONGITUDE test model knn 
test_LO <-  add_residuals(test_LO, knnFit_LO, var = "resid") 
#test_LO <- round(test_LO$resid,3) #CHECK THIS PART WITH JOAN 
treshold <-  40


test_LO %>% 
    ggplot(aes(x = pred_LO_knn, y = LONGITUDE)) +
    geom_point(aes(color = (treshold < abs(resid)))) + 
    geom_abline(intercept = 0, slope = 1, color = "darkgray") +
    geom_label(aes(label = if_else(abs(resid) > treshold, 
      paste(USERID), NULL)),
      hjust = 0, 
      vjust = 0.4, 
      colour = "red2", 
      fill = NA, 
      label.size = NA, 
      size = 2.5) +
  labs(title = "Error visualizations for Longitude model knn",
       x = "Predicted Longitude",
       y = "Actual Longitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
  geom_label(aes(x = -7700, y = -7350,
  label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(knn_test_LONGITUDE[[1]],3),
                 "\n   - Rsquared: ",round(knn_test_LONGITUDE[[2]],3),
                 "\n   - MAE: ",round(knn_test_LONGITUDE[[3]],3))),
  label.size = NA,
  size = 4.5,
  colour = "red2",
  hjust = 0,
  vjust = 0.5,
  lineheight = 0.8) 

#-Plot fitted vs residuals for LONGITUDE model knn.
ggplot(test_LO, aes(x= LONGITUDE, y=resid)) +
  geom_point(aes(color = (treshold < abs(resid)))) +
  geom_label(aes(label = if_else(abs(resid) > treshold, 
                         paste(LOCATIONID, WAP_det), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(x='Fitted Values', y='Residuals') +
  geom_hline(yintercept = 0, linetype='dashed') +
  ggtitle('Residuals Plot for Longitude model knn')
  
 # Lineair regression model to predict Latitude ---------------------------

mod_lm_LA <- lm(formula = LATITUDE ~ ., data = train_LA %>% select(starts_with("WAP"), LATITUDE, -WAP_det))

## Results on train

train_result_LA_lm <- predict(object = mod_lm_LA, newdata = train_LA)
train_LA$pred_LA_lm <- train_result_LA_lm                               

postResample(pred = train_LA$pred_LA_lm, obs = train_LA$LATITUDE)
knn_train_LATITUDE <- postResample(pred = train_LA$pred_LA_lm, obs = train_LA$LATITUDE)
## Results on test
test_result_LA_lm <- predict(object = mod_lm_LA, newdata = test_LA)
test_LA$pred_LA_lm <- test_result_LA_lm                               

postResample(pred = test_LA$pred_LA_lm, obs = test_LA$LATITUDE)
lm_test_LATITUDE <- postResample(pred = test_LA$pred_LA_lm, obs = test_LA$LATITUDE)

## 2nd error check: visualization of the errors in predicting LATITUDE test model knn 
test_LA <-  add_residuals(test_LA, mod_lm_LA, var = "resid") 
#test_LO <- round(test_LA$resid,3) #CHECK THIS PART WITH JOAN 
treshold <-  40


test_LA %>% 
  ggplot(aes(x = pred_LA_knn, y = LATITUDE)) +
  geom_point(aes(color = (treshold < abs(resid)))) + 
  geom_abline(intercept = 0, slope = 1, color = "darkgray") +
  geom_label(aes(label = if_else(abs(resid) > treshold, 
                                 paste(USERID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(title = "Error visualizations for Latitude lm model",
       x = "Predicted Latitude",
       y = "Actual Latitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
  geom_label(aes(x = 4864750, y = 4865000,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(lm_test_LATITUDE[[1]],3),
                                "\n   - Rsquared: ",round(lm_test_LATITUDE[[2]],3),
                                "\n   - MAE: ",round(lm_test_LATITUDE[[3]],3))),
             label.size = NA,
             size = 4.5,
             colour = "red2",
             hjust = 0,
             vjust = 0.5,
             lineheight = 0.8)





# Model knn to predict Latitude ---------------------------


set.seed(123)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 

knnFit_LA <- train((LATITUDE) ~ .,  
                   data = train_LA %>% select(starts_with("WAP"), LATITUDE, -wap_det),
                   method = "knn",  
                   trControl = ctrl) 

knnFit_LA 


## 1st error check: Accuracy and kappa

## Results on train
train_results_LA_knn <- predict(object = knnFit_LA, newdata = train_LA)
train_LA$pred_LA_knn <- train_results_LA_knn

postResample(pred = train_LA$pred_LA_knn, obs = train_LA$LATITUDE)
knn_train_LATITUDE <- postResample(pred = train_LA$pred_LA_knn, obs = train_LA$LATITUDE)

## Results on test
test_results_LA_knn <- predict(object = knnFit_LA, newdata = test_LA)
test_LA$pred_LA_knn <- test_results_LA_knn

postResample(pred = test_LA$pred_LA_knn, obs = test_LA$LATITUDE)
knn_test_LATITUDE <-  postResample(pred = test_LA$pred_LA_knn, obs = test_LA$LATITUDE)

## 2nd error check: visualization of the errors in predicting LATITUDE test model knn 
test_LA <-  add_residuals(test_LA, knnFit_LA, var = "resid") 
#test_LO <- round(test_LA$resid,3) #CHECK THIS PART WITH JOAN 
treshold <-  40


test_LA %>% 
  ggplot(aes(x = pred_LA_knn, y = LATITUDE)) +
  geom_point(aes(color = (treshold < abs(resid)))) + 
  geom_abline(intercept = 0, slope = 1, color = "darkgray") +
  geom_label(aes(label = if_else(abs(resid) > treshold, 
                                 paste(LOCATIONID, wap_det), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(title = "Error visualizations knn",
       x = "Predicted Latitude",
       y = "Actual Latitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
    geom_label(aes(x = 4864750, y = 4865000,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(metrics_LA[[1]],3),
                                "\n   - Rsquared: ",round(metrics_LA[[2]],3),
                              "\n   - MAE: ",round(metrics_LA[[3]],3))),
              label.size = NA,
              size = 4.5,
              colour = "red2",
              hjust = 0,
              vjust = 0.5,
              lineheight = 0.8) 

#-Plot fitted vs residuals for LATITUDE model knn.
ggplot(test_LA, aes(x= LATITUDE, y=resid)) +
  geom_point(aes(color = (treshold < abs(resid)))) +
  geom_label(aes(label = if_else(abs(resid) > treshold, 
                                 paste(LOCATIONID, wap_det), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(x='Fitted Values', y='Residuals') +
  geom_hline(yintercept = 0, linetype='dashed') +
  ggtitle('Residuals Plot for Latitude model knn')



# PREPROCESS Change WAP values from 100 to -120 --------------------------------------
WIFI_training_PP <- WIFI_training

WIFI_training_PP [, 1:520 ][ WIFI_training_PP[ , 1:520 ] == 100 ] <- -120

# Convert attributes to factor --------------------------------------------
WIFI_training_PP$BUILDINGID <- as.factor(WIFI_training_PP$BUILDINGID)
WIFI_training_PP$FLOOR <- as.factor(WIFI_training_PP$FLOOR)
WIFI_training_PP$SPACEID <- as.factor(WIFI_training_PP$SPACEID)


# Create attribute of sum of WAPS detected (value != -120)--------------------------------

WIFI_training_PP$wap_det <- rowSums(WIFI_training_PP[,1:520] != "-120")
WIFI_training_PP$wap_det<- as.numeric(WIFI_training_PP$wap_det)



# Create training and testset PreProcess----------------------------------------------

# CARET PIPELINE PREDICT BUILDINGID
WIFI_training_id_PP_B <- createDataPartition(y = WIFI_training_PP$BUILDINGID, 
                                          p = 0.75,  
                                          list = F)
train_PP_B <- WIFI_training_PP[WIFI_training_id_PP_B,]
test_PP_B <- WIFI_training_PP[-WIFI_training_id_PP_B,]


# CARET PIPELINE PREDICT FLOOR
WIFI_training_id_PP_F <- createDataPartition(y = WIFI_training_PP$FLOOR, 
                                          p = 0.75,  
                                          list = F)
train_PP_F <- WIFI_training_PP[WIFI_training_id_PP_F,]
test_PP_F <- WIFI_training_PP[-WIFI_training_id_PP_F,]


# CARET PIPELINE PREDICT LONGITUDE
WIFI_training_id_PP_LO <- createDataPartition(y = WIFI_training_PP$LONGITUDE, 
                                           p = 0.75,  
                                           list = F)
train_PP_LO <- WIFI_training_PP[WIFI_training_id_PP_LO,]
test_PP_LO <- WIFI_training_PP[-WIFI_training_id_PP_LO,]


# CARET PIPELINE PREDICT LATITUDE
WIFI_training_id_PP_LA <- createDataPartition(y = WIFI_training_PP$LATITUDE, 
                                           p = 0.75,  
                                           list = F)
train_PP_LA <- WIFI_training_PP[WIFI_training_id_PP_LA,]
test_PP_LA <- WIFI_training_PP[-WIFI_training_id_PP_LA,]

# Fit model "knn" Preprocess -------------------------------------------------------

# MODEL KNN PREPROCESS PREDICT BUILDING ID----
   
set.seed(123)
     
ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP_B <- train((BUILDINGID) ~ .,  
                  data = train_PP_B %>% select(starts_with("WAP"), BUILDINGID, -wap_det),
                  method = "knn",  
                  trControl = ctrl) 

knnFit_PP_B

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP_B_knn <- predict(object = knnFit_PP_B, newdata = train_PP_B)
  train_PP_B$pred_PP_B_knn <- train_results_PP_B_knn

postResample(pred = train_PP_B$pred_PP_B_knn, obs = train_PP_B$BUILDINGID)
knn_train_PP_BUILDINGID <- postResample(pred = train_PP_B$pred_PP_B_knn, obs = train_PP_B$BUILDINGID)

## Results on test
test_results_PP_B_knn <- predict(object = knnFit_PP_B, newdata = test_PP_B)
test_PP_B$pred_PP_B_knn <- test_results_PP_B_knn

postResample(pred = test_PP_B$pred_PP_B_knn, obs = test_PP_B$BUILDINGID)
knn_test_PP_BUILDINGID <- postResample(pred = test_PP_B$pred_PP_B_knn, obs = test_PP_B$BUILDINGID)


# MODEL KNN PREPROCESS PREDICT FLOOR----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP_F <- train((FLOOR) ~ .,  
                     data = train_PP_F %>% select(starts_with("WAP"), FLOOR, -wap_det),
                     method = "knn",  
                     trControl = ctrl) 

knnFit_PP_F

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP_F_knn <- predict(object = knnFit_PP_F, newdata = train_PP_F)
train_PP_F$pred_PP_F_knn <- train_results_PP_F_knn

postResample(pred = train_PP_F$pred_PP_F_knn, obs = train_PP_F$FLOOR)
knn_train_PP_FLOOR <- postResample(pred = train_PP_F$pred_PP_F_knn, obs = train_PP_F$FLOOR)

## Results on test
test_results_PP_F_knn <- predict(object = knnFit_PP_F, newdata = test_PP_F)
test_PP_F$pred_PP_F_knn <- test_results_PP_F_knn

postResample(pred = test_PP_F$pred_PP_F_knn, obs = test_PP_F$FLOOR)
knn_test_PP_FLOOR <- postResample(pred = test_PP_F$pred_PP_F_knn, obs = test_PP_F$FLOOR)



# MODEL KNN PREPROCESS PREDICT LONGITUDE----
      
knnFit_PP_LO

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP_LO_knn <- predict(object = knnFit_PP_LO, newdata = train_PP_LO)
 train_PP_LO$pred_PP_LO_knn <- train_results_PP_LO_knn

postResample(pred = train_PP_LO$pred_PP_LO_knn, obs = train_PP_LO$LONGITUDE)
knn_train_PP_LONGITUDE <- postResample(pred = train_PP_LO$pred_PP_LO_knn, obs = train_PP_LO$LONGITUDE)

## Results on test
test_results_PP_LO_knn <- predict(object = knnFit_PP_LO, newdata = test_PP_LO)
test_PP_LO$pred_PP_LO_knn <- test_results_PP_LO_knn

postResample(pred = test_PP_LO$pred_PP_LO_knn, obs = test_PP_LO$LONGITUDE)
knn_test_PP_LONGITUDE <- postResample(pred = test_PP_LO$pred_PP_LO_knn, obs = test_PP_LO$LONGITUDE)



# MODEL KNN PREPROCESS PREDICT LATITUDE----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP_LA <- train((LATITUDE) ~ .,  
                      data = train_PP_LA %>% select(starts_with("WAP"), LATITUDE, -wap_det),
                      method = "knn",  
                      trControl = ctrl) 

knnFit_PP_LA

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP_LA_knn <- predict(object = knnFit_PP_LA, newdata = train_PP_LA)
train_PP_LA$pred_PP_LA_knn <- train_results_PP_LA_knn 

postResample(pred = train_PP_LA$pred_PP_LA_knn, obs = train_PP_LA$LATITUDE)
knn_train_PP_LATITUDE <- postResample(pred = train_PP_LA$pred_PP_LA_knn, obs = train_PP_LA$LATITUDE)
 
## Results on test
test_results_PP_LA_knn <- predict(object = knnFit_PP_LA, newdata = test_PP_LA)
test_PP_LA$pred_PP_LA_knn <- test_results_PP_LA_knn

postResample(pred = test_PP_LA$pred_PP_LA_knn, obs = test_PP_LA$LATITUDE)
knn_test_PP_LATITUDE <- postResample(pred = test_PP_LA$pred_PP_LA_knn, obs = test_PP_LA$LATITUDE)



# PREPROCESS Check for duplicate values in Dataset WIFI_training_PP --------------------------------------

WIFI_training_PP1 <- WIFI_training_PP
dim(WIFI_training_PP1) # 19937 rows are detected

Duplicated_rows <-  WIFI_training_PP[duplicated(WIFI_training_PP1),] # 637 rows are detected as duplicate
WIFI_training_PP1 <- WIFI_training_PP[!duplicated(WIFI_training_PP1),] # remove 637 rows from the dataframe
dim(WIFI_training_PP1) # 19300 rows are detected

WIFI_training_PP1 %>% write_rds("WIFI_training_PP1.rds")

# Create training and testset PreProcess after removing duplicate rows----------------------------------------------

# CARET PIPELINE PREDICT BUILDINGID
WIFI_training_id_PP1_B <- createDataPartition(y = WIFI_training_PP1$BUILDINGID, 
                                             p = 0.75,  
                                             list = F)
train_PP1_B <- WIFI_training_PP1[WIFI_training_id_PP1_B,]
test_PP1_B <- WIFI_training_PP1[-WIFI_training_id_PP1_B,]


# CARET PIPELINE PREDICT FLOOR
WIFI_training_id_PP1_F <- createDataPartition(y = WIFI_training_PP1$FLOOR, 
                                             p = 0.75,  
                                             list = F)
train_PP1_F <- WIFI_training_PP1[WIFI_training_id_PP1_F,]
test_PP1_F <- WIFI_training_PP1[-WIFI_training_id_PP1_F,]


# CARET PIPELINE PREDICT LONGITUDE
WIFI_training_id_PP1_LO <- createDataPartition(y = WIFI_training_PP1$LONGITUDE, 
                                              p = 0.75,  
                                              list = F)
train_PP1_LO <- WIFI_training_PP1[WIFI_training_id_PP1_LO,]
test_PP1_LO <- WIFI_training_PP1[-WIFI_training_id_PP1_LO,]


# CARET PIPELINE PREDICT LATITUDE
WIFI_training_id_PP1_LA <- createDataPartition(y = WIFI_training_PP1$LATITUDE, 
                                              p = 0.75,  
                                              list = F)
train_PP1_LA <- WIFI_training_PP1[WIFI_training_id_PP1_LA,]
test_PP1_LA <- WIFI_training_PP1[-WIFI_training_id_PP1_LA,]

# Fit model "knn" Preprocess -------------------------------------------------------

# MODEL KNN PREPROCESS PREDICT BUILDING ID AFTER REMOVING DUPLICATE ROWS----

set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP1_B <- train((BUILDINGID) ~ .,  
                     data = train_PP1_B %>% select(starts_with("WAP"), BUILDINGID, -wap_det),
                     method = "knn",  
                     trControl = ctrl) 

knnFit_PP1_B

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP1_B_knn <- predict(object = knnFit_PP1_B, newdata = train_PP1_B)
train_PP1_B$pred_PP1_B_knn <- train_results_PP1_B_knn

postResample(pred = train_PP1_B$pred_PP1_B_knn, obs = train_PP1_B$BUILDINGID)
knn_train_PP1_BUILDINGID <- postResample(pred = train_PP1_B$pred_PP1_B_knn, obs = train_PP1_B$BUILDINGID)

## Results on test
test_results_PP1_B_knn <- predict(object = knnFit_PP1_B, newdata = test_PP1_B)
test_PP1_B$pred_PP1_B_knn <- test_results_PP1_B_knn

postResample(pred = test_PP1_B$pred_PP1_B_knn, obs = test_PP1_B$BUILDINGID)
knn_test_PP1_BUILDINGID <- postResample(pred = test_PP1_B$pred_PP1_B_knn, obs = test_PP1_B$BUILDINGID)

## 2nd error check: visualization of the errors in predicting BUILDING test model knn
treshold_PP1_B <- 0

test_PP1_B %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, BUILDINGID, pred_PP1_B_knn, PHONEID, wap_det) %>% 
  mutate_at(.vars = c("BUILDINGID", "pred_PP1_B_knn"), as.integer) %>% 
  mutate(resid = BUILDINGID - pred_PP1_B_knn) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point() +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_B, 
                               paste(PHONEID, wap_det), NULL)),
           hjust = 0, 
           vjust = 0.4, 
           colour = "red2", 
           fill = NA, 
           label.size = NA, 
           size = 2.5)-> Plot_residual_test_PP1_B_knn

plotly::ggplotly(Plot_residual_test_PP1_B_knn)

gw_test_PP1_B <- test_PP1_B %>% 
   mutate_at(.vars = c("BUILDINGID", "pred_PP1_B_knn"), as.integer) %>% 
  mutate(resid = BUILDINGID - pred_PP1_B_knn) %>% 
  filter(resid != 0)

# MODEL KNN PREPROCESS PREDICT FLOOR AFTER REMOVING DUPLICATE ROWS----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP1_F <- train((FLOOR) ~ .,  
                     data = train_PP1_F %>% select(starts_with("WAP"), FLOOR, -wap_det),
                     method = "knn",  
                     trControl = ctrl) 

knnFit_PP1_F

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP1_F_knn <- predict(object = knnFit_PP1_F, newdata = train_PP1_F)
train_PP1_F$pred_PP1_F_knn <- train_results_PP1_F_knn

postResample(pred = train_PP1_F$pred_PP1_F_knn, obs = train_PP1_F$FLOOR)
knn_train_PP1_FLOOR <- postResample(pred = train_PP1_F$pred_PP1_F_knn, obs = train_PP1_F$FLOOR)

## Results on test
test_results_PP1_F_knn <- predict(object = knnFit_PP1_F, newdata = test_PP1_F)
test_PP1_F$pred_PP1_F_knn <- test_results_PP1_F_knn

postResample(pred = test_PP1_F$pred_PP1_F_knn, obs = test_PP1_F$FLOOR)
knn_test_PP1_FLOOR <- postResample(pred = test_PP1_F$pred_PP1_F_knn, obs = test_PP1_F$FLOOR)


## 2nd error check: visualization of the errors in predicting FLOOR test model knn
treshold_PP1_F <- 0

test_PP1_F %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, FLOOR, pred_PP1_F_knn, PHONEID) %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point() +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_F, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5)-> Plot_residual_test_PP1_F_knn

plotly::ggplotly(Plot_residual_test_PP1_F_knn)

gw_test_PP1_F <- test_PP1_F %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn, 
        timestamp_dateTime = lubridate::as_datetime(TIMESTAMP)) %>% 
         arrange(timestamp_dateTime) %>% 
        filter(resid != 0) 



# MODEL KNN PREPROCESS PREDICT LONGITUDE AFTER REMOVING DUPLICATE ROWS----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP1_LO <- train((LONGITUDE) ~ .,  
                      data = train_PP1_LO %>% select(starts_with("WAP"), LONGITUDE, -wap_det),
                      method = "knn",  
                      trControl = ctrl) 

knnFit_PP1_LO

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP1_LO_knn <- predict(object = knnFit_PP1_LO, newdata = train_PP1_LO)
train_PP1_LO$pred_PP1_LO_knn <- train_results_PP1_LO_knn 

postResample(pred = train_PP1_LO$pred_PP1_LO_knn, obs = train_PP1_LO$LONGITUDE)
knn_train_PP1_LONGITUDE <- postResample(pred = train_PP1_LO$pred_PP1_LO_knn, obs = train_PP1_LO$LONGITUDE)

## Results on test
test_results_PP1_LO_knn <- predict(object = knnFit_PP1_LO, newdata = test_PP1_LO)
test_PP1_LO$pred_PP1_LO_knn <- test_results_PP1_LO_knn

postResample(pred = test_PP1_LO$pred_PP1_LO_knn, obs = test_PP1_LO$LONGITUDE)
knn_test_PP1_LONGITUDE <- postResample(pred = test_PP1_LO$pred_PP1_LO_knn, obs = test_PP1_LO$LONGITUDE)



## 2nd error check: visualization of the errors in predicting LONGITUDE test model knn 
test_PP1_LO <-  add_residuals(test_PP1_LO, knnFit_PP1_LO, var = "resid") 
treshold_PP1_LO <-  40


test_PP1_LO %>% 
  ggplot(aes(x = pred_PP1_LO_knn, y = LONGITUDE)) +
  geom_point(aes(color = (treshold_PP1_LO < abs(resid)))) + 
  geom_abline(intercept = 0, slope = 1, color = "darkgray") +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_LO, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(title = "Error visualizations for Longitude model knn",
       x = "Predicted Longitude",
       y = "Actual Longitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
  geom_label(aes(x = -7700, y = -7350,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(knn_test_PP1_LONGITUDE[[1]],3),
                                "\n   - Rsquared: ",round(knn_test_PP1_LONGITUDE[[2]],3),
                                "\n   - MAE: ",round(knn_test_PP1_LONGITUDE[[3]],3))),
             label.size = NA,
             size = 4.5,
             colour = "red2",
             hjust = 0,
             vjust = 0.5,
             lineheight = 0.8) -> Plot_residual_test_PP1_LO_knn

#-Plot fitted vs residuals for LONGITUDE model knn.
ggplot(test_PP1_LO, aes(x= LONGITUDE, y=resid)) +
  geom_point(aes(color = (treshold_PP1_LO < abs(resid)))) +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_LO, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(x='Fitted Values', y='Residuals') +
  geom_hline(yintercept = 0, linetype='dashed') +
  ggtitle('Residuals Plot for Longitude model knn')


gw_test_PP1_LO <- test_PP1_LO %>% 
 filter(treshold_PP1_LO < abs(resid)) 


# MODEL KNN PREPROCESS PREDICT LATITUDE AFTER REMOVING DUPLICATE ROWS----
set.seed(123)

ctrl <- trainControl(method = "cv", number = 4) 

knnFit_PP1_LA <- train((LATITUDE) ~ .,  
                      data = train_PP1_LA %>% select(starts_with("WAP"), LATITUDE, -wap_det),
                      method = "knn",  
                      trControl = ctrl) 

knnFit_PP1_LA

## 1st error check: Accuracy and kappa

## Results on train
train_results_PP1_LA_knn <- predict(object = knnFit_PP1_LA, newdata = train_PP1_LA)
train_PP1_LA$pred_PP1_LA_knn <- train_results_PP1_LA_knn

postResample(pred = train_PP1_LA$pred_PP1_LA_knn, obs = train_PP1_LA$LATITUDE)
knn_train_PP1_LATITUDE <- postResample(pred = train_PP1_LA$pred_PP1_LA_knn, obs = train_PP1_LA$LATITUDE)

## Results on test
test_results_PP1_LA_knn <- predict(object = knnFit_PP1_LA, newdata = test_PP1_LA)
test_PP1_LA$pred_PP1_LA_knn <- test_results_PP1_LA_knn

postResample(pred = test_PP1_LA$pred_PP1_LA_knn, obs = test_PP1_LA$LATITUDE)
knn_test_PP1_LATITUDE <- postResample(pred = test_PP1_LA$pred_PP1_LA_knn, obs = test_PP1_LA$LATITUDE)


## 2nd error check: visualization of the errors in predicting LATITUDE test model knn 
test_PP1_LA <-  add_residuals(test_PP1_LA, knnFit_PP1_LA, var = "resid") 
treshold_PP1_LA <-  40


test_PP1_LA %>% 
  ggplot(aes(x = pred_PP1_LA_knn, y = LATITUDE)) +
  geom_point(aes(color = (treshold_PP1_LA < abs(resid)))) + 
  geom_abline(intercept = 0, slope = 1, color = "darkgray") +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_LA, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(title = "Error visualizations for Latitude model knn",
       x = "Predicted Latitude",
       y = "Actual Latitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
  geom_label(aes(x = 4864750, y = 4865000,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(knn_test_PP1_LATITUDE[[1]],3),
                                "\n   - Rsquared: ",round(knn_test_PP1_LATITUDE[[2]],3),
                                "\n   - MAE: ",round(knn_test_PP1_LATITUDE[[3]],3))),
             label.size = NA,
             size = 4.5,
             colour = "red2",
             hjust = 0,
             vjust = 0.5,
             lineheight = 0.8) -> Plot_residual_test_PP1_LA_knn

#-Plot fitted vs residuals for LATITUDE model knn.
ggplot(test_PP1_LA, aes(x= LATITUDE, y=resid)) +
  geom_point(aes(color = (treshold_PP1_LA < abs(resid)))) +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_LA, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5) +
  labs(x='Fitted Values', y='Residuals') +
  geom_hline(yintercept = 0, linetype='dashed') +
  ggtitle('Residuals Plot for Latitude model knn')


gw_test_PP1_LA <- test_PP1_LA %>% 
  filter(treshold_PP1_LA < abs(resid)) 

