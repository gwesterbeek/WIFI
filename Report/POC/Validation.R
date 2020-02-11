# -------------------------------------------------------------------------
# GOAL: Validate best fitted model for indoor locationing
# DESCRIPTION: WIFI Locationing
# DEVELOPER: Gerhard Westerbeek
# Mon Jan 27 11:05:28 2020 ------------------------------
#  ------------------------------


# load libraries ----------------------------------------------------------

library(caret)
library(tidyverse)
library(broom)
library(purrr)
library(class)
library(modelr)
library(plyr)

# Load dataset validation------------------------------------------------------------

WIFI_validation <- read.csv("validationData.csv")
dim(WIFI_validation)
summary(WIFI_validation)

# Convert attributes to factor --------------------------------------------
WIFI_validation$BUILDINGID <- as.factor(WIFI_validation$BUILDINGID)
WIFI_validation$FLOOR <- as.factor(WIFI_validation$FLOOR)

# Fit model "knn" -------------------------------------------------------

# MODEL KNN PREDICT BUILDING ID----

## 1st error check: Accuracy and kappa

## Results on validation 
validation_results_B_knn <- predict(object = knnFit_B, newdata = WIFI_validation)
WIFI_validation$pred_B_knn <- validation_results_B_knn 

postResample(pred = WIFI_validation$pred_B_knn, obs = WIFI_validation$BUILDINGID)
knn_validation_BUILDINGID <- postResample(pred = WIFI_validation$pred_B_knn, obs = WIFI_validation$BUILDINGID)

## Results on validation after Change WAP values from 100 to -120
WIFI_validation_PP <- WIFI_validation

WIFI_validation_PP [, 1:520 ][ WIFI_validation_PP[ , 1:520 ] == 100 ] <- -120

WIFI_validation_PP %>%  write_rds("WIFI_validation_PP.rds")

validation_results_PP_B_knn <- predict(object = knnFit_PP_B, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP_B_knn <- validation_results_PP_B_knn 

postResample(pred = WIFI_validation_PP$pred_PP_B_knn, obs = WIFI_validation_PP$BUILDINGID)
knn_validation_PP_BUILDINGID <- postResample(pred = WIFI_validation_PP$pred_PP_B_knn, obs = WIFI_validation_PP$BUILDINGID)

## Results on validation after removing duplicate rows-

validation_results_PP1_B_knn <- predict(object = knnFit_PP1_B, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP1_B_knn <- validation_results_PP1_B_knn 

postResample(pred = WIFI_validation_PP$pred_PP1_B_knn, obs = WIFI_validation_PP$BUILDINGID)
knn_validation_PP1_BUILDINGID <- postResample(pred = WIFI_validation_PP$pred_PP1_B_knn, obs = WIFI_validation_PP$BUILDINGID)


# Plot results train, test, validation Building------------------------------------

results_B <- cbind(knn_train_BUILDINGID, knn_test_BUILDINGID, knn_validation_BUILDINGID,
                   knn_train_PP_BUILDINGID, knn_test_PP_BUILDINGID, knn_train_PP1_BUILDINGID,knn_test_PP1_BUILDINGID, 
                   knn_validation_PP_BUILDINGID, knn_validation_PP1_BUILDINGID)
results_B <- as.data.frame(results_B)
str(results)

results_B %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("knn_train_BUILDINGID", "knn_train_PP_BUILDINGID", "knn_train_PP1_BUILDINGID", 
                                      "knn_test_BUILDINGID", "knn_test_PP_BUILDINGID","knn_test_PP1_BUILDINGID", 
                                      "knn_validation_BUILDINGID", "knn_validation_PP_BUILDINGID", "knn_validation_PP1_BUILDINGID"))) %>%
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 4)), vjust=0, size = 3)

## 2nd error check: visualization of the errors in predicting BUILDING validation model knn
treshold_PP1_B <- 0

WIFI_validation_PP %>% as_tibble() %>% 
  select(LATITUDE, LONGITUDE, BUILDINGID, pred_PP1_B_knn, PHONEID) %>% 
  mutate_at(.vars = c("BUILDINGID", "pred_PP1_B_knn"), as.integer) %>% 
  mutate(resid = BUILDINGID - pred_PP1_B_knn) %>% 
  ggplot(aes(y = LATITUDE, x = LONGITUDE, color = factor(resid))) + 
  geom_point() +
  geom_label(aes(label = if_else(abs(resid) > treshold_PP1_B, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5)-> Plot_residual_validation_PP1_B_knn

plotly::ggplotly(Plot_residual_validation_PP1_B_knn)

gw_validation_PP1_B <- WIFI_validation_PP %>% 
  mutate_at(.vars = c("BUILDINGID", "pred_PP1_B_knn"), as.integer) %>% 
  mutate(resid = BUILDINGID - pred_PP1_B_knn) %>% 
  filter(resid != 0)

# MODEL KNN PREDICT FLOOR----

## 1st error check: Accuracy and kappa

## Results on validation
validation_results_F_knn <- predict(object = knnFit_F, newdata = WIFI_validation)
WIFI_validation$pred_F_knn <- validation_results_F_knn

postResample(pred = WIFI_validation$pred_F_knn, obs = WIFI_validation$FLOOR)
knn_validation_FLOOR <- postResample(pred = WIFI_validation$pred_F_knn, obs = WIFI_validation$FLOOR)


## Results on validation after Change WAP values from 100 to -120

validation_results_PP_F_knn <- predict(object = knnFit_PP_F, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP_F_knn <- validation_results_PP_F_knn 

postResample(pred = WIFI_validation_PP$pred_PP_F_knn, obs = WIFI_validation_PP$FLOOR)
knn_validation_PP_FLOOR <- postResample(pred = WIFI_validation_PP$pred_PP_F_knn, obs = WIFI_validation_PP$FLOOR)

## Results on validation after removing duplicate rows-

validation_results_PP1_F_knn <- predict(object = knnFit_PP1_F, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP1_F_knn <- validation_results_PP1_F_knn 

postResample(pred = WIFI_validation_PP$pred_PP1_F_knn, obs = WIFI_validation_PP$FLOOR)
knn_validation_PP1_FLOOR <- postResample(pred = WIFI_validation_PP$pred_PP1_F_knn, obs = WIFI_validation_PP$FLOOR)


## Results on validation after removing not detected WAP's (strenght DbM < -65 dBm)

validation_results_PP2_F_knn <- predict(object = knnFit_PP2_F, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP2_F_knn <- validation_results_PP2_F_knn 

postResample(pred = WIFI_validation_PP$pred_PP2_F_knn, obs = WIFI_validation_PP$FLOOR)
knn_validation_PP2_FLOOR <- postResample(pred = WIFI_validation_PP$pred_PP2_F_knn, obs = WIFI_validation_PP$FLOOR)
knn_validation_PP2_FLOOR %>%  write_rds("knn_validation_PP2_FLOOR.rds")

# Plot results train, test, validation Floor------------------------------------

results_F <- cbind(knn_train_FLOOR, knn_train_PP_FLOOR, knn_train_PP1_FLOOR, knn_train_PP2_FLOOR, knn_test_FLOOR, knn_test_PP_FLOOR,
                   knn_test_PP1_FLOOR, knn_test_PP2_FLOOR, knn_validation_FLOOR, knn_validation_PP_FLOOR, knn_validation_PP1_FLOOR)
results_F <- as.data.frame(results_F)

results_F %>% tibble::rownames_to_column("metrics") %>% 
          pivot_longer(cols = -metrics) %>% 
          mutate(name = factor(name, levels=c("knn_train_FLOOR", "knn_train_PP_FLOOR", "knn_train_PP1_FLOOR", 
                                      "knn_train_PP2_FLOOR", "knn_test_FLOOR", "knn_test_PP_FLOOR","knn_test_PP1_FLOOR", 
                                      "knn_test_PP2_FLOOR", "knn_validation_FLOOR", "knn_validation_PP_FLOOR", 
                                      "knn_validation_PP1_FLOOR"))) %>% 
          ggplot(aes(x = name, y = value, fill = name)) +
          geom_bar(position = "dodge", stat = "identity") +
          facet_grid(metrics~.) +
          geom_text(aes(label = round(value, digits = 3)), vjust=0, size = 3)
          
## 2nd error check: visualization of the errors in predicting FLOOR validation model knn
treshold_PP1_F <- 0

WIFI_validation_PP %>% as_tibble() %>% 
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
             size = 2.5)-> Plot_residual_validation_PP1_F_knn

plotly::ggplotly(Plot_residual_validation_PP1_F_knn)

gw_validation_PP1_F <- WIFI_validation_PP %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn, 
         timestamp_dateTime = lubridate::as_datetime(TIMESTAMP)) %>% 
  arrange(timestamp_dateTime) %>% 
  filter(resid != 0) 

# MODEL KNN PREDICT LONGITUDE----

## 1st error check: Accuracy and kappa

## Results on validation
validation_results_LO_knn <- predict(object = knnFit_LO, newdata = WIFI_validation)
WIFI_validation$pred_LO_knn <- validation_results_LO_knn

postResample(pred = WIFI_validation$pred_LO_knn, obs = WIFI_validation$LONGITUDE)
knn_validation_LONGITUDE <- postResample(pred = WIFI_validation$pred_LO_knn, obs = WIFI_validation$LONGITUDE)

## Results on validation after Change WAP values from 100 to -120

validation_results_PP_LO_knn <- predict(object = knnFit_PP_LO, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP_LO_knn <- validation_results_PP_LO_knn 

postResample(pred = WIFI_validation_PP$pred_PP_LO_knn, obs = WIFI_validation_PP$LONGITUDE)
knn_validation_PP_LONGITUDE <- postResample(pred = WIFI_validation_PP$pred_PP_LO_knn, obs = WIFI_validation_PP$LONGITUDE)

## Results on validation after removing duplicate rows-

validation_results_PP1_LO_knn <- predict(object = knnFit_PP1_LO, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP1_LO_knn <- validation_results_PP1_LO_knn 

postResample(pred = WIFI_validation_PP$pred_PP1_LO_knn, obs = WIFI_validation_PP$LONGITUDE)
knn_validation_PP1_LONGITUDE <- postResample(pred = WIFI_validation_PP$pred_PP1_LO_knn, obs = WIFI_validation_PP$LONGITUDE)

## Results on validation after removing not detected WAP's (strenght DbM < -65 dBm)

validation_results_PP2_LO_knn <- predict(object = knnFit_PP2_LO, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP2_LO_knn <- validation_results_PP2_LO_knn 

postResample(pred = WIFI_validation_PP$pred_PP2_LO_knn, obs = WIFI_validation_PP$LONGITUDE)
knn_validation_PP2_LONGITUDE <- postResample(pred = WIFI_validation_PP$pred_PP2_LO_knn, obs = WIFI_validation_PP$LONGITUDE)
knn_validation_PP2_LONGITUDE %>% write_rds("knn_validation_PP2_LONGITUDE.rds")
# Plot results train, test, validation Longitude ------------------------------------

results_LO <- cbind(knn_train_LONGITUDE, knn_train_PP_LONGITUDE, knn_train_PP1_LONGITUDE, 
                    knn_test_LONGITUDE, knn_test_PP_LONGITUDE, knn_test_PP1_LONGITUDE,
                    knn_validation_LONGITUDE, knn_validation_PP_LONGITUDE, knn_validation_PP1_LONGITUDE)
                    
results_LO <- as.data.frame(results_LO)

results_LO %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("knn_train_LONGITUDE", "knn_train_PP_LONGITUDE", "knn_train_PP1_LONGITUDE", 
                                      "knn_test_LONGITUDE", "knn_test_PP_LONGITUDE","knn_test_PP1_LONGITUDE", 
                                      "knn_validation_LONGITUDE", "knn_validation_PP_LONGITUDE", 
                                      "knn_validation_PP1_LONGITUDE"))) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 3)), vjust=0, size = 3)

## 2nd error check: visualization of the errors in predicting LONGITUDE validation model knn 

treshold_PP1_LO <-  40


WIFI_validation_PP %>% 
  select(LONGITUDE, pred_PP1_LO_knn, PHONEID) %>% 
  mutate(resid = LONGITUDE - pred_PP1_LO_knn ) %>% 
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
  geom_label(aes(x = -7650, y = -7350,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",round(knn_validation_PP1_LONGITUDE[[1]],3),
                                "\n   - Rsquared: ",round(knn_validation_PP1_LONGITUDE[[2]],3),
                                "\n   - MAE: ",round(knn_validation_PP1_LONGITUDE[[3]],3))),
             label.size = NA,
             size = 4.5,
             colour = "red2",
             hjust = 0,
             vjust = 0.5,
             lineheight = 0.8) -> Plot_residual_validation_PP1_LO_knn

#-Plot fitted vs residuals for LONGITUDE model knn.
WIFI_validation_PP %>% 
  select(LONGITUDE, pred_PP1_LO_knn, PHONEID) %>% 
  mutate(resid = LONGITUDE - pred_PP1_LO_knn ) %>% 
  ggplot(aes(x= LONGITUDE, y=resid)) +
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
  ggtitle('VALIDATION: Residuals Plot for Longitude model knn')




# MODEL KNN PREDICT LATITUDE----

## Results on validation
validation_results_LA_knn <- predict(object = knnFit_LA, newdata = WIFI_validation)
WIFI_validation$pred_LA_knn <- validation_results_LA_knn

postResample(pred = WIFI_validation$pred_LA_knn, obs = WIFI_validation$LATITUDE)
knn_validation_LATITUDE <- postResample(pred = WIFI_validation$pred_LA_knn, obs = WIFI_validation$LATITUDE)


## Results on validation after Change WAP values from 100 to -120

validation_results_PP_LA_knn <- predict(object = knnFit_PP_LA, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP_LA_knn <- validation_results_PP_LA_knn 

postResample(pred = WIFI_validation_PP$pred_PP_LA_knn, obs = WIFI_validation_PP$LATITUDE)
knn_validation_PP_LATITUDE <- postResample(pred = WIFI_validation_PP$pred_PP_LA_knn, obs = WIFI_validation_PP$LATITUDE)

## Results on validation after removing duplicate rows-

validation_results_PP1_LA_knn <- predict(object = knnFit_PP1_LA, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP1_LA_knn <- validation_results_PP1_LA_knn 

postResample(pred = WIFI_validation_PP$pred_PP1_LA_knn, obs = WIFI_validation_PP$LATITUDE)
knn_validation_PP1_LATITUDE <- postResample(pred = WIFI_validation_PP$pred_PP1_LA_knn, obs = WIFI_validation_PP$LATITUDE)


## Results on validation after removing not detected WAP's (strenght DbM < -65 dBm)

validation_results_PP2_LA_knn <- predict(object = knnFit_PP2_LA, newdata = WIFI_validation_PP)
WIFI_validation_PP$pred_PP2_LA_knn <- validation_results_PP2_LA_knn 

postResample(pred = WIFI_validation_PP$pred_PP2_LA_knn, obs = WIFI_validation_PP$LATITUDE)
knn_validation_PP2_LATITUDE <- postResample(pred = WIFI_validation_PP$pred_PP2_LA_knn, obs = WIFI_validation_PP$LATITUDE)

knn_validation_PP2_LATITUDE %>% write_rds("knn_validation_PP2_LATITUDE.rds")

# Plot results train, test, validation Longitude ------------------------------------

results_LA <- cbind(knn_train_LATITUDE, knn_train_PP_LATITUDE, knn_train_PP1_LATITUDE, knn_test_LATITUDE, knn_test_PP_LATITUDE,
                    knn_test_PP1_LATITUDE, knn_validation_LATITUDE, knn_validation_PP_LATITUDE, knn_validation_PP1_LATITUDE)
results_LA <- as.data.frame(results_LA)                          

results_LA %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name = factor(name, levels=c("knn_train_LATITUDE", "knn_train_PP_LATITUDE", "knn_train_PP1_LATITUDE", 
                                      "knn_test_LATITUDE", "knn_test_PP_LATITUDE","knn_test_PP1_LATITUDE", 
                                      "knn_validation_LATITUDE", "knn_validation_PP_LATITUDE", 
                                      "knn_validation_PP1_LATITUDE"))) %>% 
  ggplot(aes(x = name, y = value, fill = name)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(metrics~.) +
  geom_text(aes(label = round(value, digits = 3)), vjust=0, size = 3)


## 2nd error check: visualization of the errors in predicting LATITUDE validation model knn 

treshold_PP1_LA <-  40


WIFI_validation_PP %>% 
  select(LATITUDE, pred_PP1_LA_knn, PHONEID) %>% 
  mutate(resid = LATITUDE - pred_PP1_LA_knn ) %>% 
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
  labs(title = "VALIDATION:Error visualizations for Latitude model knn",
       x = "Predicted Latitude",
       y = "Actual Latitude") +
  scale_color_manual(values = c("grey","red2")) +
  theme_classic() +
  theme(axis.text.y = element_text(),
        axis.text.x = element_text(), 
        legend.title = element_blank(), 
        legend.position = "none") +
  geom_label(aes(x = 4864750, y = 4865000,
                 label = paste0("The metrics for the relevant product types are:\n   - RMSE: ",
                                round(knn_validation_PP1_LATITUDE[[1]],3),
                                "\n   - Rsquared: ",round(knn_validation_PP1_LATITUDE[[2]],3),
                                "\n   - MAE: ",round(knn_validation_PP1_LATITUDE[[3]],3))),
             label.size = NA,
             size = 4.5,
             colour = "red2",
             hjust = 0,
             vjust = 0.5,
             lineheight = 0.8) -> Plot_residual_validation_PP1_LA_knn

#-Plot fitted vs residuals for Latitude model knn.
WIFI_validation_PP %>% 
  select(LATITUDE, pred_PP1_LA_knn, PHONEID) %>% 
  mutate(resid = LATITUDE - pred_PP1_LA_knn ) %>% 
  ggplot(aes(x= LATITUDE, y=resid)) +
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
  ggtitle('VALIDATION: Residuals Plot for Latitude model knn')
