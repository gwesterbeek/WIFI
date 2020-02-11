# -------------------------------------------------------------------------
# GOAL: PLAY AROUND
# DESCRIPTION: WIFI Locationing
# DEVELOPER: Gerhard Westerbeek

# -------------------------------------------------------------------------

gw <- test_F%>% 
  mutate(DBM_med = apply(test_F[,1:520],1,function(x) median(x[x<0]))) %>% #sum of negative values WAP
  mutate(DBM = apply(test_F[,1:520],1,function(x) sum(x[x < 0], na.rm  = TRUE))) %>% #sum of negative values WAP
  mutate_at(.vars = c("FLOOR", "pred_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_F_knn) %>% 
  mutate(abs_resid = abs(resid)) %>% 
  #filter(resid!=0) %>% 
  mutate(DBM_avg = DBM / WAP_det)
# pivot_longer(cols = starts_with("WAP")) %>% 
# filter(value != 100) %>% 
# group_by(name) %>% 
# cou
# 
# summarise(median = median(value), 
#           abs_resid = median(abs_resid))

treshold_gw <- 0
gw %>% filter(USERID == 7 | USERID == 16) %>%  
  ggplot(aes(x = WAP_det, y = DBM_med, color = abs_resid)) +
    geom_point() +
  facet_wrap(BUILDINGID~FLOOR) +
  geom_label(aes(label = if_else(abs_resid >= treshold_gw, 
                                 paste(PHONEID), NULL)),
             hjust = 0, 
             vjust = 0.4, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2.5)
     

gw %>% 
  dplyr::select(-WAP_det) %>% 
  pivot_longer(starts_with("WAP")) %>%
  filter(value != 100) %>% 
  group_by(name) %>% 
  dplyr::summarise(median = median(value), 
                   resid = median(abs_resid)) %>% 
  ggplot(aes(y = median, x = resid)) +
  geom_point()

test_gw <-  gw %>% 
  pivot_longer(starts_with("WAP")) %>% 
  filter(value!=100) %>% 
  select("name", BUILDINGID) %>% 
  filter(name != "WAP_det") %>%
  group_by(name, BUILDINGID) %>% 
  count() %>% 
  group_by(name) %>% 
  add_count(name) %>% 
  filter(n != 1) %>% 
    ggplot(aes(x = name, y = freq, fill= BUILDINGID)) +
  geom_bar(stat = "identity")
  ggplot(aes(x = name, y = freq)) +
    geom_col()

  
  
test_gw1 <- ddply(test_gw, .(name), head, n = 2) 
  


gw %>%  filter(BUILDINGID == 1, FLOOR == 1) %>%  
  ggplot(aes(x = LONGITUDE, y = LATITUDE, GROUP = FLOOR, color = USERID)) +
  geom_jitter() +

  geom_label(aes(label = if_else(abs_resid >= treshold_gw, 
                                 paste( ), NULL)),
             hjust = 0, 
             vjust = 0, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2)

filter(BUILDINGID == 1, FLOOR == 1, USERID == 7 | USERID == 16)


gw %>% filter(BUILDINGID == 1, FLOOR == 1) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = as.factor(WAP_det))) +
  geom_point() 
  


  
  geom_label(aes(label = if_else(abs_resid >= treshold_gw, 
                                 paste( ), NULL)),
             hjust = 0, 
             vjust = 0, 
             colour = "red2", 
             fill = NA, 
             label.size = NA, 
             size = 2)
  
gw %>%  group_by(BUILDINGID, FLOOR) %>% 
  select(BUILDINGID, FLOOR, SPACEID) %>% 
  count("SPACEID")
  
  ggplot(aes(x))
  
  
# Heatmap regarding locations per building and floor
  
gw %>% group_by(BUILDINGID, FLOOR, LOCATIONID) %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE, color = LOCATIONID)) +
  geom_point()


  geom_tile(aes(alpha = SPACEID))


gw %>%  filter(BUILDINGID == 1) %>%  
ggplot(aes(x = LONGITUDE, y = LATITUDE, GROUP = FLOOR, color = SPACEID)) +
geom_point() +
facet_wrap("FLOOR")

test_PP1_F [, "max"] <- apply(test_PP1_F [, 1:520], 1, max)

gw_test_PP1_F_Play <- test_PP1_F %>% select(-wap_det) %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn, 
         timestamp_dateTime = lubridate::as_datetime(TIMESTAMP)) %>% 
  arrange(timestamp_dateTime) %>% 
  ggplot(aes(x = FLOOR, y = max)) +
  geom_point(aes(color = factor(resid))) +
  theme(axis.text.x = element_text(angle = 90))


plotly::ggplotly(gw_test_PP1_F_Play)


gw_test_PP1_F_Play_Floor1 <- test_PP1_F %>% #select(-wap_det) %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn, 
         timestamp_dateTime = lubridate::as_datetime(TIMESTAMP)) %>% 
  arrange(timestamp_dateTime) %>% 
  filter(resid != 0) %>% 
  pivot_longer(starts_with("WAP")) %>% 
  filter(value != -120) %>% 
  ggplot(aes(x = FLOOR, y = TIMESTAMP))  +
  geom_point(aes(color = factor(USERID))) +
  theme(axis.text.x = element_text(angle = 90))

plotly::ggplotly(gw_test_PP1_F_Play_Floor1)

gw_test_PP1_F_Play_Floor2 <- test_PP1_F %>% select(-wap_det) %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn, 
         timestamp_dateTime = lubridate::as_datetime(TIMESTAMP)) %>% 
  arrange(timestamp_dateTime) %>% 
  filter(FLOOR == 2) %>% 
  pivot_longer(starts_with("WAP")) %>% 
  filter(value != -120, resid !=0) %>% 
  group_by(USERID, timestamp_dateTime) %>% 
  summarise(USERID, n())

# number of captures per timestamp 
temp_gw <- test_PP1_F %>% mutate(time = as_datetime(TIMESTAMP)) %>% 
  group_by(time, PHONEID) %>% 
  dplyr::summarise(n = n()) %>% arrange(desc(n))%>% 
  ggplot(aes(x = time, y = n)) + 
  geom_point(aes(size = n, color = PHONEID))
plotly::ggplotly(temp_gw)


temp_gw1 <- test_PP1_F %>% mutate(time = as_datetime(TIMESTAMP)) %>% 
  add_count(time) %>% 
  ggplot(aes(x= time, y = n)) +
    geom_col()

temp_gw3 <- test_PP1_F %>%  
  mutate_at(.vars = c("FLOOR", "pred_PP1_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP1_F_knn)
        # timestamp_dateTime = lubridate::as_datetime(TIMESTAMP)) %>% 
  filter(resid == 0)
  #mutate(hit = ifelse(resid==0, TRUE, FALSE)) %>% # filter residual = 0
  #group_by(hit) %>% # exclude that
  # mutate_at(.vars = starts_with("WAP"),if_else(x > -65, x = 1, x = 0))
#   pivot_longer(starts_with("WAP")) %>% 
#   filter(value != -120) %>%
#   filter(FLOOR == 1) %>% 
#   arrange(timestamp_dateTime) %>% 
#   ggplot() + 
#   geom_bar(aes(x = name)) +
#   facet_wrap(~hit)
# 
#   
# if
#   
  
as.integer(test_PP1_F$FLOOR)
  
test_PP1_F$resid <-  - as.integer(test_PP1_F$pred_PP1_F_knn)

test_PP1_F %>% 
  write_rds("test_PP1_F.rds")

#Create file with GOOD predictions including filter on signal strenght

temp <- test_PP1_F %>%
  as_tibble() %>% 
  mutate(resid = as.integer(FLOOR) - as.integer(pred_PP1_F_knn)) %>% 
  filter(resid == 0) %>% 
  pivot_longer(cols = c(starts_with("WAP"))) %>% 
  mutate(value = if_else(value > -65, 1, 0)) %>% 
  filter(value == 1) 

#Create file with FALSE predictions including filter on signal strenght

temp_false <- test_PP1_F %>%
  as_tibble() %>% 
  mutate(resid = as.integer(FLOOR) - as.integer(pred_PP1_F_knn)) %>% 
  filter(resid != 0) %>% 
  pivot_longer(cols = c(starts_with("WAP"))) %>% 
  mutate(value = if_else(value > -65, 1, 0)) %>% 
  filter(value == 1)


# FLOOR 0 ANALYSIS --------------------------------------------------------

# Number of detections ground floor (=0) strong signal GOOD prediction   
temp <- filter(temp, name != "wap_det")

temp$name <- factor(temp$name)

temp_sumary_F0 <- temp %>% 
  group_by(BUILDINGID, FLOOR, name)  %>% 
  summarize(times_det = n()) %>% 
  filter(times_det >20)

  
temp_plot_F0 <- temp_sumary_F0 %>% 
  ggplot(aes(x = name, y = times_det)) +
    geom_col() +
    facet_wrap(~BUILDINGID, nrow = 3)

plotly::ggplotly(temp_plot_F0)


# Number of detections GROUNDFLOOR (=0) strong signal FALSE prediction   
temp_sumary_false_F0 <- temp_false %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", FLOOR == 0)

temp_plot_false_F0 <- temp_sumary_false_F0 %>% 
  ggplot(aes(x = name, y = freq)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3 )

  
plotly::ggplotly(temp_plot_false_F0)


#Combine the plots for FLOOR = groundfloor(0) ##
  
ggplot() +
  geom_col(data = temp_sumary_F0, aes(x = name, y = freq, fill = "Correct")) +
  geom_col(data = temp_sumary_false_F0, aes(x = name, y = freq, fill = "Incorrect")) +
  facet_wrap(~BUILDINGID, nrow = 3) +
  scale_fill_manual(values=c("Correct"='green', "Incorrect"='blue')) -> combi_plot_F0

plotly::ggplotly(combi_plot_F0)


# FLOOR 1 ANALYSIS --------------------------------------------------------

# Number of detections FLOOR 1 strong signal GOOD prediction   
temp_sumary_F1 <- temp %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", FLOOR == 1)

temp_plot_F1 <- temp_sumary_F1 %>% 
  ggplot(aes(x = name, y = freq)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3)

plotly::ggplotly(temp_plot_F1)


# Number of detections FLOOR =1 strong signal FALSE prediction   
temp_sumary_false_F1 <- temp_false %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", FLOOR == 1)

temp_plot_false_F1 <- temp_sumary_false_F1 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3 )


plotly::ggplotly(temp_plot_false_F1)


#Combine the plots for FLOOR = 1 ##

ggplot() +
  geom_col(data = temp_sumary_F1, aes(x = name, y = n, fill = "Correct")) +
  geom_col(data = temp_sumary_false_F1, aes(x = name, y = n, fill = "Incorrect")) +
  facet_wrap(~BUILDINGID, nrow = 3) +
  scale_fill_manual(values=c("Correct"='green', "Incorrect"='blue')) -> combi_plot_F1

plotly::ggplotly(combi_plot_F1)


# FLOOR 2 ANALYSIS --------------------------------------------------------

# Number of detections FLOOR 2 strong signal GOOD prediction   
temp_sumary_F2 <- temp %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", n > 20, FLOOR == 2)

temp_plot_F2 <- temp_sumary_F2 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3)

plotly::ggplotly(temp_plot_F2)


# Number of detections FLOOR = 2 strong signal FALSE prediction   
temp_sumary_false_F2 <- temp_false %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", FLOOR == 2)

temp_plot_false_F2 <- temp_sumary_false_F2 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3 )


plotly::ggplotly(temp_plot_false_F2)


#Combine the plots for FLOOR = 2 ##

ggplot() +
  geom_col(data = temp_sumary_F2, aes(x = name, y = n, fill = "Correct")) +
  geom_col(data = temp_sumary_false_F2, aes(x = name, y = n, fill = "Incorrect")) +
  facet_wrap(~BUILDINGID, nrow = 3) +
  scale_fill_manual(values=c("Correct"='green', "Incorrect"='blue')) -> combi_plot_F2

plotly::ggplotly(combi_plot_F2)

# FLOOR 3 ANALYSIS --------------------------------------------------------

# Number of detections FLOOR 3 strong signal GOOD prediction   
temp_sumary_F3 <- temp %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", n > 20, FLOOR == 3)

temp_plot_F3 <- temp_sumary_F3 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3)

plotly::ggplotly(temp_plot_F3)


# Number of detections FLOOR = 3 strong signal FALSE prediction   
temp_sumary_false_F3 <- temp_false %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", FLOOR == 3)

temp_plot_false_F3 <- temp_sumary_false_F3 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col() +
  facet_wrap(~BUILDINGID, nrow = 3 )


plotly::ggplotly(temp_plot_false_F3)


#Combine the plots for FLOOR = 3 ##

ggplot() +
  geom_col(data = temp_sumary_F3, aes(x = name, y = n, fill = "Correct")) +
  geom_col(data = temp_sumary_false_F3, aes(x = name, y = n, fill = "Incorrect")) +
  facet_wrap(~BUILDINGID, nrow = 3) +
  scale_fill_manual(values=c("Correct"='green', "Incorrect"='blue')) -> combi_plot_F3

plotly::ggplotly(combi_plot_F3)


# FLOOR 4 ANALYSIS --------------------------------------------------------

# Number of detections FLOOR 4 strong signal GOOD prediction   
temp_sumary_F4 <- temp %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", n > 20, FLOOR == 4)

temp_plot_F4 <- temp_sumary_F4 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col()
  

plotly::ggplotly(temp_plot_F4)


# Number of detections FLOOR = 4 strong signal FALSE prediction   
temp_sumary_false_F4 <- temp_false %>% 
  group_by(BUILDINGID, FLOOR, name) %>% 
  count() %>% 
  filter(name != "wap_det", FLOOR == 4)

temp_plot_false_F4 <- temp_sumary_false_F4 %>% 
  ggplot(aes(x = name, y = n)) +
  geom_col() 

plotly::ggplotly(temp_plot_false_F4)


#Combine the plots for FLOOR = 4 ##

ggplot() +
  geom_col(data = temp_sumary_F4, aes(x = name, y = n, fill = "Correct")) +
  geom_col(data = temp_sumary_false_F4, aes(x = name, y = n, fill = "Incorrect")) +
  scale_fill_manual(values=c("Correct"='green', "Incorrect"='blue')) -> combi_plot_F4

plotly::ggplotly(combi_plot_F4)
