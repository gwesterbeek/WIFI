WIFI_training_PP2_GS

# number of captures per timestamp 
temp_gw <- WIFI_training_PP2_GS %>% mutate(time = as_datetime(TIMESTAMP)) %>% 
  pivot_longer(starts_with("WAP")) %>% 
  filter(value != -120, name != "wap_det") %>% 
  group_by(name, PHONEID, BUILDINGID, FLOOR) %>% 
  arrange(desc(time)) %>% 
  dplyr::summarise(n = n()) %>% arrange(desc(n))


  ggplot(data = temp_gw, aes(x = name, y = n)) + 
  geom_col() + 
    facet_grid(FLOOR~.)
  
    
  geom_point(aes(size = n, color = PHONEID))
plotly::ggplotly(temp_gw)


temp_gw1 <- temp_gw %>% filter(n > 1) %>%  
  group_by(PHONEID) %>% 
  dplyr::summarise(n = n()) %>% arrange(desc(n))


# Remove duplicates based on Sepal.Width columns for phone ID 14 and 13.
my_data[!duplicated(my_data$Sepal.Width), ]


gw <- test_PP2_F %>% as_tibble() %>% 
  mutate(time = as_datetime(TIMESTAMP)) %>% 
  select(time, LATITUDE, LONGITUDE, FLOOR, pred_PP2_F_knn, PHONEID) %>% 
  mutate_at(.vars = c("FLOOR", "pred_PP2_F_knn"), as.integer) %>% 
  mutate(resid = FLOOR - pred_PP2_F_knn) %>% 
  filter(resid != 0) %>% 
  group_by(PHONEID, FLOOR) %>% 
  summarise(n= n())

