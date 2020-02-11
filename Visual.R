
results_B_PP_test <- cbind(knn_test_BUILDINGID, knn_test_PP_BUILDINGID, knn_test_PP1_BUILDINGID)
                          
                    
results_B_PP_test <- as.data.frame(results_B_PP_test) 
results_B_PP_test1 <- results_B_PP_test %>% tibble::rownames_to_column("metrics") %>% 
                      pivot_longer(cols = -metrics) %>% 
                      mutate(name_PP = factor(name, levels=c("knn_test_BUILDINGID", 
                                                             "knn_test_PP_BUILDINGID","knn_test_PP1_BUILDINGID"),
                                                    labels = c("Initial", "Preprocess_1", "Preprocess_2") )) %>% 
                      select(-(name))

results_B_PP_val <- cbind(knn_validation_BUILDINGID,knn_validation_PP_BUILDINGID, knn_validation_PP1_BUILDINGID)
results_B_PP_val <- as.data.frame(results_B_PP_val)

results_B_PP_val1 <- results_B_PP_val %>%  tibble::rownames_to_column("metrics") %>% 
                    pivot_longer(cols = -metrics) %>% 
                    mutate(name_PP = factor(name, levels=c(Initial = "knn_validation_BUILDINGID", 
                                                                      "knn_validation_PP_BUILDINGID", "knn_validation_PP1_BUILDINGID"),
                                                  labels = c("Initial", "Preprocess_1", "Preprocess_2"))) %>% 
                    select(-(name)) 


results_F_PP <- cbind(knn_train_FLOOR, knn_train_PP_FLOOR, knn_train_PP1_FLOOR, knn_train_PP2_FLOOR, knn_test_FLOOR, knn_test_PP_FLOOR,
                   knn_test_PP1_FLOOR, knn_test_PP2_FLOOR, knn_validation_FLOOR, knn_validation_PP_FLOOR, knn_validation_PP1_FLOOR)
results_F_PP <- as.data.frame(results_F)

Plot_results_Preprocess <- cbind()

str(results)

results_B_PP %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_test = factor(name, levels=c("knn_test_BUILDINGID", "knn_test_PP_BUILDINGID","knn_test_PP1_BUILDINGID")),
          name_val = factor(name, levels=c("knn_validation_BUILDINGID", "knn_validation_PP_BUILDINGID", "knn_validation_PP1_BUILDINGID"),
                                          labels = c("Initial", "Preprocess_1", "Preprocess_2"))) %>% 
  select(-(name)) %>% 
  
  ggplot() +
  geom_line(data = results_B_PP_test1, aes(x = name_PP, y = value, group = "name_test", color = "test")) +
  geom_line(data = results_B_PP_val1, aes(x = name_PP, y = value, group = "name_val", color = "validation")) +
  facet_grid(metrics~.) +
  scale_color_manual(name = "Metrics \n Building", values = c("test" = "darkblue", "validation" = "red")) +
  labs(title = "Metrics for Building KNN Model",
       x = "Preprocess step",
       y = "Value")




results_F_PP_test <- cbind(knn_test_FLOOR, knn_test_PP_FLOOR, knn_test_PP1_FLOOR, knn_test_PP2_FLOOR)


results_F_PP_test <- as.data.frame(results_F_PP_test) 
results_F_PP_test1 <- results_F_PP_test %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_PP = factor(name, levels=c("knn_test_FLOOR", 
                                         "knn_test_PP_FLOOR","knn_test_PP1_FLOOR", "knn_test_PP2_FLOOR"),
                          labels = c("Initial", "Preprocess_1", "Preprocess_2", "Preprocess_3") )) %>% 
  select(-(name))

results_F_PP_val <- cbind(knn_validation_FLOOR,knn_validation_PP_FLOOR, knn_validation_PP1_FLOOR, knn_validation_PP2_FLOOR)
results_F_PP_val <- as.data.frame(results_F_PP_val)

results_F_PP_val1 <- results_F_PP_val %>%  tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_PP = factor(name, levels=c(Initial = "knn_validation_FLOOR", 
                                         "knn_validation_PP_FLOOR", "knn_validation_PP1_FLOOR", "knn_validation_PP2_FLOOR"),
                          labels = c("Initial", "Preprocess_1", "Preprocess_2", "Preprocess_3"))) %>% 
  select(-(name)) 



ggplot() +
  geom_line(data = results_F_PP_test1, aes(x = name_PP, y = value, group = "name_test", color = "test")) +
  geom_point(data = results_F_PP_test1, aes(x = name_PP, y = value)) +
  geom_line(data = results_F_PP_val1, aes(x = name_PP, y = value, group = "name_val", color = "validation")) +
  geom_point(data = results_F_PP_val1, aes(x = name_PP, y = value)) +
  facet_grid(metrics~.) +
  scale_color_manual(name = "Metrics \n Floor", values = c("test" = "darkblue", "validation" = "red")) +
  labs(title = "Metrics for Floor KNN Model",
       x = "Preprocess step",
       y = "Value") +
  geom_label(data = results_F_PP_test1, aes(x= name_PP, y = value, label = round(value, 4))) +
  geom_label(data = results_F_PP_val1, aes(x= name_PP, y = value, label = round(value, 4)))
  
results_F_PP_test1 <- as.data.frame(results_F_PP_test1)

s

results_LO_PP_test <- cbind(knn_test_LONGITUDE, knn_test_PP_LONGITUDE, knn_test_PP1_LONGITUDE, knn_test_PP2_LONGITUDE)


results_LO_PP_test <- as.data.frame(results_LO_PP_test) 
results_LO_PP_test1 <- results_LO_PP_test %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_PP = factor(name, levels=c("knn_test_LONGITUDE", 
                                         "knn_test_PP_LONGITUDE","knn_test_PP1_LONGITUDE","knn_test_PP2_LONGITUDE" ),
                          labels = c("Initial", "Preprocess_1", "Preprocess_2","Preprocess_3"))) %>% 
  select(-(name))

results_LO_PP_val <- cbind(knn_validation_LONGITUDE,knn_validation_PP_LONGITUDE,
                           knn_validation_PP1_LONGITUDE, knn_validation_PP2_LONGITUDE)
results_LO_PP_val <- as.data.frame(results_LO_PP_val)

results_LO_PP_val1 <- results_LO_PP_val %>%  tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_PP = factor(name, levels=c(Initial = "knn_validation_LONGITUDE", 
                                         "knn_validation_PP_LONGITUDE", "knn_validation_PP1_LONGITUDE", "knn_validation_PP2_LONGITUDE"),
                          labels = c("Initial", "Preprocess_1", "Preprocess_2","Preprocess_3" ))) %>% 
  select(-(name)) 



ggplot() +
  geom_line(data = results_LO_PP_test1, aes(x = name_PP, y = value, group = "name_test", color = "test")) +
  geom_line(data = results_LO_PP_val1, aes(x = name_PP, y = value, group = "name_val", color = "validation")) +
  facet_grid(metrics~.) +
  scale_color_manual(name = "Metrics \n Longitude", values = c("test" = "darkblue", "validation" = "red")) +
  labs(title = "Metrics for LONGITUDE KNN Model",
       x = "Preprocess step",
       y = "Value")




results_LA_PP_test <- cbind(knn_test_LATITUDE, knn_test_PP_LATITUDE, knn_test_PP1_LATITUDE, knn_test_PP2_LATITUDE)


results_LA_PP_test <- as.data.frame(results_LA_PP_test) 
results_LA_PP_test1 <- results_LA_PP_test %>% tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_PP = factor(name, levels=c("knn_test_LATITUDE", 
                                         "knn_test_PP_LATITUDE","knn_test_PP1_LATITUDE", "knn_test_PP2_LATITUDE"),
                          labels = c("Initial", "Preprocess_1", "Preprocess_2","Preprocess_3") )) %>% 
  select(-(name))

results_LA_PP_val <- cbind(knn_validation_LATITUDE,knn_validation_PP_LATITUDE, 
                           knn_validation_PP1_LATITUDE, knn_validation_PP2_LATITUDE)
results_LA_PP_val <- as.data.frame(results_LA_PP_val)

results_LA_PP_val1 <- results_LA_PP_val %>%  tibble::rownames_to_column("metrics") %>% 
  pivot_longer(cols = -metrics) %>% 
  mutate(name_PP = factor(name, levels=c(Initial = "knn_validation_LATITUDE", 
                                         "knn_validation_PP_LATITUDE", "knn_validation_PP1_LATITUDE", "knn_validation_PP2_LATITUDE"),
                          labels = c("Initial", "Preprocess_1", "Preprocess_2", "Preprocess_3"))) %>% 
  select(-(name)) 



ggplot() +
  geom_line(data = results_LA_PP_test1, aes(x = name_PP, y = value, group = "name_test", color = "test")) +
  geom_line(data = results_LA_PP_val1, aes(x = name_PP, y = value, group = "name_val", color = "validation")) +
  facet_grid(metrics~.) +
  scale_color_manual(name = "Metrics \n Latitude", values = c("test" = "darkblue", "validation" = "red")) +
  labs(title = "Metrics for LATITUDE KNN Model",
       x = "Preprocess step",
       y = "Value")

