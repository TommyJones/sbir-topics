library(patchwork)

p1 <- blah %>% 
  filter(topic == 32) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = prevalence)) +
  xlim(1990, 2021)

p2 <- blah %>% 
  filter(topic == 96) %>% 
  ggplot() +  
  geom_line(aes(x = year, y = prevalence)) + 
  xlim(1990, 2021)

p1 / p2