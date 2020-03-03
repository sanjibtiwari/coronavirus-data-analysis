

# Loading libraries ----

devtools::install_github("RamiKrispin/coronavirus") # Data updated daily
library(coronavirus)
library(tidyverse)
library(lubridate)
library(leaflet)
library(htmltools)
library(gganimate)
library(animation)
library(gifski)
library(maps)


# Defining three layers 1. confirmed, 2. death and 3. recovered for the leaflet map

confirmed <- coronavirus %>% 
  filter(type == "confirmed") %>% 
  group_by(Province.State, Country.Region, Lat, Long, type) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup() %>% 
  mutate(label = paste("</p>", "Country: ", Country.Region, "</p>",
                       "</p>", "Province/State: ",Province.State, "</p>",
                       "</p>", "Type of Case: ", type, "</p>",
                       "</p>", "Number of Case: ", cases, "</p>"))


recovered <- coronavirus %>% 
  filter(type == "recovered") %>% 
  group_by(Province.State, Country.Region, Lat, Long, type) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup() %>% 
  mutate(label = paste("</p>", "Country: ", Country.Region, "</p>",
                       "</p>", "Province/State: ",Province.State, "</p>",
                       "</p>", "Type of Case: ", type, "</p>",
                       "</p>", "Number of Case: ", cases, "</p>"))



death <- coronavirus %>% 
  filter(type == "death") %>% 
  group_by(Province.State, Country.Region, Lat, Long, type) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup() %>% 
  mutate(label = paste("</p>", "Country: ", Country.Region, "</p>",
                       "</p>", "Province/State: ",Province.State, "</p>",
                       "</p>", "Type of Case: ", type, "</p>",
                       "</p>", "Number of Case: ", cases, "</p>"))


# Creating the leaflet map plot ----

  leaflet() %>% 
    addProviderTiles(providers$Esri.WorldStreetMap) %>%  
  # leaflet provides different providers. Check it out in this link 
  # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
   
   addMarkers(data   = confirmed, group = "Confirmed",
               lat    = ~ Lat, lng = ~Long, 
               label  = lapply(confirmed$label, HTML)) %>% 
    addMarkers(data   = recovered, group = "Recovered",
               lat    =  ~ Lat, lng = ~Long, 
               label  = lapply(recovered$label, HTML)) %>%
    addMarkers(data   = death, group = "Death",
               lat    = ~ Lat, lng = ~Long, 
               label  = lapply(death$label, HTML)) %>% 
    addLayersControl(
               baseGroups = c("Confirmed", "Recovered", "Death"),
               options    = layersControlOptions(collapsed = FALSE))


 # Animating the growth using cumulative sum by different type of cases ---- 
 # Use this link to lear more about gganimate 
 # https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/


# Preparing the data ----
animate_data <- coronavirus %>% 
  
  select(date, cases, type) %>%
  group_by(date, type) %>%
  summarize(cases =sum(cases)) %>%
  ungroup() %>%
  group_by(type) %>% 
  arrange(date) %>%
  mutate(increment = cumsum(cases)) %>% 
  ungroup() %>% 
  mutate(type = as.factor(type))
  
  
# Creating a ggplot object ---- 
animate_plot <- ggplot(data = animate_data) +
  
  geom_line(aes(date, increment, color = type)) +
  
  facet_wrap(~type, 
             ncol = 2, 
             scales = "free") +

    theme_bw() +
  
  theme(legend.position = "none",
        strip.text.x    = element_text(size = 16),
        axis.text.x     = element_text(size = 12),
        axis.text.y     = element_text(size = 12)) +
  
  labs (title           = str_c("Date: {frame_along}"),
        subtitle        =  "Cumulative Growth of Corona Virus Cases by Type",
        x               = "",
        y               = "Cases Growth") +
  
   transition_reveal(date) 


# Creating an animation output ----

data_animate <- animate(animate_plot, height =700, width = 1000, duration = 30)
  
anim_save("coronavirus_cumulative.gif", )





# Creating a map annimation with the corona virus dataset ----




int_data <- coronavirus %>% 
  select(Lat, Long, date, cases, type) %>% 
  group_by(Lat, Long, date, type) %>% 
  summarise(cases = sum (cases)) %>% 
  ungroup() 

dates <- tibble:: enframe(
  seq(min(coronavirus$date), today(), by = "days"))



map_animate_data <- int_data %>% 
  select(Lat, Long, type) %>% 
  expand(Lat, Long, type, dates$value) %>% 
  rename(date = `dates$value`) %>% 
  left_join(int_data, by = c("Lat", "Long", "type", "date")) %>% 
  mutate(cases = replace_na(cases,0)) %>% 
  arrange(date, Lat, Long, type) %>% 
  group_by(Lat, Long, type) %>% 
  arrange(date) %>% 
  mutate(total_cases = cumsum(cases)) %>% 
  ungroup() %>% 
  filter(total_cases >0)

# following 2 lines Just used to do some data validation 
# %>% 
#   filter(Lat == 	30.97560 & Long == 	112.2707 & type == "confirmed")



p <- ggplot() + 
  
  borders("world") +
  #theme_map() +
  
  geom_point(aes(x          = Long, 
                 y          = Lat, 
                 size       = total_cases,
                 color      = type
                 ), 
             data  =  map_animate_data , 
             alpha = 0.5) +
  
  scale_size_continuous(range = c(10,20)) +
  
  
  transition_manual(date) +
  
  labs(title = str_c("Date: {current_frame}")) 
                        
  

# doing the animation

  map_gif <- animate(p,height =700, width = 1000, duration = 60)  
  
  anim_save("coronavirus_map_anim.gif", map_gif)
  
  


