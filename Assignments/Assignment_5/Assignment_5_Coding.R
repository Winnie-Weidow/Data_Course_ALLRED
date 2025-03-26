#Library/Objects####
Ugly_Mushroom <- read.csv('Data/mushroom_growth.csv')

library(dplyr)
library(grid)
library(ggimage)
library(ggplot2)
library(tidyverse)
library(magick)
library(gganimate)
library(png)
library(jpeg)

image_path <- "Assignments/Assignment_5/ugly_mushroom.png"
image_path2 <- "Assignments/Assignment_5/Creepy_Mushroom.jpg"

##Trials####
Ugly_Mushroom %>% 
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_line(color = "purple", size = 4) +        # Thick purple line
  geom_point(color = "yellow", size = 6) +       # Large yellow points
  theme_minimal() +                              # Use a minimalist theme (could be okay but it's overly simple)
  ggtitle("Mushroom Growth") +                   # Ugly title with no formatting
  xlab("Temperature (ew)") +                          # Weird x-axis label
  ylab("Growth Rate (yuck)") +                        # Cringey y-axis label
  theme(axis.text.x = element_text(angle = 90, size = 20, color = "red"),  # Over-the-top text
        axis.text.y = element_text(size = 20, color = "green"),          # Over-the-top y-axis text
        axis.title.x = element_text(size = 25, color = "blue"),
        axis.title.y = element_text(size = 25, color = "blue"),
        panel.grid.major = element_line(color = "pink", size = 2),  # Loud grid lines
        panel.grid.minor = element_line(color = "orange", size = 1))+
  tibble(time = rep(1:10, each = 10),
         x = rnorm(100),
         y = rnorm(100),
         group = rep(letters[1:10], times = 10))

Ugly_Shroom <- Ugly_Mushroom %>% #trial
  ggplot(aes(x = Humidity, y = Temperature))+
  geom_boxplot(fill = "green",
               color = 'yellow',
               size = 2)+
  theme(axis.title = element_text(size = 20, color = "brown"),
        axis.text = element_text(size = 15, color = "purple"),
        panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "steelblue"),
        panel.grid = element_line(color = "orange", linewidth = 2),
        plot.margin = margin(10, 50, 10, 50))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(angle = 85, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(angle = 40))+
  geom_jitter(width = 0.1,
              size = 4,
              color = "red")+
  labs(title = "Shrooms: {frame_time}",
       x = "HumiDIty (gross)",
       y = "temperAture (ew)",
       subtitle = "Ain't Nobody Got Time Fo Dat",
       caption = "Created by WINNIE")+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  transition_time(GrowthRate)

###Ugly_Shroom####
Ugly_Shroom <- Ugly_Mushroom %>% 
  ggplot(aes(x = Humidity, y = Temperature))+
  geom_boxplot(fill = "green",
               color = 'yellow',
               size = 2)+
  geom_image(aes(image = image_path), size = 0.5)+
  theme(axis.title = element_text(size = 20, color = "brown"),
        axis.text = element_text(size = 15, color = "purple"),
        panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "steelblue"),
        panel.grid = element_line(color = "orange", linewidth = 2),
        plot.margin = margin(10, 50, 10, 50))+
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(angle = 195, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(angle = 300))+
  geom_jitter(width = 0.1,
              size = 4)+
  labs(title = "Shroom Growth: {frame_time}",
       x = "HumiDIty (gross)",
       y = "temperAture (ew)",
       subtitle = "All mushrooms are edible, but some only once in a lifetime.",
       caption = "Created by WINNIE")+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  # Animate the plot based on 'GrowthRate'
  transition_time(GrowthRate)

Ugly_Shroom

####Creepy_Mushroom####
Creepy_Mushroom <- Ugly_Mushroom %>% 
  ggplot(aes(x = Humidity, y = Temperature))+
  geom_boxplot(fill = "green",
               color = 'yellow',
               size = 2)+
  geom_image(aes(image = image_path2), size = 0.5)+
  theme(axis.title = element_text(size = 20, color = "brown"),
        axis.text = element_text(size = 15, color = "purple"),
        panel.background = element_rect(fill = "lightblue"),
        plot.background = element_rect(fill = "pink"),
        panel.grid = element_line(color = "orange", linewidth = 2),
        plot.margin = margin(10, 50, 10, 50))+
  theme(axis.text.x = element_text(angle = 20, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(angle = 195, vjust = 0.5, hjust = 0.5),
        axis.title.y = element_text(angle = 300))+
  geom_jitter(width = 0.1,
              size = 4)+
  labs(title = "Shroom Growth: {frame_time}",
       x = "HumiDIty (gross)",
       y = "temperAture (ew)",
       subtitle = "Ain't Nobody Got Time Fo Dat",
       caption = "Created by WINNIE")+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  # Animate the plot based on 'GrowthRate'
  transition_time(GrowthRate)

Creepy_Mushroom

#####Ugly_Shroom2_geom_point####
Ugly_Shroom2 <- Ugly_Mushroom %>% 
  ggplot(aes(x = Light, y = Nitrogen))+
  geom_point()+
  geom_image(aes(image = image_path), size = 0.5)+
  theme(title = element_text(family = "serif", face = "bold", size = 15, color = "salmon1"),
        axis.title = element_text(family = "mono", face = "italic", size = 20, color = "seagreen"),
        axis.text = element_text(size = 15, color = "purple"),
        panel.background = element_rect(fill = "deeppink"),
        plot.background = element_rect(fill = "gold"),
        panel.grid = element_line(color = "turquoise", linewidth = 2),
        plot.margin = margin(10, 50, 10, 50))+
  theme(axis.text.x = element_text(angle = 180, vjust = 0.1, hjust = 0.75),
        axis.title.x = element_text(angle = 7, vjust = 0.5, hjust = 0.58),
        axis.title.y = element_text(angle = 290))+
  geom_jitter(width = 0.1,
              size = 4,
              alpha = 0.2,
              color = 'lawngreen')+
  labs(title = "Shroom Hallucinations: {frame_time}",
       x = "LIGHT (ouch)",
       y = "NITROGEN (can't breath)",
       subtitle = "All mushrooms are edible, but some only once.")+
  shadow_wake(wake_length = 0.5, alpha = FALSE)+
  transition_time(GrowthRate)
Ugly_Shroom2

######Save####
anim_save("Fatty_unicorn.gif", Fatty_unicorn)
anim_save("Creepy_Mushroom_Animation.gif", Creepy_Mushroom)

#######Fat_Unicorn####

fat_unicorn <- readJPEG("Assignments/Assignment_5/fat_unicorn.jpg")

FatUnicorn_raster <- as.raster(fat_unicorn)

Fatty_unicorn <- Ugly_Mushroom %>% 
  ggplot(aes(x = Light, y = Nitrogen))+
  annotation_raster(FatUnicorn_raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+
  geom_point()+
  geom_image(aes(image = image_path), size = 0.5)+
  theme(title = element_text(family = "serif", face = "bold", size = 15, color = "salmon1"),
        axis.title = element_text(family = "mono", face = "italic", size = 20, color = "seagreen"),
        axis.text = element_text(size = 15, color = "purple"),
        plot.background = element_rect(fill = "gold"),
        plot.margin = margin(10, 50, 10, 50))+
  theme(axis.text.x = element_text(angle = 180, vjust = 0.1, hjust = 0.75),
        axis.title.x = element_text(angle = 7, vjust = 0.5, hjust = 0.58),
        axis.title.y = element_text(angle = 290))+
  geom_jitter(width = 0.1,
              size = 4,
              alpha = 0.2,
              color = 'lawngreen')+
  labs(title = "Shroom Hallucinations: {frame_time}",
       x = "LIGHT (ouch)",
       y = "NITROGEN (can't breath)",
       subtitle = "All mushrooms are edible, but some only once.",
       caption = "Created by WINNIE")+
  shadow_wake(wake_length = 0.5, alpha = FALSE)+
  transition_time(GrowthRate)
Fatty_unicorn
