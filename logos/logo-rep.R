#Install packages
install.packages("hexSticker") #hexsticker generator
install.packages("magick") #Advanced image processing
install.packages("sysfonts") #Font selection
install.packages("tidyverse") #Core data manipulation and ggplot2 graphics engine


#Load packages
require(hexSticker)
require(magick)
require(sysfonts)
require(tidyverse)

#HexSticker Creation

#a) Import Icon Elements
#tree_img <- image_read("icon elements\\ABSLIFE_TREE.png")
#graph_img <- image_read("./logos/graph.png")
graph_img <- image_read("./logos/crc-zoom-2.png")
#graph_img <- image_read("icon elements\\graph.png")

#funnel_img <- image_read("icon elements\\funnel.png")

#b) Font Import
font_data <- font_files()                                      #List all fonts available in computer
#font_add("Segoeui", "segoeui.ttf")                             #add the Segoeui font
#font_add("Segoeui Bold", "segoeuib.ttf")                       #add the Segoeui Bold font
#font_add("Georgia", "georgia.ttf")                             #add the Georgia font
font_add("Times New Roman", "times.ttf")

#d) Sticker 2 Creation
sticker(
  subplot = graph_img,                                         #load the icon
  package = "abslife",                                         #change title in the sticker
  filename = "./logos/abslife_graph.png",                       #save the sticker in the output file
  s_width = 1,                                               #change icon width
  s_height = 0.6,                                              #change icon height
  s_x = 1,                                                     #change icon location on x-axis
  s_y = 1.08,                                                  #change icon location on y-axis
  p_size = 21,                                                 #change title size
  p_x = 1,                                                     #change title location on x-axis
  p_y = 0.6,                                                   #change title location on y-axis
  p_family = "Times New Roman",                                   #add title font
  p_color = "#3A5A6C",                                         #change title color
  h_fill = "#FAF9F6",                                          #change background color
  h_color = "#3A5A6C",                                         #change border color
  h_size = 2.5,                                                #change border thickness
  url = "https://lcgodoy.me/abslife/index.html",               #add url
  u_size = 3.3,                                                #change url text size
  u_color = "#3A5A6C",    
) %>% print()
