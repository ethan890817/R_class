library(dplyr)
library(tidyr)
rairuoho <- read.table("rairuoho.txt", header=T, sep='\t', dec='.')
rairuoho
nutrient <- rairuoho$treatment == "nutrient" #找出column treatment為nutrient 
rairuoho[nutrient,"treatment"] <- "enriched" #將nutrient替代成enriched

rairuoho_day <- rairuoho%>%pivot_longer(day3:day8, names_to = "day", values_to = "length")

rairuoho_spatial <- rairuoho_day%>%mutate(spatial_coordinates=stringr::str_c(spatial1,spatial2, sep="_"))

rairuoho_remove <- rairuoho_day%>%select(-spatial1,-spatial2,-row,-column)
