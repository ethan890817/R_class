control <- read.csv("ILUC_DARE_controls_x_y.csv")
campaign <- read.csv("ILUC_DARE_campaign_x_y.csv") #this file is too big
class(campaign$timestamp)
library(ggplot2)
library(ggmap)
library(rnaturalearth)
library(leaflet)
library(tidyverse)
library(forcats)
library(vegan)
library(RColorBrewer)

campaign.rqs0.8 <- campaign[which(campaign$rqs>=0.8),]
campaign.rqs0.8_pivot <- campaign.rqs0.8%>%pivot_wider(id_cols = c(sampleid,x,y),
                                       names_from = step,values_from = answer,
                                       values_fn = function(x)paste(x,collapse = ","))

#從這開始
control_pivot <- control%>%pivot_wider(id_cols = c(sampleid,x,y),
                                       names_from = step,values_from = answer,
                                       values_fn = function(x)paste(x,collapse = ","))
control_pivot <- rename(control_pivot, architecture=step3,predominant.driver=step1,other.drivers=step2)
#有些sampleid 有多個step2

world <- ne_countries(scale = "medium", returnclass = "sf")
architecture <- ggplot(data=world)+
  geom_sf()+
  coord_sf(expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,
                                    colour=factor(architecture,levels = c("Yes","No"))),
                                    alpha=0.9)+
  scale_color_manual(name="Architecture",values= c("Yes"="red","No"="green"))+
  theme_bw()+
  theme(legend.position = "bottom")
architecture

predominant.driver <- ggplot(data=world)+
  geom_sf()+
  coord_sf(expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.7)+
  ggtitle("Predominant drivers of global forest loss")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
predominant.driver

ggplot(data=world)+  #south America
  geom_sf()+
  coord_sf(xlim=c(-120,-20),ylim=c(-45,40),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in south America")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
s.America <- filter(control_pivot, x<=-20 & x>=-120 & y<=40 & y>=-45)


ggplot(data=world)+  #Africa
  geom_sf()+
  coord_sf(xlim=c(-20,55),ylim=c(-35,23.5),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in Africa")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
Africa <- filter(control_pivot, x<=55 & x>=-20 & y<=23.5 & y>=-35)


ggplot(data=world)+  #Asia
  geom_sf()+
  coord_sf(xlim=c(63,160),ylim=c(-35,40),expand=F)+
  labs(x="Longitude",y="Latitude")+
  geom_point(data=control_pivot,aes(x=x,y=y,colour=factor(predominant.driver)),
             alpha=0.9)+
  ggtitle("Predominant drivers of forest loss in Asia")+
  scale_color_brewer(name="Predominant drivers",palette= "Set1")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 8),
        legend.text = element_text(size=7))
Asia <- filter(control_pivot, x<=160 & x>=63 & y<=40 & y>=-35)


levels(factor(control_pivot$predominant.driver))
levels(factor(control_pivot$other.drivers))
levels(factor(control_pivot[which(control_pivot$forest.loss=="No"),"other.drivers"]))


predominant.driver <- fct_count(factor(control_pivot$predominant.driver),sort = T)
fct_count(factor(control_pivot$other.drivers),sort = T)
fct_count(factor(control_pivot$architecture),sort = T)
predominant.driver$ratio <- predominant.driver$n/sum(predominant.driver$n)
ratio.pre.driver <- ggplot(data = predominant.driver, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Predominant drivers",values= c(2:10))+
  geom_text(aes(label=paste(100*round(ratio,2),"%")),vjust=-0.3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in global")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.driver

predominant.drivers.SAmerica <- fct_count(factor(s.America$predominant.driver),sort = T)
predominant.drivers.SAmerica$ratio <- predominant.drivers.SAmerica$n/sum(predominant.drivers.SAmerica$n)
ratio.pre.driver.sAmerica <- ggplot(data = predominant.drivers.SAmerica, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Predominant drivers",values= c(2:10))+
  geom_text(aes(label=paste(100*round(ratio,2),"%")),vjust=-0.3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in South America")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.driver.sAmerica

predominant.drivers.Africa <- fct_count(factor(Africa$predominant.driver),sort = T)
predominant.drivers.Africa$ratio <- predominant.drivers.Africa$n/sum(predominant.drivers.Africa$n)
ratio.pre.drivers.Africa <- ggplot(data = predominant.drivers.Africa, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Predominant drivers",values= c(2:10))+
  geom_text(aes(label=paste(100*round(ratio,2),"%")),vjust=-0.3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in Africa")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.drivers.Africa

predominant.drivers.Asia <- fct_count(factor(Asia$predominant.driver),sort = T)
predominant.drivers.Asia$ratio <- predominant.drivers.Asia$n/sum(predominant.drivers.Asia$n)
ratio.pre.drivers.Asia <- ggplot(data = predominant.drivers.Asia, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Predominant drivers",values= c(2:10))+
  geom_text(aes(label=paste(100*round(ratio,2),"%")),vjust=-0.3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of predominant tree loss driver in Asia")+
  labs(x="Predominant drivers",y="Ratio")
ratio.pre.drivers.Asia

c17 <- c(
  "dodgerblue2", "#E31A1C","green4",
  "#6A3D9A","#FF7F00","black", "gold1",
  "skyblue2", "#FB9A99","palegreen2","#CAB2D6",
  "#FDBF6F", "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1")
other.drivers <- fct_count(factor(control_pivot$other.drivers),sort = T)
other.drivers$ratio <- other.drivers$n/sum(other.drivers$n)
ratio.other.drives <- ggplot(data = other.drivers, aes(x=f,y=ratio,fill=f))+
  geom_bar(stat = "identity")+
  scale_fill_manual(name="Other drivers",values =c17) +
  geom_text(aes(label=paste(100*round(ratio,2),"%")),vjust=-0.3)+
  theme_bw()+
  theme(axis.text.x = element_blank())+
  ggtitle("Ratio of other tree loss driver in global")+
  labs(x="Other drivers",y="Ratio")
ratio.other.drives

#presence.pre.drivers <- control_pivot%>%
  #mutate(presence=1)%>%
 # pivot_wider(id_cols = c(sampleid,x,y),names_from = predominant.driver,values_from = presence)
#presence.pre.drivers[,-(1:3)] <-ifelse(is.na(presence.pre.drivers[,-(1:3)]),0,1)
#presence.pre.drivers.nmds <- metaMDS(presence.pre.drivers[,-(1:3)],distance = "bray",trymax = 10)
#plot(presence.pre.drivers.nmds,type = "t")
