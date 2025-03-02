rm(list = ls())

setwd("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/MN")

library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(tidylog)
library(ggiraph)
library(ggpubr)

df_school2324 <- read_excel("kcounty2324.xlsx",sheet = "K_County")
df_school2324<-df_school2324 %>% 
  select(`Percent of Kindergarteners Vaccinated by County 2023-24`,...2,...11) %>%
  slice(-1)

df_school2223 <- read_excel("pervaxkind2223.xlsx",
                            sheet = "K_County")
colnames(df_school2223)<-df_school2223[1,]
df_school2223<-df_school2223[-1,]
df_school2223<-df_school2223 %>%select(County,`Kindergarten Enrollment`,`MMR % Vaccinated`)

df_school2122 <- read_excel("pervaxkind2122.xlsx",sheet = "K_County")
colnames(df_school2122)<-df_school2122[1,]
df_school2122<-df_school2122[-1,]
df_school2122<-df_school2122 %>%select(County,`Kindergarten Enrollment`,`MMR % Vaccinated`)

df_school2021 <- read_excel("pervaxkind2021.xlsx",sheet = "K_County")
df_school2021<-df_school2021[-c(1:7),]
df_school2021<-df_school2021 %>%select(`Percent of Kindergarteners Vaccinated by County 2020-21`,...2,...11)

df_school1920 <- read_excel("pervaxkind1920.xlsx",sheet = "K_County")
colnames(df_school1920)<-df_school1920[1,]
df_school1920<-df_school1920[-1,]
df_school1920<-df_school1920 %>%select(County,`Kindergarten Enrollment`,`MMR % Vaccinated`)

df_school1819 <- read_excel("pervaxkind1819.xlsx",sheet = "K_County")
df_school1819<-df_school1819 %>%select(`Percent of Kindergarteners Vaccinated by County 2018-19`,...2,...11)
colnames(df_school1819)<-df_school1819[7,]
df_school1819<-df_school1819[-c(1:7),]

df_school1718 <- read_excel("pervaxkind1718.xlsx",sheet = "K_County")
df_school1718<-df_school1718 %>%select(`Percent of Kindergarteners Vaccinated by County 2017-18`,...2,...11)
colnames(df_school1718)<-df_school1718[7,]
df_school1718<-df_school1718[-c(1:7),]

df_school1617 <- read_excel("pervaxkind1617.xlsx",sheet = "K_County")
df_school1617<-df_school1617 %>%select(`Percent of Kindergarteners Vaccinated by County`,...2,...11)
colnames(df_school1617)<-df_school1617[7,]
df_school1617<-df_school1617[-c(1:7),]

df_school1516 <- read_excel("pervaxkind1516.xlsx",sheet = "K_County")
df_school1516<-df_school1516 %>%select(`Percent of Kindergarteners Vaccinated by County`,...2,...11)
colnames(df_school1516)<-df_school1516[7,]
df_school1516<-df_school1516[-c(1:7),]

df_school1415 <- read_excel("pervaxkind1415.xlsx",sheet = "K_County")
df_school1415<-df_school1415 %>%select(`*"% Partially Vaccinated or No Doses" includes students that are in the process of completing a vaccine series, have not been vaccinated and do not have an exemption on file, or who did not submit vaccine records to their school health staff.`,...2,...11)
colnames(df_school1415)<-df_school1415[2,]
df_school1415<-df_school1415[-c(1:3),]

df_school1314 <- read_excel("pervaxkind1314.xlsx",sheet = "K_County")
df_school1314<-df_school1314 %>% select(`* "% Vaccinated" includes students who are either up-to-date or are in the process of completing vaccination (partially vaccinated). As a result, "percent vaccinated"  may include students who are not fully protected against disease.`,...2,...9)
df_school1314<-df_school1314[-c(1:4),]

names(df_school1314)<-c("county","numberofkids1314","vaccinated1314")
names(df_school1415)<-c("county","numberofkids1415","vaccinated1415")
names(df_school1516)<-c("county","numberofkids1516","vaccinated1516")
names(df_school1617)<-c("county","numberofkids1617","vaccinated1617")
names(df_school1718)<-c("county","numberofkids1718","vaccinated1718")
names(df_school1819)<-c("county","numberofkids1819","vaccinated1819")
names(df_school1920)<-c("county","numberofkids1920","vaccinated1920")
names(df_school2021)<-c("county","numberofkids2021","vaccinated2021")
names(df_school2122)<-c("county","numberofkids2122","vaccinated2122")
names(df_school2223)<-c("county","numberofkids2223","vaccinated2223")
names(df_school2324)<-c("county","numberofkids2324","vaccinated2324")

df_school1314$county<-str_to_upper(df_school1314$county)
df_school1415$county<-str_to_upper(df_school1415$county)
df_school1516$county<-str_to_upper(df_school1516$county)
df_school1617$county<-str_to_upper(df_school1617$county)
df_school1718$county<-str_to_upper(df_school1718$county)
df_school1819$county<-str_to_upper(df_school1819$county)
df_school1920$county<-str_to_upper(df_school1920$county)
df_school2021$county<-str_to_upper(df_school2021$county)
df_school2122$county<-str_to_upper(df_school2122$county)
df_school2223$county<-str_to_upper(df_school2223$county)
df_school2324$county<-str_to_upper(df_school2324$county)

df <- df_school2324 %>%
  full_join(df_school2223, by="county") %>%
  full_join(df_school2122, by="county" )%>%
  full_join(df_school2021,by="county") %>%
  full_join(df_school1920,by="county") %>%
  full_join(df_school1819,by="county") %>%
  full_join(df_school1718,by="county") %>%
  full_join(df_school1617,by="county") %>%
  full_join(df_school1516,by="county") %>%
  full_join(df_school1415,by="county") %>%
  full_join(df_school1314,by="county")

df<-as.data.frame(df)
df1<-df %>% mutate(cs1=(1-as.numeric(vaccinated1314))*as.numeric(numberofkids1314),
                   cs2=(1-as.numeric(vaccinated1415))*as.numeric(numberofkids1415),
                   cs3=(1-as.numeric(vaccinated1516))*as.numeric(numberofkids1516),
                   cs4=(1-as.numeric(vaccinated1617))*as.numeric(numberofkids1617),
                   cs5=(1-as.numeric(vaccinated1718))*as.numeric(numberofkids1718),
                   cs6=(1-as.numeric(vaccinated1819))*as.numeric(numberofkids1819),
                   cs7=(1-as.numeric(vaccinated1920))*as.numeric(numberofkids1920),
                   cs8=(1-as.numeric(vaccinated2021))*as.numeric(numberofkids2021),
                   cs9=(1-as.numeric(vaccinated2122))*as.numeric(numberofkids2122),
                   cs10=(1-as.numeric(vaccinated2223))*as.numeric(numberofkids2223),
                   cs11=(1-as.numeric(vaccinated2324))*as.numeric(numberofkids2324)) %>%
  mutate(ccs1=cs1,
         ccs2=cs1+cs2,
         ccs3=cs1+cs2+cs3,
         ccs4=cs1+cs2+cs3+cs4,
         ccs5=cs1+cs2+cs3+cs4+cs5,
         ccs6=cs1+cs2+cs3+cs4+cs5+cs6,
         ccs7=cs1+cs2+cs3+cs4+cs5+cs6+cs7,
         ccs8=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8,
         ccs9=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9,
         ccs10=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9+cs10,
         ccs11=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9+cs10+cs11) %>%
  mutate(cccs1=as.numeric(numberofkids1314),
         cccs2=ccs1+as.numeric(numberofkids1415),
         cccs3=ccs2+as.numeric(numberofkids1516),
         cccs4=ccs3+as.numeric(numberofkids1617),
         cccs5=ccs4+as.numeric(numberofkids1718),
         cccs6=ccs5+as.numeric(numberofkids1819),
         cccs7=ccs6+as.numeric(numberofkids1920),
         cccs8=ccs7+as.numeric(numberofkids2021),
         cccs9=ccs8+as.numeric(numberofkids2122),
         cccs10=ccs9+as.numeric(numberofkids2223),
         cccs11=ccs10+as.numeric(numberofkids2324)) %>%
  mutate(frac1=ccs1/cccs1,
         frac2=ccs2/cccs2,
         frac3=ccs3/cccs3,
         frac4=ccs4/cccs4,
         frac5=ccs5/cccs5,
         frac6=ccs6/cccs6,
         frac7=ccs7/cccs7,
         frac8=ccs8/cccs8,
         frac9=ccs9/cccs9,
         frac10=ccs10/cccs10,
         frac11=ccs11/cccs11)

mn <- read_sf("/Users/schen/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/shp_bdry_counties_in_minnesota", "mn_county_boundaries_500")
head(mn)
colnames(mn)[6]<-"county"
mn$county<-toupper(mn$county)

mn <- mn %>% left_join(df1,by="county")
# Define the breakpoints for the categories for population
breakpoints <- c(0,0.2,0.4,0.6,0.8,1)

mn$Pop_int <- as.numeric(mn$frac1)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p1 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 

mn$Pop_int <- as.numeric(mn$frac2)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p2 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac3)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p3 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac4)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p4 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac5)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p5 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac6)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p6 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac7)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p7 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac8)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p8 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 


mn$Pop_int <- as.numeric(mn$frac9)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p9 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 

mn$Pop_int <- as.numeric(mn$frac10)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county,
    "cccs5", cccs5,
    "cccs6", cccs6,
    "cccs7", cccs7,
    "cccs9", cccs8
  ))
p10 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B") ) +
  labs(title = "", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        legend.position = "none") 
