rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(tidylog)
library(ggiraph)
library(ggpubr)

df1920 <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2019-2020-School-Vaccination-Coverage-Levels---Kindergarten.xlsx",sheet = "Coverage by District")
df2021 <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2020-2021-School-Vaccination-Coverage-Levels-by-District-Private-School-and-County---Kindergarten.xlsx",sheet = "Coverage by District")
df2122 <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2021-2022-School-Vaccination-Coverage-by-District-and-County-Kindergarten.xls.xlsx",sheet = "Coverage by District")
df2223 <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/22-23-School-Vaccination-Coverage-by-District-and-County-K.xlsx",sheet = "Coverage by District")
df2324 <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2023-2024_School_Vaccination_Coverage_Levels_Kindergarten.xlsx",sheet = "Coverage by District")

df_num1920<- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2020_KG_Readiness_Data_District.xlsx",sheet="KG ENROLL ASSMT")
df_num2021<- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2021_KG_Readiness_Data_District.xlsx",sheet="KG ENROLL ASSMT")
df_num2122<- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2022_KG_Readiness_Data_District.xlsx",sheet="KG ENROLL ASSMT")
df_num2223<- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2023_KG_Readiness_Data_District.xlsx",sheet="KG ENROLL ASSMT")
df_num2324<- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/TX/2024_KG_Readiness_Data_District.xlsx",sheet="KG ENROLL ASSMT")

names(df1920)<-df1920[2,]
names(df2021)<-df2021[2,]
names(df2122)<-df2122[2,]
names(df2223)<-df2223[2,]
names(df2324)<-df2324[2,]

names(df1920)[1]<-names(df2021)[1]<-names(df2122)[1]<-names(df2223)[1]<-names(df2324)[1]<-"ID"

df <- df1920 %>% 
  full_join(df2021,by="ID") %>% 
  full_join(df2122,by="ID") %>% 
  full_join(df2223,by="ID") %>% 
  full_join(df2324,by="ID") 

df_num1920 <- df_num1920 %>% 
  dplyr::select(DIST_NAME,TOTAL_ENROLLED,DIST_CODE) %>% 
  rename("districtname"="DIST_NAME",
         "numberenrolled1920"="TOTAL_ENROLLED")

df_num2021 <- df_num2021 %>% 
  dplyr::select(DIST_NAME,TOTAL_ENROLLED) %>% 
  rename("districtname"="DIST_NAME",
         "numberenrolled2021"="TOTAL_ENROLLED")

df_num2122 <- df_num2122 %>% 
  dplyr::select(DIST_NAME,TOTAL_ENROLLED) %>% 
  rename("districtname"="DIST_NAME",
         "numberenrolled2122"="TOTAL_ENROLLED")

df_num2223 <- df_num2223 %>% 
  dplyr::select(DIST_NAME,TOTAL_ENROLLED) %>% 
  rename("districtname"="DIST_NAME",
         "numberenrolled2223"="TOTAL_ENROLLED")

df_num2324 <- df_num2324 %>% 
  dplyr::select(DIST_NAME,TOTAL_ENROLLED) %>% 
  rename("districtname"="DIST_NAME",
         "numberenrolled2324"="TOTAL_ENROLLED")

df1920<-df1920 %>% dplyr::select(`Facility Name`,MMR,County) %>% 
  rename("districtname"="Facility Name",
         "MMR1920"="MMR")
df1920$districtname<-(toupper(df1920$districtname))

df2021<-df2021 %>% dplyr::select(`Facility Name`,MMR) %>% 
  rename("districtname"="Facility Name",
         "MMR2021"="MMR")
df2021$districtname<-(toupper(df2021$districtname))

df2122<-df2122 %>% dplyr::select(`Facility Name`,MMR) %>% 
  rename("districtname"="Facility Name",
         "MMR2122"="MMR")
df2122$districtname<-(toupper(df2122$districtname))

df2223<-df2223 %>% dplyr::select(`Facility Name`,MMR) %>% 
  rename("districtname"="Facility Name",
         "MMR2223"="MMR")
df2223$districtname<-(toupper(df2223$districtname))

df2324<-df2324 %>% dplyr::select(`Facility Name`,MMR) %>% 
  rename("districtname"="Facility Name",
         "MMR2324"="MMR")
df2324$districtname<-(toupper(df2324$districtname))

df_1920<-df1920 %>% full_join(df_num1920,by = "districtname")
df_2021<-df2021 %>% full_join(df_num2021,by = "districtname")
df_2122<-df2122 %>% full_join(df_num2122,by = "districtname")
df_2223<-df2122 %>% full_join(df_num2223,by = "districtname")
df_2324<-df2122 %>% full_join(df_num2324,by = "districtname")

df1<-df1920 %>% 
  full_join(df_num1920,by = "districtname") %>% 
  full_join(df_num2021,by = "districtname") %>% 
  full_join(df_num2122,by = "districtname") %>% 
  full_join(df_num2223,by = "districtname") %>% 
  full_join(df_num2324,by = "districtname") %>%
  full_join(df2021,by="districtname") %>%
  full_join(df2122,by="districtname") %>%
  full_join(df2223,by="districtname") %>%
  full_join(df2324,by="districtname") %>%
  slice(-1:-2)

df1<-as.data.frame(df1)
df<-df1 %>% mutate(cs1=(1-as.numeric(MMR1920))*as.numeric(numberenrolled1920),
                   cs2=(1-as.numeric(MMR2021))*as.numeric(numberenrolled2021),
                   cs3=(1-as.numeric(MMR2122))*as.numeric(numberenrolled2122),
                   cs4=(1-as.numeric(MMR2223))*as.numeric(numberenrolled2223),
                   cs5=(1-as.numeric(MMR2324))*as.numeric(numberenrolled2324)) %>%
  mutate(ccs1=cs1,
         ccs2=cs1+cs2,
         ccs3=cs1+cs2+cs3,
         ccs4=cs1+cs2+cs3+cs4,
         ccs5=cs1+cs2+cs3+cs4+cs5) %>%
  mutate(cccs1=as.numeric(numberenrolled1920),
         cccs2=ccs1+as.numeric(numberenrolled2021),
         cccs3=ccs2+as.numeric(numberenrolled2122),
         cccs4=ccs3+as.numeric(numberenrolled2223),
         cccs5=ccs4+as.numeric(numberenrolled2324)) %>%
  mutate(frac1=ccs1/cccs1,
         frac2=ccs2/cccs2,
         frac3=ccs3/cccs3,
         frac4=ccs4/cccs4,
         frac5=ccs5/cccs5)

# dat <- data.frame(year=c(rep("2019-2020",length(df$MMR.x)),rep("2020-2021",length(df$MMR.x.x)),rep("2021-2022",length(df$MMR.y)),rep("2022-2023",length(df$MMR.y.y)),rep("2023-2024",length(df$MMR))),
#                   prop=c(as.numeric(df$MMR.x),as.numeric(df$MMR.x.x),as.numeric(df$MMR.y),as.numeric(df$MMR.y.y),as.numeric(df$MMR)))
# 
# ggplot(dat)+  geom_density(aes(as.numeric(prop),fill=year),alpha=0.3)

df_new<-df %>%
  group_by(County) %>%
  summarise(ccs1.2=sum(ccs1,na.rm = TRUE),
         ccs2.2=sum(ccs2,na.rm = TRUE),
         ccs3.2=sum(ccs3,na.rm = TRUE),
         ccs4.2=sum(ccs4,na.rm = TRUE),
         ccs5.2=sum(ccs5,na.rm = TRUE),
         cccs1.2=sum(cccs1,na.rm = TRUE),
         cccs2.2=sum(cccs2,na.rm = TRUE),
         cccs3.2=sum(cccs3,na.rm = TRUE),
         cccs4.2=sum(cccs4,na.rm = TRUE),
         cccs5.2=sum(cccs5,na.rm = TRUE),
         frac1.2=ccs1.2/cccs1.2,
         frac2.2=ccs2.2/cccs2.2,
         frac3.2=ccs3.2/cccs3.2,
         frac4.2=ccs4.2/cccs4.2,
         frac5.2=ccs5.2/cccs5.2,
         num_unvaccinated1=sum(cs1,na.rm = TRUE),
         num_unvaccinated2=sum(cs2,na.rm = TRUE),
         num_unvaccinated3=sum(cs3,na.rm = TRUE),
         num_unvaccinated4=sum(cs4,na.rm = TRUE),
         num_unvaccinated5=sum(cs5,na.rm = TRUE))

TX <- read_sf("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/Counties_865228647739904830", "Counties")
head(TX)
colnames(TX)[4]<-"County"
TX$County<-toupper(TX$County)
df_new$County<-toupper(df_new$County)

dat_new <- TX %>% left_join(df_new,by="County")


# Define the breakpoints for the categories for population
breakpoints <- c(0,0.2,0.4,0.6,0.8,1)

# Convert to integer
dat_new$Pop_int <- (dat_new$frac1.2)

# Create categories
dat_new$pop_category <- cut(dat_new$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
dat_new <- dat_new %>%
  mutate(info = paste(
    "Name: ", County
  ))
p1 <- ggplot(dat_new, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.2)" = "#F7FCFD",
      "small (0.2 - 0.4)" = "#99D8C9",
      "medium (0.4 - 0.6)" = "#41AE76",
      "large (0.6 - 0.8)" = "#238B45",
      "very large (>0.8)" = "#00441B"),na.value = "gray92" ) +
  labs(title = "2019-20", 
       subtitle = "",
       fill="",
       caption = "")  +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 20,hjust = 0.5),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

p1

# pdf("rplot5.pdf") 
# plot3
# dev.off()

df_1<-data.frame(age=rep(c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o."),5),
                   year=rep(c("2019-2020","2020-2021","2021-2022","2022-2023","2023-2024"),each=5),
                   nonvac=c(sum(as.numeric(df_new$num_unvaccinated1)),rep(0,4),
                            sum(as.numeric(df_new$num_unvaccinated2)),sum(as.numeric(df_new$num_unvaccinated1)),rep(0,3),
                            sum(as.numeric(df_new$num_unvaccinated3)),sum(as.numeric(df_new$num_unvaccinated2)),sum(as.numeric(df_new$num_unvaccinated1)),rep(0,2),
                            sum(as.numeric(df_new$num_unvaccinated4)),sum(as.numeric(df_new$num_unvaccinated3)),sum(as.numeric(df_new$num_unvaccinated2)),sum(as.numeric(df_new$num_unvaccinated1)),rep(0,1),
                            sum(as.numeric(df_new$num_unvaccinated5)),sum(as.numeric(df_new$num_unvaccinated4)),sum(as.numeric(df_new$num_unvaccinated3)),sum(as.numeric(df_new$num_unvaccinated2)),sum(as.numeric(df_new$num_unvaccinated1))))
                            
df_1$age <- factor(df_1$age, levels = c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o."))

# To use for fills, add
p2<-ggplot(df_1, aes(fill=age, y=nonvac, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("")+
  ylab("Number of MMR unvaccinated")+
  theme(legend.title = element_blank(),
        text = element_text(size = 14))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = wes_palette(5, name = "Zissou1", type = "continuous"), name = "")+
  labs(tag = "(A)")+ theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

p1<-data.frame(year=c(rep("2019-2020",length(dat_new$frac1.2)),rep("2020-2021",length(dat_new$frac2.2)),rep("2021-2022",length(dat_new$frac3.2)),rep("2022-2023",length(dat_new$frac4.2)),rep("2023-2024",length(dat_new$frac5.2))),
               frac=c(dat_new$frac1.2,dat_new$frac2.2,dat_new$frac3.2,dat_new$frac4.2,dat_new$frac5.2)) 
p1$year<- factor(p1$year, levels=c("2023-2024","2022-2023","2021-2022","2020-2021","2019-2020","2018-2019","2017-2018","2016-2017"))
p<-ggplot(
  p1, 
  aes(x = frac, y = as.factor(year), fill = stat(x))
) +
  geom_density_ridges_gradient( ) +
  scale_fill_viridis_c(name = "", option = "G",begin = 0,end = 1, direction = -1)+
  labs(title = '') +
  ylab("")+
  xlab("Ratios of MMR unvaccinated")+
  coord_cartesian(clip = "off") + # To avoid cut off
  theme_minimal()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 14))  +
  labs(tag = "(B)")+xlim(0,1)

ggarrange(p2,p)



