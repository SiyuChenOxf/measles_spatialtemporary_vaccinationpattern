rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(tidylog)
library(ggiraph)
library(ggpubr)

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 1617.xlsx")
dat1617 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3)
colnames(dat1617)<-c("county","num_unvaccinated1617","num_enrolled1617","vaccinate_rate1617")

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 1718.xlsx")
dat1718 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3)
colnames(dat1718)<-c("county","num_unvaccinated1718","num_enrolled1718","vaccinate_rate1718")

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 1819.xlsx")
dat1819 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3)
colnames(dat1819)<-c("county","num_unvaccinated1819","num_enrolled1819","vaccinate_rate1819")

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 1920.xlsx")
dat1920 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3) %>%
  mutate(num_unvaccinated1920=as.numeric(...6)- as.numeric(`Grade  /  Group`))
colnames(dat1920)<-c("county","num_vaccinated1920","num_enrolled1920","vaccinate_rate1920","num_unvaccinated1920")

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 2021.xlsx")
dat2021 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3) %>%
  mutate(num_unvaccinated2021=as.numeric(...6)- as.numeric(`Grade  /  Group`))
colnames(dat2021)<-c("county","num_vaccinated2021","num_enrolled2021","vaccinate_rate2021","num_unvaccinated2021")

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 2122.xlsx")
dat2122 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3) %>%
  mutate(num_unvaccinated2021=as.numeric(...6)- as.numeric(`Grade  /  Group`))
colnames(dat2122)<-c("county","num_vaccinated2122","num_enrolled2122","vaccinate_rate2122","num_unvaccinated2122")


df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 2223.xlsx")
dat2223 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3) %>%
  mutate(num_unvaccinated2021=as.numeric(...6)- as.numeric(`Grade  /  Group`))
colnames(dat2223)<-c("county","num_vaccinated2223","num_enrolled2223","vaccinate_rate2223","num_unvaccinated2223")

df <- read_excel("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/Wash/County Data Download 2324.xlsx")
dat2324 <- df %>% 
  dplyr::select(...1,`Grade  /  Group`,...6,...7) %>% 
  slice(-1:-3) %>%
  mutate(num_unvaccinated2324=as.numeric(...6)- as.numeric(`Grade  /  Group`))
colnames(dat2324)<-c("county","num_vaccinated2324","num_enrolled2324","vaccinate_rate2324","num_unvaccinated2324")

df <- dat1617 %>% 
  full_join(dat1718, by="county" ) %>%
  full_join(dat1819,by="county")   %>%
  full_join(dat1920,by="county")   %>%
  full_join(dat2021,by="county")   %>%
  full_join(dat2122,by="county")   %>%
  full_join(dat2223,by="county")   %>%
  full_join(dat2324,by="county")   

df1<-df %>%
  mutate(cs1=as.numeric(num_unvaccinated1617),
         cs2=as.numeric(num_unvaccinated1718),
         cs3=as.numeric(num_unvaccinated1819),
         cs4=as.numeric(num_unvaccinated1920),
         cs5=as.numeric(num_unvaccinated2021),
         cs6=as.numeric(num_unvaccinated2122),
         cs7=as.numeric(num_unvaccinated2223),
         cs8=as.numeric(num_unvaccinated2324)) %>%
  mutate(ccs1=cs1,
         ccs2=cs1+cs2,
         ccs3=cs1+cs2+cs3,
         ccs4=cs1+cs2+cs3+cs4,
         ccs5=cs1+cs2+cs3+cs4+cs5,
         ccs6=cs1+cs2+cs3+cs4+cs5+cs6,
         ccs7=cs1+cs2+cs3+cs4+cs5+cs6+cs7,
         ccs8=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8) %>%
  mutate(cccs1=as.numeric(num_enrolled1617),
         cccs2=ccs1+as.numeric(num_enrolled1718),
         cccs3=ccs2+as.numeric(num_enrolled1819),
         cccs4=ccs3+as.numeric(num_enrolled1920),
         cccs5=ccs4+as.numeric(num_enrolled2021),
         cccs6=ccs5+as.numeric(num_enrolled2122),
         cccs7=ccs6+as.numeric(num_enrolled2223),
         cccs8=ccs7+as.numeric(num_enrolled2324)) %>%
  mutate(frac1=ccs1/cccs1,
         frac2=ccs2/cccs2,
         frac3=ccs3/cccs3,
         frac4=ccs4/cccs4,
         frac5=ccs5/cccs5,
         frac6=ccs6/cccs6,
         frac7=ccs7/cccs7,
         frac8=ccs8/cccs8)

mn <- read_sf("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/WA_County_Boundaries", "WA_County_Boundaries")
head(mn)
colnames(mn)[4]<-"county"
mn$county <- str_to_upper(mn$county)
df1$county <- str_to_upper(df1$county)

mn <- mn %>% left_join(df1,by="county")

setdiff(mn$schooldistrictID,df1$schooldistrictID)

# Define the breakpoints for the categories for population
breakpoints <- c(0,0.2,0.4,0.6,0.8,1)

mn$Pop_int <- as.numeric(mn$frac3)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.2)", "small (0.2 - 0.4)", "medium (0.4 - 0.6)", "large (0.6 - 0.8)", "very large (>0.8)" ), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", county
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
      "very large (>0.8)" = "#00441B"),na.value = "gray92" ) +
  labs(title = "2023-24", 
       subtitle = "",
       fill="")  +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 20,hjust = 0.5),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

p1

df_plot<-data.frame(county=rep(df1$county,8),
           year=c(rep("2016-2017",length(df1$county)),rep("2017-2018",length(df1$county)),rep("2018-2019",length(df1$county)),rep("2019-2020",length(df1$county)),rep("2020-2021",length(df1$county)),rep("2021-2022",length(df1$county)),rep("2022-2023",length(df1$county)),rep("2023-2024",length(df1$county))),
           num_unvaccinated=c(df1$num_unvaccinated1617,df1$num_unvaccinated1718,df1$num_unvaccinated1819,df1$num_unvaccinated1920,df1$num_unvaccinated2021,df1$num_unvaccinated2122,df1$num_unvaccinated2223,df1$num_unvaccinated2324))

df_plot$county<-as.factor(df_plot$county)
df_plot$year<-as.factor(df_plot$year)
df_plot$num_unvaccinated<-as.numeric(df_plot$num_unvaccinated)
ggplot(df_plot,aes(x = year,y=num_unvaccinated,colour=county,group=county))+geom_line(lwd=2)

df_new<-data.frame(age=rep(c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o.","10 y.o.","11 y.o.","12 y.o."),8),
                   year=rep(c("2016-2017","2017-2018","2018-2019","2019-2020","2020-2021","2021-2022","2022-2023","2023-2024"),each=8),
                   nonvac=c(sum(as.numeric(df1$num_unvaccinated1617)),rep(0,7),
                            sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617)),rep(0,6),
                            sum(as.numeric(df1$num_unvaccinated1819)),sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617)),rep(0,5),
                            sum(as.numeric(df1$num_unvaccinated1920)),sum(as.numeric(df1$num_unvaccinated1819)),sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617)),rep(0,4),
                            sum(as.numeric(df1$num_unvaccinated2021)),sum(as.numeric(df1$num_unvaccinated1920)),sum(as.numeric(df1$num_unvaccinated1819)),sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617)),rep(0,3),
                            sum(as.numeric(df1$num_unvaccinated2122)),sum(as.numeric(df1$num_unvaccinated2021)),sum(as.numeric(df1$num_unvaccinated1920)),sum(as.numeric(df1$num_unvaccinated1819)),sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617)),rep(0,2),
                            sum(as.numeric(df1$num_unvaccinated2223)),sum(as.numeric(df1$num_unvaccinated2122)),sum(as.numeric(df1$num_unvaccinated2021)),sum(as.numeric(df1$num_unvaccinated1920)),sum(as.numeric(df1$num_unvaccinated1819)),sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617)),rep(0,1),
                            sum(as.numeric(df1$num_unvaccinated2324)),sum(as.numeric(df1$num_unvaccinated2223)),sum(as.numeric(df1$num_unvaccinated2122)),sum(as.numeric(df1$num_unvaccinated2021)),sum(as.numeric(df1$num_unvaccinated1920)),sum(as.numeric(df1$num_unvaccinated1819)),sum(as.numeric(df1$num_unvaccinated1718)),sum(as.numeric(df1$num_unvaccinated1617))))

df_new$age <- factor(df_new$age, levels = c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o.","10 y.o.","11 y.o.","12 y.o."))

# To use for fills, add
p2<-ggplot(df_new, aes(fill=age, y=nonvac, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("")+
  ylab("Number of MMR unvaccinated")+
  theme(legend.title = element_blank(),
        text = element_text(size = 14))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = wes_palette(8, name = "Zissou1", type = "continuous"), name = "")+
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

p1<-data.frame(year=c(rep("2016-2017",length(df1$frac1)),rep("2017-2018",length(df1$frac2)),
                      rep("2018-2019",length(df1$frac3)),rep("2019-2020",length(df1$frac4)),rep("2020-2021",length(df1$frac5)),rep("2021-2022",length(df1$frac6)),rep("2022-2023",length(df1$frac7)),rep("2023-2024",length(df1$frac8))),
               frac=c(df1$frac1,df1$frac2,df1$frac3,df1$frac4,df1$frac5,df1$frac6,df1$frac7,df1$frac8)) 
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
  labs(tag = "(E)")+xlim(0,1)

ggarrange(p2,p)
p2+p
