rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(tidylog)
library(ggiraph)
library(ggpubr)

df <- read_excel("2010-11CAKindergartenData.xls")
dat1011<- df %>% 
  select(...2,...6,...21,...22) %>%
  rename("county"="...2",
         "enrollment1011"="...6",
         "num_MMR1011"="...21",
         "prop_MMR1011"="...22") %>%
  slice(-1:-4) %>%
  na.omit() %>%
  mutate(unvaccinated1011=as.numeric(enrollment1011)-as.numeric(num_MMR1011)) %>%
  group_by(county) %>%
  summarise(num_enrolled1011=sum(as.numeric(enrollment1011)),
            num_unvaccinated1011=sum(unvaccinated1011)) %>%
  mutate(county = str_to_upper(county))

df <- read_excel("2011-12CAKindergartenData.xls")
dat1112<- df %>% 
  select(...2,...7,...22,...23) %>%
  rename("county"="...2",
         "enrollment1112"="...7",
         "num_MMR1112"="...22",
         "prop_MMR1112"="...23") %>%
  slice(-1:-4) %>%
  na.omit() %>%
  mutate(unvaccinated1112=as.numeric(enrollment1112)-as.numeric(num_MMR1112)) %>%
  group_by(county) %>%
  summarise(num_enrolled1112=sum(as.numeric(enrollment1112)),
            num_unvaccinated1112=sum(unvaccinated1112)) %>%
  mutate(county = str_to_upper(county))

df <- read_excel("2012-13CAKindergartenData.xlsx")
dat1213<- df %>% 
  select(...2,...7,...20,...21) %>%
  rename("county"="...2",
         "enrollment1213"="...7",
         "num_MMR1213"="...20",
         "prop_MMR1213"="...21") %>%
  slice(-1:-4) %>%
  na.omit() %>%
  mutate(unvaccinated1213=as.numeric(enrollment1213)-as.numeric(num_MMR1213)) %>%
  group_by(county) %>%
  summarise(num_enrolled1213=sum(as.numeric(enrollment1213)),
            num_unvaccinated1213=sum(unvaccinated1213)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2013-14CAKindergartenData.xlsx")
dat1314<- df %>% 
  select(...2,...7,...20,...21) %>%
  rename("county"="...2",
         "enrollment1314"="...7",
         "num_MMR1314"="...20",
         "prop_MMR1314"="...21") %>%
  slice(-1:-4) %>%
  na.omit() %>%
  mutate(unvaccinated1314=as.numeric(enrollment1314)-as.numeric(num_MMR1314)) %>%
  group_by(county) %>%
  summarise(num_enrolled1314=sum(as.numeric(enrollment1314)),
            num_unvaccinated1314=sum(unvaccinated1314)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2014-15CAKindergartenData.xlsx")
dat1415<- df %>% 
  select(...2,...7,...26,...27) %>%
  rename("county"="...2",
         "enrollment1415"="...7",
         "num_MMR1415"="...26",
         "prop_MMR1415"="...27") %>%
  slice(-1:-4) %>%
  na.omit() %>%
  mutate(unvaccinated1415=as.numeric(enrollment1415)-as.numeric(num_MMR1415)) %>%
  group_by(county) %>%
  summarise(num_enrolled1415=sum(as.numeric(enrollment1415)),
            num_unvaccinated1415=sum(unvaccinated1415)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2015-16CAKindergartenData.xls")
dat1516<- df %>% 
  select(...2,...7,...26,...27) %>%
  rename("county"="...2",
         "enrollment1516"="...7",
         "num_MMR1516"="...26",
         "prop_MMR1516"="...27") %>%
  slice(-1:-4) %>%
  na.omit() %>%
  mutate(unvaccinated1516=as.numeric(enrollment1516)-as.numeric(num_MMR1516)) %>%
  group_by(county) %>%
  summarise(num_enrolled1516=sum(as.numeric(enrollment1516)),
            num_unvaccinated1516=sum(unvaccinated1516)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2016-17KindergartenData.xlsx",sheet="Enrollment 20 or More")
dat1617<- df %>% 
  select(...2,...7,...24,...25) %>%
  rename("county"="...2",
         "enrollment1617"="...7",
         "num_MMR1617"="...24",
         "prop_MMR1617"="...25") %>%
  slice(-1:-4) %>%
  mutate(unvaccinated1617 = ifelse(num_MMR1617 == "--*", 1, as.numeric(enrollment1617)-as.numeric(num_MMR1617))) %>%
  select(county,enrollment1617,unvaccinated1617) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled1617=sum(as.numeric(enrollment1617)),
            num_unvaccinated1617=sum(unvaccinated1617)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2017-18CA_KindergartenDataLetter.xlsx",sheet="Enrollment 20 or More")
dat1718<- df %>% 
  select(...2,...7,...24,...25) %>%
  rename("county"="...2",
         "enrollment1718"="...7",
         "num_MMR1718"="...24",
         "prop_MMR1718"="...25") %>%
  slice(-1:-4) %>%
  mutate(unvaccinated1718 = ifelse(num_MMR1718 == "--*", 1, as.numeric(enrollment1718)-as.numeric(num_MMR1718))) %>%
  select(county,enrollment1718,unvaccinated1718) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled1718=sum(as.numeric(enrollment1718)),
            num_unvaccinated1718=sum(unvaccinated1718)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2018-19CAKindergartenDataLetter.xlsx",sheet="Enrollment 20 or More")
dat1819<- df %>% 
  select(...2,...7,...22,...23) %>%
  rename("county"="...2",
         "enrollment1819"="...7",
         "num_MMR1819"="...22",
         "prop_MMR1819"="...23") %>%
  slice(-1:-4) %>%
  mutate(unvaccinated1819 = ifelse(num_MMR1819 == "--*", 1, as.numeric(enrollment1819)-as.numeric(num_MMR1819))) %>%
  select(county,enrollment1819,unvaccinated1819) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled1819=sum(as.numeric(enrollment1819)),
            num_unvaccinated1819=sum(unvaccinated1819)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2019-20CAKindergartenDataLetter.xlsx",sheet="Enrollment 20 or More")
dat1920<- df %>% 
  select(...2,...7,...22,...23) %>%
  rename("county"="...2",
         "enrollment1920"="...7",
         "num_MMR1920"="...22",
         "prop_MMR1920"="...23") %>%
  slice(-1:-4) %>%
  mutate(unvaccinated1920 = ifelse(num_MMR1920 == "--*", 1, as.numeric(enrollment1920)-as.numeric(num_MMR1920))) %>%
  select(county,enrollment1920,unvaccinated1920) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled1920=sum(as.numeric(enrollment1920)),
            num_unvaccinated1920=sum(unvaccinated1920)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2020-22CAKFirstGrade_DataLetter.xlsx",sheet="Enrollment 20 or More")
dat2021<- df %>%
  select(...2,...4,...9,...24,...25) %>%
  filter(...2=="2020-21") %>%
  rename("county"="...4",
         "enrollment2021"="...9",
         "num_MMR2021"="...24",
         "prop_MMR2021"="...25") %>%
  select(county,enrollment2021,num_MMR2021,prop_MMR2021) %>%
  mutate(unvaccinated2021 = ifelse(num_MMR2021 == "--*", 1, as.numeric(enrollment2021)-as.numeric(num_MMR2021))) %>%
  select(county,enrollment2021,unvaccinated2021) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled2021=sum(as.numeric(enrollment2021)),
            num_unvaccinated2021=sum(unvaccinated2021)) %>%
  mutate(county = str_to_upper(county))


df <- read_excel("2020-22CAKFirstGrade_DataLetter.xlsx",sheet="Enrollment 20 or More")
dat2122<- df %>%
  select(...2,...3,...4,...9,...24,...25) %>%
  filter(...2=="2021-22" & ...3=="Kindergarten") %>%
  rename("county"="...4",
         "enrollment2122"="...9",
         "num_MMR2122"="...24",
         "prop_MMR2122"="...25") %>%
  select(county,enrollment2122,num_MMR2122,prop_MMR2122) %>%
  mutate(unvaccinated2122 = ifelse(num_MMR2122 == "--*", 1, as.numeric(enrollment2122)-as.numeric(num_MMR2122))) %>%
  select(county,enrollment2122,unvaccinated2122) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled2122=sum(as.numeric(enrollment2122)),
            num_unvaccinated2122=sum(unvaccinated2122)) %>%
  mutate(county = str_to_upper(county))

df <-  read_excel("2022-23GradeKReport.xlsx",sheet="Table 4")
names(df)[1]<-"county"
dat2223<- df %>%
  select(county,...2,...3,...4,...7) %>%
  filter(...2 == "2022-23" & ...3 == "Kindergarten") %>%
  rename("enrollment2223"="...4",
         "prop_MMR2223"="...7") %>%
  select(county,enrollment2223,prop_MMR2223) %>%
  mutate(unvaccinated2223 = as.numeric(enrollment2223)*(1-as.numeric(prop_MMR2223))) %>%
  mutate(unvaccinated2223 = ifelse(prop_MMR2223 %in% c("≥95.0%","≥99.0%"), 1, unvaccinated2223)) %>%
  mutate(unvaccinated2223 = ifelse(prop_MMR2223 %in% c("N/A*"), 0, unvaccinated2223)) %>%
  select(county,enrollment2223,unvaccinated2223) %>%
  na.omit() %>%
  group_by(county) %>%
  summarise(num_enrolled2223=sum(as.numeric(enrollment2223)),
            num_unvaccinated2223=sum(unvaccinated2223)) %>%
  mutate(county = str_to_upper(county))

dat2223$county[which(dat2223$county=="SAN LUIS  OBISPO")]<-'SAN LUIS OBISPO'

df <- dat1011 %>% 
  full_join(dat1112, by="county" ) %>%
  full_join(dat1213, by="county" ) %>%
  full_join(dat1314, by="county" ) %>%
  full_join(dat1415, by="county" ) %>%
  full_join(dat1516, by="county" ) %>%
  full_join(dat1617, by="county" ) %>%
  full_join(dat1718, by="county" ) %>%
  full_join(dat1819, by="county" ) %>%
  full_join(dat1920, by="county" ) %>%
  full_join(dat2021, by="county" ) %>%
  full_join(dat2122, by="county" ) %>%
  full_join(dat2223, by="county" )
  
df<-as.data.frame(df)

df1<-df %>%
  mutate(cs1=as.numeric(num_unvaccinated1011),
         cs2=as.numeric(num_unvaccinated1112),
         cs3=as.numeric(num_unvaccinated1213),
         cs4=as.numeric(num_unvaccinated1314),
         cs5=as.numeric(num_unvaccinated1415),
         cs6=as.numeric(num_unvaccinated1516),
         cs7=as.numeric(num_unvaccinated1617),
         cs8=as.numeric(num_unvaccinated1718),
         cs9=as.numeric(num_unvaccinated1819),
         cs10=as.numeric(num_unvaccinated1920),
         cs11=as.numeric(num_unvaccinated2021),
         cs12=as.numeric(num_unvaccinated2122),
         cs13=as.numeric(num_unvaccinated2223)) %>%
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
         ccs11=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9+cs10+cs11,
         ccs12=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9+cs10+cs11+cs12,
         ccs13=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9+cs10+cs11+cs12+cs13) %>%
  mutate(cccs1=as.numeric(num_enrolled1011),
         cccs2=ccs1+as.numeric(num_enrolled1112),
         cccs3=ccs2+as.numeric(num_enrolled1213),
         cccs4=ccs3+as.numeric(num_enrolled1314),
         cccs5=ccs4+as.numeric(num_enrolled1415),
         cccs6=ccs5+as.numeric(num_enrolled1516),
         cccs7=ccs6+as.numeric(num_enrolled1617),
         cccs8=ccs7+as.numeric(num_enrolled1718),
         cccs9=ccs8+as.numeric(num_enrolled1819),
         cccs10=ccs9+as.numeric(num_enrolled1920),
         cccs11=ccs10+as.numeric(num_enrolled2021),
         cccs12=ccs11+as.numeric(num_enrolled2122),
         cccs13=ccs12+as.numeric(num_enrolled2223)) %>%
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
         frac11=ccs11/cccs11,
         frac12=ccs12/cccs12,
         frac13=ccs13/cccs13)

mn <- read_sf("/Users/schen/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/ca_counties", "CA_Counties")
head(mn)
colnames(mn)[5]<-"county"
mn$county <- toupper(mn$county)

mn <- mn %>% left_join(df1,by="county")

# Define the breakpoints for the categories for population
breakpoints <- c(0,0.2,0.4,0.6,0.8,1)

mn$Pop_int <- as.numeric(mn$frac13)

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
  labs(title = "2022-23", 
       subtitle = "",
       fill="")  +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(plot.title = element_text(size = 18,hjust = 0.5),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 
p1


df_new<-data.frame(age=rep(c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o.","10 y.o.","11 y.o.","12 y.o.","13 y.o.","14 y.o.","15 y.o.","16 y.o.","17 y.o."),13),
                   year=rep(c("2010-2011","2011-2012","2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020","2020-2021","2021-2022","2022-2023"),each=13),
                   nonvac=c(sum(df1$num_unvaccinated1011[1:58]),rep(0,12),
                            sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,11),
                            sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,10),
                            sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,9),
                            sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,8),
                            sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,7),
                            sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,6),
                            sum(df1$num_unvaccinated1718[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,5),
                            sum(df1$num_unvaccinated1819[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1718[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,4),
                            sum(df1$num_unvaccinated1920[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1819[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1718[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,3),
                            sum(df1$num_unvaccinated2021[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1920[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1819[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1718[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,2),
                            sum(df1$num_unvaccinated2122[1:58],na.rm = TRUE),sum(df1$num_unvaccinated2021[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1920[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1819[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1718[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE),rep(0,1),
                            sum(df1$num_unvaccinated2223[1:58],na.rm = TRUE),sum(df1$num_unvaccinated2122[1:58],na.rm = TRUE),sum(df1$num_unvaccinated2021[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1920[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1819[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1718[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1617[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1516[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1415[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1314[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1213[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1112[1:58],na.rm = TRUE),sum(df1$num_unvaccinated1011[1:58],na.rm = TRUE)))
# ggplot(df_new,aes(x=year,y=nonvac,fill=age,group=age,colour=age))+geom_line()

df_new$age <- factor(df_new$age, levels = c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o.","10 y.o.","11 y.o.","12 y.o.","13 y.o.","14 y.o.","15 y.o.","16 y.o.","17 y.o."))

# To use for fills, add
p2<-ggplot(df_new, aes(fill=age, y=nonvac, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("")+
  ylab("Number of MMR unvaccinated")+
  theme(legend.title = element_blank(),
        text = element_text(size = 14))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = wes_palette(13, name = "Zissou1", type = "continuous"), name = "")+
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


p1<-data.frame(year=c(rep("2010-2011",length(df1$frac1)),rep("2011-2012",length(df1$frac2)),rep("2012-2013",length(df1$frac3)),rep("2013-2014",length(df1$frac4)),rep("2014-2015",length(df1$frac5)),rep("2015-2016",length(df1$frac6)),rep("2016-2017",length(df1$frac7)),rep("2017-2018",length(df1$frac8)),
                      rep("2018-2019",length(df1$frac9)),rep("2019-2020",length(df1$frac10)),rep("2020-2021",length(df1$frac11)),rep("2021-2022",length(df1$frac12)),rep("2022-2023",length(df1$frac13))),
               frac=c(df1$frac1,df1$frac2,df1$frac3,df1$frac4,df1$frac5,df1$frac6,df1$frac7,df1$frac8,df1$frac9,df1$frac10,df1$frac11,df1$frac12,df1$frac13)) 
p1$year<- factor(p1$year, levels=c("2022-2023","2021-2022","2020-2021","2019-2020","2018-2019","2017-2018","2016-2017","2015-2016","2014-2015","2013-2014","2012-2013","2011-2012","2010-2011"))
p<-ggplot(
  p1, 
  aes(x = frac, y = as.factor(year), fill = stat(x))
) +
  geom_density_ridges_gradient( ) +
  scale_fill_viridis_c(name = "", option = "G",begin = 0,end = 1, direction = -1)+
  labs(title = '') +
  ylab("")+
  xlab("Ratio of MMR unvaccinated")+
  coord_cartesian(clip = "off") + # To avoid cut off
  theme_minimal()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 14))  +
  labs(tag = "(B)")+xlim(0,1)

ggarrange(p2,p)
