rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
library(dplyr)
library(tidylog)
library(ggiraph)
library(ggpubr)

df <- read_csv("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/schoolvaccinecoverage/schoolvaccine/CT/County_Immunizations_and_Exemption_Rates_by_School_Year__Grade__Vaccine__and_School_Type_20250222.csv")

dat <- df %>% 
  filter(`Vaccine Series`=="MMR" & Grade=="K") %>%   
  mutate(num_unvaccinated=round(`Total Enrollment Count`*(1-`Percentage Vaccinated`/100))) %>%   
  select(`School Year`,County,`Total Enrollment Count`,num_unvaccinated) 

df1<-dat %>%
  pivot_wider(names_from=`School Year`, 
              values_from=c(`Total Enrollment Count`,
                            num_unvaccinated)) %>% 
  mutate(cs1=`num_unvaccinated_2012-2013`,
         cs2=`num_unvaccinated_2013-2014`,
         cs3=`num_unvaccinated_2014-2015`,
         cs4=`num_unvaccinated_2015-2016`,
         cs5=`num_unvaccinated_2016-2017`,
         cs6=`num_unvaccinated_2017-2018`,
         cs7=`num_unvaccinated_2018-2019`,
         cs8=`num_unvaccinated_2019-2020`,
         cs9=`num_unvaccinated_2020-2021`,
         cs10=`num_unvaccinated_2021-2022`,
         cs11=`num_unvaccinated_2022-2023`,
         cs12=`num_unvaccinated_2023-2024`) %>%
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
         ccs12=cs1+cs2+cs3+cs4+cs5+cs6+cs7+cs8+cs9+cs10+cs11+cs12) %>%
  mutate(cccs1=as.numeric(`Total Enrollment Count_2012-2013`),
         cccs2=ccs1+as.numeric(`Total Enrollment Count_2013-2014`),
         cccs3=ccs2+as.numeric(`Total Enrollment Count_2014-2015`),
         cccs4=ccs3+as.numeric(`Total Enrollment Count_2015-2016`),
         cccs5=ccs4+as.numeric(`Total Enrollment Count_2016-2017`),
         cccs6=ccs5+as.numeric(`Total Enrollment Count_2017-2018`),
         cccs7=ccs6+as.numeric(`Total Enrollment Count_2018-2019`),
         cccs8=ccs7+as.numeric(`Total Enrollment Count_2019-2020`),
         cccs9=ccs8+as.numeric(`Total Enrollment Count_2020-2021`),
         cccs10=ccs9+as.numeric(`Total Enrollment Count_2021-2022`),
         cccs11=ccs10+as.numeric(`Total Enrollment Count_2022-2023`),
         cccs12=ccs11+as.numeric(`Total Enrollment Count_2023-2024`)) %>%
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
         frac12=ccs12/cccs12)

mn <- read_sf("~/Library/CloudStorage/OneDrive-CornellUniversity/research_project/6measles/age_structured_model/Connecticut_County_Index_4624072518650040895", "CT_County_Index")
head(mn)
colnames(mn)[4]<-"County"
mn$County <- str_to_upper(mn$County)
df1$County <- str_to_upper(df1$County)

mn <- mn %>% left_join(df1,by="County")

setdiff(mn$schooldistrictID,df1$schooldistrictID)

# Define the breakpoints for the categories for population
breakpoints <- c(0,0.05,0.10,0.15,0.2,0.25,0.3,0.35,1)
# breakpoints <- c(50,100,150,200,250,Inf)

mn$Pop_int <- as.numeric(mn$frac12)

# Create categories
mn$pop_category <- cut(mn$Pop_int, breaks = breakpoints, labels = c("very small (0-0.05)", "small (0.05-0.10)", "medium (0.10-0.15)", "large (0.15-0.20)", "very large (0.20-0.25)","extreme large (0.25-0.30)" ,"huge (0.30-0.35)",">0.35"), include.lowest = TRUE)
mn <- mn %>%
  mutate(info = paste(
    "Name: ", County
  ))
p1 <- ggplot(mn, aes(fill = pop_category)) +
  geom_sf() + # call geom shapefile
  geom_sf_interactive(aes(geometry = geometry, tooltip = info)) + 
  scale_fill_manual(
    values = c(
      "very small (0-0.05)" = "#eff3ff",
      "small (0.05-0.10)" = "#bdd7e7",
      "medium (0.10-0.15)" = "#6baed6",
      "large (0.15-0.20)" = "#3182bd",
      "very large (0.20-0.25)" ="#08519c",
      "extreme large (0.25-0.30)" = "#003852",
       "huge (0.30-0.35)"="#002b3e",
      ">0.35" = "#001017") ) +
  labs(title = "", 
       subtitle = "",
       fill="")  +
  theme(plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 

df_new<-data.frame(age=rep(c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o.","10 y.o.","11 y.o.","12 y.o.","13 y.o.","14 y.o.","15 y.o."),11),
                   year=rep(c("2012-2013","2013-2014","2014-2015","2015-2016","2016-2017","2017-2018","2018-2019","2019-2020","2020-2021","2021-2022","2022-2023"),each=11),
                   nonvac=c(sum(df1$`num_unvaccinated_2012-2013`),rep(0,10),
                            sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,9),
                            sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,8),
                            sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,7),
                            sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,6),
                            sum(df1$`num_unvaccinated_2017-2018`),sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,5),
                            sum(df1$`num_unvaccinated_2018-2019`),sum(df1$`num_unvaccinated_2017-2018`),sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,4),
                            sum(df1$`num_unvaccinated_2019-2020`),sum(df1$`num_unvaccinated_2018-2019`),sum(df1$`num_unvaccinated_2017-2018`),sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,3),
                            sum(df1$`num_unvaccinated_2020-2021`),sum(df1$`num_unvaccinated_2019-2020`),sum(df1$`num_unvaccinated_2018-2019`),sum(df1$`num_unvaccinated_2017-2018`),sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,2),
                            sum(df1$`num_unvaccinated_2021-2022`),sum(df1$`num_unvaccinated_2020-2021`),sum(df1$`num_unvaccinated_2019-2020`),sum(df1$`num_unvaccinated_2018-2019`),sum(df1$`num_unvaccinated_2017-2018`),sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`),rep(0,1),
                            sum(df1$`num_unvaccinated_2022-2023`),sum(df1$`num_unvaccinated_2021-2022`),sum(df1$`num_unvaccinated_2020-2021`),sum(df1$`num_unvaccinated_2019-2020`),sum(df1$`num_unvaccinated_2018-2019`),sum(df1$`num_unvaccinated_2017-2018`),sum(df1$`num_unvaccinated_2016-2017`),sum(df1$`num_unvaccinated_2015-2016`),sum(df1$`num_unvaccinated_2014-2015`),sum(df1$`num_unvaccinated_2013-2014`),sum(df1$`num_unvaccinated_2012-2013`)))
df_new$age <- factor(df_new$age, levels = c("5 y.o.","6 y.o.","7 y.o.","8 y.o.","9 y.o.","10 y.o.","11 y.o.","12 y.o.","13 y.o.","14 y.o.","15 y.o."))

# To use for fills, add
p2<-ggplot(df_new, aes(fill=age, y=nonvac, x=year)) + 
  geom_bar(position="stack", stat="identity") +
  xlab("")+
  ylab("Number of MMR unvaccinated")+
  theme(legend.title = element_blank(),
        text = element_text(size = 14))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = wes_palette(11, name = "Zissou1", type = "continuous"), name = "")+
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


p1<-data.frame(year=c(rep("2012-2013",length(df1$frac1)),rep("2013-2014",length(df1$frac2)),rep("2014-2015",length(df1$frac3)),rep("2015-2016",length(df1$frac4)),rep("2016-2017",length(df1$frac5)),rep("2017-2018",length(df1$frac6)),
                      rep("2018-2019",length(df1$frac7)),rep("2019-2020",length(df1$frac8)),rep("2020-2021",length(df1$frac9)),rep("2021-2022",length(df1$frac10)),rep("2022-2023",length(df1$frac11))),
               frac=c(df1$frac1,df1$frac2,df1$frac3,df1$frac4,df1$frac5,df1$frac6,df1$frac7,df1$frac8,df1$frac9,df1$frac10,df1$frac11)) 
p1$year<- factor(p1$year, levels=c("2022-2023","2021-2022","2020-2021","2019-2020","2018-2019","2017-2018","2016-2017","2015-2016","2014-2015","2013-2014","2012-2013"))
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






dat$County<-as.factor(dat$County)
dat$`School Year`<-as.factor(dat$`School Year`)
dat<-dat %>% filter(County!="State")
ggplot(dat,aes(x = `School Year`,y=`Total Enrollment Count`,colour=County,group=County))+geom_line(lwd=2)
