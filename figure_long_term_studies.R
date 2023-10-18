rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(EDIutils)
library(xml2)

source("read_sbc_data.r")

etdrive <- ""

line_site <- function(usedate=0,points=0,df,xin,yin, colin=NULL, legendname=NULL, ylablein,titlein,startyear,endyear=2023, yearby=2)
{
 
  p<-ggplot(df, aes(y={{yin}}, x={{xin}},col={{colin}}))+
    labs(x="",y=ylablein, title=titlein) +
    theme(axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          panel.background =  element_rect(fill = "white", color = "black", size = 1),
          panel.grid.minor.y = element_line(color = "gray90",linewidth = 0.1),
          panel.grid.major.y = element_line(color = "gray90",linewidth = 0.1),
          panel.grid.minor.x = element_line(color = "gray90",linewidth = 0.1),
          panel.grid.major.x = element_line(color = "gray90",linewidth = 0.1),
          axis.title.y = element_text(size = rel(1.3)),
          strip.text = element_text(size = 13)) +
    guides(color=guide_legend(legendname))+
    if (usedate==1) {
      scale_x_date(date_labels = "%Y",date_breaks = paste0(yearby," years"))
    } else {
      scale_x_continuous(breaks = seq(startyear,endyear, by=yearby))
    }
  
    if (points==1) {
      p+
      geom_point(size=1)
    } else {
      p+
      geom_line(linewidth=1) 
    }  
   
}


#######################
#####Reef data########
#######################
#lobster abundance

dsname <- "Lobster abundance, size and fishing pressure - Time-series of lobster abundance and size"

lob <- read_sbc_data(dsname)%>%
  rename_all(tolower)
# lob <- read.csv(paste0(sys,"internal/research/Reef/Working/Data/Lobster Abundance and Fishing Pressure/Abundance/Lobster_Abundance_All_Years.csv"),na="-99999",stringsAsFactors = F) %>%
#   rename_all(tolower)

lob1 <- lob %>% 
  group_by(year,site,transect,replicate,area) %>%
  summarise(count=sum(count),.groups='drop')%>%
  ungroup()%>%
  mutate(density=count/area) %>%
  group_by(year,site)%>%
  summarise(density=mean(density,na.rm=T),.groups='drop')%>%
  ungroup()

#plot
line_site(df =lob1,xin=year,yin=density, 
          colin=site, legendname="Site", 
          ylablein=expression(paste("Lobster density (num per  ", m^{2},")")),
          titlein="Lobster abundance",
          startyear=2012,endyear=2023)

ggsave(paste0(etdrive,"Lobster_abundance.png"),height=4,width=10,unit="in")


##########
# Annual Reef biomass
# df5 <- fread(paste0(sys,"internal/research/Reef/Working/Data/Kelp_Forest_Community_Dynamics/Biomass Data/Annual_All_Species_Biomass_at_transect.csv"),na="-99999",stringsAsFactors = F) %>%
#   rename_all(tolower)
dsname <- "KFCD Biomass of algae, invertebrates and fish - SBC LTER Annual time series of biomass for kelp forest species"

df5 <- read_sbc_data(dsname)%>%
  rename_all(tolower)

# break into functional group
df5_1<-df5 %>%
  mutate(category=case_when(
    sp_code %in% c("PACA","PU","CHOV")&coarse_grouping=="SESSILE INVERT" ~ "Clams",
    !sp_code %in% c("PACA","PU","CHOV")&coarse_grouping=="SESSILE INVERT" ~ "Suspension feeders",
    coarse_grouping=="MOBILE INVERT"~"Mobile inverts",
    coarse_grouping=="FISH"~"Fish",
    TRUE ~ "Algae"
  )) %>%
  group_by(year,site,transect,category)%>%
  summarise(dry_mass=sum(dry_gm2,na.rm=T),.groups='drop') %>%
  ungroup() %>% 
  group_by(year,site,category)%>%
  summarise(dry_mass=mean(dry_mass)/1000,.groups='drop') %>%
  ungroup() 

#plot
line_site(df =df5_1,xin=year,yin=dry_mass, 
          colin=site, legendname="Site", 
          ylablein=expression(paste("Dry mass (kg ", m^{-2},")")),
          titlein="Annual reef species biomass, five functional groups",
          startyear=2000,endyear=2023)+
  facet_grid(category~.,scales="free_y")

ggsave(paste0(etdrive,"Annual_reef_species_biomass_in_functional_groups.png"),height=9,width=10,unit="in")


# biomass for all spp
df5_2<-df5_1 %>%
  group_by(year,site)%>%
  summarise(dry_mass=sum(dry_mass),.groups='drop') %>%
  ungroup() 

line_site(df =df5_2,xin=year,yin=dry_mass, 
          colin=site, legendname="Site", 
          ylablein=expression(paste("Dry mass (kg ", m^{-2},")")),
          titlein="Annual reef species biomass, sum of all species",
          startyear=2000,endyear=2023)

ggsave(paste0(etdrive,"Annual_reef_species_biomass_all_species.png"),height=4,width=10,unit="in")

#fish density
df5_4<-df5 %>%
  filter(group=="FISH") %>%
  group_by(year,site,transect) %>%
  summarise(density=sum(density,na.rm=T)) %>%
  ungroup() %>%
  group_by(year,site) %>%
  summarise(density=mean(density)) %>%
  ungroup()

line_site(df =df5_4,xin=year,yin=density, 
          colin=site, legendname="Site", 
          ylablein=expression(paste("Fish density (num per  ", m^{2},")")),
          titlein="Annual reef fish density",
          startyear=2000,endyear=2023)

ggsave(paste0(etdrive,"Annual_reef_fish.png"),height=4,width=10,unit="in")


#kelp
df5_5<-df5 %>%
  filter(coarse_grouping=="GIANT KELP") %>%
  group_by(year,site) %>%
  summarise(density=mean(density,na.rm=T),.groups="drop") %>%
  ungroup()

line_site(df =df5_5,xin=year,yin=density, 
          colin=site, legendname="Site", 
          ylablein=expression(paste("Kelp frond density (num per  ", m^{2},")")),
          titlein="Annual reef kelp density",
          startyear=2000,endyear=2023)

ggsave(paste0(etdrive,"Annual_reef_kelp.png"),height=4,width=10,unit="in")

##############
#larval settlement
dsname <- "Larval Settlement - California invertebrate settlement, all years"

settle <- read_sbc_data(dsname)%>%
  rename_all(tolower)

settle1 <- settle %>%
  mutate(rate=total_urchins/duration, date=as.Date(date_retrieved)) %>%
  group_by(site,date) %>%
  summarise(rate=mean(rate,na.rm=T),.groups="drop") %>%
  ungroup()

#Setting sites north to south
settle1$site = factor(settle1$site,levels=c("AVILA","GAVIOTA","SBELL", "SBSTWRF", "ANACAPA", "OCNBCH", "SIO", "FBPC"))

line_site(usedate=1,df =settle1,xin=date,yin=rate, 
          colin=site, legendname="Site", 
            ylablein=expression(paste("Urchin settlement (num  ", day^{-1},")")),
          titlein="Urchin settlement, sites go from north to south in the legend",
          startyear=1990,endyear=2023,yearby=4)

ggsave(paste0(etdrive,"Urchin_settlement.png"),height=4,width=10,unit="in")

############################
#Kelp NPP at site
dsname <- "Giant kelp NPP - Macrocystis pyrifera net primary production and growth with SE"

kelpnpp <- read_sbc_data(dsname)%>%
  rename_all(tolower)

kelp1 <- kelpnpp %>%
  rename_all(tolower)%>%
  mutate(monthday=case_when(
    str_detect(season,"3")~"06-23",
    str_detect(season,"4")~"09-23",
    str_detect(season,"1")~"12-23",
    str_detect(season,"2")~"03-23"),
    date=as.Date(paste0(year,"-",monthday)))

line_site(usedate=1,df =kelp1,xin=date,yin=npp_dry, 
          colin=site, legendname="Site", 
          ylablein=expression(paste("Seasonal rate of kelp NPP (kg  ", m^{-2}," ",day^{-1},")")),
          titlein="Seasonal Kelp NPP at each site",
          startyear=2002,endyear=2023,yearby=4)

ggsave(paste0(etdrive,"Kelp_NPP_Seasonal.png"),height=4,width=10,unit="in")

#kelp frond density

dsname <- "Giant kelp NPP - Macrocystis pyrifera standing crop, plant density, and loss rates with SE"

kelpden <- read_sbc_data(dsname)%>%
  rename_all(tolower)


kelp2 <- kelpden %>%
  mutate( date=as.Date(date))

line_site(usedate=1,df =kelp2,xin=date,yin=frond_loss_rate, 
          colin=site, legendname="Site", 
          ylablein="Frond loss rate (fraction of fronds lost per plant per d-1)",
          titlein="The average loss rate of fronds",
          startyear=2002,endyear=2023,yearby=4)

ggsave(paste0(etdrive,"Kelp_lostrate_at_site.png"),height=4,width=10,unit="in")

##########################################
### bottom temperature from both reef tidbit and mooring tidbit

dsname <- "Reef bottom water temperature - Bottom water temperature, all years"

temp <- read_sbc_data(dsname)%>%
  rename_all(tolower)

temp1<-temp %>%
  filter(!is.na(temp_c)) %>%
  group_by(site,week = cut(as.Date(date_local), "week")) %>%
  summarise(temp_c=mean(temp_c,na.rm=T),.groups="drop") %>%
  ungroup() %>%
  mutate(week=as.Date(week))

#setting sites west to east with island sites last
temp1$site = factor(temp1$site,levels=c("BULL","AHND","AQUE", "NAPL",
                                            "IVEE", "GOLB", "ABUR", "MOHK","CARP", "SCDI", "SCTW"))

line_site(usedate=1,df =temp1,xin=week,yin=temp_c, 
          ylablein=expression(paste("Weekly temperature ("*~degree*C*")")),
          titlein="Temperature inside the kelp forest, west to east",
          startyear=2000,endyear=2023,yearby=4)+
  facet_grid(site~.)

ggsave(paste0(etdrive,"Temp_inside_kelp_bed_at_site.png"),height=9,width=10,unit="in")

#####################
site_in<-c("Moored CTD and ADCP: ARQ - Moored CTD and ADCP at Arroyo Quemado Reef ARQ",
           "Moored CTD and ADCP: CAR - Moored CTD and ADCP at Carpinteria Reef CAR",
           "Moored CTD and ADCP: MKO - Moored CTD and ADCP at Mohawk Reef MKO",
           "Moored CTD and ADCP: NAP - Moored CTD and ADCP at Naples Reef NAP",
           "Moored CTD and ADCP: SBH - Moored CTD and ADCP at Santa Barbara Harbor")

mr_all <-data.frame()

for (i in 1:length(site_in)) {
  
  mr_temp <- read_sbc_data(site_in[i])
  
  
  mr_temp1<- mr_temp %>%
    as.data.frame(.) %>%
    mutate(datetime=as.POSIXct(paste0(year,"-",month,"-",day),format = "%Y-%m-%d", tz = "GMT")+decimal_time*24*3600,
           datetime_local=with_tz(datetime,tz="America/Los_Angeles" ),
           date=substr(datetime_local,1,10)) %>%
    mutate(Temp_top=ifelse(is.na(Temp_top),Temperature,Temp_top),
           Temp_bot=ifelse(is.na(Temp_bot),Temp_adcp,Temp_bot)) %>%
    select(date,Temp_top,Temp_bot) %>%
    group_by(date) %>%
    summarise(temp_top=mean(Temp_top),temp_bt=mean(Temp_bot))%>%
    ungroup() %>%
    mutate(site=site_in[i]) %>%
    filter(!(is.na(temp_top&is.na(temp_bt)))) %>%
    select(site,date,temp_top,temp_bt)
  
  mr_all<-rbind(mr_all,mr_temp1)
}

mr_all1<-mr_all%>%
  group_by(site,week = cut(as.Date(date), "week")) %>%
  summarise(temp_top=mean(temp_top,na.rm=T),temp_bt=mean(temp_bt,na.rm=T),.groups="drop") %>%
  ungroup() %>%
  mutate(week=as.Date(week))


ggplot(mr_all1, aes( x=week))+
  geom_point(aes(y=temp_bt,color="bottom"), size=1) + 
  geom_point(aes(y=temp_top,color="top"), size=1) + 
  labs(x="Date",y=expression(paste("Weekly temperature ("*~degree*C*")")), title="Mooring temperature outside the kelp forests") +
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.background =  element_rect(fill = "white", color = "black", size = 1),
        axis.title.y = element_text(size = rel(1.3)),
        axis.title.x= element_text(size = rel(1.3)),
        legend.position = c(0.92,0.96),
        legend.key.size = unit(5,"point"),
        panel.grid.minor.y = element_line(color = "gray90",linewidth = 0.1),
        panel.grid.major.y = element_line(color = "gray90",linewidth = 0.1),
        panel.grid.minor.x = element_line(color = "gray90",linewidth = 0.1),
        panel.grid.major.x = element_line(color = "gray90",linewidth = 0.1),
        strip.text = element_text(size = 13)) +
  scale_color_manual(name="water column",values = c("top" = "red","bottom"= "blue"))+
  facet_grid(site~.)+
  scale_x_date(date_labels = "%Y",date_breaks = paste0(4," years"))

ggsave(paste0(etdrive,"Temp_on_mooring_at_site.png"),height=9,width=10,unit="in")

###########
##DO

dsname <- "Dissolved oxygen for all sites - Dissolved oxygen and temperature data"

do <- read_sbc_data(dsname)

# replace -999.0, -999.000, 999, NaN with NA
do1 <- do %>%
  mutate_at(vars(-datetime_UTC), ~replace(., . %in% c("-999.0", "-999.000", "999", "NaN"), NA))
    
do2<- do1 %>%
      mutate(datetime=as.POSIXct(datetime_UTC, tz = "GMT"),
             datetime_local=with_tz(datetime,tz="America/Los_Angeles" ),
             date=substr(datetime_local,1,10)) %>%
      group_by(site,date,deployment_depth_m) %>%
      summarise(DO_st=mean(DO_percent_saturation,na.rm=T),
                DO_mgl=mean(DO_mgl,na.rm=T),.groups='drop' )%>%
      ungroup() %>%
       mutate(date=as.Date(date),
         depth=as.character(case_when(
           deployment_depth_m %in% c(1:5) ~"top",
           deployment_depth_m %in% c(6:15) ~"bottom")))%>%
  filter(!is.na(DO_st)) %>%
  filter(!is.na(depth))


line_site(usedate=1,points=1,df =do2,xin=date,yin=DO_st, 
          colin=depth, legendname="Depth",
          ylablein="Daily dissolved oxygen saturation (%)",
          titlein="Dissolved Oxygen Saturation",
          startyear=2000,endyear=2023,yearby=4)+
  facet_grid(site~.)+
  theme(legend.position = c(0.92,0.95))

ggsave(paste0(etdrive,"DO_saturation_at_site.png"),height=9,width=10,unit="in")


################
##PH
dsname <- "pH SeaFET for all sites - SeaFET pH and temperature data"

ph <- read_sbc_data(dsname)

# replace with NA
ph1 <- ph%>%
  mutate_at(vars(-datetime_UTC), ~replace(., . %in% c("-9999","NaN"), NA))

ph2<- ph1 %>%
  mutate(datetime=as.POSIXct(datetime_UTC, tz = "GMT"),
         datetime_local=with_tz(datetime,tz="America/Los_Angeles" ),
         date=substr(datetime_local,1,10)) %>%
  group_by(site,date,instrument_depth_m) %>%
  summarise(ph=mean(pH,na.rm=T),.groups='drop' )%>%
  ungroup() %>%
  mutate(date=as.Date(date),
         depth=as.character(case_when(
           instrument_depth_m %in% c(1:5) ~"top",
           instrument_depth_m %in% c(6:15) ~"bottom")))%>%
  filter(!is.na(ph))

line_site(usedate=1,points=1,df =ph2,xin=date,yin=ph, 
          colin=depth, legendname="Depth",
          ylablein="Daily average pH",
          titlein="Ocean pH",
          startyear=2000,endyear=2023,yearby=2)+
  facet_grid(site~.)+
  ylim(7.5,8.5)+
  theme(legend.position = c(0.92,0.96))

ggsave(paste0(etdrive,"PH_at_site.png"),height=9,width=10,unit="in")



##########
#monthly water chemistry
dsname <- "Nearshore CTD and Rosette Bottle Profiles - Monthly bottle chemistry with CTD, registered stations, all years"

monwater <- read_sbc_data(dsname) %>% 
  rename_all(tolower) 

station_select<-monwater %>%
  group_by(station)%>%
  summarise(freq=n()) %>%
  ungroup() %>%
  filter(freq>50)

monwater2<-monwater %>%
  filter(station %in% station_select$station)%>%
  group_by(station,`yyyy-mm-dd`) %>%
  summarise(poc=mean(`poc (umol/l)`,na.rm=T),
            pon=mean(`pon (umol/l)`,na.rm=T),
            C13=mean(`mean 13c d per mil`,na.rm=T),
            N15=mean(`mean 15n d per mil`,na.rm=T),
            totalchl=mean(`total chl (ug/l)`,na.rm=T),
            doc=mean(`mean doc (umol/kg)`,na.rm=T),
            ph=mean(`mean ph`,na.rm=T),
            tco2=mean(`mean tco2 (umol/kg)`,na.rm=T),.groups='drop') %>%
  ungroup() %>%
  mutate(date=as.Date(`yyyy-mm-dd`,"%Y-%m-%d"))

line_site(usedate=1,df =monwater2,xin=date,yin=poc, 
          colin=station, legendname="Site",
          ylablein=expression(paste("POC (umol ", l^{-1},")")),
          titlein="Monthly ocean chemistry",
          startyear=2000,endyear=2023,yearby=2)

ggsave(paste0(etdrive,"Monthly_Ocean_chem_POC.png"),height=4,width=10,unit="in")

line_site(usedate=1,df =monwater2,xin=date,yin=pon, 
          colin=station, legendname="Site",
          ylablein=expression(paste("PON (umol ", l^{-1},")")),
          titlein="Monthly ocean chemistry",
          startyear=2000,endyear=2023,yearby=2)

ggsave(paste0(etdrive,"Monthly_Ocean_chem_PON.png"),height=4,width=10,unit="in")

line_site(usedate=1,df =monwater2,xin=date,yin=totalchl, 
          colin=station, legendname="Site",
          ylablein=expression(paste("Total chl (ug ", l^{-1},")")),
          titlein="Monthly ocean chemistry",
          startyear=2000,endyear=2023,yearby=2)

ggsave(paste0(etdrive,"Monthly_Ocean_chem_totalChl.png"),height=4,width=10,unit="in")


#monthly downcast data

dsname <- "Nearshore CTD and Rosette Bottle Profiles - Monthly CTD downcasts, registered stations, all years"

downcast <- read_sbc_data(dsname) %>% 
  rename_all(tolower)

station_s<-downcast %>%
  group_by(station) %>%
  summarise(freq=n()) %>%
  ungroup() %>%
  filter(freq>500)

downcast1<-downcast %>%
  filter(station %in% station_s$station)%>%
  group_by(station,`yyyy-mm-dd`) %>%
  summarise(cond=mean(`ctd_cond00`,rm.na=T),
            salinity=mean(`ctd_sal00`,rm.na=T),.groups='drop')%>%
  ungroup() %>%
  mutate(date=as.Date(`yyyy-mm-dd`))


line_site(usedate=1,df =downcast1,xin=date,yin=salinity, 
          colin=station, legendname="Site",
          ylablein="Salinity (psu)",
          titlein="Monthly ocean downcast",
          startyear=2000,endyear=2023,yearby=2)+
  ylim(32,34.2)

ggsave(paste0(etdrive,"Monthly_Ocean_downcast_salinity.png"),height=4,width=10,unit="in")

line_site(usedate=1,df =downcast1,xin=date,yin=cond, 
          colin=station, legendname="Site",
          ylablein=expression(paste("Conductivity (s ", m^{-1},")")),
          titlein="Monthly ocean downcast",
          startyear=2000,endyear=2023,yearby=2)+
  ylim(3.5,5)

ggsave(paste0(etdrive,"Monthly_Ocean_downcast_conductivity.png"),height=4,width=10,unit="in")



################
# light
dsname <- "Hourly irradiance, surface and seafloor - Hourly quanta, all years"

light <- read_sbc_data(dsname) %>% 
  rename_all(tolower)

surface <- light %>%
  filter(sensor_location=="SURFACE")%>%
  group_by(date_local) %>%
  summarise(light_mol_day=mean(light_mol_day,na.rm=T)) %>%
  ungroup()


seafloor <- light %>%
  filter(sensor_location=="SEAFLOOR") %>%
  filter(!(transect %in% c("1","MKO","MKI"))) %>%
  mutate(date=as.Date(date_local))%>%
  group_by(site,transect,week = cut(as.Date(date), "week")) %>%
  summarise(light_mol_day=mean(light_adj_conv,na.rm=T),.groups="drop") %>%
  ungroup() %>%
  mutate(week=as.Date(week))


line_site(usedate=1,points=1,df =seafloor,xin=week,yin=light_mol_day, 
          colin=transect, legendname="Treatment",
          ylablein=expression(paste("Weekly light radiation (mol ",day^{-1},")")),
          titlein="Seafloor light intensity at 5 sites",
          startyear=2000,endyear=2023,yearby=2)+
  facet_grid(site~.)

ggsave(paste0(etdrive,"Light_at_site.png"),height=9,width=10,unit="in")


########################
#beach
dsname <- "Time series of wrack cover and biomass at selected beaches - Beach wrack cover, all years"

wrack <- read_sbc_data(dsname) %>% 
  rename_all(tolower)

wrack1<-wrack %>%
  filter(taxon_group %in% c("PLANT","ALGAE"))%>%
  group_by(year,month,site,transect) %>%
  summarise(vol=sum(wrack_vol,na.rm=T),.groups = 'drop') %>%
  ungroup() %>%
  group_by(year,month,site) %>%
  summarise(vol=mean(vol),.groups = 'drop')%>%
  ungroup()%>%
  mutate(date=as.Date(paste0(year,"-",month,"-01"))) %>%
  filter(site!="CSB")


line_site(usedate=1,df =wrack1,xin=date,yin=vol, 
          colin=site, legendname="Site",
          ylablein=expression(paste("Wrack cover volumn ( ",m^{3},")")),
          titlein="Beach wrack cover",
          startyear=2000,endyear=2023,yearby=2)+
  facet_grid(site~.)

ggsave(paste0(etdrive,"Beach_wrack_cover_at_site.png"),height=9,width=10,unit="in")


####
#shore bird

dsname <- "Beach birds and stranded kelp - Beach birds abundance, all years"

shorebird <- read_sbc_data(dsname) %>% 
  rename_all(tolower)

shorebird1 <- shorebird %>%
  group_by(date,site) %>%
  summarise(count=sum(total,na.rm=T)) %>%
  ungroup() %>%
  mutate(date=as.Date(date,"%m/%d/%Y"))


line_site(usedate=1,df =shorebird1,xin=date,yin=count, 
          colin=site, legendname="Site",
          ylablein="Total bird count",
          titlein="Beach shore bird",
          startyear=2000,endyear=2023,yearby=2)+
  facet_grid(site~.)

ggsave(paste0(etdrive,"Beach_shore_bird_at_site.png"),height=9,width=10,unit="in")

####
#shore consumers

dsname <- "Macroinvertebrates counts and biomass  - Beach wrack consumers, all years"

consum <- read_sbc_data(dsname) %>% 
  rename_all(tolower)

consum1 <- consum %>%
  group_by(year,month,day,site,transect) %>%
  summarise(count=sum(count,na.rm=T),weight=sum(wet_weight,na.rm=T),.groups='drop') %>%
  ungroup() %>%
  group_by(year,month,day,site) %>%
  summarise(count=mean(count,na.rm=T),weight=mean(weight,na.rm=T),.groups='drop') %>%
  ungroup() %>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)))

line_site(usedate=1,df =consum1,xin=date,yin=weight, 
          colin=site, legendname="Site",
          ylablein="Wet-weight of macroinvertebrate per soil core (g)",
          titlein="Beach consumer",
          startyear=2000,endyear=2023,yearby=2)+
  facet_grid(site~.)

ggsave(paste0(etdrive,"Beach_consumer_at_site.png"),height=9,width=10,unit="in")

