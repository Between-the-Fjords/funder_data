library(tidyverse)
library(readxl)
library(tidylog)
#File with nematode counts
nema<-read_excel("raw_data/Nematodes_families_FUNDER_2022_test.xlsx")

#File containing information about the amount of soil used per sample
weight<-read_csv2(file = "raw_data/FUNDER_raw_Nematode_sample_weight_2023.csv")

#Rename columns, add metadata in both files

nema <- nema |> mutate( plotID = str_replace_all(nema$PlotID, "-", "_"))

nema <- nema |> separate(plotID, sep = "_", c("plotID","sample"))  %>%
  select(-PlotID)


nema <- nema |> dplyr::mutate(sample = replace_na(sample, "1"))

nema<- nema |>  mutate ( siteID = substr (plotID,1,3) ,
         blockID=substr (plotID,4,4),
         treatment=substr (plotID,5,7)) %>%
  mutate(plotID=str_to_lower(siteID)) %>%
  mutate(plotID=str_to_title(plotID)) %>%
  mutate(siteID = recode(plotID,
                         # old name (replace) = valid name (do not change)
                         'Gud' = "Gudmedalen",
                         'Lav' = "Lavisdalen",
                         'Ram' = "Rambera",
                         'Ulv' = "Ulvehaugen",
                         'Skj' = "Skjelingahaugen",
                         'Alr' = "Alrust",
                         'Arh' = "Arhelleren",
                         'Fau' = "Fauske",
                         'Hog' = "Hogsete",
                         'Ovs' = "Ovstedalen",
                         'Vik' = "Vikesland",
                         'Ves' = "Veskre")) %>%
  mutate(plotID=paste0(plotID,blockID,treatment))%>%
  select(plotID:treatment, Abundance:Tripylidae)



weight<- weight |> mutate ( siteID = substr (PlotID,1,3) ,
                                                     blockID=substr (PlotID,4,4),
                                                     treatment=substr (PlotID,5,7))

  weight<- weight |> mutate(treatment=recode(treatment,
                          'BF'='FB',
                          'BG'= 'GB',
                          'FG'='GF',
                          'BFG'='FGB'
  )) |>
    mutate(plotID=paste0(siteID,blockID,treatment)) |>
    select(-PlotID)


#Sum of the 150 nematodes identified per plot depending on their family group
str(nema)

nema$Unknown=as.numeric(nema$Unknown)
nema$Unknown_Predator=as.numeric(nema$Unknown_Predator)
nema$Unknown_bacterial_feeder=as.numeric(nema$Unknown_bacterial_feeder)
nema$Psilenchidae=as.numeric(nema$Psilenchidae)
nema$Criconematidae=as.numeric(nema$Criconematidae)
nema$Alaimidae=as.numeric(nema$Alaimidae)
nema$Alaimidae=as.numeric(nema$Alaimidae)
nema$Bastianiidae=as.numeric(nema$Bastianiidae)
nema$Diphterophoridae=as.numeric(nema$Diphterophoridae)
nema$Tylencholaimidae=as.numeric(nema$Tylencholaimidae)
nema$Anguinidae=as.numeric(nema$Anguinidae)
nema$Tripylidae=as.numeric(nema$Tripylidae)


nema[is.na(nema)]<-0
nemaclean<-nema %>% group_by(plotID, siteID, blockID,treatment,sample) %>%
  summarise(Abundance = mean (Abundance),
            Unknown = sum(Unknown),
            Unknown_Predator = sum(Unknown_Predator),
            Unknown_bacterial_feeder = sum(Unknown_bacterial_feeder),
            Unknown_plant_feeder = sum(Unknown_plant_feeder),
            Dolichodoridae = sum(Dolichodoridae),
            Paratylenchidae = sum(Paratylenchidae),
            Pratylenchidae = sum(Pratylenchidae),
            Tylenchidae = sum(Tylenchidae),
            Psilenchidae = sum(Psilenchidae),
            Mononchidae = sum(Mononchidae),
            Hoplolaimidae  = sum(Hoplolaimidae),
            Dorylaimoidea  = sum(Dorylaimoidea),
            Aphelenchoididae  = sum(Aphelenchoididae),
            Prismatolaimidae  = sum(Prismatolaimidae),
            Cephalobidae  = sum(Cephalobidae),
            Criconematidae = sum(Criconematidae),
            Hemicycliophoridae  = sum(Hemicycliophoridae),
            Ecphyadophoridae  = sum(Ecphyadophoridae),
            Plectidae  = sum(Plectidae),
            Rhabditidae  = sum(Rhabditidae),
            Achromadoridae  = sum(Achromadoridae),
            Alaimidae  = sum(Alaimidae),
            Bastianiidae  = sum(Bastianiidae),
            Bunonematidae  = sum(Bunonematidae),
            Desmodoridae  = sum(Desmodoridae),
            Diplogasteridae  = sum(Diplogasteridae),
            Monhysteridae  = sum(Monhysteridae),
            Neodiplogasteridae  = sum(Neodiplogasteridae),
            Odontolaimidae  = sum(Odontolaimidae),
            Panagrolaimidae  = sum(Panagrolaimidae),
            Teratocephalidae  = sum(Teratocephalidae),
            Metateratocephalidae  = sum(Metateratocephalidae),
            Diphterophoridae  = sum(Diphterophoridae),
            Tylencholaimidae  = sum(Tylencholaimidae),
            Anguinidae  = sum(Anguinidae),
            Tripylidae  = sum(Tripylidae))


  #Dealing with the samples that were in 2 tubes: sum of their abundances
nemaclean<-nemaclean %>% group_by(plotID, siteID, blockID,treatment) %>%
  summarise(Abundance = sum (Abundance),
            Unknown = sum(Unknown),
            Unknown_Predator = sum(Unknown_Predator),
            Unknown_bacterial_feeder = sum(Unknown_bacterial_feeder),
            Unknown_plant_feeder = sum(Unknown_plant_feeder),
            Dolichodoridae = sum(Dolichodoridae),
            Paratylenchidae = sum(Paratylenchidae),
            Pratylenchidae = sum(Pratylenchidae),
            Tylenchidae = sum(Tylenchidae),
            Psilenchidae = sum(Psilenchidae),
            Mononchidae = sum(Mononchidae),
            Hoplolaimidae  = sum(Hoplolaimidae),
            Dorylaimoidea  = sum(Dorylaimoidea),
            Aphelenchoididae  = sum(Aphelenchoididae),
            Prismatolaimidae  = sum(Prismatolaimidae),
            Cephalobidae  = sum(Cephalobidae),
            Criconematidae = sum(Criconematidae),
            Hemicycliophoridae  = sum(Hemicycliophoridae),
            Ecphyadophoridae  = sum(Ecphyadophoridae),
            Plectidae  = sum(Plectidae),
            Rhabditidae  = sum(Rhabditidae),
            Achromadoridae  = sum(Achromadoridae),
            Alaimidae  = sum(Alaimidae),
            Bastianiidae  = sum(Bastianiidae),
            Bunonematidae  = sum(Bunonematidae),
            Desmodoridae  = sum(Desmodoridae),
            Diplogasteridae  = sum(Diplogasteridae),
            Monhysteridae  = sum(Monhysteridae),
            Neodiplogasteridae  = sum(Neodiplogasteridae),
            Odontolaimidae  = sum(Odontolaimidae),
            Panagrolaimidae  = sum(Panagrolaimidae),
            Teratocephalidae  = sum(Teratocephalidae),
            Metateratocephalidae  = sum(Metateratocephalidae),
            Diphterophoridae  = sum(Diphterophoridae),
            Tylencholaimidae  = sum(Tylencholaimidae),
            Anguinidae  = sum(Anguinidae),
            Tripylidae  = sum(Tripylidae))

#Create and merge the new column abundance / dry soil weight -> abundance per g
str(nemaclean)

weight <- weight %>% rename(fresh_soil="Fresh soil sample weight for extraction (g)")|>
   rename(dry_soil="Dry soil sample weight for extraction (g)") |>
  select(plotID,fresh_soil,dry_soil)

nemaclean<-nemaclean %>% left_join(weight,by=c("plotID"))

nemaclean<-nemaclean %>% mutate(abundance_per_g=Abundance/dry_soil)


# Number of nematodes by feeding groups

#File containing families group by feeding categories
feed<-read_csv2(file = "raw_data/FUNDER_raw_Nematode_feeding_group_2023.csv")


Unknown_Predator<-list("Unknown_Predator", "Predator")
Unknown_bacterial_feeder<-list("Unknown_bacterial_feeder", "Bacteria")
Unknown_plant_feeder<-list("Unknown_plant_feeder", "Plant")
feed<-rbind(feed, Unknown_Predator, Unknown_bacterial_feeder, Unknown_plant_feeder)


nemaclean_t<-nemaclean %>%pivot_longer(cols= Unknown_Predator:Tripylidae,
             names_to="Family",
             values_to="Abundance_family")

nemaclean_t<-nemaclean_t %>% left_join(feed,by=c("Family"))

nemaclean_t<-nemaclean_t %>%  rename(Functional_Group="Functional Group")

nemaclean_t2<-nemaclean_t %>% group_by(plotID, Functional_Group) %>%
  summarise(Abundance_family=sum(Abundance_family))

nemaclean_t2<-nemaclean_t2 %>% pivot_wider(names_from=Functional_Group,
                                             values_from=Abundance_family)


nemaclean<-nemaclean %>% left_join(nemaclean_t2,by=c("plotID"))


#abundance functional group per 100g of dry soil

nemaclean<-nemaclean %>% mutate (bacterivores_per_100g_dry_soil= (Bacteria/(Bacteria+Fungi+Omnivor+Plant+Predator))*abundance_per_g*100,
                                 fungivores_per_100g_dry_soil= (Fungi/(Bacteria+Fungi+Omnivor+Plant+Predator))*abundance_per_g*100,
                                 Omnivores_per_100g_dry_soil= (Omnivor/(Bacteria+Fungi+Omnivor+Plant+Predator))*abundance_per_g*100,
                                 Herbivores_per_100g_dry_soil= (Plant/(Bacteria+Fungi+Omnivor+Plant+Predator))*abundance_per_g*100,
                                 Predators_per_100g_dry_soil= (Predator/(Bacteria+Fungi+Omnivor+Plant+Predator))*abundance_per_g*100)


#Saving the final table
nemacleanFinal<-nemaclean %>% select(plotID, siteID, blockID,treatment,abundance_per_g, bacterivores_per_100g_dry_soil, fungivores_per_100g_dry_soil,  Omnivores_per_100g_dry_soil, Herbivores_per_100g_dry_soil, Predators_per_100g_dry_soil) |> mutate(temperature_level=case_when(
  siteID%in%c("Fauske","Vikesland","Arhelleren","Ovstedalen")~10.5,
  siteID%in%c("Alrust","Hogsete","Rambera","Veskre")~8.5,
  TRUE~6.5
)) %>%
  mutate(precipitation_level=case_when(
    siteID%in%c("Fauske","Alrust","Ulvehaugen")~700,
    siteID%in%c("Vikesland","Hogsete","Lavisdalen")~1400,
    siteID%in%c("Gudmedalen","Rambera","Arhelleren")~2100,
    TRUE~2800
  ))


#Checking outliers

plot(nemacleanFinal$bacterivores_per_100g_dry_soil)
plot(nemacleanFinal$fungivores_per_100g_dry_soil)
plot(nemacleanFinal$Omnivores_per_100g_dry_soil)
plot(nemacleanFinal$Herbivores_per_100g_dry_soil)
plot(nemacleanFinal$Predators_per_100g_dry_soil)
#No extreme value that does not seem realistic

#Saving csv
write_csv(nemacleanFinal, file = "FUNDER_clean_nematode_composition_2023.csv")



#Checking with some graphs
nemacleanFinal %>% group_by(treatment) %>%
  summarise(n=n(),
            bacterivores = mean(bacterivores_per_100g_dry_soil),
            fungivores = mean(fungivores_per_100g_dry_soil),
            Omnivores = mean(Omnivores_per_100g_dry_soil),
            Herbivores = mean(Herbivores_per_100g_dry_soil),
            Predators = mean(Predators_per_100g_dry_soil)) %>%
  gather("Group", "abundance", - c(treatment, n)) %>%
  ggplot(aes(x = treatment, y = abundance, group = Group, fill = Group)) + geom_col()


nemacleanFinal %>% group_by(temperature_level) %>%
  summarise(n=n(),
            bacterivores = mean(bacterivores_per_100g_dry_soil),
            fungivores = mean(fungivores_per_100g_dry_soil),
            Omnivores = mean(Omnivores_per_100g_dry_soil),
            Herbivores = mean(Herbivores_per_100g_dry_soil),
            Predators = mean(Predators_per_100g_dry_soil)) %>%
  gather("Group", "abundance", - c(temperature_level, n)) %>%
  ggplot(aes(x = temperature_level, y = abundance, group = Group, fill = Group)) + geom_col()


nemacleanFinal %>% group_by(precipitation_level) %>%
  summarise(n=n(),
            bacterivores = mean(bacterivores_per_100g_dry_soil),
            fungivores = mean(fungivores_per_100g_dry_soil),
            Omnivores = mean(Omnivores_per_100g_dry_soil),
            Herbivores = mean(Herbivores_per_100g_dry_soil),
            Predators = mean(Predators_per_100g_dry_soil)) %>%
  gather("Group", "abundance", - c(precipitation_level, n)) %>%
  ggplot(aes(x = precipitation_level, y = abundance, group = Group, fill = Group)) + geom_col()





