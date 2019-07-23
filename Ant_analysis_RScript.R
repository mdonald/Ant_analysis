### Title: Ant analysis
### Author: Marion Donald (pulling from scripts by Gabriela Zambrano and Meghan Hager)
### Date Started: 23 July 2019
### Purpose: Re-run analyses for species accumulation, richness, community composition, etc.
### Date Updated: 23 July 2019


library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

## read in data
data_MH <- read_excel("Meghan Hager Big Thicket Ant Data Updated 07-22-19 (1).xlsx")

data_GZ_all <- read_excel("big_thicket_ants20142018.xlsx", sheet = "Pitfall Data")



data_MH_2 <- data_MH %>% 
  filter(`Collection Method` == "Pit") %>% ## select just the pitfall trap data
  select("Aphenogaster carolinensis":"Trachymyrmex septentrionalis") ## select just the species names

## since MH data doesn't have as many species names as GZ data, we need to add these columns in, in order to merge the two dfs

## get list of species from GZ 
data_GZ_species <- data_GZ_all %>% 
                   filter(Genus != "NA") %>% 
                   unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) ## finding out that there are some data frame issues -- some of the species names are capitalized while others are not
## this is resulting in duplicates

data_GZ_clean <- data_GZ_all %>% 
  filter(Genus != "NA") %>% 
  mutate(Species = ifelse(Species == "Carolinensis", "carolinensis", ## correct the species names (mispellings and uppercase-to-lowercase)
                          ifelse(Species == "Depilis", "depilis", 
                          ifelse(Species == "Patagonicus", "patagonicus",
                          ifelse(Species == "Castaneus", "castaneus",
                          ifelse(Species == "pennsylanicus" | Species == "Pennsylvanicus" | Species == "pennsylvancius", "pennsylvanicus", 
                          ifelse(Species == "ashmeadii", "ashmeadi", 
                          ifelse(Species == "Rimosus", "rimosus",
                          ifelse(Species == "Opacior", "opacior",
                          ifelse(Species == "Coecus", "coecus", 
                          ifelse(Species == "Americana", "americana", 
                          ifelse(Species == "fasionensis", "faisonensis", 
                          ifelse(Species == "Fulva", "fulva",
                          ifelse(Species == "Harpax", "harpax",
                          ifelse(Species == "Dentata", "dentata",
                          ifelse(Species == "Dentigula", "dentigula",
                          ifelse(Species == "Metallescens", "metallescens",
                          ifelse(Species == "Invicta", "invicta", 
                          ifelse(Species == "Molesta", "molesta",
                          ifelse(Species == "Louisianae", "louisianae", Species))))))))))))))))))),
         Genus = ifelse(Genus == "Pachydondyla", "Pachycondyla", Genus))

data_GZ_clean_sp <- data_GZ_clean %>% 
  filter(Year == 2014 |
           Year == 2015,
         Abundance != "NA") %>% 
  filter(Genus != "NA") %>% 
  unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) 



## check species names in MH data **** THERE ARE SPELLING MISTAKES HERE
data_MH_clean <- data_MH %>% 
  rename("Aphaenogaster carolinensis" = 'Aphenogaster carolinensis',
         "Aphaenogaster texana" = "Aphenogaster texana",
         "Hypoponera opaciceps" = "Hyponera opaciceps",
         "Cyphomyrmex rimosus" = "Cyphomyrmex rimosous")

data_MH_sp <- data_MH_clean %>% 
  gather("Aphaenogaster carolinensis":"Trachymyrmex septentrionalis", key = "Species", value = "Abundance") %>% 
  select(Species) %>% 
  distinct(Species) %>% 
  arrange(Species) %>% 
  rename(species = Species)

full_sp_list <- full_join(data_GZ_clean_sp, data_MH_sp) ## this is the full species list across the two datasets (but including all data from 2014-2018)

#### dataset up for species accumulation curve
## filter GZ data to be the samples Gabriela collected in 2014 and 2015 and transform to wide df for species accumulation curve 

data_GZ_wide <- data_GZ_clean %>% 
  filter(Year == 2014 |
           Year == 2015,
         Abundance != "NA") %>% 
  select(Year, Site, Station, Month, Genus, Species, Abundance) %>% 
    unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  # group_by(species) %>% 
  #  mutate(grouped_id = row_number()) %>% 
  spread(key = "species", value = "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%  ## make all the NAs under the species names zeros (for this analysis) 
  select(-Year:-Month) ## drop the identifiers and just keep the species names


## find the species that are in MH that we need to add to this df in order to merge them
missing_GZ <- anti_join(data_MH_sp, data_GZ_clean_sp) ## 5 species that are in MH that need to be added to GZ

data_GZ_wide2 <- data_GZ_wide %>% 
                mutate("Aphaenogaster texana" = 0,
                       "Brachymyrmex patagonicus" = 0,
                       "Nylanderia terricola" = 0,
                       "Pheidole moerens" = 0,
                       "Prenolepis imparis" = 0)

missing_MH <- anti_join(data_GZ_clean_sp, data_MH_sp) ## 14 spp that are in GZ that need to be added to MH

data_MH_2a <- data_MH_clean %>% 
  mutate("Aphaenogaster rudis-fulva-texana complex" = 0,
         "Aphaenogaster treatae" = 0,
         "Brachymyrmex depilis" = 0,
         "Crematogaster ashmeadi" = 0,
         "Crematogaster lineolata" = 0,
         "Crematogaster minutissima" = 0,
         "Crematogaster sp" = 0,
         "Hypoponera opacior" = 0,
         "Labidus coecus" = 0,
         "Pheidole flavens complex" = 0,
         "Pheidole sp" = 0, 
         "Pseudomyrmex pallidus" = 0, 
         "Solenopsis nickersoni" = 0, 
         "Tapinoma sessile" = 0) %>% 
  select("Aphaenogaster carolinensis":"Tapinoma sessile",
         -Site_Code, -`Inside or Outside (Inside=1)`)

data_spacc <- data_MH_2a %>% 
  bind_rows(data_GZ_wide2)

curve <- specaccum(data_spacc, method = "exact")

pitfalls <- as.data.frame(curve$sites)
richness <- as.data.frame(curve$richness)
sd <- as.data.frame(curve$sd)

spacc_df <- pitfalls %>% 
  bind_cols(richness, sd) %>% 
  dplyr::rename(sites = `curve$sites`, 
         richness = `curve$richness`,
         sd = `curve$sd`)

ggplot(spacc_df, aes(sites, richness))+
  geom_point(alpha = .5)+
  geom_ribbon(aes(x = sites, ymin = richness-sd, ymax = richness+sd), alpha = 0.2)+
  theme_classic()+
  labs(x = "Number of pitfall traps",
       y = "Species richness")



data_MH_tidy <- data_MH %>% 
  gather("Aphenogaster carolinensis":"Trachymyrmex septentrionalis", key = "Species", value = "Abundance") %>% 
  mutate(Year = as.factor(2015))

## subset just the pitfall data from MH that we'll combine with Gabriela's 
## for accumulation, rank abu, and proporion non-native/Diversity analyses 
data_MH_pitfall <- data_MH_tidy %>% 
  filter(`Collection Method` == "Pit")

## subset just Gabriela's data (2014 and 2015) and drop pitfalls for which there were no ants collected (Ants? == 0)
data_GZ <- data_GZ_all %>% 
  filter(Year == 2014 |
           Year == 2015) %>% 
  mutate(Year = as.factor(Year)) %>% 
  filter(`Ants?` != 0)

## condense to pitfall level (# species per pitfall)
data_GZ_pitfall <- data_GZ %>% 
  group_by(Year, Site, Station) %>% 
  summarize(species_num = n())
