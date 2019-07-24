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
library(BiodiversityR)
library(extrafont)
library(ggpubr)
library(broom)

## read in data
data_MH <- read_excel("Meghan Hager Big Thicket Ant Data Updated 07-22-19 (1).xlsx")

data_GZ_all <- read_excel("big_thicket_ants20142018.xlsx", sheet = "Pitfall Data")

native_classification <- read.csv("ant_species_native_classification.csv") 



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
  select(-Site:-Month) ## drop the identifiers and just keep the species names and Year


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
         -Site_Code, -`Inside or Outside (Inside=1)`) %>% 
  mutate(Year = 2015)

data_spacc_all <- data_MH_2a %>% 
  bind_rows(data_GZ_wide2)

data_spacc_year <- data_spacc_all %>% 
                  select(Year)

data_spacc <- data_spacc_all %>% 
              select(-Year) # drop year, just keep the species by site matrix

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

## Chao estimated (37 species observed, 43 +/- 6 species predicted by Chao, 43 predicted back jack1 +/- 2)
richness_est_df <- vegan::specpool(data_spacc)


data_spacc_df <- as.data.frame(data_spacc)


## calculate species rank abundance using the rankabundance package from BiodiversityR (only accepts dataframes not tibbles)
RankAbun.1 <- BiodiversityR::rankabundance(as.data.frame(data_spacc))

## conver to df for ggplot
rank_abu_df <- as.data.frame(RankAbun.1) %>% 
  rownames_to_column(var = "species") #%>% 

## native classification df to merge
native_info <- native_classification %>% 
  rename(species = "Species") %>% 
  mutate(species = as.character(species))


rank_abu_df2 <- rank_abu_df  %>% 
  full_join(native_info) %>% 
  mutate(Native = as.factor(Native)) 

## Rank abundance curve 
ggplot(rank_abu_df2, aes(x=rank, y = abundance, label=species))+
  geom_point(aes(color = Native, shape = Native))+
  geom_line()+
  theme_classic()+
  labs(x = "Species rank",
       y = "Abundance")+
  geom_text(aes(label=ifelse(abundance>150,as.character(species),'')),hjust=-0.05,vjust=0, check_overlap = T)+
  scale_color_manual(values = c("#055864", "#04C0DD", "gray49"))+
  scale_shape_manual(values = c(16,17,21))#+
  #theme(text = element_text(family = "Times New Roman"))

## diversity measures -- no difference in alpha diversity across the years
shannon_div <- diversity(data_spacc, "shannon")

shannon_div_df <- as.data.frame(shannon_div) %>% 
                  bind_cols(data_spacc_year) %>% 
  mutate(Year = as.factor(Year))

ggplot(shannon_div_df, aes(x = Year, y = shannon_div))+
  geom_boxplot()+
  geom_jitter(width = 0.05, height = 0)+
  theme_classic()+
  labs(x = "Sampling year",
       y = "Shannon diversity index")+
  stat_compare_means(method = "t.test")

## simpson div - no difference in alpha diversity across the years
simpson_div <- diversity(data_spacc, "simpson")

simpson_div_df <- as.data.frame(simpson_div) %>% 
  bind_cols(data_spacc_year) %>% 
  mutate(Year = as.factor(Year))

ggplot(simpson_div_df, aes(x = Year, y = simpson_div))+
  geom_boxplot()+
  geom_jitter(width = 0.05, height = 0)+
  theme_classic()+
  labs(x = "Sampling year",
       y = "Simpson diversity index")+
  stat_compare_means(method = "t.test")

## get df with info on sample ID (from row number) and year to get proportion of non-native to native ants (and also do it as occurrence for sp)
data_spacc_all_tidy <- data_spacc_all %>% 
  rownames_to_column("ID") %>% 
  gather(key = "species", value = "abundance", c(-Year, -ID)) %>% 
  full_join(native_info)
  
data_spacc_all_tidy_abu <- data_spacc_all_tidy %>% 
  group_by(ID, Year, Native) %>% 
  summarize(abundance = sum(abundance))

total_abu <- data_spacc_all_tidy_abu %>% 
  ungroup() %>% 
  group_by(ID, Year) %>% 
  summarize(total_abundance = sum(abundance))

non_native_abu <- data_spacc_all_tidy_abu %>% 
  filter(Native == 0) %>% 
  rename(non_native_abu = abundance) %>% 
  select(-Native)

non_native_propotion_df <- total_abu %>% 
  left_join(non_native_abu) %>% 
  mutate(non_native_abu_prop = non_native_abu/total_abundance) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df,
            simpson_div_df)

#  linear model
lm_fit <- lm(shannon_div ~ non_native_abu_prop, data=non_native_propotion_df)
summary(lm_fit)
plot(lm_fit) ## residuals are not "random" -- they form an inverted U, linear model is not a great fit for these data
tidy(lm_fit)

glance(lm_fit)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, shannon_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit), aes(x = non_native_abu_prop, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
    labs(x = "Proportion of non-native ant abundance",
         y = "Shannon diversity index")

### for simpson div
#  linear model
lm_fit_simp<- lm(simpson_div ~ non_native_abu_prop, data=non_native_propotion_df)
summary(lm_fit_simp)

plot(lm_fit_simp)
tidy(lm_fit_simp)

glance(lm_fit_simp)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, simpson_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit), aes(x = non_native_abu_prop, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant abundance",
       y = "Shannon diversity index")

### do the same as above but this time by species occurrence (presence/absence) rather than abundance
data_spacc_all_tidy_occ <- data_spacc_all_tidy %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
 filter(Native == 1) %>% 
  group_by(ID, Year) %>% 
  summarize(num_native_sp = n())

data_spacc_all_tidy_non_native <- data_spacc_all_tidy %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 0 | Native == 2) %>% 
  group_by(ID, Year) %>% 
  summarize(num_nonnative_sp = n())

occurrence_proportion_df <- data_spacc_all_tidy_occ %>% 
  full_join(data_spacc_all_tidy_non_native) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(total_sp = num_native_sp + num_nonnative_sp,
         prop_nonnative_sp = num_nonnative_sp/total_sp) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df, 
            simpson_div_df)


### for shannon div
#  linear model
lm_fit_shan_occ<- lm(shannon_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
summary(lm_fit_shan_occ)

plot(lm_fit_shan_occ)
tidy(lm_fit_shan_occ)

glance(lm_fit_shan_occ)

## residuals plot looks more "random" -- linear model is more appropriate for these data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, shannon_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit_shan_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Shannon diversity index")

### for simpson div
#  linear model
lm_fit_simp_occ<- lm(simpson_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
summary(lm_fit_simp_occ)

plot(lm_fit_simp_occ)
tidy(lm_fit_simp_occ)

glance(lm_fit_simp_occ)

## residuals plot looks more "random" -- linear model is more appropriate for these data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, simpson_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit_simp_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Simpson diversity index")




## Next step is to pull just Gabriela's data and look at year to year trends + seasonality in species composition
## using ordination (nMDS or PCoA and PERMANOVA + permdist)
