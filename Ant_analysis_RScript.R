### Title: Ant analysis
### Author: Marion Donald (pulling from scripts by Gabriela Zambrano and Meghan Hager)
### Date Started: 23 July 2019
### Purpose: Re-run analyses for species accumulation, richness, community composition, etc.
### Date Updated: 30 July 2019


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
library(gridExtra)
library(cowplot)
library(ggpubr)
library(wesanderson)


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
  bind_rows(data_GZ_wide2) %>% 
  mutate(`Pheidole flavens complex` = `Pheidole flavens complex` + `Pheidole moerens`) %>% ## combining P. moerens since it can't be reliably identified from P. flavens complex
  select(-`Pheidole moerens`) ## drop P. moerens now that it's been combined with P. flavens


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

## Chao estimated (36 species observed, ~42 +/- 6 species predicted by Chao, 42 predicted by jack1 +/- 2)
richness_est_df <- vegan::specpool(data_spacc)


data_spacc_df <- as.data.frame(data_spacc)


## calculate species rank abundance using the rankabundance package from BiodiversityR (only accepts dataframes not tibbles)
RankAbun.1 <- BiodiversityR::rankabundance(as.data.frame(data_spacc))

## conver to df for ggplot
rank_abu_df <- as.data.frame(RankAbun.1) %>% 
  rownames_to_column(var = "species") #%>% 

## native classification df to merge (native = 1, invasive = 0)
native_info <- native_classification %>% 
  rename(species = "Species") %>% 
  mutate(species = as.character(species))


rank_abu_df2 <- rank_abu_df  %>% 
  full_join(native_info) %>% 
  mutate(Native = as.character(Native))
         
## Pheidole flavens complex as Native for rank abundance
rank_abu_P.flav.native <- rank_abu_df2 %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 1, Native),
         Classification = ifelse(Native == 1, "native", "non-native")) 



## Rank abundance curve with Pheidole flavens complex as native (1)
rank_abu_fig <- ggplot(rank_abu_P.flav.native, aes(x=rank, y = abundance, label=species))+
  geom_point(aes(color = Classification, shape = Classification), size = 4)+
  geom_line()+
  theme_classic()+
  labs(x = "Species rank",
       y = "Abundance")+
  geom_text(aes(label=ifelse(abundance>150,as.character(species),'')),hjust=-0.08,vjust=0, check_overlap = T)+
  scale_color_manual(values = c("#055864","#04C0DD", "gray49"))+
  scale_shape_manual(values = c(16, 16))+
  theme(text = element_text(family = "Times New Roman", size = 14))

## Pheidole flavens complex as Native for rank abundance
rank_abu_P.flav.non.native <- rank_abu_df2 %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 0, Native),
         Classification = ifelse(Native == 1, "native", "non-native")) 



## Rank abundance curve with Pheidole flavens complex as non-native (NN) (0)
rank_abu_NN_fig <- ggplot(rank_abu_P.flav.non.native, aes(x=rank, y = abundance, label=species))+
  geom_point(aes(color = Classification, shape = Classification), size = 4)+
  geom_line()+
  theme_classic()+
  labs(x = "Species rank",
       y = "Abundance")+
  geom_text(aes(label=ifelse(abundance>100,as.character(species),'')),hjust=-0.08,vjust=0, check_overlap = F)+
  scale_color_manual(values = c("#055864","#04C0DD", "gray49"))+
  scale_shape_manual(values = c(16, 16))+
  theme(text = element_text(family = "Times New Roman", size = 14))


plot_grid(rank_abu_fig, rank_abu_NN_fig)

##

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


## Pheidole flavens complex is classed as "2" -- convert to native (1) or invasive (0) and run the analyses for proportion of invasives
data_spacc_all_tidy_PF_native <- data_spacc_all_tidy %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 1, Native))

##### P. flavens as native -- proportion non-native analysis
data_spacc_all_tidy_abu <- data_spacc_all_tidy_PF_native %>% 
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

non_native_propotion_df_cat <- non_native_propotion_df %>% 
  filter(total_abundance != 0) %>% 
  mutate(category = ifelse(non_native_abu_prop < .33, "Low", 
                           ifelse(non_native_abu_prop > .66, "High", "Medium"))) %>% 
  left_join(sp_richness, by = "ID")

non_native_propotion_df_cat$category <- factor(non_native_propotion_df_cat$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )



non_native_propotion_df_cat_tidy <- non_native_propotion_df_cat %>% 
  select(ID, Year, non_native_abu_prop, shannon_div, simpson_div, total_sp, category) %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, non_native_abu_prop, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                    diversity_type == "simpson_div" ~ "Simpson diversity index",
                                    diversity_type == "total_sp" ~ "Species richness"))

abundance_prop_alpha_fig <- ggplot(non_native_propotion_df_cat_tidy, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by abundance P.flavens as native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")

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

### do the same as above but this time by species occurrence (presence/absence) rather than abundance (Pheidole flavens complex as native)
data_spacc_all_tidy_occ <- data_spacc_all_tidy_PF_native %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
 filter(Native == 1) %>% 
  group_by(ID, Year) %>% 
  summarize(num_native_sp = n())

data_spacc_all_tidy_non_native <- data_spacc_all_tidy %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 0) %>% 
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

## residuals plot looks more "random" -- linear model is more appropriate for these occurrence data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, shannon_div))+
  geom_jitter()+
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
  geom_jitter()+
  geom_smooth()+
  geom_line(data = broom::augment(lm_fit_simp_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Simpson diversity index")

occurrence_proportion_df_cat <- occurrence_proportion_df %>% 
  filter(total_sp != 0) %>% 
  mutate(category = ifelse(prop_nonnative_sp < .33, "Low", 
                           ifelse(prop_nonnative_sp > .66, "High", "Medium")))

occurrence_proportion_df_cat$category <- factor(occurrence_proportion_df_cat$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )

## get species richness (total # sp per trap) to use as the species richness diversity measure by abundance later
sp_richness<- occurrence_proportion_df_cat %>% 
  select(total_sp, ID)

occurrence_proportion_df_cat_tidy <- occurrence_proportion_df_cat %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, num_native_sp, num_nonnative_sp,
                                               prop_nonnative_sp, Year1, Year2, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                   diversity_type == "simpson_div" ~ "Simpson diversity index",
                                   diversity_type == "total_sp" ~ "Species richness"))

occurrence_prop_sp_fig <- ggplot(occurrence_proportion_df_cat_tidy, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by occurrence with P.flavens as native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")



##### RUN THE ABOVE WITH P. flavens as non-native
data_spacc_all_tidy_PF_non_native <- data_spacc_all_tidy %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 0, Native))


data_spacc_all_tidy_abu_NN <- data_spacc_all_tidy_PF_non_native %>% 
  group_by(ID, Year, Native) %>% 
  summarize(abundance = sum(abundance))

total_abu_NN <- data_spacc_all_tidy_abu_NN %>% 
  ungroup() %>% 
  group_by(ID, Year) %>% 
  summarize(total_abundance = sum(abundance))

non_native_abu_NN <- data_spacc_all_tidy_abu_NN %>% 
  filter(Native == 0) %>% 
  rename(non_native_abu = abundance) %>% 
  select(-Native)

non_native_propotion_df_NN <- total_abu_NN %>% 
  left_join(non_native_abu) %>% 
  mutate(non_native_abu_prop = non_native_abu/total_abundance) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df,
            simpson_div_df)


non_native_propotion_df_NN_cat <- non_native_propotion_df_NN %>% 
  filter(total_abundance != 0) %>% 
  mutate(category = ifelse(non_native_abu_prop < .33, "Low", 
                           ifelse(non_native_abu_prop > .66, "High", "Medium"))) %>% 
  left_join(sp_richness, by = "ID")

non_native_propotion_df_NN_cat$category <- factor(non_native_propotion_df_NN_cat$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )



non_native_propotion_df_NN_cat_tidy <- non_native_propotion_df_NN_cat %>% 
  select(ID, Year, non_native_abu_prop, shannon_div, simpson_div, total_sp, category) %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, non_native_abu_prop, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                    diversity_type == "simpson_div" ~ "Simpson diversity index",
                                    diversity_type == "total_sp" ~ "Species richness"))

abundance_prop_alpha_fig_NN <- ggplot(non_native_propotion_df_NN_cat_tidy, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by abundance with P.flavens as non-native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")


plot_grid(occurrence_prop_sp_fig, abundance_prop_alpha_fig)
#  linear model
lm_fit <- lm(shannon_div ~ non_native_abu_prop, data=non_native_propotion_df_NN)
summary(lm_fit)
plot(lm_fit) ## residuals are not "random" -- they form an inverted U, linear model is not a great fit for these data
tidy(lm_fit)

glance(lm_fit)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, shannon_div), color = "red")+
  geom_jitter(data = non_native_propotion_df, color = "black")+
  geom_point(data= non_native_propotion_df_NN, aes(color = "red"))+
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

### do the same as above but this time by species occurrence (presence/absence) rather than abundance (Pheidole flavens complex as native)
data_spacc_all_tidy_occ_NN <- data_spacc_all_tidy_PF_non_native %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 1) %>% 
  group_by(ID, Year) %>% 
  summarize(num_native_sp = n())

data_spacc_all_tidy_non_native <- data_spacc_all_tidy %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 0) %>% 
  group_by(ID, Year) %>% 
  summarize(num_nonnative_sp = n())

occurrence_proportion_df_NN <- data_spacc_all_tidy_occ_NN %>% 
  full_join(data_spacc_all_tidy_non_native) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(total_sp = num_native_sp + num_nonnative_sp,
         prop_nonnative_sp = num_nonnative_sp/total_sp) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df, 
            simpson_div_df)

occurrence_proportion_df_cat_NN <- occurrence_proportion_df_NN %>% 
  filter(total_sp != 0) %>% 
  mutate(category = ifelse(prop_nonnative_sp < .33, "Low", 
                           ifelse(prop_nonnative_sp > .66, "High", "Medium")))

occurrence_proportion_df_cat_NN$category <- factor(occurrence_proportion_df_cat_NN$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )


occurrence_proportion_df_cat_tidy_NN <- occurrence_proportion_df_cat_NN %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, num_native_sp, num_nonnative_sp,
                                               prop_nonnative_sp, Year1, Year2, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                    diversity_type == "simpson_div" ~ "Simpson diversity index",
                                    diversity_type == "total_sp" ~ "Species richness"))

occurrence_prop_sp_fig_NN <- ggplot(occurrence_proportion_df_cat_tidy_NN, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by occurrence with P.flavens as non-native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")


plot_grid(occurrence_prop_sp_fig, occurrence_prop_sp_fig_NN, abundance_prop_alpha_fig, abundance_prop_alpha_fig_NN, cols = 2, rows =2)
#### ^^ Not much (if any) real difference between outcomes of treating P. flavens as native or non-native
### or by having the proportion of non-native species be based on occurrence or abundance


### for shannon div
#  linear model
lm_fit_shan_occ<- lm(shannon_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
summary(lm_fit_shan_occ)

plot(lm_fit_shan_occ)
tidy(lm_fit_shan_occ)

glance(lm_fit_shan_occ)

## residuals plot looks more "random" -- linear model is more appropriate for these occurrence data than the abundance proportion
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


## residuals plot looks more "random" -- linear model is still not v. appropriate for these data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, simpson_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit_simp_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Simpson diversity index")


## Next step is to pull just Gabriela's data and look at year to year trends + seasonality in species composition
## using ordination (nMDS or PCoA and PERMANOVA + permdist)
data_NMDS <- data_GZ_wide %>% 
  select(-Year)




# Then calculate multivariate distances between samples using vegdist() with one of various distance metrics:
dist_jac <- vegdist(data_NMDS, method = "jaccard", binary = TRUE) # Binary jaccard distance (presence/absence)
dist_BC <- vegdist(data_NMDS, method = "bray")#, binary = TRUE) # Bray-Curtis distance

# Then generate an MDS object using metaMDS():
mds <- metaMDS(dist_BC)
str(mds)
# Plot the mds using basic graphics
plot(mds)

# Make this into a ggplot:
# Extract the coordinates:
points <- as.data.frame(mds$points)
points

# Add metadata to the points:
data_GZ_wide <- data_GZ_wide %>% 
  rownames_to_column()

points <- points %>% mutate(Sample = rownames(points),
                            Year = data_GZ_wide$Year[match(rownames(points), data_GZ_wide$rowname)]) 
                            Altitude = metadata$Altitude[match(rownames(points), metadata$Plotid)])
points

# Q4: Now that we have the MDS ordination data, how can we make it into a nice-looking ggplot?

nmds <- ggplot(data = points, aes(x = MDS1, y = MDS2))+
  geom_point(aes(color = Year), size = 9)


nmds +
  theme_classic()

## two outliers are really pulling this apart -- drop these - but stress is basically zero -- re-run NMDS without these samples
outliers_rmvd <- points %>% 
  filter(MDS1 < .5,
         MDS2 < .002)
ggplot(data = outliers_rmvd, aes(x = MDS1, y = MDS2))+
  geom_point(aes(color = as.factor(Year)))+
  theme_classic()


## pull out these two points (#79 and 168) and drop these from the original df
outliers_ID <- points %>% 
  rownames_to_column() %>% 
  filter(MDS1 > .4 |
         MDS2 > .002)

data_GZ_wide_new <- data_GZ_wide %>% 
  filter(rowname != 79,
         rowname != 168)


data_NMDS <- data_GZ_wide_new %>% 
  select(-Year, -rowname)



# Then calculate multivariate distances between samples using vegdist() with one of various distance metrics:
dist_jac <- vegdist(data_NMDS, method = "jaccard", binary = TRUE) # Binary jaccard distance (presence/absence)
dist_BC <- vegdist(data_NMDS, method = "bray")#, binary = TRUE) # Bray-Curtis distance

# Then generate an MDS object using metaMDS():
mds <- metaMDS(dist_BC, trymax = 100)

mds2<- metaMDS(dist_BC, trymax = 100, previous.best = mds, noshare=0.1)
str(mds)
# Plot the mds using basic graphics
plot(mds)

# Make this into a ggplot:
# Extract the coordinates:
points <- as.data.frame(mds2$points)
points

# Add metadata to the points:
data_GZ_wide_new <- data_GZ_wide_new %>% 
  rownames_to_column()

points <- points %>% mutate(Sample = rownames(points),
                            Year = data_GZ_wide$Year[match(rownames(points), data_GZ_wide$rowname)]) 
Altitude = metadata$Altitude[match(rownames(points), metadata$Plotid)])
points

# Q4: Now that we have the MDS ordination data, how can we make it into a nice-looking ggplot?

nmds <- ggplot(data = points, aes(x = MDS1, y = MDS2))+
  geom_point(aes(color = Year))


nmds +
  theme_classic()


## NMDS isn't converging with all of the individual pitfalls -- likely because many of them have few but differenct ant spp
## try this again but now grouped by site within year and season
GZ_wide <- data_GZ_clean %>% 
  filter(Year == 2014 |
           Year == 2015,
         Abundance != "NA") %>% 
  select(Year, Site, Station, Month, Genus, Species, Abundance) %>% 
  unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  # group_by(species) %>% 
  #  mutate(grouped_id = row_number()) %>% 
  spread(key = "species", value = "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%  ## make all the NAs under the species names zeros (for this analysis) 
group_by(Year, Site, Month) %>% 
  summarize_at(vars(`Aphaenogaster carolinensis`:`Trachymyrmex septentrionalis`), sum)

GZ_wide_NMDS <- GZ_wide %>% 
  ungroup() %>% 
  select(-Year, -Site, -Month)



# Then calculate multivariate distances between samples using vegdist() with one of various distance metrics:
dist_jac <- vegdist(GZ_wide_NMDS, method = "jaccard", binary = TRUE) # Binary jaccard distance (presence/absence)
dist_BC <- vegdist(GZ_wide_NMDS, method = "bray")#, binary = TRUE) # Bray-Curtis distance

# Then generate an MDS object using metaMDS(): ### STILL NOT WORKING WITH NMDS
mds <- metaMDS(dist_jac, trymax = 100, noshare=0.1)

mds2<- metaMDS(dist_BC, trymax = 100, previous.best = mds, noshare=0.1)
str(mds)
# Plot the mds using basic graphics
plot(mds)

# Make this into a ggplot:
# Extract the coordinates:
points <- as.data.frame(mds2$points)
points

# Add metadata to the points:
data_GZ_wide_new <- data_GZ_wide_new %>% 
  rownames_to_column()

points <- points %>% mutate(Sample = rownames(points),
                            Year = data_GZ_wide$Year[match(rownames(points), data_GZ_wide$rowname)]) 
Altitude = metadata$Altitude[match(rownames(points), metadata$Plotid)])
points

# Q4: Now that we have the MDS ordination data, how can we make it into a nice-looking ggplot?

nmds <- ggplot(data = points, aes(x = MDS1, y = MDS2))+
  geom_point(aes(color = Year))


nmds +
  theme_classic()


## trying PoA
library(ape)
PCOA <- pcoa(dist_BC)


# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, GZ_wide_NMDS)



# Make this into a ggplot:
# Extract the coordinates:
points <- as.data.frame(PCOA$vectors) %>% 
  select(Axis.1, Axis.2) %>% 
  rownames_to_column()
points

# Add metadata to the points:
GZ_wide_2 <- GZ_wide %>% 
  rownames_to_column()

points <- points %>% mutate(Sample = rownames(points),
                            Year = GZ_wide_2$Year[match(rownames(points), GZ_wide_2$rowname)],
                            Month = GZ_wide_2$Month[match(rownames(points), GZ_wide_2$rowname)]) 

points

# Q4: Now that we have the MDS ordination data, how can we make it into a nice-looking ggplot?

points<- points %>% 
  unite(month_yr, c("Month", "Year"), remove = F)

PCoA<- ggplot(data = points, aes(x = Axis.1, y = Axis.2, color = as.factor(Month)))+
  geom_point(aes(shape = as.factor(Year)))+
  stat_ellipse()+
  theme_classic()


PCoA

adonis(dist_BC ~ Year*Month, data = GZ_wide_2, strata = GZ_wide_2$Month)
## Composition in ant community affected by seasonality but not by year

## check for homogeneity in dispersion - if there is, it may invalidate the PERMANOVA sp. composition results

group <- GZ_wide_2$Month
## Calculate multivariate dispersions
mod <- betadisper(dist_BC, group)
mod

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## no difference in homogeneity of dispersion, can continue with the seasonality differences being due to compositional differences
## rather than being driven by differences in composition within sample groups (higher vs lower beta diversity)


## trying again with multiple samples per site:
GZ_wide_orig <- data_GZ_clean %>% 
  filter(Year == 2014 |
           Year == 2015,
         Abundance != "NA") %>% 
  select(Year, Site, Station, Month, Genus, Species, Abundance) %>% 
  unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  # group_by(species) %>% 
  #  mutate(grouped_id = row_number()) %>% 
  spread(key = "species", value = "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0)) ## make all the NAs under the species names zeros (for this analysis) 


GZ_wide_NMDS <- GZ_wide_orig %>% 
  ungroup() %>% 
  select(-Year, -Site, -Month, -Station)

dist_BC <- vegdist(GZ_wide_NMDS, method = "bray")#, binary = TRUE) # Bray-Curtis distance

library(ape)
PCOA <- pcoa(dist_BC)


# plot the eigenvalues and interpret
barplot(PCOA$values$Relative_eig[1:10])
# Can you also calculate the cumulative explained variance of the first 3 axes?

# Some distance measures may result in negative eigenvalues. In that case, add a correction:
PCOA <- pcoa(dist, correction = "cailliez")

# Plot your results
biplot.pcoa(PCOA)

# You see what`s missing? 
# Indeed, there are no species plotted on this biplot. 
# That's because we used a dissimilarity matrix (sites x sites) 
# as input for the PCOA function. 
# Hence, no species scores could be calculated. 
#However, we could work around this problem like this:
biplot.pcoa(PCOA, GZ_wide_NMDS)



# Make this into a ggplot:
# Extract the coordinates:
points <- as.data.frame(PCOA$vectors) %>% 
  select(Axis.1, Axis.2) %>% 
  rownames_to_column()
points

# Add metadata to the points:
GZ_wide_2 <- GZ_wide_orig %>% 
  rownames_to_column()

points <- points %>% mutate(Sample = rownames(points),
                            Year = GZ_wide_2$Year[match(rownames(points), GZ_wide_2$rowname)],
                            Month = GZ_wide_2$Month[match(rownames(points), GZ_wide_2$rowname)]) 

points

# Q4: Now that we have the MDS ordination data, how can we make it into a nice-looking ggplot?

points<- points %>% 
  unite(month_yr, c("Month", "Year"), remove = F)

PCoA<- ggplot(data = points, aes(x = Axis.1, y = Axis.2, color = as.factor(Month)))+
  geom_point(aes(shape = as.factor(Year)))+
  stat_ellipse()+
  theme_classic()


PCoA

adonis(dist_BC ~ Year*Month, data = GZ_wide_2, strata = GZ_wide_2$Site)
## Composition in ant community affected by the interaction of season and year --- but we're getting the weird horseshoe shape here... 
## think it's better to pool stations at the site level

## check for homogeneity in dispersion - if there is, it may invalidate the PERMANOVA sp. composition results

group <- GZ_wide_2$Month
## Calculate multivariate dispersions
mod <- betadisper(dist_BC, group)
mod

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)
