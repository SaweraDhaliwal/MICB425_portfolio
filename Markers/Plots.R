library(tidyverse)
library(cowplot)


#DNA data
nirS.DNA.10m = read_tsv("nirS_DNA_10m_marker_contig_map.tsv") %>%
  select(Tax.DNA.10 = Confident_Taxonomy, Abund.DNA.10 = Abundance, Query)
nirS.DNA.100m = read_tsv("nirS_DNA_100m_marker_contig_map.tsv") %>%
  select(Tax.DNA.100 = Confident_Taxonomy, Abund.DNA.100 = Abundance, Query)
nirS.DNA.120m = read_tsv("nirS_DNA_120m_marker_contig_map.tsv") %>%
  select(Tax.DNA.120 = Confident_Taxonomy, Abund.DNA.120 = Abundance, Query)
nirS.DNA.135m = read_tsv("nirS_DNA_135m_marker_contig_map.tsv") %>%
  select(Tax.DNA.135 = Confident_Taxonomy, Abund.DNA.135 = Abundance, Query)
nirS.DNA.150m = read_tsv("nirS_DNA_150m_marker_contig_map.tsv") %>%
  select(Tax.DNA.150 = Confident_Taxonomy, Abund.DNA.150 = Abundance, Query)
nirS.DNA.165m = read_tsv("nirS_DNA_165m_marker_contig_map.tsv") %>%
  select(Tax.DNA.165 = Confident_Taxonomy, Abund.DNA.165 = Abundance, Query)
nirS.DNA.200m = read_tsv("nirS_DNA_200m_marker_contig_map.tsv") %>%
  select(Tax.DNA.200 = Confident_Taxonomy, Abund.DNA.200 = Abundance, Query)

#RNA data
nirS.RNA.10m = read_tsv("nirS_RNA_10m_marker_contig_map.tsv") %>%
  select(Tax.RNA.10 = Confident_Taxonomy, Abund.RNA.10 = Abundance, Query)
nirS.RNA.100m = read_tsv("nirS_RNA_100m_marker_contig_map.tsv") %>%
  select(Tax.RNA.100 = Confident_Taxonomy, Abund.RNA.100 = Abundance, Query)
nirS.RNA.120m = read_tsv("nirS_RNA_120m_marker_contig_map.tsv") %>%
  select(Tax.RNA.120 = Confident_Taxonomy, Abund.RNA.120 = Abundance, Query)
nirS.RNA.135m = read_tsv("nirS_RNA_135m_marker_contig_map.tsv") %>%
  select(Tax.RNA.135 = Confident_Taxonomy, Abund.RNA.135 = Abundance, Query)
nirS.RNA.150m = read_tsv("nirS_RNA_150m_marker_contig_map.tsv") %>%
  select(Tax.RNA.150 = Confident_Taxonomy, Abund.RNA.150 = Abundance, Query)
nirS.RNA.165m = read_tsv("nirS_RNA_165m_marker_contig_map.tsv") %>%
  select(Tax.RNA.165 = Confident_Taxonomy, Abund.RNA.165 = Abundance, Query)
nirS.RNA.200m = read_tsv("nirS_RNA_200m_marker_contig_map.tsv") %>%
  select(Tax.RNA.200 = Confident_Taxonomy, Abund.RNA.200 = Abundance, Query)

#Combine all data frames
nirS.all = nirS.DNA.100m %>%
  full_join(nirS.DNA.120m, by = "Query") %>%
  full_join(nirS.DNA.135m, by = "Query") %>%  
  full_join(nirS.DNA.150m, by = "Query") %>%  
  full_join(nirS.DNA.165m, by = "Query") %>%
  full_join(nirS.DNA.200m, by = "Query") %>%
  full_join(nirS.RNA.100m, by = "Query") %>%
  full_join(nirS.RNA.120m, by = "Query") %>%
  full_join(nirS.RNA.135m, by = "Query") %>%
  full_join(nirS.RNA.150m, by = "Query") %>%
  full_join(nirS.RNA.165m, by = "Query") %>%
  full_join(nirS.RNA.200m, by = "Query") %>%
  mutate(Taxonomy = ifelse(!is.na(Tax.DNA.100), Tax.DNA.100,
                    ifelse(!is.na(Tax.DNA.120), Tax.DNA.120,
                    ifelse(!is.na(Tax.DNA.135), Tax.DNA.135,
                    ifelse(!is.na(Tax.DNA.150), Tax.DNA.150,
                    ifelse(!is.na(Tax.DNA.165), Tax.DNA.165,
                    ifelse(!is.na(Tax.DNA.200), Tax.DNA.200,
                    ifelse(!is.na(Tax.RNA.100), Tax.RNA.100,
                    ifelse(!is.na(Tax.RNA.120), Tax.RNA.120,                           
                    ifelse(!is.na(Tax.RNA.135), Tax.RNA.135,
                    ifelse(!is.na(Tax.RNA.150), Tax.RNA.150,
                    ifelse(!is.na(Tax.RNA.165), Tax.RNA.165,
                    ifelse(!is.na(Tax.RNA.200), Tax.RNA.200,
                           "unclassified"))))))))))))) %>%
  select(-starts_with("Tax.")) %>%
  gather("Key", "Abundance", starts_with("Abund")) %>%
  separate(Key, c("Key","Type","Depth_m"), by = ".") %>%
  select(Depth_m, Type, Abundance, Taxonomy, Query) %>%
  mutate(Depth_m = as.numeric(Depth_m)) %>%
  separate(Taxonomy, into = c("Domain", "Phylum", "Class", "Order", 
                              "Family","Genus", "Species"), sep=";") 

#DNA Abundance
nirS.all %>%
  filter(Type == "DNA") %>%
  mutate(Genus = ifelse(is.na(Genus), "unclassified", Genus)) %>%
  ggplot(aes(x = "nirS", y = Depth_m))+
  geom_point(aes(size = Abundance)) +
  scale_y_reverse(lim = c(200,10)) +
  labs(title = "Abundance of the nirS gene (DNA) at different depths",
       x = "") + 
  theme_classic()

#RNA Abundance
nirS.all %>%
  filter(Type == "RNA") %>%
  mutate(Genus = ifelse(is.na(Genus), "unclassified", Genus)) %>%
  ggplot(aes(x = "nirS", y = Depth_m))+
  geom_point(aes(size = Abundance)) +
  scale_y_reverse(lim = c(200,10)) +
  labs(title = "Abundance of the nirS gene (RNA) at different depths",
       x = "") + 
  theme_classic()

#Comparing DNA and RNA
nirS.all %>% 
  mutate(Genus = ifelse(is.na(Genus), "unclassified", Genus)) %>% 
  ggplot(aes(x = Type, y = Depth_m)) +
  geom_point(aes(size = Abundance)) +
  scale_y_reverse(lim=c(200,10)) +
  labs(title = "Abundance of the nirS gene (DNA vs. RNA) at different depths",
       x = "") +
  theme_classic()

#Comparing by genus
nirS.all %>% 
  mutate(Genus = ifelse(is.na(Genus), "unclassified", Genus)) %>% 
  ggplot(aes(x = Genus, y = Depth_m)) +
  geom_point(aes(size = ifelse(Abundance == 0, NA, Abundance), colour = Type), 
             position = position_dodge(0.5)) +
  scale_y_reverse(lim=c(200,10)) +
  labs(title = "Abundance of the nirS gene (DNA vs. RNA) across different genera at different depths",
       x = "") +
  theme_classic()+scale_size_continuous(name = "Abundance")

#Comparing to [NO3]
load("mothur_phyloseq.RData")
metadata = data.frame(mothur@sam_data)

plot1 = nirS.all %>% 
  mutate(Genus = ifelse(is.na(Genus), "unclassified", Genus)) %>% 
  ggplot(aes(x = Genus, y = Depth_m)) +
  geom_point(aes(size = ifelse(Abundance == 0, NA, Abundance), colour = Type), 
             position = position_dodge(0.5)) +
  scale_y_reverse(lim=c(200,10)) +
  theme_classic()+scale_size_continuous(name = "Abundance")

plot2 = metadata %>%
  arrange(Depth_m) %>%
  ggplot(aes(x = NO3_uM, y = Depth_m))+
  geom_point()+
  geom_path(aes(group = 1)) +
  scale_y_reverse(lim=c(200,10))+
  theme_classic() + 
  labs(y = "Depth (m)",
       x = "Nitrate (μM)")

plot_grid(plot2, plot1, labels=c("A","B"), rel_widths = c(1/8, 7/8))

