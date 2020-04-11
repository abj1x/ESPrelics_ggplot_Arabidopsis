## after https://www.rpubs.com/spoonerf/countrymapggplot2

library(maptools)
library(raster)
library(ggplot2)
library(rgdal)
library(plyr)
library(tibble)
library(dplyr)

## tg_ecotypeid for 21 relic accessions defined in Table S3 (Iberian relics) of '1,135 Genomes Reveal the Global Pattern
## of Polymorphism in Arabidopsis thaliana' Cell Vol.166 plus Cvi-0 (6911; Cape Verde Islands), Can-0 (7063; Canary Islands), and Etna-2 (9762; Italy)

## for final figure in James et al. 2018 'Global spatial analysis of Arabidopsis natural variants implicates 5’UTR splicing of LATE ELONGATED HYPOCOTYL in responses to temperature'
## Plant cell & Environ. the accessions Cvi-0 (6911; Cape Verde Islands) and Etna-2 (9762; Italy) were removed and added manually to figure.

relics<-c("9832","9837","9947","9533","9871","9905","9542","9869","9600","9543","9598","9555","9545","9550","9887","9549","9554","9944","9574","9583","9879","6911","7063","9762")

## all ecotypes from James et al. 2018 'Global spatial analysis of Arabidopsis natural variants implicates 5’UTR splicing of LATE ELONGATED HYPOCOTYL in responses to temperature'
## Plant cell & Environ.
ecotypes_all<-read.csv('WRLDacc2.csv', header=TRUE)

## subset just the relic accessions and select a subset of columns of data
relics_ecotypes_all<-ecotypes_all[ecotypes_all$tg_ecotypeid %in% relics,] %>% select(tg_ecotypeid, name, long, lat, genotype_comp)

## subset just the Spanish (ESP) accessions
ESPdata<-ecotypes_all[which(ecotypes_all$country=="ESP"),]

## subset just the Spanish accessions that have the AUGCA LHY haplotype from the previous subset
ESPdata_rare<-ESPdata[which(ESPdata$genotype_comp == "A/ U/ G/ C/ A/"),]

## subset the previous subset based on the same column identifiers for relics_ecotypes_all
ESPdata_rare<- ESPdata_rare %>% select(tg_ecotypeid, name, long, lat, genotype_comp)

## find relic accessions that are not in ESPdata_rare
aj<- anti_join(relics_ecotypes_all, ESPdata_rare, by = 'tg_ecotypeid')

## add relic non_ESPdata_rare to ESPdata_rare
total<-rbind(aj,ESPdata_rare)

## test what rows in total are the defined relic accessions and return a boolean TRUE FALSE vector column
total$relic<- is.element(total$tg_ecotypeid,relics)

## rename booleans in relic column
total$relic<-ifelse(total$relic == TRUE, "relic", "non-relic")

## https://github.com/tidyverse/dplyr/issues/3893
spain<-getData("GADM",country="ES",level=0)

## make base map

esp<-ggplot()+geom_polygon(data=spain,aes(x=long,y=lat,group=group))+coord_fixed(1.3)

esp+theme_dark() +
  geom_point(data=total,aes(x=long,y=lat,color=factor(genotype_comp),shape=factor(relic)),size=2.5,alpha=0.5) +
  labs(x="longitude", y="latitude",col="haplotype") +
  scale_color_manual(values=c("chartreuse2","firebrick1","lightgoldenrod2")) +
  theme(legend.background = element_rect(fill="gray 90"),legend.title=element_blank()) +
  theme(legend.justification = c(0,1),legend.position = c(0,1)) +
  theme(legend.background = element_rect(colour="black",size=0.25)) +
  annotate(geom="text",x=3.0,y=37.0,label="Etna-2",color="white") +
  annotate(geom="text",x=-11,y=28.5,label="Cvi-0",color="white")

ggsave('Spain_relic_map_LHY_haplotype.png')
