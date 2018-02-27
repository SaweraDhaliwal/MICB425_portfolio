library(tidyverse)
read.table(file="Saanich.metadata.txt")
read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")
metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")
read.table(file="Saanich.OTU.txt")
metadata = read.table(file="Saanich.OTU.txt")
metadata %>% 
  select(O2_uM)
