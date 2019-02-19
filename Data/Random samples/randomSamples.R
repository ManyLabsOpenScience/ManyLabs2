library(plyr)
library(tidyverse)
library(rio)

setwd("~/Documents/GitHub/ManyLabs2/Data/")
# ML2.S1 <- load("ML2_S1.Rdata")
# ML2.S2 <- load("ML2_S2.Rdata")

set.seed(321)
S1.sources <- unique(ML2.S1$Source.Global)
S1.IDs <- llply(S1.sources, function(s) sample(ML2.S1$ResponseID[ML2.S1$Source.Global%in%s],round(NROW(ML2.S1$ResponseID[ML2.S1$Source.Global%in%s])*(1/3))))
names(S1.IDs) <- S1.sources
S1.IDs.selected <- unlist(S1.IDs)

ML2.S1rnd <- ML2.S1 %>% filter(ResponseID%in%S1.IDs.selected)
export(ML2.S1rnd,"ML2.Slate1.Random3rd.xlsx")
export(ML2.S1rnd,"ML2.Slate1.Random3rd.sav")
export(ML2.S1rnd,"ML2.Slate1.Random3rd.csv")
save(ML2.S1rnd,file = "ML2.Slate1.Random3rd.RData")

S2.sources <- unique(ML2.S2$Source.Global)
S2.IDs <- llply(S2.sources, function(s) sample(ML2.S2$ResponseID[ML2.S2$Source.Global%in%s],round(NROW(ML2.S2$ResponseID[ML2.S2$Source.Global%in%s])*(1/3))))
names(S2.IDs) <- S2.sources
S2.IDs.selected <- unlist(S2.IDs)

ML2.S2rnd <- ML2.S2 %>% filter(ResponseID%in%S2.IDs.selected)
export(ML2.S2rnd,"ML2.Slate2.Random3rd.xlsx")
export(ML2.S2rnd,"ML2.Slate2.Random3rd.sav")
export(ML2.S2rnd,"ML2.Slate2.Random3rd.csv")
save(ML2.S2rnd,file = "ML2.Slate2.Random3rd.RData")
