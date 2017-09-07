# SPLIT VIOLIN PLOTS - MANYLABS 2 -
# corresponding coder: Fred Hasselman (https://osf.io/ujgs6/)

# devtools::source_url('https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/C-3PR_ASCII.R')
# init()

source('~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manylabRs/pkg/R/C-3PR_ASCII.R')
init()

srcDir <- "~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manylabRs/pkg/R/"
Slabels <- c("non-WEIRD","WEIRD","Multi",".")

ML2.key    <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key    <- ML2.key[ML2.key$study.name!="",]
dir.in     <- "~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/"
outlist1   <- rio::import(paste0(dir.in,"Data_Figure.rds"))
outlist1$study.labels <- ""

for(s in unique(ML2.key$study.id)){
  outlist1$study.labels[outlist1$study.id%in%s] <- ML2.key$study.description[ML2.key$study.id%in%s]
}
table(outlist1$study.labels)
oriEffects <- ML2.key[(ML2.key$study.figure2.include==1|ML2.key$ori.study.figure2.include==1)&ML2.key$study.name!="",]

tab   <- table(oriEffects$study.name)
IDrem <- (oriEffects$study.figure2.include==1)&(oriEffects$ori.study.figure2.include==0)

anaName    <- oriEffects$study.analysis[IDrem]
oriEffects <- oriEffects[!IDrem,]


# WEIRDness of original effect sizes ------------------------------------------------------------------------------

oriEffects$ori.WEIRD <- oriEffects$ori.sample.weird
oriEffects$ori.WEIRD[oriEffects$ori.study.figure2.include==0] <- 99
oriEffects$ori.WEIRD.f <- factor(oriEffects$ori.WEIRD,
                                    levels = c(0,1,10,99),
                                    labels = Slabels)
oriEffects$orig.ES.r[oriEffects$ori.WEIRD==99] <- 1
oriEffects <- select(oriEffects, c(1:5,11:14,22:24,43:49))

df <- outlist1
df$labels <- factor(as.character(df$analysis.name))
df <- df %>% group_by(labels) %>% mutate(meanES   = mean(ESCI.r, na.rm=TRUE),
                                         medianES = median(ESCI.r, na.rm=TRUE))

# Arrange MEAN
df <- df[!is.na(df$ESCI.r),]
df <- dplyr::arrange(df, labels, meanES)
df$study.labels <- reorder(df$study.labels, df$meanES, order=TRUE)
df$study.labels <- reorder(df$study.labels, df$meanES, order=TRUE)
studOrder <- attributes(df$study.labels)$scores
offsets   <- names(studOrder) %>% {setNames(0:(length(.) - 1), .)}
all.equal(names(studOrder),names(offsets))

#df$meanES <- df$medianES

# dfG <- summarise(group_by(df,study.labels),
#                  y   = median(ESCI.r,na.rm  = T),
#                  ymin= mean(ESCI.l.r, na.rm = T),
#                  ymax= mean(ESCI.u.r, na.rm = T))
#
# dfG   <- arrange(dfG, y)
#
# dfAg <- aggregate(ESCI.r ~ study.labels, df, mean)

okN <- df$stat.N>=30
okCond <- (df$stat.n1>=15)|(df$stat.n1>=15)

df <- df[okN|okCond, ]

df$sigf     <- "p > .05"
df$sigf[df$test.p.value<.05] <- "p < .05"
df$sigf.f <- factor(df$sigf)

df$sigf.f <- relevel(df$sigf.f, ref = "p > .05")

df$USA <- "non-USA"
df$USA[df$source.Country=="USA"] <- "USA"


df$source.WEIRD.f <- factor(df$source.Weird,
                            levels = c(0,1),
                            labels = c("non-WEIRD","WEIRD"))
df$splitv <- df$source.WEIRD.f

#Get the group levels for the split variable
splits <- unique(df[['splitv']])
splits <- relevel(splits, ref = "WEIRD")


# Calculate and scale group densities
#
# I'm rescaling the density curves so that they all have the same peak height and don't overlap. Edit the mutate() line if you want to have the density polygons be different sizes (e.g., scaled so that they show the relative amount of data in each group).

tblF <- summarise(group_by(df, study.labels, splitv),
                  N = n())
nnames   <- tblF$study.labels[tblF$N==1]
ssplitv  <- tblF$splitv[tblF$N==1]

for(c in seq_along(nnames)){
  df[nrow(df)+1,] <- df[df$study.labels%in%nnames[c]&df$splitv%in%ssplitv[c],]
}

pdat = df %>%
  group_by(study.labels, splitv) %>%
  do(tidy(density(.[['ESCI.r']]))) %>%
  rename(loc = x, dens = y) %>%
  mutate(dens = 0.45 * dens / max(dens)) %>%
  ungroup()

pdat$splitv <- relevel(pdat$splitv, ref = "WEIRD")

# Clonnect the extrema of the split distrubutions
for(sl in levels(pdat$study.labels)){
  ID <- pdat$study.labels%in%sl
  tmp <- pdat[ID,]
  mxID <- which.max(tmp$loc)
  mnID <- which.min(tmp$loc)
  mxLab <- ifelse(tmp$splitv[mxID]%in%splits[1],splits[2],splits[1])
  mnLab <- ifelse(tmp$splitv[mnID]%in%splits[2],splits[1],splits[2])
  pdat <- add_row(pdat,
                  study.labels = rep(sl,2),
                  splitv      =  c(mxLab,mnLab),
                  loc         =  c(max(tmp$loc),min(tmp$loc)),
                  dens        = c(0,0)
                  )
  rm(tmp)
}

# Calculate summary statistics in a separate dataframe
#
# If you need more summary statistics, add new variables to the summarise() call here, and add the additional variable names to the gather() call.

sums = df %>%
  group_by(study.labels, splitv, source.Source) %>%
  summarise(sample_loc = first(ESCI.r)) %>%
  ungroup() %>%
  gather(segment, loc_sum, sample_loc)

means = df %>%
  group_by(study.labels, splitv) %>%
  summarise(mean_loc   = mean(ESCI.r, na.rm = TRUE)) %>% #median_loc = median(ESCI.r, na.rm = TRUE),
  ungroup() %>%
  gather(segment, loc_sum, mean_loc)

# Calculate the corresponding points on each group's density curve
#
# To do this, I'm taking the scaled density curves stored in pdat, then feeding them into the approx() function, plus the y-axis locations of the summary statistics (loc_sum), to get the corresponding x-axis values for the summary statistics.

sums = left_join(pdat, sums, by=c('study.labels', 'splitv')) %>%
  group_by(study.labels, splitv, source.Source) %>%
  do(data.frame(loc     = unique(.$loc_sum),
                dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
  ungroup()

means = left_join(pdat, means, by=c('study.labels', 'splitv')) %>%
  group_by(study.labels, splitv) %>%
  do(data.frame(loc     = unique(.$loc_sum),
                dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
  ungroup()

# Offset the densities and summary statistics
#
# Modify pdat and sums to add offset_dens columns, which are the density curve values offset by the x-axis group. Also, for the groups that are on the left side of the split violins, invert the densities.
pdat <- arrange(pdat, study.labels,splitv,loc)
pdat <- pdat %>%
  mutate(offset_dens = offsets[.[['study.labels']]] + ifelse(.[['splitv']] == splits[2], -dens, dens))


means <- means %>%
  mutate(offset = offsets[.[['study.labels']]],
         offset_dens = offset + ifelse(.[['splitv']] == splits[2], -dens, dens),
         offset_fix  = offset + ifelse(.[['splitv']] == splits[2], -.45, .45))

means$oriES      <- NA
means$oriWEIRD   <- NA
means$oriWEIRD.f <- NA
labs <- levels(means$study.labels)
for(n in seq_along(labs)){
  # means$oriES[means$study.labels%in%as.character(means$study.labels[n])]  <-  oriEffects$orig.ES.r[oriEffects$study.description%in%as.character(means$study.labels[n])] %||% NA
   esID    <- oriEffects$study.description%in%labs[n]
   meansID <- means$study.labels%in%oriEffects$study.description[esID]

  if(any(meansID&means$splitv%in%Slabels[1])){
    means$oriWEIRD[meansID&means$splitv%in%Slabels[1]]  <- Slabels[1]
    means$oriES[meansID&means$splitv%in%Slabels[1]]     <- oriEffects$orig.ES.r[esID&oriEffects$ori.WEIRD==0]%0!0%NA
  }
   if(any(meansID&means$splitv%in%Slabels[2])){
     means$oriWEIRD[meansID&means$splitv%in%Slabels[2]]  <- Slabels[2]
     means$oriES[meansID&means$splitv%in%Slabels[2]]     <- oriEffects$orig.ES.r[esID&oriEffects$ori.sample.weird==1]%0!0%NA
   }
}

#means$offset_fix[means$oriWEIRD%in%"Mixed"] <- means$offset_fix[means$oriWEIRD%in%"Mixed"]+c(.225,-.225)

sums = sums %>%
  mutate(offset      = offsets[.[['study.labels']]],
         offset_dens = offset + ifelse(.[['splitv']] == splits[2], -dens, dens),
         offset_fix  = offset + ifelse(.[['splitv']] == splits[2], -.2, .2))



df$ESCI.N.bin <- cut(df$ESCI.N.total,c(0,80,200,900))
sums$offset_densN <- NA
for(smpl in unique(df$source.Source)){
  tblN <- df[df$source.Source%in%smpl,c('study.labels','ESCI.N.bin')]
  for(l in unique(tblN$study.labels)){
    sums$offset_densN[sums$source.Source%in%smpl&sums$study.labels%in%l] <- tblN$ESCI.N.bin[tblN$study.labels == l]
  }
}

sums$tsize   <- sums$offset + ifelse(sums$splitv == splits[2],-.2, 2)
sums$relSize <- sums$offset + (ifelse(sums$splitv == splits[2],-1,1) * (ifelse(as.numeric(sums$offset_densN)==1, 1/6, ifelse(as.numeric(sums$offset_densN)==2,2/6,3/6))))

# Colour blind safe
myCols <- brewer_pal(palette="RdYlBu")(11)
#myCols <- c("#d73027","#4575b4","#5aae61")


#Colorblindsafe colors
cwhite = "#f7f7f7"
ccream = "#2166ac"
cblank = "#d1e5f0"
corange = "#f4a582"
cblue  = myCols[11]  #"#2166ac"
cblueL = myCols[10]  #"#d1e5f0"
cred   = myCols[1] #"#d6604d"
credL  = myCols[2]  #"#f7f7f7"
cpurp  = "#b2abd2"

mypalette <- c(credL,cblueL)

outdir <- "/Users/Fred/Dropbox/Manylabs2/Figures"

cols <- c("non-WEIRD" = credL, "WEIRD" = cblueL, "Mixed"="#5aae61")

pdat$study.labels <- ordered(as.character(pdat$study.labels), levels = names(studOrder[order(studOrder)]))

means <- means %>%
  mutate( offset_fix  = offset + ifelse(.[['splitv']] == splits[2], -.2, .2))

# means$oriWEIRD.n[means$oriWEIRD%in%"WEIRD"] <- 21
# means$oriWEIRD.n[means$oriWEIRD%in%"non-WEIRD"] <- 22
# means$oriWEIRD.n[means$oriWEIRD%in%"."] <- 25

Sbreaks <- c(25,24,23,21)
Slabels <- c("non-WEIRD","WEIRD","Mixed",".")
OriShapes <- list()
means$oriWEIRD.n <- NA
for(s in seq_along(Sbreaks)){
  means$oriWEIRD.n[means$oriWEIRD%in%Slabels[s]] <- Sbreaks[s]
  OriShapes[[s]] <- Sbreaks[s]
}
means$oriWEIRD.n <- factor(means$oriWEIRD.n,Sbreaks,Sbreaks)
names(OriShapes) <- paste(Sbreaks)
OriShapes<-unlist(OriShapes)

# pdat$splitv <- relevel(pdat$splitv,ref="non-WEIRD")
# sums$splitv <- relevel(sums$splitv,ref="non-WEIRD")
# means$splitv<- relevel(means$splitv,ref="non-WEIRD")

pdat <- na.omit(pdat)

for(s in unique(pdat$study.labels)){
  IDs<-pdat$study.labels%in%s

  locmn <- min(pdat$loc[IDs])
  locmx <- max(pdat$loc[IDs])

  IDminSV1 <- which.min(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[1]])
  IDminSV2 <- which.min(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[2]])
  IDmaxSV1 <- which.max(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[1]])
  IDmaxSV2 <- which.max(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[2]])

  # IDmin <- which.min(c(pdat$loc[IDs][IDminSV1],pdat$loc[IDs][IDminSV2]))
  # IDmax <- which.min(c(pdat$loc[IDs][IDmaxSV1],pdat$loc[IDs][IDmaxSV2]))

  pdat$loc[IDs][IDminSV1]         <- pdat$loc[IDs][IDminSV2]         <- locmn
  pdat$offset_dens[IDs][IDminSV1] <- pdat$offset_dens[IDs][IDminSV2] <- offsets[names(offsets)%in%s]

  pdat$loc[IDs][IDmaxSV1]         <- pdat$loc[IDs][IDmaxSV2]         <- locmx
  pdat$offset_dens[IDs][IDmaxSV1] <- pdat$offset_dens[IDs][IDmaxSV2] <- offsets[names(offsets)%in%s]

}

#OriShapes <- c("21"="WEIRD","22"="non-WEIRD","23"="Ori ≠ ML2")

# G2 --------------------------------------------------------------------------------------------------------------


g2 <- ggplot(pdat, aes(offset_dens, loc, group = interaction(pdat[['study.labels']], pdat[['splitv']])), colour = splitv) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_path(size=1, alpha = .5, color = "grey70")+
  geom_path(aes(colour=splitv)) +
  geom_segment(data=sums, aes(x = offset,
                              y = loc,
                              xend = offset_fix,
                              yend = loc,
                              colour = splitv),
               inherit.aes=FALSE, alpha=.7, size= .2) +
  geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc, colour =  splitv), inherit.aes=FALSE, size=.8, alpha=.8) +
  geom_point(data=means, aes(x = offset_fix, y = oriES, shape=oriWEIRD.n, fill=splitv), colour = "black", inherit.aes = FALSE, size=1.2, alpha=.8) +
  scale_colour_manual('Sample', values = cols) +
  scale_x_continuous(name = '', breaks = unname(offsets), labels = names(offsets), limits = c(-0.5,27.5)) +
  scale_fill_manual('',values = cols, guide=FALSE) +
  scale_shape_manual('Original Effect Size',values=OriShapes, breaks = Sbreaks,labels = c("","","","")) +
  guides(colour = guide_legend(order = 1, ncol=1,title.vjust=.81,
                               title.theme = element_text(size = 9,angle=0,face="bold")),
         shape = guide_legend(ncol =1,title.position = "right",title.vjust=.81,
                              title.theme = element_text(size = 9,angle=0,face="bold"))) +
  ylim(c(-1,1)) +
  ylab('Effect Size r') +  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size=rel(.8)),
        legend.box.background  =element_blank(),
        legend.box.just = "left",
        legend.spacing.x = unit(1,"lines"),
        legend.margin = margin(0,0,0,0),
        panel.grid.major.y =element_line(colour="grey70"),
        panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank()) +
  coord_flip()
g2


png(filename = paste0(outdir,"/ML2_SplitViolin_MEANsort_Ori_altLegend.png"),width=2400,height=2177, res=250)
g2
 dev.off()
g2




# Arrange GLOBAL
df <- df[!is.na(df$ESCI.r),]
df <- dplyr::arrange(df, labels, GlobalES.r)
df$study.labels <- reorder(df$study.labels, df$GlobalES.r, order=TRUE)
df$study.labels <- reorder(df$study.labels, df$GlobalES.r, order=TRUE)
studOrder <- attributes(df$study.labels)$scores
offsets   <- names(studOrder) %>% {setNames(0:(length(.) - 1), .)}
all.equal(names(studOrder),names(offsets))

#df$meanES <- df$medianES

# dfG <- summarise(group_by(df,study.labels),
#                  y   = median(ESCI.r,na.rm  = T),
#                  ymin= mean(ESCI.l.r, na.rm = T),
#                  ymax= mean(ESCI.u.r, na.rm = T))
#
# dfG   <- arrange(dfG, y)
#
# dfAg <- aggregate(ESCI.r ~ study.labels, df, mean)

okN <- df$stat.N>=30
okCond <- (df$stat.n1>=15)|(df$stat.n1>=15)

df <- df[okN|okCond, ]

df$sigf     <- "p > .05"
df$sigf[df$test.p.value<.05] <- "p < .05"
df$sigf.f <- factor(df$sigf)

df$sigf.f <- relevel(df$sigf.f, ref = "p > .05")

df$USA <- "non-USA"
df$USA[df$source.Country=="USA"] <- "USA"


df$source.WEIRD.f <- factor(df$source.Weird,
                            levels = c(0,1),
                            labels = c("non-WEIRD","WEIRD"))
df$splitv <- df$source.WEIRD.f

#Get the group levels for the split variable
splits <- unique(df[['splitv']])
splits <- relevel(splits, ref = "WEIRD")


# Calculate and scale group densities
#
# I'm rescaling the density curves so that they all have the same peak height and don't overlap. Edit the mutate() line if you want to have the density polygons be different sizes (e.g., scaled so that they show the relative amount of data in each group).

tblF <- summarise(group_by(df, study.labels, splitv),
                  N = n())
nnames   <- tblF$study.labels[tblF$N==1]
ssplitv  <- tblF$splitv[tblF$N==1]

for(c in seq_along(nnames)){
  df[nrow(df)+1,] <- df[df$study.labels%in%nnames[c]&df$splitv%in%ssplitv[c],]
}

pdat = df %>%
  group_by(study.labels, splitv) %>%
  do(tidy(density(.[['ESCI.r']]))) %>%
  rename(loc = x, dens = y) %>%
  mutate(dens = 0.45 * dens / max(dens)) %>%
  ungroup()

pdat$splitv <- relevel(pdat$splitv, ref = "WEIRD")

# Clonnect the extrema of the split distrubutions
for(sl in levels(pdat$study.labels)){
  ID <- pdat$study.labels%in%sl
  tmp <- pdat[ID,]
  mxID <- which.max(tmp$loc)
  mnID <- which.min(tmp$loc)
  mxLab <- ifelse(tmp$splitv[mxID]%in%splits[1],splits[2],splits[1])
  mnLab <- ifelse(tmp$splitv[mnID]%in%splits[2],splits[1],splits[2])
  pdat <- add_row(pdat,
                  study.labels = rep(sl,2),
                  splitv      =  c(mxLab,mnLab),
                  loc         =  c(max(tmp$loc),min(tmp$loc)),
                  dens        = c(0,0)
  )
  rm(tmp)
}

# Calculate summary statistics in a separate dataframe
#
# If you need more summary statistics, add new variables to the summarise() call here, and add the additional variable names to the gather() call.

sums = df %>%
  group_by(study.labels, splitv, source.Source) %>%
  summarise(sample_loc = first(ESCI.r)) %>%
  ungroup() %>%
  gather(segment, loc_sum, sample_loc)

means = df %>%
  group_by(study.labels, splitv) %>%
  summarise(mean_loc   = mean(ESCI.r, na.rm = TRUE)) %>% #median_loc = median(ESCI.r, na.rm = TRUE),
  ungroup() %>%
  gather(segment, loc_sum, mean_loc)

# Calculate the corresponding points on each group's density curve
#
# To do this, I'm taking the scaled density curves stored in pdat, then feeding them into the approx() function, plus the y-axis locations of the summary statistics (loc_sum), to get the corresponding x-axis values for the summary statistics.

sums = left_join(pdat, sums, by=c('study.labels', 'splitv')) %>%
  group_by(study.labels, splitv, source.Source) %>%
  do(data.frame(loc     = unique(.$loc_sum),
                dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
  ungroup()

means = left_join(pdat, means, by=c('study.labels', 'splitv')) %>%
  group_by(study.labels, splitv) %>%
  do(data.frame(loc     = unique(.$loc_sum),
                dens = approx(.$loc, .$dens, unique(.$loc_sum))[['y']])) %>%
  ungroup()

# Offset the densities and summary statistics
#
# Modify pdat and sums to add offset_dens columns, which are the density curve values offset by the x-axis group. Also, for the groups that are on the left side of the split violins, invert the densities.
pdat <- arrange(pdat, study.labels,splitv,loc)
pdat <- pdat %>%
  mutate(offset_dens = offsets[.[['study.labels']]] + ifelse(.[['splitv']] == splits[2], -dens, dens))


means <- means %>%
  mutate(offset = offsets[.[['study.labels']]],
         offset_dens = offset + ifelse(.[['splitv']] == splits[2], -dens, dens),
         offset_fix  = offset + ifelse(.[['splitv']] == splits[2], -.45, .45))

means$oriES      <- NA
means$oriWEIRD   <- NA
means$oriWEIRD.f <- NA
labs <- levels(means$study.labels)
for(n in seq_along(labs)){
  # means$oriES[means$study.labels%in%as.character(means$study.labels[n])]  <-  oriEffects$orig.ES.r[oriEffects$study.description%in%as.character(means$study.labels[n])] %||% NA
  esID    <- oriEffects$study.description%in%labs[n]
  meansID <- means$study.labels%in%oriEffects$study.description[esID]

  if(any(meansID&means$splitv%in%Slabels[1])){
    means$oriWEIRD[meansID&means$splitv%in%Slabels[1]]  <- Slabels[1]
    means$oriES[meansID&means$splitv%in%Slabels[1]]     <- oriEffects$orig.ES.r[esID&oriEffects$ori.WEIRD==0]%0!0%NA
  }
  if(any(meansID&means$splitv%in%Slabels[2])){
    means$oriWEIRD[meansID&means$splitv%in%Slabels[2]]  <- Slabels[2]
    means$oriES[meansID&means$splitv%in%Slabels[2]]     <- oriEffects$orig.ES.r[esID&oriEffects$ori.sample.weird==1]%0!0%NA
  }
}

#means$offset_fix[means$oriWEIRD%in%"Mixed"] <- means$offset_fix[means$oriWEIRD%in%"Mixed"]+c(.225,-.225)

sums = sums %>%
  mutate(offset      = offsets[.[['study.labels']]],
         offset_dens = offset + ifelse(.[['splitv']] == splits[2], -dens, dens),
         offset_fix  = offset + ifelse(.[['splitv']] == splits[2], -.2, .2))



df$ESCI.N.bin <- cut(df$ESCI.N.total,c(0,80,200,900))
sums$offset_densN <- NA
for(smpl in unique(df$source.Source)){
  tblN <- df[df$source.Source%in%smpl,c('study.labels','ESCI.N.bin')]
  for(l in unique(tblN$study.labels)){
    sums$offset_densN[sums$source.Source%in%smpl&sums$study.labels%in%l] <- tblN$ESCI.N.bin[tblN$study.labels == l]
  }
}

sums$tsize   <- sums$offset + ifelse(sums$splitv == splits[2],-.2, 2)
sums$relSize <- sums$offset + (ifelse(sums$splitv == splits[2],-1,1) * (ifelse(as.numeric(sums$offset_densN)==1, 1/6, ifelse(as.numeric(sums$offset_densN)==2,2/6,3/6))))

# Colour blind safe
myCols <- brewer_pal(palette="RdYlBu")(11)
#myCols <- c("#d73027","#4575b4","#5aae61")


#Colorblindsafe colors
cwhite = "#f7f7f7"
ccream = "#2166ac"
cblank = "#d1e5f0"
corange = "#f4a582"
cblue  = myCols[11]  #"#2166ac"
cblueL = myCols[10]  #"#d1e5f0"
cred   = myCols[1] #"#d6604d"
credL  = myCols[2]  #"#f7f7f7"
cpurp  = "#b2abd2"

mypalette <- c(credL,cblueL)

outdir <- "/Users/Fred/Dropbox/Manylabs2/Figures"

cols <- c("non-WEIRD" = credL, "WEIRD" = cblueL, "Mixed"="#5aae61")

pdat$study.labels <- ordered(as.character(pdat$study.labels), levels = names(studOrder[order(studOrder)]))

means <- means %>%
  mutate( offset_fix  = offset + ifelse(.[['splitv']] == splits[2], -.2, .2))

# means$oriWEIRD.n[means$oriWEIRD%in%"WEIRD"] <- 21
# means$oriWEIRD.n[means$oriWEIRD%in%"non-WEIRD"] <- 22
# means$oriWEIRD.n[means$oriWEIRD%in%"."] <- 25

Sbreaks <- c(25,24,23,21)
Slabels <- c("non-WEIRD","WEIRD","Mixed",".")
OriShapes <- list()
means$oriWEIRD.n <- NA
for(s in seq_along(Sbreaks)){
  means$oriWEIRD.n[means$oriWEIRD%in%Slabels[s]] <- Sbreaks[s]
  OriShapes[[s]] <- Sbreaks[s]
}
means$oriWEIRD.n <- factor(means$oriWEIRD.n,Sbreaks,Sbreaks)
names(OriShapes) <- paste(Sbreaks)
OriShapes<-unlist(OriShapes)

# pdat$splitv <- relevel(pdat$splitv,ref="non-WEIRD")
# sums$splitv <- relevel(sums$splitv,ref="non-WEIRD")
# means$splitv<- relevel(means$splitv,ref="non-WEIRD")

pdat <- na.omit(pdat)

for(s in unique(pdat$study.labels)){
  IDs<-pdat$study.labels%in%s

  locmn <- min(pdat$loc[IDs])
  locmx <- max(pdat$loc[IDs])

  IDminSV1 <- which.min(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[1]])
  IDminSV2 <- which.min(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[2]])
  IDmaxSV1 <- which.max(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[1]])
  IDmaxSV2 <- which.max(pdat$loc[IDs&pdat$splitv%in%levels(pdat$splitv)[2]])

  # IDmin <- which.min(c(pdat$loc[IDs][IDminSV1],pdat$loc[IDs][IDminSV2]))
  # IDmax <- which.min(c(pdat$loc[IDs][IDmaxSV1],pdat$loc[IDs][IDmaxSV2]))

  pdat$loc[IDs][IDminSV1]         <- pdat$loc[IDs][IDminSV2]         <- locmn
  pdat$offset_dens[IDs][IDminSV1] <- pdat$offset_dens[IDs][IDminSV2] <- offsets[names(offsets)%in%s]

  pdat$loc[IDs][IDmaxSV1]         <- pdat$loc[IDs][IDmaxSV2]         <- locmx
  pdat$offset_dens[IDs][IDmaxSV1] <- pdat$offset_dens[IDs][IDmaxSV2] <- offsets[names(offsets)%in%s]

}



g3 <- ggplot(pdat, aes(offset_dens, loc, group = interaction(pdat[['study.labels']], pdat[['splitv']])), colour = splitv) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_path(size=1, alpha = .5, color = "grey70")+
  geom_path(aes(colour=splitv)) +
  geom_segment(data=sums, aes(x = offset,
                              y = loc,
                              xend = offset_fix,
                              yend = loc,
                              colour = splitv),
               inherit.aes=FALSE, alpha=.7, size= .2) +
  geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc, colour =  splitv), inherit.aes=FALSE, size=.8, alpha=.8) +
  geom_point(data=means, aes(x = offset_fix, y = oriES, shape=oriWEIRD.n, fill=splitv), colour = "black", inherit.aes = FALSE, size=1.2, alpha=.8) +
  scale_colour_manual('Sample', values = cols) +
  scale_x_continuous(name = '', breaks = unname(offsets), labels = names(offsets), limits = c(-0.5,27.5)) +
  scale_fill_manual('',values = cols, guide=FALSE) +
  scale_shape_manual('Original Effect Size',values=OriShapes, breaks = Sbreaks,labels = c("","","","")) +
  guides(colour = guide_legend(order = 1, ncol=1,title.vjust=.81,
                               title.theme = element_text(size = 9,angle=0,face="bold")),
         shape = guide_legend(ncol =1,title.position = "right",title.vjust=.81,
                              title.theme = element_text(size = 9,angle=0,face="bold"))) +
  ylim(c(-1,1)) +
  ylab('Effect Size r') +  theme_minimal() +
  theme(legend.position = "top",
        legend.text = element_text(size=rel(.8)),
        legend.box.background  =element_blank(),
        legend.box.just = "left",
        legend.spacing.x = unit(1,"lines"),
        legend.margin = margin(0,0,0,0),
        panel.grid.major.y =element_line(colour="grey70"),
        panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank()) +
  coord_flip()
g3


png(filename = paste0(outdir,"/ML2_SplitViolin_GLOBALsort_Ori_altLegend.png"),width=2400,height=2177, res=250)
g3
dev.off()
g3


# #Plot
# g2 <- ggplot(pdat, aes(offset_dens, loc, fill = splitv, group = interaction(pdat[['labels']], pdat[['splitv']]))) +
#   geom_hline(yintercept = 0, colour = "ivory4") +
#   geom_polygon() +
#   # geom_segment(data=sums, aes(x = offset, y = loc,
#   #                             xend = tsize, yend = loc), inherit.aes=FALSE, alpha=1, size=.2, colour = "white") +
#   geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc, colour = splitv), inherit.aes=FALSE, size=.8, alpha=1) +
#   geom_point(data=dfSumS, aes(x=labloc, y = ES, shape = EStype.f), inherit.aes=FALSE, alpha=1, size=5, colour = "#5aae61") +
#   scale_x_continuous(name = '', breaks = unname(offsets), labels = names(offsets)) +
#   scale_colour_manual('ES Distribution',values = c("#d1e5f0","#f4a582")) +
#   scale_fill_manual('ES Distribution',values = c(cred,cblue)) +
#   scale_shape_manual('',values = c(73,19,17)) +
#   ylab('Effect Size r') +  theme_minimal() +
#   theme(legend.position = "top",
#         legend.background  =element_rect(),
#         panel.grid.major.y =element_line(colour="grey80"),
#         panel.grid.major.x =element_blank(),
#         panel.grid.minor.x =element_blank()) +
#   coord_flip()
# g2
#
#
# ggsave(filename = paste0(outdir,"/ML2_SplitViolin_Ori_Mean_130417.pdf"),
#        plot = g2,
#        scale = 3,
#        width = wd,
#        height = hg,
#        units = "cm"
# )
#
#
# g3 <- ggplot(pdat, aes(offset_dens, loc, fill = splitv, group = interaction(pdat[['labels']], pdat[['splitv']]))) +
#   geom_hline(yintercept = 0, colour = "ivory4") +
#   geom_polygon() +
#   geom_segment(data=sums, aes(x = offset, y = loc,
#                               xend = offset_fix, yend = loc), inherit.aes=FALSE, alpha=1, size=.2, colour = "white") +
#   geom_segment(data=means, aes(x = offset, y = loc, xend = offset_dens, yend = loc, colour = splitv), inherit.aes=FALSE, size=.8, alpha=1) +
#   #geom_segment(data=dfSumS, aes(x = labloc+.4, y = ES, xend = labloc-.4, yend = ES, colour=EStype.f), inherit.aes=FALSE, size = .8) +
#   geom_point(data=dfSumS, aes(x=labloc, y = ES, shape = EStype.f), inherit.aes=FALSE, alpha=1, size=5, colour = "#5aae61") +
#   scale_x_continuous(name = '', breaks = unname(offsets), labels = names(offsets)) +
#   scale_colour_manual('ES Distribution',values = c("#d1e5f0","#f4a582")) +
#   scale_fill_manual('ES Distribution',values = c(cred,cblue)) +
#   scale_shape_manual('',values = c(73,19,17)) +
#   ylab('Effect Size r') +  theme_minimal() +
#   theme(legend.position = "top",
#         legend.background  =element_rect(),
#         panel.grid.major.y =element_line(colour="grey80"),
#         panel.grid.major.x =element_blank(),
#         panel.grid.minor.x =element_blank()) +
#   coord_flip()
# g3
#
# ggsave(filename = paste0(outdir,"/","ML2_SplitViolin_Samples_Fixed_Ori_Mean_130417.pdf"),
#        plot = g3,
#        scale = 3,
#        width = wd,
#        height = hg,
#        units = "cm"
# )
