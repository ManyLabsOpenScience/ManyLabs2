# PRESENTATION ORDER PLOT - MANYLABS 2 -
# corresponding coder: (Fred Hasselman)[https://osf.io/ujgs6/]

# SETUP -----------------------------------------------------------------------------------------------------------

# devtools::source_url('https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/C-3PR_ASCII.R')
# init()

source('/Users/Fred/Documents/GitHub/manylabRs/manylabRs/R/C-3PR_ASCII 2.R')
init()

# GENERATE DATA -------------------------------------------------------------------------------------------------------
dir.out <- "~/Dropbox/Manylabs2/Figures/"
dir.in <- "~/Dropbox/Manylabs2/TestOutput/RESULTS_RDS/"

# # RUN THIS TO ANALYSE THE DATA AGGREGATED ON PRESENTATION ORDER [or load the pre-run datasets below]

ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key <- ML2.key[ML2.key$study.name!="",]
studies <- ML2.key$unique.id[ML2.key$study.figure2.include==1]

# # This is the file that contains the data used for the splitviolin plot
outlist   <- rio::import(paste0(dir.in,"Data_Figure_NEW.csv"))

df <- outlist
df$labels <- factor(as.character(df$analysis.name))
df$study.labels <- factor(as.character(df$study.description))
df <- df %>% group_by(labels) %>%
  mutate(meanES   = mean(ESCI.r, na.rm=TRUE),
         medianES = median(ESCI.r, na.rm=TRUE),
         oriES = first(ori.ESCI.r)) %>%
  ungroup()



# Arrange by MEAN
df <- df[!is.na(df$ESCI.r),]
# df <- add_row(df,study.labels="Cohen's q", meanES = 1, cohensQ=1)
# df <- add_row(df,study.labels="Effect size r", meanES = -1, cohensQ=0)
df <- dplyr::arrange(df, labels, cohensQ, GlobalES)
df$study.labels <- factor(df$study.labels,ordered = TRUE)
df$study.labels <- reorder(df$study.labels, df$cohensQ+df$meanES, order=TRUE)
#df$cohensQ[df$study.labels%in%"Disgust & Homophobia (Inbar et al., 2009)"] <- -100
df$study.labels <- reorder(df$study.labels, df$cohensQ+df$meanES, order=TRUE)
studOrder <- attributes(df$study.labels)$scores
offsets   <- names(studOrder) %>% {setNames(0:(length(.) - 1), .)}
all.equal(names(studOrder),names(offsets))
#df$cohensQ[df$study.labels%in%"Disgust & Homophobia (Inbar et al., 2009)"] <- 1

Qlabs <- paste(unique(df$study.labels[df$cohensQ==1]))
ID <- df$study.labels%in%Qlabs
df$ESCI.r[ID] <- df$ESCI.cohensQ[ID]
df$ori.ESCI.r[ID] <- df$ori.ESCI.cohensQ[ID]

okN <- df$stat.N>=30
okCond <- (df$stat.n1>=15)|(df$stat.n2>=15)

df <- df[okN|okCond, ]

df$sigf     <- "p > .05"
df$sigf[df$test.p.value<.05] <- "p < .05"
df$sigf.f <- factor(df$sigf)

df$sigf.f <- relevel(df$sigf.f, ref = "p > .05")

# df$USA <- "non-USA"
# df$USA[df$source.Country=="USA"] <- "USA"

df$source.WEIRD.f <- factor(df$source.Weird,
                            levels = c(0,1),
                            labels = c("non-WEIRD","WEIRD"))
df$splitv <- df$source.WEIRD.f
df$splitv <- relevel(df$splitv, ref = "non-WEIRD")

#Get the group levels for the split variable
splits <- factor(c("non-WEIRD","WEIRD"))
splits <- relevel(splits, ref = "non-WEIRD")

# # Get the ORDER AGGREGATED DATA
#
# # This file was saved by the code above and is a list object with analysis results for each order
dir.in <- "~/Dropbox/Manylabs2/TestOutput/RESULTS_RDS/"

dfout  <-readRDS("~/Dropbox/Manylabs2/TestOutput/RESULTS_RDS/by_order/Data_Figure_StudyOrder_by_order.rds")
length(dfout$aggregated)

# # Get the list into a data frame
dfo <- ldply(dfout$aggregated)
dfo <- dfo[(!is.na(dfo$ESCI.r)|!is.na(dfo$ESCI.cohensQ)),]

# Add some varaibles
dfo$study.labels <- ""
dfo$meanES <- dfo$medianES <- dfo$GlobalES <- dfo$cohensQ <- NA

for(s in unique(dfo$study.id)){
  dfo$study.labels[dfo$study.id%in%s] <- unique(ML2.key$study.description[ML2.key$study.id%in%s])
  dfo$meanES[dfo$study.id%in%s]       <- df$meanES[df$study.id%in%s][1]
  dfo$cohensQ[dfo$study.id%in%s]   <- df$cohensQ[df$study.id%in%s][1]
  # dfo$sdES[dfo$study.id%in%s]         <- df$sdES[df$study.id%in%s][1]
  dfo$medianESS[dfo$study.id%in%s]    <- df$medianES[df$study.id%in%s][1]
  dfo$GlobalES[dfo$study.id%in%s]  <- df$GlobalES.r[df$study.id%in%s][1]
  #dfo$madESS[dfo$study.id%in%s]       <- df$madES[df$study.id%in%s][1]
  # dfo$meanES_dS[dfo$study.id%in%s]    <- df$meanES_d[df$study.id%in%s][1]
  # dfo$sdES_dS[dfo$study.id%in%s]      <- df$sdES_d[df$study.id%in%s][1]
  # dfo$medianES_dS[dfo$study.id%in%s]  <- df$medianES_d[df$study.id%in%s][1]
  # dfo$madESS[dfo$study.id%in%s]       <- df$madES[df$study.id%in%s][1]
}


df <- dfo
#df <- df[!is.na(df$ESCI.r),]
# df <- add_row(df,study.labels="Cohen's q", meanES = 1, cohensQ=1)
# df <- add_row(df,study.labels="Effect size r", meanES = -1, cohensQ=0)
df <- dplyr::arrange(df, analysis.name, cohensQ, GlobalES)
df$study.labels <- factor(df$study.labels,ordered = TRUE)
df$study.labels <- reorder(df$study.labels, df$cohensQ+df$GlobalES, order=TRUE)
#df$cohensQ[df$study.labels%in%"Disgust & Homophobia (Inbar et al., 2009)"] <- -100
df$study.labels <- reorder(df$study.labels, df$cohensQ+df$GlobalES, order=TRUE)
studOrder <- attributes(df$study.labels)$scores

gap <- .5
offsets  <- names(studOrder) %>% {setNames(seq(0,gap*(length(.) -1),by=gap), .)}
#offsets  <- names(studOrder) %>% {setNames(0:(length(.) - 1), .)}
all.equal(names(studOrder),names(offsets))
#df$cohensQ[df$study.labels%in%"Disgust & Homophobia (Inbar et al., 2009)"] <- 1

Qlabs <- paste(unique(df$study.labels[df$cohensQ==1]))
ID <- df$study.labels%in%Qlabs
df$ESCI.r[ID] <- df$ESCI.cohensQ[ID]
df$ESCI.u.r[ID] <- df$ESCI.cohensQ.u[ID]
df$ESCI.l.r[ID] <- df$ESCI.cohensQ.l[ID]


# okN <- df$stat.N>=30
# okCond <- (df$stat.n1>=15)|(df$stat.n2>=15)
# df <- df[okN|okCond, ]

df$sigf     <- "p > .05"
df$sigf[df$test.p.value<.05] <- "p < .05"
df$sigf.f <- factor(df$sigf)

df$sigf.f <- relevel(df$sigf.f, ref = "p > .05")

# df$USA <- "non-USA"
# df$USA[df$source.Country=="USA"] <- "USA"

df$source.WEIRD.f <- factor(df$source.Weird,
                            levels = c(0,1),
                            labels = c("non-WEIRD","WEIRD"))
df$splitv <- df$source.WEIRD.f
df$splitv <- relevel(df$splitv, ref = "non-WEIRD")

#Get the group levels for the split variable
splits <- factor(c("non-WEIRD","WEIRD"))
splits <- relevel(splits, ref = "non-WEIRD")

#df <- dplyr::arrange(df, study.labels, study.source, meanES)

# # SAVE as Figure dataset
saveRDS(df,paste0(dir.in,"Data_Figure_StudyOrder.rds"))
rio::export(df,paste0(dir.in,"Data_Figure_StudyOrder.xlsx"))
rio::export(df,paste0(dir.in,"Data_Figure_StudyOrder.csv"))

# LOAD DATA instead of generating the data load the prepared datasets---------------------------------------------------
#df <- import(paste0(dir.in,"ALL_study_by_order_by_order.csv"))

# labels_noQ <-unique(ML2.key$study.analysis[is.na(ML2.key$cohensQ)&ML2.key$study.figure2.include==1])
# df <- df[df$analysis.name%in%labels_noQ,]


# CREATE DATASET TO PLOT -----------------------------------------------------------------------------------------------
df$loc.m <- df$GlobalES

# Gap between offsets
#gap = .5

# df$study.labels <- stats::reorder(df$study.labels, df$loc.m, order=TRUE)
# studOrder       <- sort(attributes(df$study.labels)$scores)
# offsets         <- names(studOrder) %>% {setNames(seq(0,gap*(length(.) -1),by=gap), .)}
# all.equal(names(studOrder),names(offsets))

# OLD: df <- df %>% mutate(offset_loc = offsets[.[['study.labels']]])

df$offset_loc <- NA
for(s in unique(df$study.labels)){
  df$offset_loc[df$study.labels%in%s] <- offsets[names(offsets)%in%s]
}


IDq <- df$study.labels%in%Qlabs[2]
df$offset_loc[IDq] <- df$offset_loc[IDq]+gap
IDq <- df$study.labels%in%Qlabs[1]
df$offset_loc[IDq] <- df$offset_loc[IDq]+gap




# # Add Global effects of the regular analyses
df$loc.glob <- NA
for(s in unique(df$study.labels)){
  df$loc.glob[df$study.labels%in%s] <- attributes(df$study.labels)$scores[names(attributes(df$study.labels)$scores)%in%s]
}

IDq <- df$study.labels%in%Qlabs[2]
df$loc.glob[IDq] <- df$loc.glob[IDq]+gap
IDq <- df$study.labels%in%Qlabs[1]
df$loc.glob[IDq] <- df$loc.glob[IDq]+gap

# MEAN effect to display
df$loc.r <- (df$offset_loc  +  (df$ESCI.r - df$GlobalES)) #* ifelse(df$study.labels%in%Qlabs,1,1.1)
df$loc.u <- df$loc.r + (df$ESCI.u.r - df$ESCI.r) * ifelse(df$study.labels%in%Qlabs,.9,1.1)
df$loc.l <- df$loc.r + (df$ESCI.l.r - df$ESCI.r) * ifelse(df$study.labels%in%Qlabs,.9,1.1)


# Glob effect to display
df$locg.r <- (df$offset_loc  +  (df$ESCI.r - df$loc.glob)) #* ifelse(df$study.labels%in%Qlabs,1,1.1)
df$locg.u <- df$loc.r + (df$ESCI.u.r - df$ESCI.r) * ifelse(df$study.labels%in%Qlabs,.9,1.1)
df$locg.l <- df$loc.r + (df$ESCI.l.r - df$ESCI.r) * ifelse(df$study.labels%in%Qlabs,.9,1.1)

df$loc    <- as.numeric(df$loc.m)
df$loc.m  <- format(round(df$loc,3),nsmall=2)
df$loc.gm <- format(round(df$loc.glob,3),nsmall=2)

# 'is in CI' variable
df$inCI   <- laply(seq_along(df$loc), function(r) between(df$loc[r],df$ESCI.l.r[r],df$ESCI.u.r[r]))
df$inCI.f <- factor(as.numeric(df$inCI), levels=c(0,1), labels = c("No","Yes"))


# Colorblindsafe colors
myCols <- brewer_pal(palette="RdYlBu")(11)
cwhite = "#f7f7f7"
ccream = "#2166ac"
cblank = "#d1e5f0"
corange = "#f4a582"
cblue  = myCols[11]  #"#2166ac"
cblueL = myCols[10]  #"#d1e5f0"
cred   = myCols[1] #"#d6604d"
credL  = myCols[2]  #"#f7f7f7"
cpurp  = "#b2abd2"

mypalette <- c(cblueL,credL)


# pdat$study.labels<-mapvalues(pdat$study.labels, from = c("Assimilation & Contrast (Schwarz et al., 1991)", "Disgust & Homophobia (Inbar et al., 2009)"), to = c("Disgust & Homophobia (Inbar et al., 2009)", "Assimilation & Contrast (Schwarz et al., 1991)"))

locs.m <- df %>% group_by(study.labels) %>% summarise(labs = unique(loc.m))

xbreaks <- c(unname(offsets), 14,14.5)
xlabels <- c(names(offsets)[1:26],"",names(offsets)[c(28,27)],"")
xmeans <- c(as.numeric(locs.m$labs)[1:26],NA,as.numeric(locs.m$labs)[c(28,27)],NA)

# dflab <- summarise(group_by(df,study.labels),
#                    y = unique(offset_loc),
#                    lab = unique(loc.m))


dflab <- data.frame(study.labels = xlabels, y = xbreaks, lab = xmeans)

# G2 ----

g2 <- ggplot(df,aes(x= study.source, y = offset_loc, group = study.labels)) +
  #geom_hline(aes(yintercept = offset_loc), colour="grey70") +
  geom_vline(xintercept = 0, color = "grey70") +
  geom_hline(yintercept = xbreaks[-c(27,30)], color = "grey70") +
  #geom_segment(y = 13, yend=13,x=0,xend=15,  color = "grey70", linetype=2) +
  geom_errorbar(aes(ymin=loc.l, ymax=loc.u),size=.3, width=.2) +
  geom_point(aes(y = loc.r, fill = inCI.f, colour= inCI.f), size=1, pch=21) +
  geom_label(data=dflab,aes(x=16, y=y, label=lab), size=2.5) +
  geom_text(x=15.8,y=14.5,label="Global q",parse = FALSE) +
  geom_text(x=15.8,y=13,label="Global r",parse = FALSE) +
  scale_y_continuous("", breaks = xbreaks, labels = xlabels, expand = c(.03,.03)) +
  scale_x_discrete("Presentation Order", breaks=1:15, labels=c(paste(1:15)), limits= c(1:20)) +
  scale_colour_manual('Global Order ES in 95%CI of \n Presentation Order?', values = rev(mypalette)) +
  scale_fill_manual('Global Order ES in 95%CI of \n Presentation Order?', values = rev(mypalette)) +
  theme_bw()+
  guides(fill=guide_legend(override.aes = list(size=3),
                           title.theme = element_text(size=10, angle=0,face="bold")),
         colour=guide_legend(title.theme = element_text(size=10, angle=0,face="bold"))) +
  theme(panel.grid.major.y =element_blank(),
        panel.grid.minor.y =element_blank(),
        panel.grid.major.x =element_blank(),
        panel.grid.minor.x =element_blank(),
        legend.position = "top")
g2


ggsave(plot = g2,filename = paste0(dir.out,"/ML2_OrderPlot_labels_bars_Global_r_altLegend_",as.Date(now()),".png"),width=10, dpi = 600, units = "in")

# ggsave(plot = g2,filename = paste0(dir.out,"/ML2_OrderPlot_labels_bars_Mean_r_altLegend_24032017.tiff"),width=10, dpi = 600, units = "in")

ggsave(plot = g2,filename = paste0(dir.out,"/ML2_OrderPlot_labels_bars_Global_r_altLegend_",as.Date(now()),".eps"),width=10, dpi = 600, units = "in")

ggsave(plot = g2,filename = paste0(dir.out,"/ML2_OrderPlot_labels_bars_Global_r_altLegend_",as.Date(now()),".pdf"),width=10, dpi = 600, units = "in")

