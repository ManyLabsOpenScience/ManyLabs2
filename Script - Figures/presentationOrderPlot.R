# PRESENTATION ORDER PLOT - MANYLABS 2 -
# corresponding coder: (Fred Hasselman)[https://osf.io/ujgs6/]

# SETUP -----------------------------------------------------------------------------------------------------------

# devtools::source_url('https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/C-3PR_ASCII.R')
# init()

source('~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manylabRs/pkg/R/C-3PR_ASCII.R')
init()

# GENERATE DATA -------------------------------------------------------------------------------------------------------
dir.out <- "~/Dropbox/Manylabs2/Figures/"
dir.in <- "~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/"

# # RUN THIS TO ANALYSE THE DATA AGGREGATED ON PRESENTATION ORDER [or load the pre-run datasets below]

ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
ML2.key <- ML2.key[ML2.key$study.name!="",]
studies <- ML2.key$unique.id[ML2.key$study.figure2.include==1]
#
# # This is the file that contains the data used for the splitviolin plot
# outlist   <- rio::import(paste0(dir.in,"Data_Figure.rds"))
# outlist$study.labels <- ""
#
# for(s in unique(ML2.key$study.id)){
#   outlist$study.labels[outlist$study.id%in%s] <- unique(ML2.key$study.description[ML2.key$study.id%in%s])
# }
# outlist$labels <- factor(as.character(outlist$analysis.name))
# outlist <- outlist %>% group_by(labels) %>% mutate(meanES   = mean(ESCI.r, na.rm=TRUE),
#                                                    sdES     = sd(ESCI.r, na.rm=TRUE),
#                                                    medianES = median(ESCI.r, na.rm=TRUE),
#                                                    madES    = mad(ESCI.r, na.rm=TRUE),
#                                                    meanES_d = mean(ESCI.d, na.rm = TRUE),
#                                                    sdES_d   = sd(ESCI.d, na.rm=TRUE),
#                                                    medianES_d = median(ESCI.d, na.rm = TRUE),
#                                                    madES    = mad(ESCI.d, na.rm=TRUE))

# tp=4
# startLog <- function(){
#   con <- file("~/Dropbox/Manylabs2/TestOutput/RESULTS.LOG/ML2log_ES_by_order.txt")
#   # Set the sink to 'con'
#   sink(con, append=TRUE)
#   sink(con, append=TRUE, type="message")
#   return(con)
# }
#
# restore <- function(con){
#   # Restore output to console
#   sink()
#   sink(type="message")
#
#   close(con)
#   closeAllConnections()
# }
#
# con <- startLog()
# saveRDSfile  <- TRUE
# saveCSVfile  <- TRUE
# # This will echo all input and not truncate 150+ character lines...
# tryCatch(testScript(studies,tp,subset = "all",saveCSVfile,saveRDSfile),finally = restore(con))
#
#
# # Get the ORDER AGGREGATED DATA
#
# # This file was saved by the code above and is a list object with analysis results for each order
# dfout  <-readRDS(paste0(dir.in,"Data_Figure_StudyOrder_all.rds"))
#
# # Get the list into a data frame
#  df <- ldply(dfout$aggregated)
#
#
#
#  length(dfout$aggregated)
#  df <- df[!is.na(df$ESCI.r),]
#
# # Add some varaibles
# df$study.labels <- ""
# df$meanES <- df$sdES <- df$medianES <- df$madES <- df$meanES_d <- df$sdES_d <- df$medianES_d <- df$madES_d <- NA
#
# for(s in unique(df$study.id)){
#   df$study.labels[df$study.id%in%s] <- unique(ML2.key$study.description[ML2.key$study.id%in%s])
#   df$meanES[df$study.id%in%s]       <- outlist$meanES[outlist$study.id%in%s][1]
#   df$sdES[df$study.id%in%s]         <- outlist$sdES[outlist$study.id%in%s][1]
#   df$medianESS[df$study.id%in%s]    <- outlist$medianES[outlist$study.id%in%s][1]
#   df$madESS[df$study.id%in%s]       <- outlist$madES[outlist$study.id%in%s][1]
#   df$meanES_dS[df$study.id%in%s]    <- outlist$meanES_d[outlist$study.id%in%s][1]
#   df$sdES_dS[df$study.id%in%s]      <- outlist$sdES_d[outlist$study.id%in%s][1]
#   df$medianES_dS[df$study.id%in%s]  <- outlist$medianES_d[outlist$study.id%in%s][1]
#   df$madESS[df$study.id%in%s]       <- outlist$madES[outlist$study.id%in%s][1]
# }
#
# df <- dplyr::arrange(df, study.labels, study.source, meanES)
#
# # SAVE as Figure dataset
# saveRDS(df,paste0(dir.in,"Data_Figure_StudyOrder.rds"))
# rio::export(df,paste0(dir.in,"Data_Figure_StudyOrder.xlsx"))
# rio::export(df,paste0(dir.in,"Data_Figure_StudyOrder.csv"))
#

# LOAD DATA instead of generating the data load the prepared datasets---------------------------------------------------
#df <- import(paste0(dir.in,"Data_Figure_StudyOrder.rds"))


# CREATE DATASET TO PLOT -----------------------------------------------------------------------------------------------

# Gap between offsets
gap = .5

df$loc.m <- df$meanES
df$study.labels <- stats::reorder(df$study.labels, df$loc.m, order=TRUE)
studOrder       <- sort(attributes(df$study.labels)$scores)
offsets         <- names(studOrder) %>% {setNames(seq(0,gap*(length(.) -1),by=gap), .)}
all.equal(names(studOrder),names(offsets))

# OLD: df <- df %>% mutate(offset_loc = offsets[.[['study.labels']]])
df$offset_loc <- NA
for(s in unique(df$study.labels)){
  df$offset_loc[df$study.labels%in%s] <- offsets[names(offsets)%in%s]
  }

# # Add Global effects of the regular analyses
df$loc.glob <- NA
for(s in unique(df$study.labels)){
  df$loc.glob[df$study.labels%in%s] <- attributes(df$study.labels)$scores[names(attributes(df$study.labels)$scores)%in%s]
}


for(s in unique(df$study.labels)){

M <- mean(df$ESCI.r[df$study.labels%in%s],na.rm=T) #%0!0%0  #mean(df$ESCI.r[df$study.labels%in%s], na.rm=T)
# R <- c(df$ESCI.l.r[df$study.labels%in%s&(df$study.source==1)],df$ESCI.u.r[df$study.labels%in%s&(df$study.source==1)])%0!0%c(0,0)

R.l <- min(df$ESCI.l.r[df$study.labels%in%s], na.rm = TRUE) #%0!0%-1
R.u <- max(df$ESCI.u.r[df$study.labels%in%s], na.rm = TRUE) #%0!0%1

#M <- rescale(df$ESCI.r[df$study.labels%in%s&(df$study.source==1)]%0!0%0,to = rr, from = R)

# df$dens[df$study.labels%in%s]   <- rescale_mid(df$ESCI.r[df$study.labels%in%s],   to = rr, from = c(R.l,R.u), mid = M)
# df$dens.u[df$study.labels%in%s] <- rescale_mid(df$ESCI.u.r[df$study.labels%in%s], to = rr, from = c(R.l,R.u), mid = M)
# df$dens.l[df$study.labels%in%s] <- rescale_mid(df$ESCI.l.r[df$study.labels%in%s], to = rr, from = c(R.l,R.u), mid = M)

# df$dens[df$study.labels%in%s]   <- rescale(df$ESCI.r[df$study.labels%in%s],     to = rr, from = c(R.l,R.u))
# df$dens.u[df$study.labels%in%s] <- rescale(df$ESCI.u.r[df$study.labels%in%s], to = rr, from = c(R.l,R.u))
# df$dens.l[df$study.labels%in%s] <- rescale(df$ESCI.l.r[df$study.labels%in%s], to = rr, from = c(R.l,R.u))
# df$dens[df$study.labels%in%s]   <- 0
# df$dens.m[df$study.labels%in%s] <- M
#
#   df$diffc <- ""
#   ID <- between(df$ESCI.r, df$ESCI.l.r[df$study.labels%in%s&(df$study.source==1)][1], df$ESCI.u.r[df$study.labels%in%s&(df$study.source==1)][1])
#   df$diffc[ID]  <- "Inside"
#   df$diffc[!ID] <- "Outside"
#scale_colour_manual('95%CI of Mean', values = rev(mypalette)) +

df$offset_loc <- NA
for(s in unique(df$study.labels)){
  df$offset_loc[df$study.labels%in%s] <- offsets[names(offsets)%in%s]
}


df$loc.glob[df$study.labels%in%s] <- attributes(df$study.labels)$scores[names(attributes(df$study.labels)$scores)%in%s]
}

 df$loc    <- as.numeric(df$loc.m)
 df$loc.m  <- format(round(df$loc,2),nsmall=2)
 df$loc.gm <- format(round(df$loc.glob,2),nsmall=2)


# MEAN effect to display
 df$loc.r <- df$offset_loc  +  (df$ESCI.r - df$meanES)
 df$loc.u <- df$loc.r + (df$ESCI.u.r - df$ESCI.r)
 df$loc.l <- df$loc.r + (df$ESCI.l.r - df$ESCI.r)

 dflab <- summarise(group_by(df,study.labels),
                    y = unique(offset_loc),
                    lab = unique(loc.m))

# 'is in CI' variable
df$inCI <-laply(seq_along(df$loc), function(r) between(df$loc[r],df$ESCI.l.r[r],df$ESCI.u.r[r]))
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

g2<-ggplot(df,aes(x= study.source, y = offset_loc, group = study.labels)) +
  geom_hline(aes(yintercept = offset_loc), colour="grey70") +
  geom_errorbar(aes(ymin=loc.l, ymax=loc.u),size=.3, width=.2) +
  geom_point(aes(y = loc.r, fill = inCI.f, colour= inCI.f), size=1, pch=21) +
  geom_label(data=dflab,aes(x=16, y=y, label=lab), size=2.5) +
  geom_text(x=15.8,y=max(df$offset_loc)+ gap,label="Mean r",parse = FALSE) +
  scale_y_continuous("", breaks = unname(offsets), labels = names(offsets)) +
  scale_x_discrete("Presentation Order", breaks=1:15, labels=c(paste(1:15)), limits= c(1:20)) +
  scale_colour_manual('Mean Order r in 95%CI of \n Presentation Order?', values = rev(mypalette)) +
  scale_fill_manual('Mean Order r in 95%CI of \n Presentation Order?', values = rev(mypalette)) +
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

png(filename = paste0(dir.out,"/ML2_OrderPlot_labels_bars_Mean_r_altLegend.png"),width=2400,height=2177, res=250)
g2
dev.off()



# Use global

df$loc.r <- df$offset_loc  +  (df$ESCI.r - df$loc.glob)
df$loc.u <- df$loc.r + (df$ESCI.u.r - df$ESCI.r)
df$loc.l <- df$loc.r + (df$ESCI.l.r - df$ESCI.r)

dflab <- summarise(group_by(df,study.labels),
                   y = unique(offset_loc),
                   lab = unique(loc.gm))
df$inCI <-laply(seq_along(df$loc.glob), function(r) between(df$loc.glob[r],df$ESCI.l.r[r],df$ESCI.u.r[r]))
df$inCI.f <- factor(as.numeric(df$inCI), levels=c(0,1), labels = c("No","Yes"))


 g3<-ggplot(df,aes(x= study.source, y = offset_loc, group = study.labels)) +
   geom_hline(aes(yintercept = offset_loc), colour="grey70") +
   geom_errorbar(aes(ymin=loc.l, ymax=loc.u),size=.3, width=.2) +
   geom_point(aes(y = loc.r, fill = inCI.f, colour= inCI.f), size=1, pch=21) +
   geom_label(data=dflab,aes(x=16, y=y, label=lab), size=2.5) +
   geom_text(x=15.8,y=max(df$offset_loc)+ gap,label="Global r",parse = FALSE) +
   scale_y_continuous("", breaks = unname(offsets), labels = names(offsets)) +
   scale_x_discrete("Presentation Order", breaks=1:15, labels=paste(1:15), limits= c(1:20)) +
   scale_colour_manual('Global r in 95%CI of \n Presentation Order?', values = rev(mypalette)) +
   scale_fill_manual('Global r in 95%CI of \n Presentation Order?', values = rev(mypalette)) +
   theme_bw()+
   guides(fill=guide_legend(override.aes = list(size=3),
                            title.theme = element_text(size=10, angle=0,face="bold")),
          colour=guide_legend(override.aes = list(size=3),
                              title.theme = element_text(size=10, angle=0,face="bold"))) +
   theme(panel.grid.major.y =element_blank(), #element_line(colour="grey80"),
         panel.grid.minor.y =element_blank(),
         panel.grid.major.x =element_blank(),
         panel.grid.minor.x =element_blank(),
         legend.position = "top")

g3
 png(filename = paste0(dir.out,"/ML2_OrderPlot_labels_bars_Global_r_altLegend.png"),width=2400,height=2177, res=250)
 g3
 dev.off()
