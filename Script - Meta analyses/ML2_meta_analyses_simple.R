## META ANALYSES - MANYLABS 2 -
#
# Just load the data and plot the Figure!
#
# corresponding coder: Fred Hasselman (https://osf.io/ujgs6/)

# SETUP ----
library(rio)
library(metafor)
require(reshape2)
library(plyr)
library(tidyverse)
require(lattice)
library(gplots)

# NOTE: Don't run init() if you do not want to load, and possiblt install packages we need to run this script!!
library(devtools)
devtools::source_url("https://raw.githubusercontent.com/ManyLabsOpenScience/manylabRs/master/R/manylabRs_SOURCE.R")
init()

devtools::source_url("https://raw.githubusercontent.com/FredHasselman/invctr/master/R/invictor.R")


# Provide root for OSFdata folder
dir.in <- "~/Documents/GitHub/ManyLabs2/"
# Output directory
dir.out <- getwd()

# Get data ----
ML2.key     <- get.GoogleSheet(data='ML2masteRkey')$df

outlist1 <- rio::import(file.path(dir.in,"OSFdata","!!RawData","Data_Figure_NOweird.csv"))

outlist1$online <- NA
outlist1$online[outlist1$source.Setting%in%c("In a classroom","In a lab")] <- "lab"
outlist1$online[outlist1$source.Setting%in%c("Online (at home)")] <- "online"
outlist1$online.f <- factor(outlist1$online)
outlist1$source.country.f <- factor(outlist1$source.Country)
outlist1$source.WEIRD.f <- factor(outlist1$source.Weird,
                            levels = c(0,1),
                            labels = c("less WEIRD","WEIRD"))

rmaR0 <- rmaR1 <- rmaR2 <- dfol <- list()

cnt=0
for(an in unique(as.character(outlist1$analysis.name))){

  ID <- as.character(outlist1$analysis.name)%in%an
  d  <- outlist1[ID,]

  if((nrow(d)>0) & !grepl("Graham|Inbar|Schwarz",an)){
    cnt<-cnt+1

    r   <- sum(ID)
    tmp <- data.frame(
      analysis.name = c(rep(an,r)),
      study.source  = as.character(d$study.source),
      study.slate   = c(d$study.slate),
      source.WEIRD.f  = d$source.Weird,
      source.online.f = d$online.f,
      source.country.f = d$source.country.f,
      stat.N        = c(d$stat.N),
      test.p.value  = c(d$ESCI.pval.r),
      test.l.r      = c(d$ESCI.l.r),
      test.r        = c(d$ESCI.r),
      test.h.r      = c(d$ESCI.u.r),
      test.vi.r    = c(d$ESCI.var.r),
      test.l.d      = c(d$ESCI.l.d),
      test.d        = c(d$ESCI.d),
      test.h.d      = c(d$ESCI.u.d),
      test.vi.d    = c(d$ESCI.var.d)
    )

    id0 <- tmp$test.vi.r <= 1e-8
    if(sum(id0,na.rm = T)>0){
      tmp$test.vi.r[id0] <- tmp$test.vi.r[id0] + 1e-8
    }

    tmp <- tmp[!is.na(tmp$test.vi.r),]

    dfol[[cnt]] <- tmp

    metR0 <- rma(yi=test.r,
                 vi=test.vi.r,
                 slab = analysis.name,
                 data=tmp)
    
    rmaR0[[cnt]] <- list(model = metR0, CI= confint(metR0))

    tmp2 <- tmp[!is.na(tmp$source.online),]

    metR1 <- rma(yi=test.r,
                 vi=test.vi.r,
                 mods = ~ source.online.f,
                 slab = analysis.name,
                 data=tmp2)

    rmaR1[[cnt]] <- list(model = metR1, CI= confint(metR1))

    metR2 <- rma(yi=test.r,
                 vi=test.vi.r,
                 mods = ~ source.WEIRD.f,
                 slab = analysis.name,
                 data=tmp)

    rmaR2[[cnt]] <- list(model = metR2, CI= confint(metR2))
  }
}


get.MetaResults <- function(metaObj){
  out <- ldply(metaObj,function(d){
    data.frame(
      analysis.name = d$model$slab[1],
      QE = d$model$QE%00%NA,
      pval.QE = d$model$QEp%00%NA,
      QM = d$model$QM%00%NA,
      pval.QM = d$model$QMp%00%NA,
      df   = (d$model$k-d$model$m)%00%NA,
      sig2 = sum(d$model$sigma2, na.rm = TRUE)%00%NA,
      tau2 = d$model$tau2%00%NA,
      tau2.l = d$CI$random[1,2]%00%NA,
      tau2.u = d$CI$random[1,3]%00%NA,
      tau   = d$CI$random[2,1]%00%NA,
      tau.l = d$CI$random[2,2]%00%NA,
      tau.u = d$CI$random[2,3]%00%NA,
      I2     = d$model$I2%00%NA,
      I2pct   = d$CI$random[3,1]%00%NA,
      I2pct.l = d$CI$random[3,2]%00%NA,
      I2pct.u = d$CI$random[3,3]%00%NA,
      H2   = d$CI$random[4,1]%00%NA,
      H2.l = d$CI$random[4,2]%00%NA,
      H2.u = d$CI$random[4,3]%00%NA,
      # rho  = d$model$rho%00%NA,
      # ICC  = (d$model$sigma2[1]/sum(d$model$sigma2, na.rm = TRUE))%00%0,
      console_out = paste(capture.output(print(summary(d))),collapse="\n"))
  })
  return(out)
}


mods1 <-  list(nomod_uni = rmaR0,
               online_uni = rmaR1,
               weird_uni = rmaR2)

outMeta_uni <- ldply(mods1,get.MetaResults)

# Write output ----
export(outMeta_uni,file.path(dir.out,"meta_analysis_wide.xlsx"))

pdf(file.path(dir.out,"FunnelPerAnalysis.pdf"), paper="a4r")

for(m in seq_along(dfol)){

  if(!is.null(dfol[[m]])){

    gplots::textplot(capture.output(summary(rmaR0[[m]]$model),rmaR0[[m]]$CI))
  title(paste("no moderator:",dfol[[m]]$analysis.name[1], "\nI2:",round(rmaR0[[m]]$model$I2,10)))


    funnel(rmaR0[[m]]$model, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), pch = (rmaR0[[m]]$model$X[,1]), refline=0, main = paste(dfol[[m]]$analysis.name[1]))


    gplots::textplot(capture.output(summary(rmaR1[[m]]$model),rmaR1[[m]]$CI))
    title(paste("online moderator:",dfol[[m]]$analysis.name[1], "\nI2:",round(rmaR1[[m]]$model$I2,10)))


    funnel(rmaR1[[m]]$model, level=c(90, 95, 99), shade=c("white", "gray", "darkgray"), pch = (rmaR1[[m]]$model$X[,1]), refline=0, main = paste(dfol[[m]]$analysis.name[1]))

    gplots::textplot(capture.output(summary(rmaR2[[m]]$model),rmaR2[[m]]$CI))
    title(paste("weird moderator:",dfol[[m]]$analysis.name[1], "\nI2:",round(rmaR1[[m]]$model$I2,10)))

    funnel(rmaR2[[m]]$model,level=c(90, 95, 99), shade=c("white", "gray", "darkgray"),pch = (rmaR2[[m]]$model$X[,1]), refline=0, main = paste(dfol[[m]]$analysis.name[1]))
}

}
dev.off()



