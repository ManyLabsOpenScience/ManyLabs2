# SPLIT VIOLIN PLOTS - MANYLABS 2 -
# corresponding coder: Fred Hasselman (https://osf.io/ujgs6/)

# require(devtools)
# srcDir <- "https://raw.githubusercontent.com/FredHasselman/manylabRs/master/pkg/R/"
# devtools::source_url(paste0(srcDir,"C-3PR_ASCII.R"))
# init()

source('~/Library/Mobile Documents/com~apple~CloudDocs/GitHub/ManyLabRs/manylabRs/pkg/R/C-3PR_ASCII.R')
init()


ML2.key     <- get.GoogleSheet(data='ML2masteRkey')$df
oriEffects  <- ML2.key[ML2.key$study.figure2.include==1,]   #[nchar(ML2.key$orig.stat.type)>0, c(2:5,22:28,34:36)]
oriEffectsT <- ML2.key[ML2.key$study.table1.include==1,]   #[nchar(ML2.key$orig.stat.type)>0, c(2:5,22:28,34:36)]

dir.in <- "~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/"
outlist.tmp <- readRDS(paste0(dir.in,"ML2_results_primary_all.rds"))
outlist.tmp <- outlist.tmp$aggregated
outlist1 <-ldply(outlist.tmp)

rm(outlist.tmp)
outlist.tmp <- readRDS(paste0(dir.in,"ML2_results_secondary_all.rds"))
outlist.tmp <- outlist.tmp$aggregated
outlist2 <- ldply(outlist.tmp)

outlistAll <- rbind(outlist1[,which(colnames(outlist1)%in%colnames(outlist2))],outlist2[,which(colnames(outlist2)%in%colnames(outlist1))])

rm(outlist.tmp)
outlist.tmp <- readRDS(paste0(dir.in,"ML2_results_global_all.rds"))
outlist.tmp <- outlist.tmp$aggregated
outlistG <- ldply(outlist.tmp)

outlistAll$GlobalES <- NA
outlistAll$splitWEIRD <- NA
for(l in unique(outlistAll$analysis.name)){
  id  <- outlistG$analysis.name%in%l
  id2 <- ML2.key$study.analysis%in%l
  if(any(id)){
    outlistAll$GlobalES.r[outlistAll$analysis.name%in%l]    <- outlistG$ESCI.r[id]
    outlistAll$GlobalESlo.r[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.l.r[id]
    outlistAll$GlobalEShi.r[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.u.r[id]
    outlistAll$GlobalES.d[outlistAll$analysis.name%in%l]    <- outlistG$ESCI.d[id]
    outlistAll$GlobalESlo.d[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.l.d[id]
    outlistAll$GlobalEShi.d[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.u.d[id]
    outlistAll$GlobalES.q[outlistAll$analysis.name%in%l]    <- outlistG$ESCI.cohensQ[id]
    outlistAll$GlobalESlo.q[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.cohensQ.l[id]
    outlistAll$GlobalEShi.q[outlistAll$analysis.name%in%l]  <- outlistG$ESCI.cohensQ.u[id]
    outlistAll$GlobalN[outlistAll$analysis.name%in%l]       <- outlistG$stat.N[id]
    outlistAll$GlobalConsole[outlistAll$analysis.name%in%l] <- paste(outlistG$test.ConsoleOutput[id])
    outlistAll$splitWEIRD[outlistAll$analysis.name%in%l]    <- ML2.key$study.table1.weird.split[id2]
  }
}

outlistAll$USA <- "non-USA"
outlistAll$USA[outlistAll$source.Country=="USA"] <- "USA"

outlistAll$source.WEIRD.f <- factor(outlistAll$source.Weird,
                                    levels = c(0,1),
                                    labels = c("non-WEIRD","WEIRD"))
outlistAll$source.WEIRD.f <- relevel(outlistAll$source.WEIRD.f,ref="WEIRD")

outlistAll.aggr <- summarise(group_by(outlistAll,
                                      study.id,
                                      study.slate,
                                      analysis.name,
                                      splitWEIRD),
                             N.rows                 = n(),
                             N.missing              = sum(is.na(test.p.value), na.rm = TRUE),
                             N.tests                = sum(!is.na(test.p.value), na.rm = TRUE),
                             N.sig.05               = sum(test.p.value<.05, na.rm = TRUE),
                             N.nonsig.05            = sum(test.p.value>=.05, na.rm = TRUE),
                             N.sig.05.samedir.r     = sum((test.p.value<.05)&(ESCI.r > 0), na.rm = TRUE),
                             N.sig.05.oppdir.r      = sum((test.p.value<.05)&(ESCI.r < 0), na.rm = TRUE),
                             N.sig.05.eq0.r         = sum((test.p.value<.05)&(ESCI.r == 0), na.rm = TRUE),
                             N.nonsig.05.samedir.r  = sum((test.p.value>=.05)&(ESCI.r > 0), na.rm = TRUE),
                             N.nonsig.05.oppdir.r   = sum((test.p.value>=.05)&(ESCI.r < 0), na.rm = TRUE),
                             N.nonsig.05.eq0.r      = sum((test.p.value>=.05)&(ESCI.r == 0), na.rm = TRUE),
                             N.sig.05.samedir.d     = sum((test.p.value<.05)&(ESCI.d > 0), na.rm = TRUE),
                             N.sig.05.oppdir.d      = sum((test.p.value<.05)&(ESCI.d < 0), na.rm = TRUE),
                             N.sig.05.eq0.d         = sum((test.p.value<.05)&(ESCI.d == 0), na.rm = TRUE),
                             N.nonsig.05.samedir.d  = sum((test.p.value>=.05)&(ESCI.d > 0), na.rm = TRUE),
                             N.nonsig.05.oppdir.d   = sum((test.p.value>=.05)&(ESCI.d < 0), na.rm = TRUE),
                             N.nonsig.05.eq0.d      = sum((test.p.value>=.05)&(ESCI.d == 0), na.rm = TRUE),
                             N.sig.05.samedir.q     = sum((test.p.value<.05)&(ESCI.cohensQ > 0), na.rm = TRUE),
                             N.sig.05.oppdir.q      = sum((test.p.value<.05)&(ESCI.cohensQ < 0), na.rm = TRUE),
                             N.sig.05.eq0.q         = sum((test.p.value<.05)&(ESCI.cohensQ == 0), na.rm = TRUE),
                             N.nonsig.05.samedir.q  = sum((test.p.value>=.05)&(ESCI.cohensQ > 0), na.rm = TRUE),
                             N.nonsig.05.oppdir.q   = sum((test.p.value>=.05)&(ESCI.cohensQ < 0), na.rm = TRUE),
                             N.nonsig.05.eq0.q      = sum((test.p.value>=.05)&(ESCI.cohensQ == 0), na.rm = TRUE),
                             N.samedir.r            = sum(ESCI.r > 0, na.rm = TRUE),
                             N.oppdir.r             = sum(ESCI.r < 0, na.rm = TRUE),
                             N.eq0.r                = sum(ESCI.r == 0, na.rm = TRUE),
                             N.none.r.00            = sum(ESCI.r <.10, na.rm = TRUE),
                             N.small.r.10           = sum(between(ESCI.r,.10,.30), na.rm = TRUE),
                             N.medium.r.30          = sum(between(ESCI.r,.30,.50), na.rm = TRUE),
                             N.large.r.50           = sum(ESCI.r >= .50, na.rm = TRUE),
                             N.samedir.d            = sum(ESCI.d > 0, na.rm = TRUE),
                             N.oppdir.d             = sum(ESCI.d < 0, na.rm = TRUE),
                             N.eq0.d                = sum(ESCI.d == 0, na.rm = TRUE),
                             N.none.d.00            = sum(ESCI.d <.01, na.rm = TRUE),
                             N.verysmall.d.01       = sum(between(ESCI.d,01,.20), na.rm = TRUE),
                             N.small.d.20           = sum(between(ESCI.d,.20,.40), na.rm = TRUE),
                             N.medium.d.50          = sum(between(ESCI.d,.50,.80), na.rm = TRUE),
                             N.large.d.80           = sum(between(ESCI.d,.80,1.20), na.rm = TRUE),
                             N.verylarge.d.120      = sum(between(ESCI.d,1.20,2.0), na.rm = TRUE),
                             N.large.d.50           = sum(ESCI.d >= 2, na.rm = TRUE),
                             N.samedir.q            = sum(ESCI.cohensQ > 0, na.rm = TRUE),
                             N.oppdir.q             = sum(ESCI.cohensQ < 0, na.rm = TRUE),
                             N.eq0.q                = sum(ESCI.cohensQ == 0, na.rm = TRUE),
                             N.none.q.00            = sum(ESCI.cohensQ <.10, na.rm = TRUE),
                             N.small.q.10           = sum(between(ESCI.cohensQ,.10,.30), na.rm = TRUE),
                             N.intermediate.q.30    = sum(between(ESCI.cohensQ,.30,.50), na.rm = TRUE),
                             N.large.q.50           = sum(ESCI.cohensQ >= .50, na.rm = TRUE)
)

outlistAll.aggr.WEIRD <- summarise(group_by(outlistAll,
                                            study.id,
                                            study.slate,
                                            analysis.name,
                                            source.WEIRD.f),
                                   N.rows                 = n(),
                                   N.missing              = sum(is.na(test.p.value), na.rm = TRUE),
                                   N.tests                = sum(!is.na(test.p.value), na.rm = TRUE),
                                   N.sig.05               = sum(test.p.value<.05, na.rm = TRUE),
                                   N.nonsig.05            = sum(test.p.value>=.05, na.rm = TRUE),
                                   N.sig.05.samedir.r     = sum((test.p.value<.05)&(ESCI.r > 0), na.rm = TRUE),
                                   N.sig.05.oppdir.r      = sum((test.p.value<.05)&(ESCI.r < 0), na.rm = TRUE),
                                   N.sig.05.eq0.r         = sum((test.p.value<.05)&(ESCI.r == 0), na.rm = TRUE),
                                   N.nonsig.05.samedir.r  = sum((test.p.value>=.05)&(ESCI.r > 0), na.rm = TRUE),
                                   N.nonsig.05.oppdir.r   = sum((test.p.value>=.05)&(ESCI.r < 0), na.rm = TRUE),
                                   N.nonsig.05.eq0.r      = sum((test.p.value>=.05)&(ESCI.r == 0), na.rm = TRUE),
                                   N.sig.05.samedir.d     = sum((test.p.value<.05)&(ESCI.d > 0), na.rm = TRUE),
                                   N.sig.05.oppdir.d      = sum((test.p.value<.05)&(ESCI.d < 0), na.rm = TRUE),
                                   N.sig.05.eq0.d         = sum((test.p.value<.05)&(ESCI.d == 0), na.rm = TRUE),
                                   N.nonsig.05.samedir.d  = sum((test.p.value>=.05)&(ESCI.d > 0), na.rm = TRUE),
                                   N.nonsig.05.oppdir.d   = sum((test.p.value>=.05)&(ESCI.d < 0), na.rm = TRUE),
                                   N.nonsig.05.eq0.d      = sum((test.p.value>=.05)&(ESCI.d == 0), na.rm = TRUE),
                                   N.sig.05.samedir.q     = sum((test.p.value<.05)&(ESCI.cohensQ > 0), na.rm = TRUE),
                                   N.sig.05.oppdir.q      = sum((test.p.value<.05)&(ESCI.cohensQ < 0), na.rm = TRUE),
                                   N.sig.05.eq0.q         = sum((test.p.value<.05)&(ESCI.cohensQ == 0), na.rm = TRUE),
                                   N.nonsig.05.samedir.q  = sum((test.p.value>=.05)&(ESCI.cohensQ > 0), na.rm = TRUE),
                                   N.nonsig.05.oppdir.q   = sum((test.p.value>=.05)&(ESCI.cohensQ < 0), na.rm = TRUE),
                                   N.nonsig.05.eq0.q      = sum((test.p.value>=.05)&(ESCI.cohensQ == 0), na.rm = TRUE),
                                   N.samedir.r            = sum(ESCI.r > 0, na.rm = TRUE),
                                   N.oppdir.r             = sum(ESCI.r < 0, na.rm = TRUE),
                                   N.eq0.r                = sum(ESCI.r == 0, na.rm = TRUE),
                                   N.none.r.00            = sum(ESCI.r <.10, na.rm = TRUE),
                                   N.small.r.10           = sum(between(ESCI.r,.10,.30), na.rm = TRUE),
                                   N.medium.r.30          = sum(between(ESCI.r,.30,.50), na.rm = TRUE),
                                   N.large.r.50           = sum(ESCI.r >= .50, na.rm = TRUE),
                                   N.samedir.d            = sum(ESCI.d > 0, na.rm = TRUE),
                                   N.oppdir.d             = sum(ESCI.d < 0, na.rm = TRUE),
                                   N.eq0.d                = sum(ESCI.d == 0, na.rm = TRUE),
                                   N.none.d.00            = sum(ESCI.d <.01, na.rm = TRUE),
                                   N.verysmall.d.01       = sum(between(ESCI.d,01,.20), na.rm = TRUE),
                                   N.small.d.20           = sum(between(ESCI.d,.20,.40), na.rm = TRUE),
                                   N.medium.d.50          = sum(between(ESCI.d,.50,.80), na.rm = TRUE),
                                   N.large.d.80           = sum(between(ESCI.d,.80,1.20), na.rm = TRUE),
                                   N.verylarge.d.120      = sum(between(ESCI.d,1.20,2.0), na.rm = TRUE),
                                   N.large.d.50           = sum(ESCI.d >= 2, na.rm = TRUE),
                                   N.samedir.q            = sum(ESCI.cohensQ > 0, na.rm = TRUE),
                                   N.oppdir.q             = sum(ESCI.cohensQ < 0, na.rm = TRUE),
                                   N.eq0.q                = sum(ESCI.cohensQ == 0, na.rm = TRUE),
                                   N.none.q.00            = sum(ESCI.cohensQ <.10, na.rm = TRUE),
                                   N.small.q.10           = sum(between(ESCI.cohensQ,.10,.30), na.rm = TRUE),
                                   N.intermediate.q.30    = sum(between(ESCI.cohensQ,.30,.50), na.rm = TRUE),
                                   N.large.q.50           = sum(ESCI.cohensQ >= .50, na.rm = TRUE)
)

outlistAll.aggr.USA <- summarise(group_by(outlistAll,
                                          study.id,
                                          study.slate,
                                          analysis.name,
                                          USA),
                                 N.rows                 = n(),
                                 N.missing              = sum(is.na(test.p.value), na.rm = TRUE),
                                 N.tests                = sum(!is.na(test.p.value), na.rm = TRUE),
                                 N.sig.05               = sum(test.p.value<.05, na.rm = TRUE),
                                 N.nonsig.05            = sum(test.p.value>=.05, na.rm = TRUE),
                                 N.sig.05.samedir.r     = sum((test.p.value<.05)&(ESCI.r > 0), na.rm = TRUE),
                                 N.sig.05.oppdir.r      = sum((test.p.value<.05)&(ESCI.r < 0), na.rm = TRUE),
                                 N.sig.05.eq0.r         = sum((test.p.value<.05)&(ESCI.r == 0), na.rm = TRUE),
                                 N.nonsig.05.samedir.r  = sum((test.p.value>=.05)&(ESCI.r > 0), na.rm = TRUE),
                                 N.nonsig.05.oppdir.r   = sum((test.p.value>=.05)&(ESCI.r < 0), na.rm = TRUE),
                                 N.nonsig.05.eq0.r      = sum((test.p.value>=.05)&(ESCI.r == 0), na.rm = TRUE),
                                 N.sig.05.samedir.d     = sum((test.p.value<.05)&(ESCI.d > 0), na.rm = TRUE),
                                 N.sig.05.oppdir.d      = sum((test.p.value<.05)&(ESCI.d < 0), na.rm = TRUE),
                                 N.sig.05.eq0.d         = sum((test.p.value<.05)&(ESCI.d == 0), na.rm = TRUE),
                                 N.nonsig.05.samedir.d  = sum((test.p.value>=.05)&(ESCI.d > 0), na.rm = TRUE),
                                 N.nonsig.05.oppdir.d   = sum((test.p.value>=.05)&(ESCI.d < 0), na.rm = TRUE),
                                 N.nonsig.05.eq0.d      = sum((test.p.value>=.05)&(ESCI.d == 0), na.rm = TRUE),
                                 N.sig.05.samedir.q     = sum((test.p.value<.05)&(ESCI.cohensQ > 0), na.rm = TRUE),
                                 N.sig.05.oppdir.q      = sum((test.p.value<.05)&(ESCI.cohensQ < 0), na.rm = TRUE),
                                 N.sig.05.eq0.q         = sum((test.p.value<.05)&(ESCI.cohensQ == 0), na.rm = TRUE),
                                 N.nonsig.05.samedir.q  = sum((test.p.value>=.05)&(ESCI.cohensQ > 0), na.rm = TRUE),
                                 N.nonsig.05.oppdir.q   = sum((test.p.value>=.05)&(ESCI.cohensQ < 0), na.rm = TRUE),
                                 N.nonsig.05.eq0.q      = sum((test.p.value>=.05)&(ESCI.cohensQ == 0), na.rm = TRUE),
                                 N.samedir.r            = sum(ESCI.r > 0, na.rm = TRUE),
                                 N.oppdir.r             = sum(ESCI.r < 0, na.rm = TRUE),
                                 N.eq0.r                = sum(ESCI.r == 0, na.rm = TRUE),
                                 N.none.r.00            = sum(ESCI.r <.10, na.rm = TRUE),
                                 N.small.r.10           = sum(between(ESCI.r,.10,.30), na.rm = TRUE),
                                 N.medium.r.30          = sum(between(ESCI.r,.30,.50), na.rm = TRUE),
                                 N.large.r.50           = sum(ESCI.r >= .50, na.rm = TRUE),
                                 N.samedir.d            = sum(ESCI.d > 0, na.rm = TRUE),
                                 N.oppdir.d             = sum(ESCI.d < 0, na.rm = TRUE),
                                 N.eq0.d                = sum(ESCI.d == 0, na.rm = TRUE),
                                 N.none.d.00            = sum(ESCI.d <.01, na.rm = TRUE),
                                 N.verysmall.d.01       = sum(between(ESCI.d,01,.20), na.rm = TRUE),
                                 N.small.d.20           = sum(between(ESCI.d,.20,.40), na.rm = TRUE),
                                 N.medium.d.50          = sum(between(ESCI.d,.50,.80), na.rm = TRUE),
                                 N.large.d.80           = sum(between(ESCI.d,.80,1.20), na.rm = TRUE),
                                 N.verylarge.d.120      = sum(between(ESCI.d,1.20,2.0), na.rm = TRUE),
                                 N.large.d.50           = sum(ESCI.d >= 2, na.rm = TRUE),
                                 N.samedir.q            = sum(ESCI.cohensQ > 0, na.rm = TRUE),
                                 N.oppdir.q             = sum(ESCI.cohensQ < 0, na.rm = TRUE),
                                 N.eq0.q                = sum(ESCI.cohensQ == 0, na.rm = TRUE),
                                 N.none.q.00            = sum(ESCI.cohensQ <.10, na.rm = TRUE),
                                 N.small.q.10           = sum(between(ESCI.cohensQ,.10,.30), na.rm = TRUE),
                                 N.intermediate.q.30    = sum(between(ESCI.cohensQ,.30,.50), na.rm = TRUE),
                                 N.large.q.50           = sum(ESCI.cohensQ >= .50, na.rm = TRUE)
                                 )

outlistFigure <- outlistAll[as.character(outlistAll$analysis.name)%in%oriEffects$study.analysis,]
outlistTable  <- outlistAll[as.character(outlistAll$analysis.name)%in%oriEffectsT$study.analysis,]

outlistTable.aggr.WEIRD <- summarise(group_by(outlistTable,
                                              study.id,
                                              study.name,
                                              study.slate,
                                              source.WEIRD.f),
                                     Nrows   = n(),
                                     Nsources = length(unique(study.source)),
                                     Ntot.sum = sum(ESCI.N.total, na.rm = TRUE),
                                     Ntot.mean = mean(ESCI.N.total, na.rm = TRUE),
                                     Ntot.sd   = sd(ESCI.N.total, na.rm = TRUE),
                                     Ntot.median = median(ESCI.N.total, na.rm = TRUE),
                                     N1.sum   = sum(ESCI.n.1, na.rm = TRUE),
                                     N1.mean   = mean(ESCI.n.1, na.rm = TRUE),
                                     N1.sd     = sd(ESCI.n.1, na.rm = TRUE),
                                     N1.median   = median(ESCI.n.1, na.rm = TRUE),
                                     N2.sum   = sum(ESCI.n.2, na.rm = TRUE),
                                     N2.mean  = mean(ESCI.n.2, na.rm = TRUE),
                                     N2.sd    = sd(ESCI.n.2, na.rm = TRUE),
                                     N2.median  = mean(ESCI.n.2, na.rm = TRUE),
                                     ES.r.mean = mean(ESCI.r, na.rm = TRUE),
                                     ES.r.sd   = sd(ESCI.r, na.rm = TRUE),
                                     ES.r.median = median(ESCI.r, na.rm = TRUE),
                                     ES.d.mean = mean(ESCI.d, na.rm = TRUE),
                                     ES.d.sd   = sd(ESCI.d, na.rm = TRUE),
                                     ES.d.median = median(ESCI.d, na.rm = TRUE),
                                     ES.q.mean = mean(ESCI.cohensQ, na.rm = TRUE),
                                     ES.q.sd   = sd(ESCI.cohensQ, na.rm = TRUE),
                                     ES.q.median = median(ESCI.cohensQ, na.rm = TRUE),
                                     ES.Global.r = paste0(unique(GlobalES.r), collapse = "|"),
                                     ES.Global.d = paste0(unique(GlobalES.d), collapse = "|"),
                                     ES.Global.q = paste0(unique(GlobalES.q), collapse = "|")
                                     )

outlistTable.aggr.USA <- summarise(group_by(outlistTable,
                                            study.id,
                                            study.name,
                                            study.slate,
                                            USA),
                                   Nrows   = n(),
                                   Nsources = length(unique(study.source)),
                                   Ntot.sum = sum(ESCI.N.total, na.rm = TRUE),
                                   Ntot.mean = mean(ESCI.N.total, na.rm = TRUE),
                                   Ntot.sd   = sd(ESCI.N.total, na.rm = TRUE),
                                   Ntot.median = median(ESCI.N.total, na.rm = TRUE),
                                   N1.sum   = sum(ESCI.n.1, na.rm = TRUE),
                                   N1.mean   = mean(ESCI.n.1, na.rm = TRUE),
                                   N1.sd     = sd(ESCI.n.1, na.rm = TRUE),
                                   N1.median   = median(ESCI.n.1, na.rm = TRUE),
                                   N2.sum   = sum(ESCI.n.2, na.rm = TRUE),
                                   N2.mean  = mean(ESCI.n.2, na.rm = TRUE),
                                   N2.sd    = sd(ESCI.n.2, na.rm = TRUE),
                                   N2.median  = mean(ESCI.n.2, na.rm = TRUE),
                                   ES.r.mean = mean(ESCI.r, na.rm = TRUE),
                                   ES.r.sd   = sd(ESCI.r, na.rm = TRUE),
                                   ES.r.median = median(ESCI.d, na.rm = TRUE),
                                   ES.d.mean = mean(ESCI.d, na.rm = TRUE),
                                   ES.d.sd   = sd(ESCI.d, na.rm = TRUE),
                                   ES.d.median = median(ESCI.d, na.rm = TRUE),
                                   ES.q.mean = mean(ESCI.cohensQ, na.rm = TRUE),
                                   ES.q.sd   = sd(ESCI.cohensQ, na.rm = TRUE),
                                   ES.q.median = median(ESCI.cohensQ, na.rm = TRUE),
                                   ES.Global.r = paste0(unique(GlobalES.r), collapse = "|"),
                                   ES.Global.d = paste0(unique(GlobalES.d), collapse = "|"),
                                   ES.Global.q = paste0(unique(GlobalES.q), collapse = "|")
                                   )

outlistTable.aggr <- summarise(group_by(outlistTable,
                                        study.id,
                                        study.name,
                                        study.slate,
                                        splitWEIRD),
                               Nrows   = n(),
                               Nsources = length(unique(study.source)),
                               Ntot.sum = sum(ESCI.N.total, na.rm = TRUE),
                               Ntot.mean = mean(ESCI.N.total, na.rm = TRUE),
                               Ntot.sd   = sd(ESCI.N.total, na.rm = TRUE),
                               Ntot.median = median(ESCI.N.total, na.rm = TRUE),
                               N1.sum   = sum(ESCI.n.1, na.rm = TRUE),
                               N1.mean   = mean(ESCI.n.1, na.rm = TRUE),
                               N1.sd     = sd(ESCI.n.1, na.rm = TRUE),
                               N1.median   = median(ESCI.n.1, na.rm = TRUE),
                               N2.sum   = sum(ESCI.n.2, na.rm = TRUE),
                               N2.mean  = mean(ESCI.n.2, na.rm = TRUE),
                               N2.sd    = sd(ESCI.n.2, na.rm = TRUE),
                               N2.median  = mean(ESCI.n.2, na.rm = TRUE),
                               ES.r.mean = mean(ESCI.r, na.rm = TRUE),
                               ES.r.sd   = sd(ESCI.r, na.rm = TRUE),
                               ES.r.median = median(ESCI.d, na.rm = TRUE),
                               ES.d.mean = mean(ESCI.d, na.rm = TRUE),
                               ES.d.sd   = sd(ESCI.d, na.rm = TRUE),
                               ES.d.median = median(ESCI.d, na.rm = TRUE),
                               ES.q.mean = mean(ESCI.cohensQ, na.rm = TRUE),
                               ES.q.sd   = sd(ESCI.cohensQ, na.rm = TRUE),
                               ES.q.median = median(ESCI.cohensQ, na.rm = TRUE),
                               ES.Global.r = paste0(unique(GlobalES.r), collapse = "|"),
                               ES.Global.d = paste0(unique(GlobalES.d), collapse = "|"),
                               ES.Global.q = paste0(unique(GlobalES.q), collapse = "|"))


outlistFigure.aggr <- summarise(group_by(outlistFigure,
                                               study.id,
                                               study.name,
                                               study.slate,
                                               splitWEIRD),
                                      Nrows   = n(),
                                      Nsources = length(unique(study.source)),
                                      Ntot.sum = sum(ESCI.N.total, na.rm = TRUE),
                                      Ntot.mean = mean(ESCI.N.total, na.rm = TRUE),
                                      Ntot.sd   = sd(ESCI.N.total, na.rm = TRUE),
                                      Ntot.median = median(ESCI.N.total, na.rm = TRUE),
                                      N1.sum   = sum(ESCI.n.1, na.rm = TRUE),
                                      N1.mean   = mean(ESCI.n.1, na.rm = TRUE),
                                      N1.sd     = sd(ESCI.n.1, na.rm = TRUE),
                                      N1.median   = median(ESCI.n.1, na.rm = TRUE),
                                      N2.sum   = sum(ESCI.n.2, na.rm = TRUE),
                                      N2.mean  = mean(ESCI.n.2, na.rm = TRUE),
                                      N2.sd    = sd(ESCI.n.2, na.rm = TRUE),
                                      N2.median  = mean(ESCI.n.2, na.rm = TRUE),
                                      ES.r.mean = mean(ESCI.r, na.rm = TRUE),
                                      ES.r.sd   = sd(ESCI.r, na.rm = TRUE),
                                      ES.r.median = median(ESCI.d, na.rm = TRUE),
                                      ES.d.mean = mean(ESCI.d, na.rm = TRUE),
                                      ES.d.sd   = sd(ESCI.d, na.rm = TRUE),
                                      ES.d.median = median(ESCI.d, na.rm = TRUE),
                                ES.q.mean = mean(ESCI.cohensQ, na.rm = TRUE),
                                ES.q.sd   = sd(ESCI.cohensQ, na.rm = TRUE),
                                ES.q.median = median(ESCI.cohensQ, na.rm = TRUE),
                                ES.Global.r = paste0(unique(GlobalES.r), collapse = "|"),
                                ES.Global.d = paste0(unique(GlobalES.d), collapse = "|"),
                                ES.Global.q = paste0(unique(GlobalES.q), collapse = "|")
                                )


outlistFigure.aggr.WEIRD <- summarise(group_by(outlistFigure,
                                              study.id,
                                              study.name,
                                              study.slate,
                                              source.WEIRD.f),
                                     Nrows   = n(),
                                     Nsources = length(unique(study.source)),
                                     Ntot.sum = sum(ESCI.N.total, na.rm = TRUE),
                                     Ntot.mean = mean(ESCI.N.total, na.rm = TRUE),
                                     Ntot.sd   = sd(ESCI.N.total, na.rm = TRUE),
                                     Ntot.median = median(ESCI.N.total, na.rm = TRUE),
                                     N1.sum   = sum(ESCI.n.1, na.rm = TRUE),
                                     N1.mean   = mean(ESCI.n.1, na.rm = TRUE),
                                     N1.sd     = sd(ESCI.n.1, na.rm = TRUE),
                                     N1.median   = median(ESCI.n.1, na.rm = TRUE),
                                     N2.sum   = sum(ESCI.n.2, na.rm = TRUE),
                                     N2.mean  = mean(ESCI.n.2, na.rm = TRUE),
                                     N2.sd    = sd(ESCI.n.2, na.rm = TRUE),
                                     N2.median  = mean(ESCI.n.2, na.rm = TRUE),
                                     ES.r.mean = mean(ESCI.r, na.rm = TRUE),
                                     ES.r.sd   = sd(ESCI.r, na.rm = TRUE),
                                     ES.r.median = median(ESCI.d, na.rm = TRUE),
                                     ES.d.mean = mean(ESCI.d, na.rm = TRUE),
                                     ES.d.sd   = sd(ESCI.d, na.rm = TRUE),
                                     ES.d.median = median(ESCI.d, na.rm = TRUE),
                                     ES.q.mean = mean(ESCI.cohensQ, na.rm = TRUE),
                                     ES.q.sd   = sd(ESCI.cohensQ, na.rm = TRUE),
                                     ES.q.median = median(ESCI.cohensQ, na.rm = TRUE),
                                     ES.Global.r = paste0(unique(GlobalES.r), collapse = "|"),
                                     ES.Global.d = paste0(unique(GlobalES.d), collapse = "|"),
                                     ES.Global.q = paste0(unique(GlobalES.q), collapse = "|"))


outlistFigure.aggr.USA <- summarise(group_by(outlistFigure,
                                            study.id,
                                            study.name,
                                            study.slate,
                                            USA),
                                   Nrows   = n(),
                                   Nsources = length(unique(study.source)),
                                   Ntot.sum = sum(ESCI.N.total, na.rm = TRUE),
                                   Ntot.mean = mean(ESCI.N.total, na.rm = TRUE),
                                   Ntot.sd   = sd(ESCI.N.total, na.rm = TRUE),
                                   Ntot.median = median(ESCI.N.total, na.rm = TRUE),
                                   N1.sum   = sum(ESCI.n.1, na.rm = TRUE),
                                   N1.mean   = mean(ESCI.n.1, na.rm = TRUE),
                                   N1.sd     = sd(ESCI.n.1, na.rm = TRUE),
                                   N1.median   = median(ESCI.n.1, na.rm = TRUE),
                                   N2.sum   = sum(ESCI.n.2, na.rm = TRUE),
                                   N2.mean  = mean(ESCI.n.2, na.rm = TRUE),
                                   N2.sd    = sd(ESCI.n.2, na.rm = TRUE),
                                   N2.median  = mean(ESCI.n.2, na.rm = TRUE),
                                   ES.r.mean = mean(ESCI.r, na.rm = TRUE),
                                   ES.r.sd   = sd(ESCI.r, na.rm = TRUE),
                                   ES.r.median = median(ESCI.d, na.rm = TRUE),
                                   ES.d.mean = mean(ESCI.d, na.rm = TRUE),
                                   ES.d.sd   = sd(ESCI.d, na.rm = TRUE),
                                   ES.d.median = median(ESCI.d, na.rm = TRUE),
                                   ES.q.mean = mean(ESCI.cohensQ, na.rm = TRUE),
                                   ES.q.sd   = sd(ESCI.cohensQ, na.rm = TRUE),
                                   ES.q.median = median(ESCI.cohensQ, na.rm = TRUE),
                                   ES.Global.r = paste0(unique(GlobalES.r), collapse = "|"),
                                   ES.Global.d = paste0(unique(GlobalES.d), collapse = "|"),
                                   ES.Global.q = paste0(unique(GlobalES.q), collapse = "|")
)


dir.in <- "~/Dropbox/Manylabs2/TestOutput/RESULTS.RDS/"
rio::export(outlistAll,paste0(dir.in,"Data_All.rds"))
rio::export(outlistFigure,paste0(dir.in,"Data_Figure.rds"))
rio::export(outlistTable,paste0(dir.in,"Data_Table.rds"))
rio::export(outlistG,paste0(dir.in,"Data_Global.rds"))
rio::export(outlistAll,paste0(dir.in,"Data_All.xlsx"))
rio::export(outlistFigure,paste0(dir.in,"Data_Figure.xlsx"))
rio::export(outlistTable,paste0(dir.in,"Data_Table.xlsx"))
rio::export(outlistG,paste0(dir.in,"Data_Global.xlsx"))
rio::export(outlistAll,paste0(dir.in,"Data_All.csv"))
rio::export(outlistFigure,paste0(dir.in,"Data_Figure.csv"))
rio::export(outlistTable,paste0(dir.in,"Data_Table.csv"))
rio::export(outlistG,paste0(dir.in,"Data_Global.csv"))


rio::export(outlistTable.aggr,paste0(dir.in,"Data_Table_Aggregated.xlsx"))
rio::export(outlistTable.aggr.WEIRD,paste0(dir.in,"Data_Table_Aggregated_WEIRD.xlsx"))
rio::export(outlistTable.aggr.USA,paste0(dir.in,"Data_Table_Aggregated_USA.xlsx"))

rio::export(outlistFigure.aggr,paste0(dir.in,"Data_Figure_Aggregated.xlsx"))
rio::export(outlistFigure.aggr.WEIRD,paste0(dir.in,"Data_Figure_Aggregated_WEIRD.xlsx"))
rio::export(outlistFigure.aggr.USA,paste0(dir.in,"Data_Figure_Aggregated_USA.xlsx"))

rio::export(outlistAll.aggr,paste0(dir.in,"Data_ALL_Aggregated_NHST.xlsx"))
rio::export(outlistAll.aggr.WEIRD,paste0(dir.in,"Data_ALL_Aggregated_NHST_WEIRD.xlsx"))
rio::export(outlistAll.aggr.USA,paste0(dir.in,"Data_ALL_Aggregated_NHST_USA.xlsx"))



