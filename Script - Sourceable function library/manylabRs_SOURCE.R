#' @import stats
#' @import plyr
#' @import tidyverse
NULL

# Data Cleaning functions  ------------------

clean.Source <- function(source.raw, SourceTable){
  source.clean <- gsub("([[:blank:]]|[[:punct:]]|[[:cntrl:]])+","",source.raw)
  for(s in seq_along(SourceTable$Source.Field.Raw)){
    ID <- which(source.clean%in%SourceTable$Source.Field.Raw[[s]])
    source.clean[ID] <- SourceTable$Source.Field.Correct[[s]]
  }
  return(as.data.frame(source.clean))
}

# func: Get additional changes, variables, etc.
get.fieldAdd <- function(data,stable){
  data$Source.Global    <- NA
  data$Source.Primary   <- NA
  data$Source.Secondary <- NA
  data$Country      <- NA
  data$Location     <- NA
  data$Language     <- NA
  data$Weird        <- NA
  data$Execution    <- NA
  data$SubjectPool  <- NA
  data$Setting      <- NA
  data$Tablet       <- NA
  data$Pencil       <- NA
  data$StudyOrder   <- NA
  data$IDiffOrder   <- NA

  for(s in seq_along(stable$Source)){
    ID <- which((stable$Source[[s]]==data$source)&(stable$Filename[[s]]==data$.id))
    if(length(ID)>0){
      data$Source.Global[ID]    <- stable$Source.Global[[s]]
      data$Source.Primary[ID]   <- stable$Source.Global[[s]]
      data$Source.Secondary[ID] <- stable$Source[[s]]
      data$Country[ID]      <- stable$Country[[s]]
      data$Location[ID]     <- stable$Location[[s]]
      data$Language[ID]     <- stable$Language[[s]]
      data$Weird[ID]        <- stable$Weird[[s]]
      data$Execution[ID]    <- stable$Execution[[s]]
      data$SubjectPool[ID]  <- stable$SubjectPool[[s]]
      data$Setting[ID]      <- stable$Setting[[s]]
      data$Tablet[ID]       <- stable$Tablet[[s]]
      data$Pencil[ID]       <- stable$Pencil[[s]]
      data[ID, "StudyOrder"]   <- data[ID, stable$StudyOrder[[s]]]
      data[ID, "IDiffOrder"]   <- data[ID, stable$IDiffOrder[[s]]]
    }
  }
  return(as.data.frame(data))
}

scanColumns <- function(pattern, data){
  # Function to scan columns of a dataframe for a regex pattern and return a variable (in rows) by case (in columns) logical matrix
  # Includes some error catching, correcting and reporting
  idS    <- list()
  cNames <- colnames(data)
  for(c in seq(1,ncol(data))){
    tmp      <- try.CATCH(c(grepl(pattern,data[[c]],useBytes = T,ignore.case=T)))
    if(is.null(tmp$warning)){
      idS[[c]]<-tmp$value
    } else {
      cat('Error in column: ', colnames(data)[c],'->\t',paste0(tmp$warning),'\n')
    }
  }
  idD   <- plyr::ldply(idS)
  cs    <- rowSums(idD)
  colID <- which(cs>0)
  idS   <- t(idD[colID, ])

  return(list(idMatrix = idS,
              idColnames = cNames[colID])
  )
}

# func: Clean data fields to NA according to pattern
# Default: Set test trials and -99 to NA
clean.ML2fieldsNA <- function(source.raw, pattern="(test|-99)", S1 = TRUE, ps = "# "){

  # First mark list of known test runs for removal
  disp(message = 'Marking known test sessions for removal', header = 'Clean ML2 Test Data - Step 1', footer = F)

  source.raw$Finished[which(c("R_blS3XHmD4p14kf3",
                              "R_8Cjo1Lguez90bqt",
                              "R_bC85NEijsCjOqep")%in%source.raw$ResponseID)] <- 0

  textcolumns <- c('age','sex','hometown','education','comments','raceother','cogref.1','cogref.2','cogref.3')

  if(!S1){
    textcolumns <- c(textcolumns, 'ross.s2.1_1_TEXT', 'ross.s2.1_2_TEXT', 'sava1.2', 'sava1.3', 'sava1.7', 'sava1.8',
                     'sava1.12', 'sava1.13', 'sava1.18', 'sava1.19', 'sava1.24', 'sava1.25', 'sava1.30', 'sava1.31',
                     'sava1.36', 'sava1.37', 'sava1.41', 'sava1.42', 'sava2.2', 'sava2.3', 'sava2.7', 'sava2.8',
                     'sava2.12', 'sava2.13', 'sava2.18', 'sava2.19', 'sava2.24', 'sava2.25', 'sava2.30', 'sava2.31',
                     'sava2.36', 'sava2.37', 'sava2.41', 'sava2.42', 'zhon1.1', 'zhon2.1', 'zav.int.1', 'zav1.1',
                     'zav1.2', 'zav1.3', 'zav1.4', 'zav1.5', 'zav1.6', 'zav1.7', 'zav1.8', 'zav1.9', 'zav1.10',
                     'zav1.11', 'zav1.12', 'zav1.13', 'zav2.1', 'zav2.2', 'zav2.3', 'zav2.4', 'zav2.5', 'zav2.6',
                     'zav2.7', 'zav2.8', 'zav2.9', 'zav2.10', 'zav2.11', 'zav2.12', 'zav2.13')
  }

  if(S1){
    textcolumns <- c(textcolumns, 'crit1.1_1_TEXT', 'crit1.1_2_TEXT', 'crit1.1_3_TEXT',
                     'ross.s1.1_1_TEXT', 'ross.s1.1_2_TEXT', 'kay1.1', 'kay1.2_7_TEXT',
                     'kay1.2_8_TEXT', 'kay1.2_9_TEXT', 'kay1.3',  'kay2.1', 'kay2.2_7_TEXT', 'kay2.2_8_TEXT',
                     'kay2.2_9_TEXT', 'kay2.3', 'and1.1', 'and2.1'
    )
  }

  # Now search for 'test' and apply Finished == 1 filter
  disp(message =  paste(c("Checking columns:\n\t\t ",paste(textcolumns, collapse="\n\t",ps,"\t "),
                          "\n\t",ps," for a variant of pattern: 'test'"),
                        collapse=""), header = 'Clean ML2 Test Data - Step 2', footer = F)

  idS <- scanColumns(pattern='test',data = source.raw[, textcolumns])
  if(sum(colSums(idS$idMatrix) > 0)){
    source.raw$Finished[(rowSums(idS$idMatrix)>0)] <- 0
  } else {
    source.clean <- source.raw
  }

  # Remove the test sessions
  source.clean <- source.raw %>% dplyr::filter(Finished == 1)

  # Now look for '-99'
  disp(message = 'Checking all columns except "LocationLongitude" for pattern: "-99"',
       header = 'Clean Test ML2 Data - Step 3', footer = F)
  idS <- scanColumns(pattern='-99', data = source.clean[ ,which(!colnames(source.clean)%in%c("LocationLongitude"))])
  # Set -99 to NA
  for(c in seq(1,ncol(idS$idMatrix))){
    source.clean[idS$idMatrix[ ,c], as.numeric(colnames(idS$idMatrix)[c])] <- NA
  }
  disp(message = 'Done!')
  return(source.clean)
}

# 'get' functions ----------------

#' get.chain
#'
#' @param inf Internal
#'
#' @export
#'
get.chain <- function(inf){
  # Build a filter chain

  filt.vars <- unlist(inf$study.cases.include,recursive=F)

  # include sites
  filt.site <- paste0(" %>% dplyr::filter(",paste0(inf$study.sites.include),")")
  #filt.site <- paste0(" %>% dplyr::filter(is.character(.id))")

  # Data frame filter
  filt.df <- paste0(" %>% dplyr::select(", paste0(inf$id.vars,collapse=","),")", filt.site)

  #Variables filter
  return(list(df=filt.df,vars=filt.vars))
}

#' get.cases
#'
#' @param rule Internal
#' @param study.vars Internal
#' @param study.vars.labels Internal
#' @param stat.params Internal
#'
#' @export
#'
get.cases <- function(rule,study.vars,study.vars.labels,stat.params){
  #rule <- cases.include
  type  <-names(rule)
  if(!is.matrix(rule[[1]])){rule  <- rbind(rule[[1]])} else {rule <- rule[[1]]}
  Nrule <- nrow(rule)

  isna <- ifelse(stat.params$censorNA,{" & !is.na(X)"},{""})
  do <- list()

  # Rule 1 should always be about the variables in 'study.vars'
  # Rule 2..N can concern a subset of the variables in 'study.vars'.
  # Assumption for type="each": Rule 2 number of variables is a multiple of number of conditions, with variables grouped to fit to each condition.
  # Assumption for type="sep": Rule 2 contains a separate rule independent of variables listed in the first rule.

  r = 1
  filtvars <- study.vars.labels[names(study.vars.labels)%in%rule[r,1]]

  # Start with 'pre'
  pre <- plyr::llply(unlist(filtvars), function(v){paste0(v,' %>% dplyr::filter(')})

  if(type == "each"){

    if(all(filtvars[[1]]%in%names(study.vars))){
      filtvars <- study.vars
      do <- plyr::laply(seq_along(filtvars),
                  function(v) plyr::laply(seq_along(filtvars[[v]]),
                                    function(vv) paste0(gsub("X", filtvars[[v]][vv],rule[r,2]),
                                                        gsub("X",filtvars[[v]][vv], isna)))
      )
    }

    if(!is.matrix(do)){do <- as.matrix(do)}

    if(Nrule > 1){
      s <- ncol(do)
      for(r in 2:Nrule){
        filtvars <- unlist(study.vars.labels[names(study.vars.labels)%in%rule[r,1]])
        do  <- cbind(do, plyr::laply(seq_along(filtvars),
                               function(vv) paste0(gsub("X",filtvars[[vv]],rule[r,2]))))
      }
    }
    case <- plyr::llply(seq_along(pre), function(fr) paste0(pre[[fr]], paste0(do[fr, ],collapse = ' & '),")"))
    names(case) <- names(study.vars)
  }

  if(type == "sep"){
    if(all(filtvars[[1]]%in%names(study.vars))){
      filtvars <- study.vars
      do <- plyr::llply(seq_along(filtvars),
                  function(v) plyr::laply(seq_along(filtvars[[v]]),
                                    function(vv) paste0(gsub("X",filtvars[[v]][vv],rule[r,2]),
                                                        gsub("X",filtvars[[v]][vv],isna)))
      )
    }

    names(do) <- names(filtvars)

    if(Nrule > 1){
      for(r in 2:Nrule){
        if(rule[r,1]%in%names(do)){
          filtvars <- unlist(study.vars.labels[names(study.vars.labels)%in%rule[r,1]])
          do[[r]] <- c(do[[r]], plyr::laply(seq_along(filtvars),
                                      function(vv) paste0(gsub("X",filtvars[[vv]],rule[r,2])))
          )
        }
      }
    }

    case <- plyr::llply(seq_along(pre), function(fr) paste0(pre[[fr]], paste0(do[[fr]],collapse = ' & '),")"))
    names(case) <- names(study.vars)
  }

  return(eval(parse(text=paste0('list(',rule[1],'=case)'))))
}

#' get.info
#'
#' @param keytable Internal
#' @param cols Internal
#'

#' @export
#'
get.info <- function(keytable,cols, subset){
  # Read Variables and Parameters from:
  #keytable <- ML2.key[s,]
  study.vars         <- eval(parse(text=keytable[,'study.vars']))
  study.vars.labels  <- eval(parse(text=keytable[,'study.vars.labels']))
  cases.include      <- eval(parse(text=keytable[,'study.cases.include']))
  stat.params        <- eval(parse(text=keytable[,'stat.params']))
  #cases <- plyr::llply(seq_along(cases.include),function(i) get.cases(cases.include[i],study.vars,study.vars.labels,stat.params))
  cases              <- get.cases(cases.include, study.vars, study.vars.labels, stat.params)
  sites.include      <- eval(parse(text=keytable[,'study.sites.include']))
  if(sites.include[[1]][1]=="all"){sites.include[[1]]<-'is.character(source)'}
  if(subset!="all"){
    W <- ifelse(subset=="WEIRD",1,0)
    sites.include[[1]] <- paste0(sites.include[[1]][1],' & (Weird == ',W,')')
    }

  # Find correct columns in this dataset according to ML2.key: 'ML2.in$study.vars'
  id.vars  <- which(cols%in%c(unlist(study.vars),'uID','.id','age','sex','source','Source.Global','Source.Primary','Source.Secondary','Country','Location','Language','Weird','SubjectPool','Setting','Tablet','Pencil','Execution', 'StudyOrderN','IDiffOrderN'))
  return(list(study.vars          = study.vars,
              study.vars.labels   = study.vars.labels,
              stat.params         = stat.params,
              study.cases.include = cases,
              study.sites.include = sites.include,
              id.vars             = id.vars))
}

#' get.sourceData
#'
#' @param ML2.id  Internal
#' @param ML2.df  Internal
#' @param ML2.in  Inernal
#'
#' @return A list with fields \code{study.vars} (data organised according to the \code{masteRkey} spreadsheet), \code{study.vars/labels}, \code{N}, and \code{RawDataFilter}(raw data, unfiltered).
#' @export
#'

get.sourceData <- function(ML2.id,ML2.df,ML2.in){
  N       <- numeric(length(ML2.in$study.vars))
  if(is.null(ML2.df$uID)){ML2.df$uID <- seq_along(ML2.df$source)}
  #study.vars[1]    <- unlist(ML2.in$study.vars)
  vars    <- list()
  dfname  <- list()
  RawData <- list()
  #id <- factor(ML2.df$.id)
  for(i in seq_along(ML2.in$study.vars)){
    dfname[i] <- names(ML2.in$study.vars)[[i]]
    eval(parse(text=paste0(names(ML2.in$study.vars)[[i]],
                           " <- dplyr::tbl_df(dplyr::select(ML2.df,",
                           paste0(c(ML2.in$study.vars[[i]],'uID'), collapse=","),"))"))
    )

    # Change text to numbers
    suppressWarnings(if(
      any(eval(parse(text = paste0('apply(',
                                   names(ML2.in$study.vars)[i],',2,is.numeric)==FALSE'))))){
      eval(parse(text = paste0(names(ML2.in$study.vars)[i],' <- data.frame(sapply(which(apply(',
                               names(ML2.in$study.vars)[i],', 2, is.numeric)==FALSE), function(si) as.numeric(',
                               names(ML2.in$study.vars)[i],'[[si]])))')))
    })
  }

  for(i in seq_along(dfname)){
    eval(parse(text=paste0(dfname[i],' <- ', unlist(ML2.id$vars)[i])))
    eval(parse(text=paste0("attr(",names(ML2.in$study.vars)[[i]],",'uID')",
                           " <- ",paste0(names(ML2.in$study.vars)[[i]],"[['uID']]"))))
    RawData[[i]] <- dplyr::mutate(ML2.df,
                                  Included = eval(parse(text=paste0('ML2.df$uID %in% ',dfname[i],'$uID')))
    )
    #    N[i]         <- eval(parse(text=paste0("nrow(",dfname[i],")"))
  }

  if(ML2.in$stat.params$within){sameID <- ML2.df$uID[RawData[[1]]$Included&RawData[[2]]$Included]}

  #  sameID <- eval(parse(text=paste0(dfname[which.max(N)],"$uID %in% ", dfname[which.min(N)],"$uID") ))
  for(i in seq_along(dfname)){
    # Check if datasets are equal length for within subject analyses
    if(ML2.in$stat.params$within){
      eval(parse(text=paste0(dfname[i]," <- ",dfname[i],"[",dfname[i],"$uID %in% sameID, ]")))
      RawData[[i]] <- mutate(ML2.df,
                             Included = eval(parse(text=paste0('ML2.df$uID %in% ', dfname[i],'$uID',collapse="&")))
      )
    }
    N[i] <- eval(parse(text=paste0("sum(!is.na(",dfname[i],"), na.rm = TRUE)")))
    # eval(parse(text=paste0(dfname[i],"<-", dfname[i]," %>% dplyr::select(which(colnames(",dfname[i],")!='uID'))")))
    eval(parse(text=paste0(dfname[i],' -> vars[[i]]')))
  }

  vars[[length(ML2.in$study.vars)+1]] <- N
  #if(length(ML2.in$study.vars.labels)==0){ML2.in$study.vars.labels <- list(NoLabels="None")}
  vars[[length(ML2.in$study.vars)+2]] <- ML2.in$study.vars.labels
  vars[[length(ML2.in$study.vars)+3]] <- RawData
  names(vars) <- c(names(ML2.in$study.vars),"N","labels","RawDataFilter")
  return(vars)
}


# #' get.SwarmPlot
# #'
# #' @param df  A data frame.
# #'
#
# #' @export
# #'
#
# get.SwarmPlot <- function(df){
#
# df<-outlist1
#     sourceInfo <- get.GoogleSheet(url="https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/export?format=csv")$df
#
#
#     nameChange <- list(
#         Huang.1 = "Direction & SES (Huang et al., 2014)",
#         Kay.1 = "Structure & Goal Pursuit (Kay et al., 2014)",
#         Alter.1 = "Incidental Disfluency (Alter et al., 2007)",
#         Graham.1 = "Moral Foundations (Graham et al., 2009)",
#         Rottenstreich.1 = "Affect & Risk (Rottenstreich & Hsee, 2001)",
#         Bauer.1 = "Priming Consumerism (Bauer et al., 2012)",
#         Miyamoto.1 = "Correspondence Bias (Miyamoto & Kitayama, 2002)",
#         Inbar.1 = "Disgust & Homophobia (Inbar et al., 2009)",
#         Critcher.1 = "Incidental Anchors (Critcher & Gilovich, 2008)",
#         vanLange.1 = "Social Value Orientation (Van Lange et al., 1997)",
#         Hauser.1 = "Trolley Dilemma 1 (Hauser et al., 2007)",
#         Anderson.1 = "SMS & Well-Being (Anderson et al., 2012)",
#         Ross.1 = "False Consensus 1 (Ross et al., 1977)",
#         Ross.2 = "False Consensus 2 (Ross et al., 1977)",
#         Giessner.1 = "Position & Power (Giessner & Schubert, 2007)",
#         Tversky.1 = "Framing (Tversky & Kahneman, 1981)",
#         Hauser.2 = "Trolley Dilemma 2 (Hauser et al., 2007)",
#         Risen.1 = "Tempting Fate (Risen & Gilovich, 2008)",
#         Savani.1 = "Actions are Choices (Savani et al., 2010)",
#         Norenzayan.1 = "Intuitive Reasoning (Norenzayan et al., 2002)",
#         Hsee.1 = "Less is Better (Hsee, 1998)",
#         Gray.1 = "Moral Typecasting (Gray & Wegner, 2009)",
#         Zhong.1 = "Moral Cleansing (Zhong & Liljenquist, 2006)",
#         Schwarz.1 = "Assimilation & Contrast (Schwarz et al., 1991)",
#         Shafir.1 = "Choosing or Rejecting (Shafir, 1993)",
#         Zaval.3 = "Priming Warmth (Zaval et al., 2014)",
#         Knobe.1 = "Intentional Side-Effects (Knobe, 2003)",
#         Gati.1a = "Direction & Similarity (Tversky & Gati, 1978)",
#         Risen.3 = "Label for Risen.3"
#     )
#
#     df$slabel <- "No label"
#     l_ply(seq_along(nameChange), function(l) df$slabel[as.character(df$.id)==names(nameChange)[[l]]] <<- nameChange[[l]])
#     df$slabel <- factor(df$slabel)
#
#     df$Country <- "No Country"
#     l_ply(seq_along(df$.id), function(l) df$Country[l] <<- sourceInfo$Country[sourceInfo$Source.Global==df$study.source[l]])
#
#     df$USA                    <- "Non-USA"
#     df$USA[df$Country=="USA"] <- "USA"
#
#     df$.id    <- factor(df$.id)
#     btype     <- "swarm"
#     pdf(tempfile())
#     bs    <- beeswarm(ESCI.r ~ slabel, data = df,
#                       horizontal = FALSE, corral = "none",
#                       corralWidth = 5,
#                       pch = 21, spacing = 2, cex = .5,
#                       priority = "ascending", method = btype, do.plot = TRUE)[, c(1, 2, 4, 6)]
#     dev.off()
#
#     colnames(bs)[4] <- "labels"
#     #plyr::laply(unique(df$analysis), function(r) strsplit(x = r, split = "[.]")[[1]][1])
#
#     df <- data.frame(df,bs)
#     se <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
#
#     df$meanES <- plyr::ldply(unique(df$labels),
#                        function(r) cbind(rep(mean(df$y[df$labels==r], na.rm = TRUE),
#                                              sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
#     df$seES <- plyr::ldply(unique(df$labels),
#                      function(r) cbind(rep(se(df$y[df$labels==r]),
#                                            sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
#
#     #df    <- df[order(df$meanES, decreasing = F), ]
#     df <- arrange(df, meanES)
#
#     df$xx <- plyr::ldply(unique(df$labels),
#                    function(r) cbind(scale(df$x[df$labels==r], scale = F)))[,1]
#     df$xf <- plyr::ldply(seq_along(unique(df$labels)),
#                    function(r) cbind(df$xx[df$labels==unique(df$labels)[r]] +
#                                          seq(10,10*length(unique(df$labels)), by=10)[r]))[,1]
#     df$xn <- plyr::ldply(seq_along(unique(df$labels)),
#                    function(r) cbind(rep(seq(10,10*length(unique(df$labels)),by=10)[r],
#                                          sum(df$labels==unique(df$labels)[r]))))[,1]
#
#     keylabs <- c("Mean of Sample Effect Sizes","Effect Size of Global Mean")
#
#     mxN       <- max(df$ESCI.N.total)
#     mypalette <- c("red3","steelblue")
#
#     df$sigf <- NA
#     df$sigf[df$test.p.value> .05] <- "Not Significant"
#     df$sigf[df$test.p.value<=.05] <- "Significant"
#     df$sigf <- factor(df$sigf)
#
#     df$USA <- factor(df$USA)
#
#     df <- df[df$ESCI.N.total>=30,]
#
#     dfG <- dplyr::summarise(group_by(df,.id),
#                      y= median(ESCI.r,na.rm = T),
#                      ymin=median(ESCI.l.r, na.rm = T),
#                      ymax=median(ESCI.u.r, na.rm = T))
#
#      dfG  <- arrange(dfG, y)
#
#      dfG$x <- seq(10,10*nrow(dfG),by=10)
#
#
#     #gnsize <-
#   g <- ggplot(df, aes(x=xf, y=y)) +
#         geom_vline(xintercept = unique(df$xn), colour = "grey80",alpha = 1) +
#         geom_hline(yintercept = 0, colour = "ivory4") +
#       # geom_errorbar(data=dfG,aes(x=x,y=y,ymin=ymin,ymax=ymax),
#       #               color="black",alpha=1,size=1.2) +
#         geom_point(aes(fill = USA), size = 2, #ESCI.N.total/mxN),
#                    col="snow2", pch=21) +
#        geom_point(data=dfG,aes(x=x,y=y),
#                     color="black",fill="grey80",alpha=1,size=3,pch=23) +
#       scale_y_continuous("Effect Size r", limits = c(-1,1)) +
#         scale_x_continuous("", breaks = unique(df$xn),
#                                labels = unique(paste(1:nrow(dfG))),
#                                expand = c(0, 10)) +
#         scale_fill_manual("",values = mypalette,
#                           guide   = guide_legend(override.aes = list(size = 4),
#                                                  byrow = TRUE)
#                           ) +
#         # scale_size_continuous("Sample Size", breaks = c(0.01, 0.1, 0.3, 0.5,0.8,1),
#         #                       labels = round(c(0.01, 0.1, 0.3, 0.5,0.8, 1) * mxN),
#         #                       guide = guide_legend(override.aes=list(colour="grey30",fill="grey70"), byrow = TRUE)
#         # ) +
#         scale_shape_manual(labels=keylabs, values=c(24,25),guide=guide_legend("")) +
#         gg.theme() + coord_flip()  +
#         theme(legend.position = "top", legend.background=element_rect())
#
#     return(g)
#
# }


get.oriESCI <- function(CL=.95){

  shafir.ori   <-  as.table(matrix(c(31,54,38,47),nrow = 2, ncol = 2, dimnames = list(c("parent A", "parent B"), c("Award","Deny"))))

  tverski.ori  <- as.table(matrix(c(69,24,25,63),nrow = 2, ncol = 2, dimnames = list(c("Go","Don't go"),c("Cheap", "Costly"))))

  # savani.ori   <- as.table(matrix(c(45,60,45,68),nrow = 2, ncol = 2, dimnames = list(c("Americans", "Indians"), c("Personal","Interpersonal"))))

  savani.ori   <- as.table(matrix(c(60,45,68,45),nrow = 2, ncol = 2, dimnames = list(c("Indians", "Americans"), c("Personal","Interpersonal"))))


  # Load Key Table
  ML2.ori <- get.GoogleSheet(data='ML2masteRkey')$df
  ID.ori  <- which(nchar(ML2.ori$orig.stat.type)>0)
  out     <- list()

  cnt <- 0

  #skip <- c(4,16)
  skip = NULL
  for(s in ID.ori){

    test <- data.frame(statistic = NA,
                       estimate  = NA,
                       estimate1 = NA,
                       estimate2 = NA, alternative=NA,
                       parameter=NA, parameter1=NA, parameter2=NA,
                       conf.low=NA, conf.high=NA,method=NA)

    cnt<-cnt+1

    study.id         <- ML2.ori$study.id[[s]]
    study.slate      <- ML2.ori$study.slate[[s]]
    study.name       <- ML2.ori$study.name[[s]]
    study.analysis   <- ML2.ori$study.analysis[[s]]
    test$statistic   <- ML2.ori$orig.stat.ncp[[s]]

    cat(paste(study.analysis,"\n"))
    # for OR and t.r and Z.f
    test$estimate    <- ML2.ori$orig.stat.estimate[[s]]
    test$estimate1   <- ML2.ori$orig.stat.estimate1[[s]]
    test$estimate2   <- ML2.ori$orig.stat.estimate2[[s]]
    test$alternative <- ML2.ori$orig.test.alternative[[s]]
    df1 <- ML2.ori$orig.stat.df1[[s]]
    df2 <- ML2.ori$orig.stat.df1[[s]]
    N   <- ML2.ori$orig.stat.N[[s]]
    n1  <- ML2.ori$orig.stat.n1[[s]]
    n2  <- ML2.ori$orig.stat.n2[[s]]
    esType <- ML2.ori$orig.stat.type[[s]]

    # var.lor <- ifelse(grepl("OR",esType),
    #                   sum(1/(table()), na.rm = TRUE),
    #                   NA)
    var.lor <- NA

    if(study.name=="Tversky"){
      var.lor    <- sum(1/(tverski.ori))
      stat.test  <- fisher.exact(tverski.ori)
      test <- broom::tidy(stat.test)
      test$parameter <- NA
    }

    if(study.analysis=="Savani.1a"){
      var.lor    <- sum(1/(savani.ori))
      stat.test  <- fisher.exact(savani.ori)
      test <- broom::tidy(stat.test)
      colnames(test)[1] <- "statistic"
      test$parameter <- NA
    }

    if(esType=="Z.f"){
      stat.test <- cor_test_fisherZ(r1=ML2.ori$orig.stat.estimate1[[s]],
                                    r2= ML2.ori$orig.stat.estimate2[[s]],
                                    n1=n1,
                                    n2=n2)
      tmp <- unclass(stat.test)
      stat.test <- tmp[1:8]
      test$estimate <- test$statistic <- tanh(tmp$effect.size[[1]])
      test$conf.low <-  tanh(tmp$effect.size.ci[[1]])
      test$conf.high <- tanh(tmp$effect.size.ci[[2]])
      test$method <- stat.test$method
      #class(stat.test) <- "htest"
    }

    if(esType=="t.r"){
      stat.test <- cor_test_fisherZ(r1 = test$estimate,
                                    r2 = NULL,
                                    n1=n1,
                                    n2=NULL)
      tmp <- unclass(stat.test)
      stat.test <- tmp[1:8]
      test$estimate <- tanh(tmp$effect.size[[1]])
      test$statistic <- test$estimate * sqrt((N-2)/(1-test$estimate^2))
      df1 <- N-2
      # test$conf.low <-  tanh(tmp$effect.size.ci[[1]])
      # test$conf.high <- tanh(tmp$effect.size.ci[[2]])
      #class(stat.test) <- "htest"

    }

    if(esType=="f"){
      test$parameter1 <- df1
      test$parameter2 <- df2
    } else {
      test$parameter1 <- df1
    }



    if(!(cnt%in%skip)){

      #if(cnt==27){var.lor <- sum(1/shafir.ori)}

      if(!is.na(test[1,1])){

        ESCI <- try.CATCH(any2any(testInfo  = test,
                                  df1       = df1,
                                  df2       = df2,
                                  N         = N,
                                  n1        = n1,
                                  n2        = n2,
                                  esType    = esType,
                                  var.lor   = var.lor,
                                  CL        = .95,
                                  keepSign    = TRUE,
                                  alternative = ifelse(is.null(test$alternative),
                                                       ML2.ori$stat.params[[4]],
                                                       as.character(test$alternative)))
                          )
      }

    } else {
      ESCI <- NA
    }

    if(is.null(ESCI$warning)|all(grepl("simpleWarning",ESCI$warning),
           !grepl("Error", ESCI$value[[1]]),
           !grepl("message", names(unlist(ESCI))[1]),
           !is.na(test[1,1]))){
      ESCI  <- ESCI$value
      es.id <- which(colnames(ESCI)%in%"(ESCI.r|r)+")
    } else {
      ESCI  <- test
      es.id <- 1
    }


    # Add columns for correlation comparison ES
    ESCI$cohensQ   <- NA
    ESCI$cohensQ.l <- NA
    ESCI$cohensQ.u <- NA
    ESCI$bootR1    <- NA
    ESCI$bootR2    <- NA
    ESCI$bootCI.l  <- NA
    ESCI$bootCI.u  <- NA

    if(esType=="Z.f"){

      if(stat.test$method=="Fisher r-to-Z transformed test for difference between 2 independent correlations"){
        ID <- grep("d",colnames(ESCI))[1]
        if(!is.na(ID)%00%NA){ESCI[,ID:NCOL(ESCI)] <- NA}
        # ESCI$bootR1    <- test$fZ.bootR1
        # ESCI$bootR2    <- test$fZ.bootR2
        # ESCI$bootCI.l  <- test$fZ.bootcCI.l
        # ESCI$bootCI.u  <- test$fZ.bootcCI.u
        # ESCI$r         <- ESCI$r
        # ESCI$l.r       <- ESCI$l.r
        # ESCI$u.r       <- test$fZ.u.r
      }
      ESCI$ncp       <- test$statistic
      ESCI$ncp.lo    <- test$ncp.lo
      ESCI$ncp.hi    <- test$ncp.hi
      ESCI$cohensQ   <- tmp$effect.size
      ESCI$cohensQ.l <- tmp$effect.size.ci[[1]]
      ESCI$cohensQ.u <- tmp$effect.size.ci[[2]]
    }



    ML2.ori$orig.stat.ncp[s]     <- ESCI$ncp%00%NA
    ML2.ori$orig.stat.ncp.ciL[s] <- ESCI$ncp.lo%00%NA
    ML2.ori$orig.stat.ncp.ciU[s] <- ESCI$ncp.hi%00%NA
    #ML2.ori$orig.stat.p.value
    ML2.ori$orig.ES.d[s]         <- ESCI$d%00%NA
    ML2.ori$orig.ES.d.ciL[s]     <- ESCI$l.d%00%NA
    ML2.ori$orig.ES.d.ciU[s]     <- ESCI$u.d%00%NA
    ML2.ori$orig.ES.r[s]         <- ESCI$r%00%NA
    ML2.ori$orig.ES.r.ciL[s]     <- ESCI$l.r%00%NA
    ML2.ori$orig.ES.r.ciU[s]     <- ESCI$u.r%00%NA


    out[[cnt]] <- data.frame(
      study.id      = study.id,
      study.slate   = study.slate,
      study.name    = study.name,
      study.analysis=study.analysis,
      testInfo  = test[1:2],
      df1 = df1,
      df2 = df2,
      N   = N,
      n1  = n1,
      n2  = n2,
      esType = esType,
      test$alternative,
      ESCI = ESCI)
  }

  df<-plyr::ldply(out)

  return(list(oriFULL   = df,
              masterKey = ML2.ori)
  )
}


#' get.analyses
#'
#' @param studies    Numeric vector with unique study IDs listed in the `masteRkey` table (default = all IDs).
#' @param analysis.type An optional number indicating Global (1), Primary (2, default) or Secondary (3) analyses.
#' @param Nmin.raw     Minimum raw sample size allowed to be included in the analyses.
#' @param Nmin.cond     Minimum sample size per condition allowed to be included in the analyses.
#' @param subset  Can be "all","WEIRD" or "NONWEIRD"
#' @param onlineTables  Download most recent online versions of \code{masteRkey} and \code{Source} information.
#' @param staticData   Use data from external directory (see argument \code{indir})
#' @param saveRdata Save data in `R` format. If `TRUE` a list object is expected with two fieldnames: `R_OBJECTS` and `RDS_RESULTS`, one for storing a seperate Robject for each analysis conducted, the other for storing a single data image containing the output returned by the function. An empty character vector will prevent saving any objects
#' @param rootdir   which dir to look for data and files
#' @param data.names   names of the datafiles for slate 1 and slate 2 (change e.g. if you study a custom subsample)
#' @param indir   List of directories to use for different files. Change if you have files in different locations, but keep the list structure!
#' @param outdir   List of directories to use for outputting different result files. Change if you want to save in a different location, but keep the list structure!
#'
#' @return A list object with analysis results.
#' @export
#'
#' @family "get." functions
#'
#' @details Run analyses for (selected) ML2 studies.
#'
#' @examples
#' # Get the data from analysis 1 [Huang.1] listed on the \href{masteRkey spreadsheet}{https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/}
#'
#' library(dplyr)
#' df <- get.analyses(studies = 1, analysis.type = 1)
#'
#' # 'raw' pre-filter dataset
#' head(tbl_df(df$raw.case$Huang.1))
#'
#' # analysis results
#' glimpse(tbl_df(df$merged.results$Huang.1))
#'
get.analyses <- function(studies       = NA,
                         analysis.type = NA,
                         Nmin.raw      = 30,
                         Nmin.cond     = 15,
                         subset        = c("all","WEIRD","NONWEIRD")[1],
                         onlineTables  = TRUE,
                         staticData    = TRUE,
                         saveRdata     = FALSE,
                         rootdir   = normalizePath(paste0(find.package("manylabRs"))),
                         indir     = list(RAW_DATA    = ifelse(staticData,file.path(rootdir,"extdata","RAW_DATA"),""),
                                          MASTERKEY   = ifelse(onlineTables,"",file.path(rootdir,"extdata","KEYTABLES")),
                                          SOURCE_INFO = ifelse(onlineTables,"",file.path(rootdir,"extdata","KEYTABLES"))),
                         outdir    = list(R_OBJECTS   = ifelse(saveRdata, file.path(rootdir,"extdata","R_OBJECTS"),""),
                                          RDS_RESULTS = ifelse(saveRdata, file.path(rootdir,"extdata","RESULTS_RDS"),"")),
                         data.names = list(Slate1 = ifelse(staticData,file.path(rootdir,"extdata","RAW_DATA","ML2_Rawdata_S1.RData"),"ML2_S1"),
                                           Slate2 = ifelse(staticData,file.path(rootdir,"extdata","RAW_DATA","ML2_Rawdata_S2.RData"),"ML2_S2"))){

  # require(lme4)
  # require(lmerTest)
  # paths <- unique(c(paste0(rootdir,"/",indir), paste0(rootdir,"/",outdir)))
  # for(d in paths[!dir.exists(paths)]){
  #   dir.create(d, showWarnings = TRUE, recursive = FALSE, mode = "0777")
  # }

  tp  <- analysis.type
  wop <- options(warn=-1, expressions=10000)

  # Load Key Table

  if(indir$MASTERKEY==""|onlineTables){
    ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
    disp(paste("Downloaded keytable Googlesheet: ML2_masteRkey [https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/]"), header = "get.analyses", footer = FALSE)
  } else {
    ML2.key <- openxlsx::read.xlsx(file.path(normalizePath(indir$MASTERKEY),"ML2_masteRkey.xlsx"))
    disp(paste0("Loaded keytable from disk: ML2_masteRkey.xlsx [",normalizePath(indir$MASTERKEY),"]"), header = "get.analyses", footer = FALSE)
  }
  ML2.key <- ML2.key[!is.na(ML2.key$unique.id),]

  # Load data
  if(indir$RAW_DATA==""){
    #ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df
    # data("ML2_S1","ML2_S2")
    # S1loaded <-is.data.frame(ML2_S1)
    # S2loaded <-is.data.frame(ML2_S2)
    ML2_S1 <- get.OSFfile()
    ML2_S2 <- get.OSFfile()
    disp(paste("Downloaded data from OSF: ",data.names$Slate1," and ",data.names$Slate2,""), header = FALSE, footer = FALSE)

  } else {
    load(normalizePath(data.names$Slate1))
    load(normalizePath(data.names$Slate2))
    disp("Using data in /manylabRs/extdata/...", header = FALSE, footer = FALSE)
    #disp(paste0("Loaded data from disk: ",data.names$Slate1," and ",data.names$Slate1,"[",file.path(rootdir,indir$RAW.DATA),"]"), header = FALSE, footer = FALSE)
  }

  # Load information about sources
  if(indir$SOURCE_INFO==""|onlineTables){
    SourceInfoTable    <- get.GoogleSheet(url = "https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/pub?gid=1435507167&single=true&output=csv")$df
    disp(paste("Downloaded information about the data sources from Googlesheet: 'ML2_SourceInfo.xlsx' [https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/]"), header = FALSE, footer = FALSE)
  } else {
    SourceInfoTable    <- openxlsx::read.xlsx(file.path(normalizePath(indir$SOURCE_INFO),"ML2_SourceInfo.xlsx"), sheet = "ML2_SourceInfo")
    disp(paste0("Loaded information about the data sources from disk: 'MML2_SourceInfo.xlsx' [",file.path(rootdir,indir$SOURCEINFO),"]"), header = FALSE, footer = FALSE)
  }

  # Decide which analyses to run on which groups
  toRun  <- decide.analysis(ML2.key, studies, tp)

  # Prepare list objects
  ML2.data           <- vector("list", length = length(ML2.key$study.analysis))
  names(ML2.data)    <- ML2.key$study.analysis
  ML2.output         <- vector("list", length = length(ML2.key$study.analysis))
  names(ML2.output)  <- ML2.key$study.analysis
  ML2.rawdata        <- vector("list", length = length(ML2.key$study.analysis))
  names(ML2.rawdata) <- ML2.key$study.analysis
  #ML2.descriptivesDump <- list()

  studs <- toRun$studiess
  cnt   <- 0

  # START STUDIES ----------------------------------

  for(s in studs){

    # Get the correct slate according to info in ML2.key['study.slate']
    if(ML2.key[s,'study.slate'] == 1){ML2.df <- ML2_S1
    } else {
      ML2.df <- ML2_S2
    }

    # Add a unique ID
    ML2.df$uID = seq(1, nrow(ML2.df))

    # Get info to create a dataset for the current study
    # keytable <- ML2.key[s,]
    ML2.in <- get.info(ML2.key[s, ], colnames(ML2.df), subset)


    # Generate chain to select variables for the data frame and create a filter chain for the variables to use for analysis
    # Info based on KeyTable information in study.vars, cases.include, site.include, params.NA
    ML2.id <- get.chain(ML2.in)

    # Apply the df chain to select relevant subset of variables
    ML2.df <- eval(parse(text=paste("ML2.df", ML2.id$df)))


    if(NROW(ML2.df)>0){

      ML2.df$study.order <- NA
      stmp <- strsplit(ML2.df$StudyOrderN,"[|]")

      Stud <- ML2.key$study.name[[s]]
      if(Stud%in%"Tversky"){Stud <- "Tversky.Gati"}
      if(Stud%in%"Rottenstreich"){Stud <- "Rottenstrich"}
      if(Stud%in%"Ross"&(ML2.key[s,'study.slate'] == 1)){Stud <- "Ross.Slate1"}
      if(Stud%in%"Ross"&(ML2.key[s,'study.slate'] == 2)){Stud <- "Ross.Slate2"}
      if(Stud%in%"vanLange"){Stud <- "VanLange"}
      if(Stud%in%"Giessner"){Stud <- "Geissner"}

      ML2.df$study.order <- plyr::laply(seq_along(stmp), function(o){which(grepl(Stud,stmp[[o]]))%00%NA})

      # Loop over groups within study
      tp           <- toRun$tp
      ML2.sr       <- list()
      ML2.var      <- list()
      outputSource <- list()
      dataSource   <- list()
      raw.df       <- list()
      clean.df     <- list()
      testVarEqual <- ML2.in$stat.params$var.equal


      # runGroups <- sort(na.exclude(unique(ML2.df$study.order)))

      cnt          <- cnt + 1

      # Loop over sites in runGroups within a study
      if(tp[cnt]==1){
        runGroups <- "all"
      } else {
        runGroups <- sort(na.exclude(unique(ML2.df[[toRun$ugroup]])))
      }

      disp(paste(s, ML2.key$study.analysis[[s]],"- START"), header = toupper(ML2.key$study.analysis[[s]]), footer = FALSE)
      cat("\n")

      # START GROUPS ----------------------------------------------


      for(g in seq_along(runGroups)){

        listIT     <- FALSE
        nMin1      <- FALSE
        nMin2      <- FALSE
        compN <- compN1 <- compN2 <- 0

        if(tp[cnt]<4){
          if(runGroups[g]=="all"){
            gID <- rep(TRUE, nrow(ML2.df))
          } else {
              gID <- ML2.df$source%in%runGroups[g]
              }
        } else {
          gID <-  ML2.df$study.order%in%runGroups[g]
        }

        # Check nMin
        if(sum(gID, na.rm=TRUE) >= Nmin.raw){
          nMin1 <- TRUE
          # Get a list containing the data frames to be used in the analysis
          ML2.sr[[g]] <- get.sourceData(ML2.id, ML2.df[gID, ], ML2.in)
        }

        # Double-check nMin
        if(nMin1){
          compN  <- ML2.sr[[g]]$N
          compN1 <- sum(ML2.sr[[g]]$RawDataFilter[[1]]$Included, na.rm = TRUE)
          compN2 <- sum(ML2.sr[[g]]$RawDataFilter[[2]]$Included, na.rm = TRUE)
          if(any(compN >= Nmin.raw)&(all(compN1>=Nmin.cond, compN2>=Nmin.cond))){nMin2 <- TRUE}
        }

        # START ANALYSIS ----------------------------------------

        if(all(nMin1,nMin2)){

          # Organize and Calculate variables for the analysis using function according to ML2.info: 'stat.vars'
          ML2.var[[g]] <- eval(parse(text=paste0(ML2.key[s,'stat.vars'],'(ML2.sr[[g]])',collapse="")))

          # Check equal variance assumption
          if(!is.na(testVarEqual)){
            if(testVarEqual){
              logtxt <- paste(s,ML2.key$study.analysis[[s]],'-', runGroups[g])
              ML2.in$stat.params$var.equal <- decide.EqualVar(ML2.var[[g]],ML2.in$study.vars.labels, ML2.key[s, ], group = logtxt)
            }}

          # Run the analysis according to ML2.key: 'stat.test'
          stat.params <<- ML2.in$stat.params
          stat.test   <- try.CATCH(with(ML2.var[[g]],eval(parse(text = ML2.key$stat.test[[s]]))))

          # if(grepl("approximation", stat.test$warning)){stat.test$warning <- NULL}

          if(all(is.null(stat.test$warning), grepl("simpleWarning",stat.test$warning),
                 !grepl("Error", stat.test$value[[1]]),
                 !grepl("message", names(unlist(stat.test))[1]))){
            stat.test  <- stat.test$value
            ConsoleOut <- paste(capture.output(print(stat.test)),collapse="\n")
            listIT     <- TRUE
          }

          if(listIT){

          describe <- get.descriptives(stat.test = stat.test,
                                       vars      = ML2.var[[g]],
                                       keytable  = ML2.key[s,])

          if(any(describe$descr.raw$n<Nmin.cond)){
            listIT<- FALSE
            nMin2 <- FALSE
          }

          rm(describe)

          }

          # START RECORD DATA -------------------------------------

          if(listIT){

            describe <- get.descriptives(stat.test = stat.test,
                                         vars      = ML2.var[[g]],
                                         keytable  = ML2.key[s,])

            var.lor <- ifelse(grepl("OR",describe$test$estype),
                              sum(1/(table(ML2.var[[g]]$Condition,ML2.var[[g]]$Response)), na.rm = TRUE),
                              NA)

            ESCI  <-   generateOutput(describe        = describe,
                                      var.lor         = var.lor,
                                      runningGroup    = runGroups[g],
                                      runningAnalysis = paste(s,ML2.key$study.analysis[[s]]))

            #colnames(describe$test) <- gsub("fZ[.]","",colnames(describe$test))

            # Raw and clean datasets

            if(length(ML2.sr[[g]]$RawDataFilter)>1){
              case.include <- ML2.sr[[g]]$RawDataFilter[[1]]$Included|ML2.sr[[g]]$RawDataFilter[[2]]$Included
              df1 <- ML2.sr[[g]]$RawDataFilter[[1]][ ,-which(colnames( ML2.sr[[g]]$RawDataFilter[[1]])=="Included")]
              raw.df[[g]] <-  cbind.data.frame(df1, analysis.type = c("Global","Primary","Secondary","Order")[tp[cnt]],subset=subset,case.include = case.include)
            } else {
              case.include <- ML2.sr[[g]]$RawDataFilter[[1]]$Included
              df1 <- ML2.sr[[g]]$RawDataFilter[[1]][ ,-which(colnames( ML2.sr[[g]]$RawDataFilter[[1]])=="Included")]
              raw.df[[g]] <-  cbind.data.frame(df1, analysis.type = c("Global","Primary","Secondary","Order")[tp[cnt]],subset=subset,cases.include = case.include)
            }


            if(tp<4){

              if(runGroups[g]!="all"){

                fID <- unique(ML2.df$.id[ML2.df$source==runGroups[g]])
                sID <- SourceInfoTable$Source%in%runGroups[g]&SourceInfoTable$Filename%in%fID
                if(sum(sID)==1){

                  SourceInfo1 <- SourceInfoTable[sID, ]
                  SourceInfo2 <- raw.df[[g]] %>% dplyr::filter(case.include) %>% group_by(source) %>%
                    dplyr::summarise(
                      N.sources.global    = length(unique(Source.Global)),
                      N.sources.primary   = length(unique(Source.Primary)),
                      N.sources.secondary = length(unique(Source.Secondary)),
                      N.countries         = length(unique(Country)),
                      N.locations         = length(unique(Location)),
                      N.languages         = length(unique(Language)),
                      Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
                      Tbl.Execution       = paste0(capture.output(table(Execution)),collapse="\n"),
                      Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse="\n"),
                      Tbl.setting       = paste0(capture.output(table(Setting)),collapse="\n"),
                      Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse="\n"),
                      Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse="\n"),
                      N.studyorders1    = length(unique(StudyOrderN)),
                      N.IDiffOrderN     = length(unique(IDiffOrderN)),
                      N.uIDs            = length(unique(uID)),
                      N.studyorders2    = length(unique(study.order)),
                      Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse="\n"),
                      Tbl.subset        = paste0(capture.output(table(subset)),collapse="\n"),
                      N.cases.included  = sum(case.include, na.rm=TRUE),
                      N.cases.excluded  = sum(raw.df[[g]]$case.include==FALSE,na.rm=TRUE)
                    )
                  SourceInfo<-cbind(SourceInfo1,SourceInfo2)
                  # colnames(SourceInfo) <- c("name","name.Global",colnames(SourceInfoTable)[3:NCOL(SourceInfoTable)])
                }

              } else { # tp < 4 & rungroups == all

                SourceInfo <- raw.df[[g]] %>% dplyr::filter(case.include) %>%
                  dplyr::summarise(
                    N.sources.global    = length(unique(Source.Global)),
                    N.sources.primary   = length(unique(Source.Primary)),
                    N.sources.secondary = length(unique(Source.Secondary)),
                    N.countries         = length(unique(Country)),
                    N.locations         = length(unique(Location)),
                    N.languages         = length(unique(Language)),
                    Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
                    Tbl.Execution       = paste0(capture.output(table(Execution)),collapse="\n"),
                    Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse="\n"),
                    Tbl.setting       = paste0(capture.output(table(Setting)),collapse="\n"),
                    Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse="\n"),
                    Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse="\n"),
                    N.studyorders1    = length(unique(StudyOrderN)),
                    N.IDiffOrderN     = length(unique(IDiffOrderN)),
                    N.uIDs            = length(unique(uID)),
                    N.studyorders2    = length(unique(study.order)),
                    Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse="\n"),
                    Tbl.subset        = paste0(capture.output(table(subset)),collapse="\n"),
                    N.cases.included  = n(),
                    N.cases.excluded  = sum(raw.df[[g]]$case.include==FALSE,na.rm=TRUE)
                  )
              }
            } else { # tp < 4

              SourceInfo <- raw.df[[g]] %>% dplyr::filter(case.include) %>%
                dplyr::summarise(
                  N.sources.global    = length(unique(Source.Global)),
                  N.sources.primary   = length(unique(Source.Primary)),
                  N.sources.secondary = length(unique(Source.Secondary)),
                  N.countries         = length(unique(Country)),
                  N.locations         = length(unique(Location)),
                  N.languages         = length(unique(Language)),
                  Pct.WEIRD           = mean(Weird, na.rm=TRUE)*100,
                  Tbl.Execution       = paste0(capture.output(table(Execution)),collapse="\n"),
                  Tbl.subjectpool     = paste0(capture.output(table(SubjectPool)),collapse="\n"),
                  Tbl.setting       = paste0(capture.output(table(Setting)),collapse="\n"),
                  Tbl.Tablet        = paste0(capture.output(table(Tablet)),collapse="\n"),
                  Tbl.Pencil        = paste0(capture.output(table(Pencil)),collapse="\n"),
                  N.studyorders1    = length(unique(StudyOrderN)),
                  N.IDiffOrderN     = length(unique(IDiffOrderN)),
                  N.uIDs            = length(unique(uID)),
                  N.studyorders2    = length(unique(study.order)),
                  Tbl.analysistype  = paste0(capture.output(table(analysis.type)),collapse="\n"),
                  Tbl.subset        = paste0(capture.output(table(subset)),collapse="\n"),
                  N.cases.included  = n(),
                  N.cases.excluded  = sum(raw.df[[g]]$case.include==FALSE,na.rm=TRUE)
                )
            }

            rownames(SourceInfo) <- NULL

            test  <- describe$test
            descr <- describe$descr.raw
            outputSource[[g]] <- get.output(key      = ML2.key[s,],
                                            vars     = ML2.var[[g]],
                                            descr    = descr,
                                            group    = runGroups[g],
                                            analysis = c("Global","Primary","Secondary","Order")[tp[cnt]],
                                            varEqual = stat.params$var.equal,
                                            test     = test,
                                            ESCI     = ESCI,
                                            test.ConsoleOutput = ConsoleOut,
                                            SourceInfo = SourceInfo,
                                            stat.test = stat.test)

            # Data list for output to spreadsheet
            dataSource[[g]] <- list(
              study.id      = ML2.key$study.id[[s]],
              study.slate   = ML2.key$study.slate[[s]],
              study.name    = ML2.key$study.name[[s]],
              study.source  = runGroups[g],
              analysis.type = c("Global","Primary","Secondary","Order")[tp[cnt]],
              analysis.name = ML2.key$study.analysis[[s]],
              subset        = subset,
              stat.info     = ML2.in,
              stat.data.cleanchain = ML2.id,
              stat.data.raw       = raw.df[[g]],
              stat.data.cleaned   = ML2.sr[[g]][1:length(ML2.sr[[g]])-1],
              stat.data.analysed  = ML2.var[[g]][1:length(ML2.var[[g]])-1],
              stat.test = stat.test)

            suppressMessages(clean.df[[g]] <- plyr::ldply(dataSource[[g]]$stat.data.analysed,reshape2::melt))
            colnames(clean.df[[g]])[colnames(clean.df[[g]])==".id"] <- "Condition"

            rm(stat.params)
          } else { # LISTIT
            cat("\nListIT = FALSE\n")
            if(!is.null(stat.test$value)){

            if(grepl("observations",as.character(stat.test$value))){
              disp(paste(s, ML2.key$study.analysis[[s]],'-',
                         runGroups[g],'>> Not enough observations'),
                   header = FALSE, footer = FALSE)}
            } else {
              disp(paste(s,ML2.key$study.analysis[[s]],'-', runGroups[g],'>> stat.test failed:'),
                   header = FALSE, footer = FALSE)
              # disp(paste('value: ',stat.test$value),
              #      header = FALSE, footer = FALSE)
              disp(paste('warning:',stat.test$warning),
                   header = FALSE, footer = FALSE)
            }
            ConsoleOut <- paste(gsub("[[:punct:]]", "", stat.test$warning, perl = TRUE), collapse="\n")
            NN <- lengths(ML2.var[[g]])
            NN <- NN[!names(NN)=="N"]
            N  <- rep(ML2.var[[g]]$N,length.out = length(NN))
            ML2.rnd <- plyr::llply(seq_along(NN), function(nn) rnorm(N[nn])) #eval(parse(text=paste(names(NN[nn])," = rnorm(101)"))))
            names(ML2.rnd) <- names(NN)
            stat.test  <- try.CATCH(with(ML2.var[[g]], eval(parse(text = ML2.key$stat.test[[s]]))))
            stat.test  <- stat.test$value
            #RANDOMdata <- TRUE
            # Analysis error, there may be descriptives, but set ESCI to NA
            ESCI[1:length(ESCI)] <- rep(NA,length(ESCI))
          }#Listit = FALSE
        } # all nMin 1,2

        # Report on errors

        if(!nMin1){
          disp(paste0(s,' ',ML2.key$study.analysis[[s]],' - ',
                      runGroups[g],' not included in results >> Cases in source file (',
                      sum(gID, na.rm = TRUE),') < Nmin.raw (',Nmin.raw,')'),
               header = FALSE, footer = FALSE)
        } # Check nMin 1}
        if(!nMin2){
          disp(paste0(s,' ',ML2.key$study.analysis[[s]],' - ',
                      runGroups[g],' not included in results >> Valid cases after varfun (n',
                      c(1,2)[compN < Nmin.cond],"=", compN[compN < Nmin.cond],') < Nmin.cond (',Nmin.cond,')'),
               header = FALSE, footer = FALSE)
        } # Double-check nMin

      } # iterate groups

      disp(paste(s, ML2.key$study.analysis[[s]],"- COMPLETED"), header = FALSE)

      ML2.output[[s]]  <- plyr::ldply(outputSource)
      ML2.rawdata[[s]] <- plyr::ldply(raw.df)

      if(outdir$R_OBJECTS!=""){
        save(dataSource, file = file.path(normalizePath(outdir$R_OBJECTS),paste0(ML2.key$study.analysis[[s]],"_",c("Global","Primary","Secondary","Order")[tp[cnt]],".RData")))
      }

      rm(ML2.in, ML2.var, ML2.id, ML2.df, ML2.sr, outputSource, dataSource, raw.df, clean.df, descr, SourceInfo, nMin1, nMin2, listIT)

    } else { # if nrow > 0

      disp(paste(s, ML2.key$study.analysis[[s]],"- SKIPPED"), header = FALSE)

      ML2.output[[s]]  <- NULL
      ML2.rawdata[[s]] <- NULL

      rm(ML2.in, ML2.var, ML2.id, ML2.df, ML2.sr)
    }

  } # for s i studies

  if(outdir$RDS_RESULTS!=""){

  fname <- c(file.path(normalizePath(outdir$RDS_RESULTS),paste0("ML2_results_global_",subset,".rds")),
             file.path(normalizePath(outdir$RDS_RESULTS),paste0("ML2_results_primary_",subset,".rds")),
             file.path(normalizePath(outdir$RDS_RESULTS),paste0("ML2_results_secondary_",subset,".rds")),
             file.path(normalizePath(outdir$RDS_RESULTS),paste0("Data_Figure_StudyOrder_",subset,".rds")))[tp]

  saveRDS(list(raw.case   = ML2.rawdata,
               aggregated = ML2.output),file=fname)

  disp(message = paste0("Saved RDS list object with data and ananlysis results to:\n",fname))
  }

  options(wop)
  return(list(raw.case   = ML2.rawdata,
              aggregated = ML2.output)
  ) #), descriptivesDump = ML2.descriptivesDump))
}


#' get.GoogleSheet
#'
#' @param url    Hyperlink to the GoogleSheet, ending in command \code{".../export?format=csv"}.
#' @param data    If no URL is provided, which dataset? (default = "ML2masteRkey").
#' @param dfCln    Should the variable names be cleaned (replace spaces and punctuation by a period "."). Default is \code{FALSE}.
#' @param Sep     Symbol to use when changing column names (default: ".").
#'
#' @family "get." functions
#'
#' @return A list object with fields:
#'
#' \itemize{
#' \item Returned if \code{dataSet = TRUE}  (default):
#' \itemize{
#' \item \code{df}:  A data table generated by \code{\link{tbl_df}} from package \code{dplyr}.
#' \item \code{info}: Information about the downloaded file including a time stamp, the URL and original row and column names.
#' }
#' \item Returned if \code{dataSet = FALSE}:
#' \itemize{
#' \item \code{FilePath}: The local path to the downloaded file.
#' }
#' }
#' @export
#'
get.GoogleSheet <- function(url=NULL,data=c('ML1data','ML2masteRkey','ML2data')[2],dfCln=FALSE,Sep = "."){
  if(is.null(url)){
    switch(data,
           ML1data        = url <- 'https://docs.google.com/spreadsheets/d/19ay71M8jiqIZhSj3HR0vqaJUwAae1QHzBybjBu5yBg8/export?format=csv',
           ML2masteRkey   = url <- 'https://docs.google.com/spreadsheets/d/1fqK3WHwFPMIjNVVvmxpMEjzUETftq_DmP5LzEhXxUHA/export?format=csv',
           ML2data        = url <- ''
    )}
  # GET(url) will only get 100 rows, thanks to Sacha Epskamp for this "complete scrape" code.
  tmp  <- tempfile()
  info <- httr::GET(url, httr::write_disk(tmp, overwrite = TRUE))
  df   <- dplyr::tbl_df(read.csv(tmp,  stringsAsFactors = FALSE, header = TRUE)) #import(tmp, format = "csv", stringsAsFactors = FALSE, header = TRUE))
  if(dfCln==TRUE){
    df   <- df.Clean(df)
  } else {
    df$df  <- df
    df$nms <- NA
    df$rws <- NA
  }

  return(list(df = df$df,
              info = list(Info=info,
                          GoogleSheet.colnames=dplyr::tbl_df(data.frame(ori.colnames=df$nms)),
                          GoogleSheet.rownames=dplyr::tbl_df(data.frame(ori.rownames=df$rws))))
  )
}

df.Clean <- function(df,Sep="."){
  #  require(dplyr)
  nms.ori   <- colnames(df)
  rws.ori   <- rownames(df)

  # Change punctuation and blankss in variable names to points
  nmsP  <- gsub("([[:punct:]]|[[:blank:]])+","+",nms.ori)
  nmsPP <- gsub("(^[+]|[+]$)+","",nmsP)
  nmsPP <- gsub("[+]",Sep,nmsPP)
  # Check for double names
  ifelse(length(unique(nmsPP))==length(nmsPP),{nms <- nmsPP},{
    id2 <- which(plyr::laply(nmsPP,function(n) sum(nmsPP%in%n))>1)
    nms <- nmsPP
    nms[id2] <- paste(nmsPP[id2],id2,sep=".")})

  colnames(df) <- nms
  df      <- dplyr::select(df,which(nms%in%nms[nms!=""]))
  return(list(df=df,
              nms=nms.ori,
              rws=rws.ori)
  )
}

#' get.OSFfile
#'
#' @param code    Either a full url ("https://osf.io/XXXXX/"), or just the OSF code.
#' @param dir   Output location (default is \code{tempdir()}).
#' @param scanMethod     Either \code{readLines} or \code{RCurl}. Leave missing to choose automatically.
#' @param downloadMethod    One of \code{httr} (default), \code{downloader} or \code{curl}.
#' @param dataSet     Is the file data set which can be imported using \code{\link{import}} from package \code{rio}?
#' @param dfCln    Should the variable names be cleaned (replace spaces and punctuation by a period "."). Default is \code{FALSE}.
#'
#' @author Fred Hasselman, based on code by Sasha Epskamp
#' @family "get." functions
#'
#' @details Function to download a file hosted on OSF. Modified from code originally written by Sacha Epskamp.
#' @return A list object with fields:
#' \itemize{
#' \item Returned if \code{dataSet = TRUE}  (default):
#' \itemize{
#' \item \code{df}:  A data table generated by \code{\link{tbl_df}} from package \code{dplyr}.
#' \item \code{info}: Information about the downloaded file including a time stamp, the URL and original row and column names.
#' }
#' \item Returned if \code{dataSet = FALSE}:
#' \itemize{
#' \item \code{FilePath}: The local path to the downloaded file.
#' }
#' }
#' @export
#'
#' @examples
#' #Get the RP:P data hosted on OSF.
#' \dontrun{dfRPP <- get.OSFfile(code='https://osf.io/fgjvw/', dfCln=TRUE)$df}
get.OSFfile <- function(code, dir = tempdir(), scanMethod, downloadMethod = c("httr","downloader","curl"), dataSet = TRUE, dfCln = FALSE){

  # Check if input is code:
  if (!grepl("osf\\.io",code)){
    URL <- sprintf("https://osf.io/%s/",code)
  } else URL <- code

  # Scan page:
  if (grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE)){
    try(setInternet2(TRUE))
  }

  if (missing(scanMethod)){
    scanMethod <- ifelse(grepl("Windows",Sys.info()[['sysname']],ignore.case=TRUE), "readLines", "RCurl")
  }
  if (scanMethod == "readLines"){
    Page <- paste(readLines(URL),collapse="\n")
  } else if (scanMethod == "RCurl"){
    Page <- RCurl::getURL(URL)
  } else if (scanMethod == "httr"){
    Page <- httr::GET(URL)
    Page <- paste(Page,collapse="\n")
  } else stop("Invalid scanMethod")

  # Create download link:
  URL <- gsub("/$","",URL)
  #   Link <- paste0(URL,"/?action=download&version=1")
  Link <- paste0(URL,"/?action=download")

  # Extract file name:
  FileName <- regmatches(Page,gregexpr("(?<=\\<title\\>OSF \\| ).*?(?=\\</title\\>)", Page, perl=TRUE))[[1]]
  FullPath <- paste0(dir,"/",FileName)

  info <- NULL
  # Download file:
  if (downloadMethod[[1]]=="httr"){
    info <- httr::GET(Link, httr::write_disk(FullPath, overwrite = TRUE))
  } else if (downloadMethod[[1]]=="downloader"){
    downloader::download(Link, destfile = FullPath, quiet=TRUE)
  } else if (downloadMethod[[1]]=="curl"){
    system(sprintf("curl -J -L %s > %s", Link, FullPath), ignore.stderr = TRUE)
  }  else stop("invalid downloadMethod")

  df <- NULL
  if(dataSet==TRUE){
    suppressWarnings(df <- rio::import(FullPath,setclass = "tbl_df"))
    if(dfCln==TRUE){df <- df.Clean(df)} else {df$df <- df}

    return(list(df   = df$df,
                info = list(FilePath=FullPath,
                            Info=info,
                            ori.Colnames= dplyr::tbl_df(data.frame(ori.colnames=df$nms)),
                            ori.Rownames= dplyr::tbl_df(data.frame(ori.rownames=df$rws))
                ))
    )
  } else {
    # Return location of file:
    return(FilePath=FullPath)
  }
}


#' get.CSVdata
#'
#' @param path    Path to the data.
#' @param files    A list of \code{.csv} / \code{.xlsx} files containing raw ML2 data.
#' @param finishedOnly    Only import cases with value of variable \code{Finished = 1} (default).
#'
#'
#' @export
get.CSVdata <- function(path = NA, fID, finishedOnly=TRUE){
  if(!is.na(path)){
  files <- file.path(path,fID)
  } else {
    files <- fID
  }

  if(grepl(".xlsx",files)){
    temp <- rio::import(files, sheet=1, format = "xlsx", setclass = "tbl_df")
    colnames(temp)[1:10] <- temp[1,1:10]
    df <- dplyr::slice(temp,-1)
  } else {
    t1 <- scan(files,sep=",",nlines = 1,what="character", quiet = T)
    t2 <- scan(files,sep=",",skip = 1, nlines = 1,what="character", quiet = T)
    t1[1:10] <- t2[1:10]
    df <- try.CATCH(dplyr::tbl_df(read.csv(files,skip=2,header=F,stringsAsFactors=F,col.names=t1)))
    ifelse(is.data.frame(df$value), {df <- df$value}, {df <- dplyr::tbl_df(read.csv(files,skip = 2,header = F,stringsAsFactors = F,col.names = t1, fileEncoding="latin1"))})
  }

  if(finishedOnly){
    # Remove cases that did not complete the study
    df <- df %>% dplyr::filter(Finished == 1)
  }
  #df$.id <- fID
  return(df)
}

#' get.Order
#'
#' @param df    A ManyLabs2 data frame.
#' @param S1    Are the data from slate1 (default) or slate2?
#'
#' @author Fred Hasselman
#' @family "get." functions
#'
#' @return A list object with fields:
#' \itemize{
#' \item \code{df}: ManyLabs2 data frame in which the Qualtrics study order has been added to each case.
#' \item \code{Problems}: Cases for which the study order information could not be retrieved.
#' }
#'
get.Order <- function(df, S1=TRUE){
  #   require(plyr)
  ifelse(S1,{
    url <-  "https://docs.google.com/spreadsheets/d/1al8b5nv9AoNdOOlI-RfXf5bCTJY2ophdUIiYhCFGegI/pub?gid=269287357&single=true&output=csv"
    cols <- 3:15
  },{
    url <- "https://docs.google.com/spreadsheets/d/1AvJguRhN8i7MsbcjnGtEnNZ9b-LNTNKINSoHm8Z2f28/pub?gid=1370595041&single=true&output=csv"
    cols <- 3:17
  }
  )
  df.Order <- get.GoogleSheet(url = url)$df
  ProblemID <- list()
  cnt = 0

  for(i in 1:nrow(df)){
    if(!nzchar(df$StudyOrder[i]%00%0)){
      cnt = cnt + 1
      #             if(df$IDiffOrder[i]==""){
      #                 df$StudyOrderN[i] <- NA
      #                 df$IDiffOrderN[i] <- NA
      #                 Problem = paste("No study order strings in",df$StudyOrder[i],"and",df$IDiffOrder[i])
      #             } else {
      #                 if(!nzchar(df$StudyOrder[i])){df$StudyOrderN[i] <- NA}
      #                 if(df$IDiffOrder[i]==""){
      #                     df$IDiffOrderN[i] <- NA
      #                     Problem = paste("Wrong study order var? Ind.Diff var",df$IDiffOrder[i],"is empty")
      #                 }
      #             }
      #             ProblemID[[cnt]] <- cbind(rowNum     = i,
      #                                       fileName   = df$.id[i],
      #                                       ResponseID = df$ResponseID[i],
      #                                       Problem    = Problem,
      #                                       StudyOrder = df$StudyOrder[i],
      #                                       IDiffOrder = df$IDiffOrder[i])
    } else {
      OrderString <- unlist(strsplit(x = df$StudyOrder[i], split = "[|]"))
      ls   <- list()
      Problem.s <- list()
      cnt2 <- 0
      for(s in 1:length(OrderString)){
        ls[[s]] <- colnames(df.Order)[cols][df.Order[df.Order$Filename%in%df$.id[i], cols] %in% OrderString[[s]]]
        if(length(ls[[s]])==0){
          cnt2 = cnt2 + 1
          if(OrderString[[s]]%in%df.Order[df.Order$Filename%in%df$.id[i], cols]){
            Problem.s[[cnt2]] <- paste(OrderString[[s]],"(", colnames(df.Order)[cols][df.Order[df.Order$Filename%in%df$.id[i], cols] == OrderString[[s]]], ")")
            ls[[s]] <- NA
          } else {
            Problem.s[[cnt2]] <- paste(OrderString[[s]], "( mismatch )")
          }
        }
      }

      #             if(cnt2!=0){
      #                 cnt = cnt + 1
      #                 ProblemID[[cnt]] <- cbind(rowNum     = i,
      #                                           fileName   = df$.id[i],
      #                                           ResponseID = df$ResponseID[i],
      #                                           Problem    = paste(unlist(Problem.s), collapse="|"),
      #                                           StudyOrder = paste(unlist(ls) ,collapse="|"),
      #                                           IDiffOrder = df$IDiffOrder[i])
      #             }

      df$StudyOrderN[i] <- paste(unlist(ls) ,collapse="|")

    }

    df$IDiffOrderN[i] <- df$IDiffOrder[i]

  }

  return(list(df = df,
              Problems = plyr::ldply(ProblemID))
  )

}


#' get.zavCode
#'
#' @param df    Dataset: ML2_S2
#' @param lookup     The lookup table
#'
#' @return The code for each sentence.
#' @export
#'
#' @family "get." functions
#'
get.zavCode <- function(df = NULL, lookup = NULL){
  # Generate variable names.
  varsCOLD <- paste0("zav1.",1:13)
  varsHOT  <- paste0("zav2.",1:13)
  # Decide wether case took HOT or COLD priming condition.
  if(all(df[,varsCOLD] == "")){prime <- varsHOT} else {prime <- varsCOLD}
  # For each sentence, check whether it was correctly unscrambled
  code <- numeric()
  for(v in seq_along(prime)){
    # Search only options corresponding to the Country and Language for this case.
    options <- lookup$value[(lookup$variable == prime[v]) & (lookup$Country == df$Country) & (lookup$Language == df$Language)] %in% df[prime[v]]
    # Return the code (0,1,2) for this sentence.
    rtrn <- na.exclude(lookup$Correct[(lookup$variable == prime[v]) & (lookup$Country == df$Country) & (lookup$Language == df$Language)][options])
    if(length(rtrn) == 0){code[v] <- NA} else {code[v] <- rtrn[1]}
  }
  # Add variables for this case.
  df[paste0("zav.code",1:13)] <- code
  df$zav.include.strict <- all(code == 1)
  df$zav.include.primed <- all(code > 0)
  if(all(df[varsCOLD] == "")){df$zav.condition <- "Hot"} else {df$zav.condition <- "Cold"}
  return(df)
}

#' get.descriptives
#'
#' @param stat.test Output of the analysis
#' @param vars Data used
#' @param keytable masteRkey
#'
#'
#' @export
#'
get.descriptives <- function(stat.test, vars, keytable){

  if(all(c("htest","fisherZ")%in%class(stat.test))){
    tmp <- unclass(stat.test)
    stat.test <- tmp[1:8]
    stat.test$estimate <- tanh(tmp$effect.size[[1]])
    stat.test$conf.int <- c(tanh(tmp$effect.size.ci[[1]]),tanh(tmp$effect.size.ci[[2]]))
    #attr(stat.test$conf.int,"conf.level")
    class(stat.test) <- "htest"
  }

  # if(grepl("lm",esType)){
  #
  # }

  esType <- gsub("lm[.]","",keytable$stat.type)


  # Descriptive structures ---------------------------------------
  N <- vars$N
  vars$N <- NULL

  if(any(names(vars) == "df")){vars$df <- NULL}
  if(any(names(vars) == "cleanDataFilter")){vars$cleanDataFilter <- NULL}

  if(any(names(stat.test) == "method")){
    method = stat.test$method
  } else {
    method <- class(stat.test)[1]
  }

  if(any(plyr::laply(vars,is.factor))){
    if((esType!="X2")&(!grepl("OR",esType))){
      descr.sum    <- plyr::ldply(unique(vars$Condition), function(c){
        Response <- unlist(vars[1])
        cbind(name = c,
              broom::tidy(summary(na.exclude(Response[vars$Condition==c])))
        )}
      )

      id        <- unlist(plyr::colwise(is.factor)(as.data.frame(vars)))
      tmp       <- as.data.frame(vars)
      Cid <- which(grepl("(ID)",colnames(tmp)))
      if(length(Cid)!=0){tmp  <- select(tmp, -Cid)}
      # cID <- laply(1:NCOL(tmp), function(c) is.discrete(tmp[,c]))
      # tmp <- tmp[,-which(cID)]

      descr.raw <- eval(parse(text = paste0("tmp %>% group_by(",names(id[id]),") %>% summarise(column = '",names(id[id]),"', n = n())")))
      #descr.raw <- plyr::ddply(tmp, names(tmp)[id], broom::tidy)
      #descr.raw <- descr.raw[!grepl("[*]",descr.raw$column), ]
     colnames(descr.raw)[colnames(descr.raw)%in%names(id)[id]] <- paste("name")
    }

    if((esType=="X2")|(grepl("OR",esType))){
      N <- c(N[1],N[1],N[2],N[2])
      descr.sum <- plyr::ldply(unique(vars$Condition), function(c){
        dfOut <- broom::tidy(summary(na.exclude(factor(vars$Response[vars$Condition==c]))))
        cbind.data.frame(name      = paste0(c,".",rownames(dfOut)),
                         count     = dfOut$x,
                         n         = N[c],
                         prop.cond = dfOut$x/sum(dfOut$x))
      })
      descr.sum$prop.tot <- descr.sum$count/sum(descr.sum$count)
      descr.raw <- descr.sum
    }

  } else {

    descr.sum  <- plyr::ldply(vars, function(gd){
      if(is.list(gd)){gd<-unlist(gd)}
      cbind(broom::tidy(summary(na.exclude(gd)))
      )})

    colnames(descr.sum)[colnames(descr.sum)==".id"] <- "name"

    descr.raw  <- plyr::ldply(vars, function(ignore){
      #if(is.list(gd)){gd<-unlist(gd)}
      if(!is.data.frame(ignore)){ignore <- data.frame(as.numeric(ignore))}
      return(broom::tidy(ignore))})

    colnames(descr.raw)[colnames(descr.raw)==".id"] <- "name"
  }

  # Test structure --------------------------------------------------------------------------------------------------

  if(esType=="f"|grepl("lm",keytable$stat.type)){

    test <- cbind.data.frame(statistic = (eval(parse(text=keytable$stat.ncp))),
                             parameter = (rbind(eval(parse(text=keytable$stat.df)))),
                             p.value   = (eval(parse(text=keytable$stat.p.value)))
    )

    if(esType=="f"){
      colnames(test) <- c('statistic','parameter1','parameter2','p.value')
    } else {
      colnames(test) <- c('statistic','parameter1','p.value')
    }

  } else {

    suppressMessages(test <- broom::tidy(stat.test))

    if(esType=="OR"){
      test$parameter <- NA
    }

  }

  if(any(colnames(test)%in%c("n1","n2"))){
    colnames(test)[colnames(test)%in%c("n1","n2")] <- gsub("n","parameter",colnames(test)[colnames(test)%in%c("n1","n2")])
  }

  test$estype    <- keytable$stat.type
  test$method    <- stat.test$method

  if(test$estype=="Z.f"){
    if(test$method=="Fisher r-to-Z transformed test for difference between 2 independent correlations"){

      Wilcox.out<- twopcor(x1=vars$r1[[1]],
                           x2=vars$r1[[2]],
                           y1=vars$r2[[1]],
                           y2=vars$r2[[2]])

      test$fZ.ncp       <- stat.test$statistic
      test$fZ.ncp.lo    <- "fisherZ - 2 corr"
      test$fZ.ncp.hi    <- "fisherZ - 2 corr"
      test$fZ.cohensQ   <- stat.test$effect.size
      test$fZ.cohensQ.l <- stat.test$effect.size.ci[1]
      test$fZ.cohensQ.u <- stat.test$effect.size.ci[2]
      test$fZ.bootR1    <- Wilcox.out$r1
      test$fZ.bootR2    <- Wilcox.out$r2
      test$fZ.bootCI.l  <- Wilcox.out$ci[1]
      test$fZ.bootCI.u  <- Wilcox.out$ci[2]
      test$fZ.r         <- Wilcox.out$r1-Wilcox.out$r2
      test$fZ.l.r       <- Wilcox.out$ci[1]
      test$fZ.u.r       <- Wilcox.out$ci[2]
    } else {
      test$fZ.ncp       <- stat.test$statistic
      test$fZ.ncp.lo    <- "fisherZ - 1 corr"
      test$fZ.ncp.hi    <- "fisherZ - 1 corr"
      test$fZ.cohensQ   <- stat.test$effect.size
      test$fZ.cohensQ.l <- stat.test$effect.size.ci[1]
      test$fZ.cohensQ.u <- stat.test$effect.size.ci[2]
    }
  }

  return(list(descr.raw = descr.raw,
              descr.sum = descr.sum,
              test      = test)
  )
}


#' get.output
#'
#' @param key key
#' @param vars vars
#' @param descr descr
#' @param group group
#' @param analysis analysis
#' @param varEqual varEqual
#' @param test test
#' @param ESCI ESCI
#' @param test.ConsoleOutput test.ConsoleOutput
#' @param SourceInfo SourceInfo
#' @param stat.test stat.test
#'
#' @return output
#' @export
#'
get.output <- function(key, vars, descr, group, analysis, varEqual, test, ESCI, test.ConsoleOutput, SourceInfo, stat.test){

  if(key$stat.type%in%c("OR","lm.OR")){
    test.table    = table(vars$Condition,vars$Response)

    descr$value  <- NA
    descr$value[descr$count%in%test.table[,colnames(test.table)[1]]] <- colnames(test.table)[1]
    descr$value[descr$count%in%test.table[,colnames(test.table)[2]]] <- colnames(test.table)[2]
    descr$mean <- descr$prop.cond
    sd   <- try.CATCH(sqrt((1-descr$prop.cond)*descr$prop.cond))
    if(is.numeric(sd$value)){sd <- sd$value} else {sd <- NA}

    # Data list for calculating Effect Sizes CI based on NCP
    output <- data.frame(
      study.id      = key$study.id,
      study.slate   = key$study.slate,
      study.name    = key$study.name,
      study.source  = group,
      analysis.type = analysis,
      analysis.name = key$study.analysis,
      stat.N        = sum(vars$N, na.rm = T),
      stat.n1       = ifelse(length(vars$N)==2,vars$N[[1]],vars$N),
      stat.n2       = ifelse(length(vars$N)==2,vars$N[[2]],NA),
      stat.cond1    = descr[1, ],
      stat.cond2    = descr[2, ],
      stat.cond3    = descr[3, ],
      stat.cond4    = descr[4, ],
      test.type     = key$stat.type,
      test          = test,
      test.varequal = varEqual,
      test.table    = paste(capture.output(print(knitr::kable(table(vars$Condition,vars$Response),format = 'rst'))),collapse="\n"),  #paste0(knitr::kable(table(vars$Condition,vars$Response),format = 'rst'),collapse="\n"),
      ESCI          = ESCI,
      test.ConsoleOutput = test.ConsoleOutput,
      source        = SourceInfo
    )
  } else {

  # if(key$stat.type"OR"){
    # Data list for calculating Effect Sizes CI based on NCP
    # output <- data.frame(
    #     study.id      = key$study.id,
    #     study.slate   = key$study.slate,
    #     study.name    = key$study.name,
    #     study.source  = group,
    #     analysis.type = analysis,
    #     analysis.name = key$study.analysis,
    #     stat.N        = sum(vars$N, na.rm = T),
    #     stat.n1       = ifelse(length(vars$N)==2,vars$N[[1]],vars$N),
    #     stat.n2       = ifelse(length(vars$N)==2,vars$N[[2]],NA),
    #     ,
    #     test.type     = key$stat.type,
    #     test          = test,
    #     test.varequal = varEqual,
    #     ESCI          = ESCI
    # )

    for(r in 1:nrow(descr)){
      eval(parse(text = paste0("stat.cond",r," =  descr[r, ]",colapse="")))
    }

    eval(parse(text=paste0("output <- data.frame(study.id = key$study.id, study.slate = key$study.slate, study.name = key$study.name, study.source  = group, analysis.type = analysis, analysis.name = key$study.analysis, stat.N = sum(vars$N, na.rm = T), stat.n1 = ifelse(length(vars$N)==2,vars$N[[1]],vars$N), stat.n2 = ifelse(length(vars$N)==2,vars$N[[2]],NA), ", paste0("stat.cond",1:nrow(descr)," = ","stat.cond",1:nrow(descr), collapse = ", "),", test.type = key$stat.type, test = test, test.varequal = varEqual,test.ConsoleOutput = test.ConsoleOutput, ESCI = ESCI, source = SourceInfo)", collapse = ", ")))
  }

  clean_out <- OutputTemplate()

  names_both <- colnames(clean_out)[colnames(clean_out)%in%colnames(output)]

  clean_out[,names_both] <- output[,names_both]

  names_missing <- colnames(output)[!colnames(output)%in%colnames(clean_out)]

  if(!all(grepl("fZ",names_missing))){
     warning(paste(names_missing,collapse = " | "))
  }
  return(clean_out)
}


#' decide.analysis
#'
#' @param ML2.key Key
#' @param studies studies
#' @param tp analysis type
#' @param doAll use all studies
#'
#' @return list of studies
#' @export
#'
decide.analysis <- function(ML2.key, studies=NA, tp = NA, doAll = FALSE){

  analysis <- c('study.global.include', 'study.primary.include', 'study.secondary.include','study.global.include')
  groups   <- c('Source.Global','Source.Primary','Source.Secondary','study.order')

  if(is.null(tp)){tp <- NA}
  if(any(is.na(studies))){studies <- na.exclude(ML2.key$unique.id)}
  if(tp==4){
    if(doAll){
      studies <- studies[studies%in%ML2.key$unique.id]
    } else {
      studies <- studies[studies%in%ML2.key$unique.id[ML2.key$study.figure2.include==1]]
    }
  }

  if(is.na(tp[1])){
    disp(paste0("Analyzing global, primary and secondary studies"), header = FALSE, footer = TRUE)
    studiess <- c(ML2.key$unique.id[ML2.key$study.global.include[studies]==1],
                  ML2.key$unique.id[ML2.key$study.primary.include[studies]==1],
                  ML2.key$unique.id[ML2.key$study.secondary.include[studies]==1])
    tps = c(rep(1,length(ML2.key$unique.id[ML2.key$study.global.include[studies]==1])),
            rep(2,length(ML2.key$unique.id[ML2.key$study.primary.include[studies]==1])),
            rep(3,length(ML2.key$unique.id[ML2.key$study.secondary.include[studies]==1]))
    )
  } else {
    disp(paste0("Analyzing studies in ", analysis[tp]), header = FALSE, footer = TRUE)
    studiess <- lapply(analysis[tp], function(c) ML2.key$unique.id[ML2.key[,c]==1])
    tps      <- unlist(sapply(seq_along(studiess), function(s) rep(tp[s], length(studiess[[s]]))))
    studiess <- unlist(studiess)
  }

  tp           <- tps[!is.na(studiess)&studiess%in%studies]

  if(any(is.na(studiess[!is.na(studiess)&studiess%in%studies]))){stop("Analysis ID and analysis type do not agree [e.g. analysis type is 'primary', but analysis ID refers to 'secondary']")}

  return(list(studiess = studiess[!is.na(studiess)&studiess%in%studies],
              ugroup   = groups[tp],
              tp       = tp)
  )
}





#' testScript
#'
#' FOR TESTING PURPOSES
#'
#' @param studies     Unique analysis number(s) from the matsterkey sheet.
#' @param tp      Analysis type (1 = 'study.global.include', 2 = 'study.primary.include', 3 = 'study.secondary.include').
#' @param saveRDSfile     Save an RDS file of the output.
#'
#' @export
#' @keywords internal
#'
testScript <- function(studies,
                       tp,
                       saveCSVfile=NA,
                       saveRDSfile=NA,
                       subset = c("all","WEIRD","NONWEIRD")[1],
                       dir.out = "~/Dropbox/Manylabs2/TestOutput"){

  analysis  <- c('study_global_include', 'study_primary_include', 'study_secondary_include', 'study_by_order')
  #studies = gsub("[_]+",".",analysis)
  if(dplyr::between(tp,2,3)){
    cat("\nsetting subset to all\n")
    subset <- "all"}

  dfout <- get.analyses(studies = studies, analysis.type = tp, subset = subset)

  if(tp==4){subset <- "by_order"}

  ML2.key <- get.GoogleSheet(data='ML2masteRkey')$df

  if(saveRDSfile){
    fname <- c(file.path(normalizePath(dir.out),"RESULTS_RDS",subset,paste0("ML2_results_global_",subset,".rds")),
               file.path(normalizePath(dir.out),"RESULTS_RDS",subset,paste0("ML2_results_primary_",subset,".rds")),
               file.path(normalizePath(dir.out),"RESULTS_RDS",subset,paste0("ML2_results_secondary_",subset,".rds")),
               file.path(normalizePath(dir.out),"RESULTS_RDS","by_order",paste0("Data_Figure_StudyOrder_",subset,".rds")))[tp]
    cat(paste0("\nSaving list object with analyses...\n"))
    # only save non-empty objects

    saveRDS(dfout,file=fname)
  }

   #setwd(dir.out)

  if(saveCSVfile){
    cat(paste0("\nSaving raw cases...\n"))
    l_ply(seq_along(dfout$raw.case),
          function(d){
            if(!is.null(dfout$raw.case[[d]])&NCOL(dfout$raw.case[[d]])>0){
              rio::export(dfout$raw.case[[d]],
                          file = file.path(normalizePath(dir.out),"RAW_CASE",subset, paste0(names(dfout$raw.case)[d],"_",analysis[[tp]],"_RAW_CASE_",subset,".csv")))
            }
          })
    }

    if(tp==1){
      cat(paste0("\nSaving global results...\n"))
    }

    if(between(tp,2,3)){
      cat(paste0("\nSaving by_group results...\n"))

      l_ply(seq_along(dfout$aggregated),
            function(d){
              if(!is.null(dfout$merged.results[[d]])&NCOL(dfout$aggregated[[d]])>0){
                rio::export(dfout$merged.results[[d]],
                            paste0(dir.out,"/RESULTS_ANALYSIS/",subset,"/",
                                 names(dfout$merged.results)[d],"_", analysis[[tp]],".csv"))}})
      }

    if(tp==4){
      cat(paste0("\nSaving by_order results...\n"))
    }

l_ply(seq_along(dfout$aggregated),
      function(d){
        if(!is.null(dfout$aggregated[[d]])&NCOL(dfout$aggregated[[d]])>0){
          rio::export(dfout$aggregated[[d]],
                      file = file.path(normalizePath(dir.out),"RESULTS_ANALYSIS",subset,paste0(names(dfout$aggregated)[d],"_", analysis[[tp]],"_",subset,".csv")))}})


      # l_ply(seq_along(dfout$merged.results),
      #       function(d){
      #         if(!is.null(dfout$merged.results[[d]])&NCOL(dfout$merged.results[[d]])>0){
      #           rio::export(dfout$merged.results[[d]],
      #                       paste0(dir.out,"/RESULTS_ANALYSIS/by_order/",
      #                              names(dfout$merged.results)[d],"_", analysis[[tp]],"_",subset,".csv"))}})}}

  if(saveRDSfile){
    all <- plyr::ldply(dfout$aggregated)
    if(!is.null(all)&NCOL(all)>0){
      rio::export(all, file.path(normalizePath(dir.out),"RESULTS_RDS",paste0("ALL_",analysis[[tp]],"_",subset,".csv")))
      rio::export(all, file.path(normalizePath(dir.out),"RESULTS_RDS",paste0("ALL_",analysis[[tp]],"_",subset,".xlsx")))
    }
  }

}


#' generateOutput
#'
#' @param describe The output of function \link{\code{get.descriptives}}
#' @param var.lor Variance of log OR effect size
#' @param runningGroup The group on which the statistics in `describe` were calculated (can be: 'all', a source name, or a presentation order)
#' @param runningAnalysis The analysis that generated the statistics in `describe`
#'
#' @export
#'
#' @author
#' Fred Hasselman
#'
generateOutput <-  function(describe = describe,
                            var.lor  = NA,
                            runningGroup = "None",
                            runningAnalysis = "None",
                            stat.params = NA){

  ESCI <- list(value = NULL,
               warning = "init")

  test  = describe$test
  descr = describe$descr.raw

  if(grepl("OR",test$estype, fixed = TRUE)){
    Nv <- c(descr$n[1],descr$n[3])
  } else {
    if(NROW(descr$n)==2){
      Nv <- c(descr$n[1],descr$n[2])
    } else {
      Nv <- c(descr$n[1]+descr$n[3],descr$n[2]+descr$n[4])
    }
  }

if(is.null(test$method)){
  test$method <- test$estype
}

  if(!is.na(test[1,1])){

    ESCI <- try.CATCH(any2any(testInfo  = test,
                              df1       = as.numeric(test[, which(grepl("parameter",colnames(test)))[1]]),
                              df2       = as.numeric(test[, which(grepl("parameter2",colnames(test)))]),
                              N         = sum(Nv, na.rm = TRUE),
                              n1        = Nv[1],
                              n2        = Nv[2],
                              esType    = test$estype,
                              var.lor   = var.lor,CIcalc = TRUE,
                              CL        = ifelse(is.null(stat.params$conf.level),
                                                 .95,
                                                 stat.params$conf.level),
                              keepSign    = TRUE,
                              alternative = ifelse(is.null(test$alternative),
                                                   stat.params[[4]],
                                                   as.character(test$alternative))
                              )
                      )
    }


  if(all(is.null(ESCI$warning),
         grepl("simpleWarning",ESCI$warning),
         !grepl("Error", ESCI$value[[1]]),
         !grepl("message", names(unlist(ESCI))[1]),
         !is.na(test[1,1]))){
    ESCI  <- ESCI$value
    es.id <- which(colnames(ESCI)%in%"r")
  } else {
    ESCI  <- test
    es.id <- 1
  }

  ifelse(is.na(ESCI[es.id]),
         disp(paste(runningAnalysis,'-',
                    runningGroup,'>> ES r is NA'), header = FALSE, footer = FALSE),

         ifelse(abs(ESCI[es.id][1])>=.95,
                disp(paste(runningAnalysis,'-',
                           runningGroup,'>> ES r is extreme hi/lo: ',
                           round(ESCI[es.id][1],digits=2)), header = FALSE, footer = FALSE),

                ifelse(abs(ESCI[es.id][1])<=0.05,
                       disp(paste(runningAnalysis,'-',
                                  runningGroup,'>> ES r is close to 0: ',
                                  round(ESCI[es.id][1],digits=2)), header = FALSE, footer = FALSE),

                       ifelse(any(is.infinite(ESCI[es.id][[1]])),
                              disp(paste(runningAnalysis,'-',
                                         runningGroup,'>> ES r (CI) is infinite'),
                                   header = FALSE, footer = FALSE),
                              NA)
                )
         )
  )


  # Add columns for correlation comparison ES
  ESCI$cohensQ   <- NA
  ESCI$cohensQ.l <- NA
  ESCI$cohensQ.u <- NA
  ESCI$bootR1    <- NA
  ESCI$bootR2    <- NA
  ESCI$bootCI.l  <- NA
  ESCI$bootCI.u  <- NA

  if(test$estype=="Z.f"){

    if(test$method=="Fisher r-to-Z transformed test for difference between 2 independent correlations"){
      ID <- grep("d",colnames(ESCI))[1]
      if(!is.na(ID)%00%NA){ESCI[,ID:NCOL(ESCI)] <- NA}
      ESCI$bootR1    <- test$fZ.bootR1
      ESCI$bootR2    <- test$fZ.bootR2
      ESCI$bootCI.l  <- test$fZ.bootcCI.l
      ESCI$bootCI.u  <- test$fZ.bootcCI.u
      ESCI$r         <- test$fZ.r
      ESCI$l.r       <- test$fZ.l.r
      ESCI$u.r       <- test$fZ.u.r
    }
    ESCI$ncp       <- test$fZ.statistic
    ESCI$ncp.lo    <- test$fZ.ncp.lo
    ESCI$ncp.hi    <- test$fZ.ncp.hi
    ESCI$cohensQ   <- test$fZ.cohensQ
    ESCI$cohensQ.l <- test$fZ.cohensQ.l
    ESCI$cohensQ.u <- test$fZ.cohensQ.u
  }

  return(ESCI)
}

# FREDSRUTILS ----

#' gg.theme
#'
#' A gg theme
#'
#' @param type One of \code{clean}, or \code{noax}
#' @param useArial Use the Arial font (requires \code{.afm} font files in the \code{afmPath})
#' @param afmPATH Path to Arial \code{.afm} font files.
#'
#'
#' @details Will generate a \code{clean} ggplot theme, or a theme without any axes (\code{noax}).
#'
#' Some scientific journals explicitly request the Arial font should be used in figures.
#' This can be achieved by using \code{.afm} font format (see, e.g. \url{http://www.pure-mac.com/font.html}).
#'
#' @return A theme for \code{ggplot2}.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' g <- ggplot(data.frame(x = rnorm(n = 100), y = rnorm(n = 100)), aes(x = x, y = y)) + geom_point()
#' g + gg.theme()
#' g + gg.theme("noax")
#'
gg.theme <- function(type=c("clean","noax"),useArial = FALSE, afmPATH="~/Dropbox"){

  if(length(type)>1){type <- type[1]}

  if(useArial){
    set.Arial(afmPATH)
    bf_font="Arial"
  } else {bf_font="Helvetica"}

  switch(type,
         clean = ggplot2::theme_bw(base_size = 12, base_family=bf_font) +
           theme(axis.text.x     = element_text(size = 10),
                 axis.title.y    = element_text(vjust = +1.5),
                 panel.grid.major  = element_blank(),
                 panel.grid.minor  = element_blank(),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.line  = element_line(colour = "black")),
         noax = ggplot2::theme(line = element_blank(),
                               text  = element_blank(),
                               title = element_blank(),
                               plot.background = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank())
  )
}

#' gg.plotHolder
#'
#' @param useArial    Use the Arial font (requires \code{.afm} font files in the \code{afmPath})
#' @param afmPATH    Path to Arial \code{.afm} font files.
#'
#' @return A blank \code{ggplot2} object that can be used in concordance with \code{grid.arrange}.
#' @export
#'
#' @examples
#' # Create a plot with marginal distributions.
#' library(ggplot2)
#' library(scales)
#'
#' df <- data.frame(x = rnorm(n = 100), y = rnorm(n = 100),
#'                  group = factor(sample(x=c(0,1),
#'                  size = 100, replace = TRUE))
#'                  )
#'
#' scatterP <- ggplot(df, aes(x = x, y =y, colour = group)) +
#'             geom_point() +
#'             gg.theme()
#'
#' xDense <- ggplot(df, aes(x = x, fill = group)) +
#'           geom_density(aes(y= ..count..),trim=FALSE, alpha=.5) +
#'           gg.theme("noax") +
#'           theme(legend.position = "none")
#'
#' yDense <- ggplot(df, aes(x = y, fill = group)) +
#'           geom_density(aes(y= ..count..),trim=FALSE, alpha=.5) +
#'           coord_flip() +
#'           gg.theme("noax") +
#'           theme(legend.position = "none")
#'
#' library(gridExtra)
#'
#' grid.arrange(xDense,
#'              gg.plotHolder(),
#'              scatterP,
#'              yDense,
#'              ncol=2, nrow=2,
#'              widths=c(4, 1.4), heights=c(1.4, 4)
#'              )
#'
gg.plotHolder <- function(useArial = F,afmPATH="~/Dropbox"){
  # require(ggplot2)
  ggplot2::ggplot() +
    geom_blank(aes(1,1)) +
    theme(line = element_blank(),
          text  = element_blank(),
          title = element_blank(),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
    )
}

#' fill_viol
#'
#' @details  Function to create geom_ploygon calls for \code{\link{vioQtile}}
#'
#' @param gr.df Internal.
#' @param gr Internal.
#' @param qtile Internal.
#' @param probs Internal.
#'
#' @return A list for \code{\link{vioQtile}}
#'
#' @description  This is adapted from: \url{http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot}
#'
#' @seealso vioQtile
#'
fill_viol<-function(gr.df,gr,qtile,probs){

  ifelse(is.null(qtile),{
    cuts <- cut(gr.df$y, breaks = quantile(gr.df$y, probs, na.rm=T, type=3, include.lowest = T, right = T), na.rm=T)},{
      cuts <- cut(gr.df$y, breaks = qtile, na.rm=T)
    }
  )

  quants <- dplyr::mutate(gr.df,
                          x.l=x-violinwidth/2,
                          x.r=x+violinwidth/2,
                          cuts=cuts)

  plotquants <- data.frame(x=c(quants$x.l,rev(quants$x.r)),
                           y=c(quants$y,rev(quants$y)),
                           id=c(quants$cuts,rev(quants$cuts)))

  #cut by quantile to create polygon id
  geom <- geom_polygon(aes(x=x,y=y,fill=factor(id)),data=plotquants,alpha=1)

  return(list(quants=quants,plotquants=plotquants,geom=geom))
}

#' vioQtile
#'
#' @param gg     A ggplot.
#' @param qtiles    Quantiles.
#' @param probs     Probabilities.
#' @param labels    Labels.
#' @param withData    Return Data.
#'
#' @details
#'  This is adapted from: \url{http://stackoverflow.com/questions/22278951/combining-violin-plot-with-box-plot}
#'
#'  \strong{Changed:}
#'  Deal with 'empty' quantile groups
#'  Deal with original data
#'  More input, more output
#'
#' @export
#'
vioQtile <- function(gg=NULL,qtiles=NULL,probs=seq(0,1,.25),labels=paste(probs[-1]*100),withData=FALSE){
  #  require(ggplot2)

  g.df <- ggplot2::ggplot_build(gg)$data[[1]]    # use ggbuild to get the outline co-ords

  ifelse(is.null(qtiles),{
    gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,NULL,probs)$geom)},{
      gg <- gg + lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x, ],x,qtiles[x, ],probs)$geom)}
  )

  gg <- gg + ggplot2::geom_hline(aes(yintercept=0)) +
    ggplot2::scale_fill_grey(name="Quantile\n",labels=labels,guide=guide_legend(reverse=T,label.position="right")) +
    ggplot2::stat_summary(fun.y=median, geom="point", size=8, color="grey80",shape=21,fill="white")

  if(withData){
    ifelse(is.null(qtiles),{
      ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,NULL,probs))},{
        ggData <- lapply(unique(g.df$group), function(x) fill_viol(g.df[g.df$group==x,],x,qtiles[x,],probs))
      }
    )
    return(list(ggGraph=gg,ggData=ggData))
  } else {
    return(gg)
  }
}


swarmPlot <- function(df, anonymous=FALSE, addSize=FALSE, addMedian=TRUE, addGlobalES = TRUE, addOriES=TRUE, addLabel=FALSE, oriES=NULL, fillvar=c("USA","sigf")[1]){

  #sourceInfo <- get.GoogleSheet(url="https://docs.google.com/spreadsheets/d/1Qn_kVkVGwffBAmhAbpgrTjdxKLP1bb2chHjBMVyGl1s/export?format=csv")$df

  # if(is.null(oriES)&addOriES){
  #  oriES <- import("/Users/Fred/Dropbox/Manylabs2/TestOutput/ORI.EFFECTS/ML2_ori_effects_MasterKey.xlsx")
  # }

  if(!is.null(oriES)){addOriES=FALSE}
  if(addOriES){
    # Load Key Table
    oriES   <- get.GoogleSheet(data='ML2masteRkey')$df
    ID.ori  <- which(nchar(oriES$orig.ES.r)>0)
    ID.add  <- which(oriES$study.figure2.include==1)
    Analyses.ori <- sort(oriES$study.analysis[ID.ori])
    Analyses.add <- sort(oriES$study.analysis[ID.add])
    # dft<- data.frame(includeFig2=Analyses.add)
    # dft$oriEffect <-NA
    # dft$oriEffect[Analyses.add%in%Analyses.ori] <- Analyses.ori[Analyses.ori%in%Analyses.add]
    # rest<-Analyses.ori[!(Analyses.ori%in%Analyses.add)]
    # dft[(nrow(dft)+1):(nrow(dft)+length(rest)),1:2] <- cbind(rep(NA,length(rest)),rest)
  }


  df$slabel <- df$study.id

  # if(!anonymous){
  #     l_ply(seq_along(oriES$study.analysis), function(l) df$slabel[tolower(as.character(df$.id))==oriES$study.analysis[l]] <<- oriES$description[l])
  #     df$slabel <- factor(df$slabel)
  # } else {
  #     l_ply(seq_along(oriES$study.analysis), function(l) df$slabel[tolower(as.character(df$.id))==oriES$study.analysis[l]] <<- l)
  #     df$slabel <- factor(df$slabel)
  # }

  df$.id    <- factor(df$.id)
  btype     <- "swarm"
  pdf(tempfile())
  bs    <- beeswarm(ESCI.r ~ slabel, data = df,
                    horizontal = FALSE, corral = "none",
                    corralWidth = 5,
                    pch = 21, spacing = 2, cex = .5,
                    priority = "ascending", method = btype, do.plot = TRUE)[, c(1, 2, 4, 6)]
  dev.off()

  colnames(bs)[4] <- "labels"

  df <- data.frame(df,bs)
  se <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}

  df$meanES <- plyr::ldply(unique(df$labels),
                           function(r) cbind(rep(mean(df$y[df$labels==r], na.rm = TRUE),
                                                 sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]
  df$seES <- plyr::ldply(unique(df$labels),
                         function(r) cbind(rep(se(df$y[df$labels==r]),
                                               sum(df$labels==r, na.rm = TRUE) ) ))[ ,1]

  df <- arrange(df, meanES)

  df$xx <- plyr::ldply(unique(df$labels),
                       function(r) cbind(scale(df$x[df$labels==r], scale = F)))[,1]
  df$xf <- plyr::ldply(seq_along(unique(df$labels)),
                       function(r) cbind(df$xx[df$labels==unique(df$labels)[r]] +
                                           seq(10,10*length(unique(df$labels)), by=10)[r]))[,1]
  df$xn <- plyr::ldply(seq_along(unique(df$labels)),
                       function(r) cbind(rep(seq(10,10*length(unique(df$labels)),by=10)[r],
                                             sum(df$labels==unique(df$labels)[r]))))[,1]

  keylabs <- c("Mean of sample ES","ES of grand mean","Original ES")[c(addMedian,addGlobalES,addOriES)]
  mrkrs  <- c(22,21,23)[c(addMedian,addGlobalES,addOriES)]

  mxN       <- max(df$ESCI.N.total)

  #Colorblindsafe colors
  cwhite = "#f7f7f7"
  ccream = "#2166ac"
  cblank = "#d1e5f0"
  corange = "#f4a582"
  cblue  = "#2166ac"
  cred   = "#b2182b"
  cpurp  = "#b2abd2"

  mypalette <- c(cred,cblue)

  df$sigf <- NA
  df$sigf[df$test.p.value> .05] <- "Not Significant"
  df$sigf[df$test.p.value<=.05] <- "Significant"
  df$sigf <- factor(df$sigf)

  # df$Country <- "No Country"
  # l_ply(seq_along(df$.id), function(l) df$Country[l] <<- sourceInfo$Country[sourceInfo$Source.Global==df$study.source[l]])

  df$USA                    <- "Non-USA"
  df$USA[df$source.Country=="USA"] <- "USA"

  df$USA <- factor(df$USA)

  df <- df[df$ESCI.N.total>=30,]

  dfG <- dplyr::summarise(group_by(df,.id),
                          y= mean(ESCI.r,na.rm = T),
                          ymin=mean(ESCI.l.r, na.rm = T),
                          ymax=mean(ESCI.u.r, na.rm = T))

  dfG   <- arrange(dfG, y)
  dfG$x <- seq(10,10*nrow(dfG),by=10)

  dfGlobal <- dplyr::summarise(group_by(df,.id),
                               y= max(GlobalES,na.rm = TRUE),
                               ymin=max(GlobalESlo, na.rm = TRUE),
                               ymax=max(GlobalEShi, na.rm = TRUE))
  dfGlobal   <- arrange(dfGlobal, y)
  dfGlobal$x <- seq(10,10*nrow(dfGlobal),by=10)

  if(addOriES){
    oriES      <- oriES[ID.ori,]
    oriES$mES <- plyr::laply(na.exclude(oriES$study.analysis), function(s) dfG$y[tolower(dfG$.id)%in%tolower(s)])
    oriES     <- dplyr::arrange(oriES, mES)
    oriES$x   <- seq(10,10*nrow(oriES),by=10)
  }
  df$fillvar<- df[,fillvar]

  g <-ggplot(df, aes(x=xf, y=y)) +
    geom_vline(xintercept = unique(df$xn), colour = "grey80",alpha = 1) +
    geom_hline(yintercept = 0, colour = "ivory4")

  if(addSize){
    g <-  g +
      geom_point(aes(fill = fillvar, size = ESCI.N), col=cwhite, pch=21)
    # scale_size_continuous("Sample Size", breaks = c(0.01, 0.1, 0.3, 0.5,0.8,1),
    #                       labels = round(c(0.01, 0.1, 0.3, 0.5,0.8, 1) * mxN),
    #                       guide = guide_legend(override.aes=list(colour="grey30",fill="grey70"), byrow = TRUE)
    # )
  } else {
    g <-  g +
      geom_point(aes(fill = fillvar), size = 2, col=cwhite, pch=21)
  }

  if(addMedian){
    g <- g + geom_point(data=dfG,aes(x=x,y=y),
                        color="black",fill=cpurp,alpha=1,size=3,pch=22)
  }

  if(addGlobalES){
    g <- g + geom_point(data=dfGlobal,aes(x=x,y=y),
                        color="black",fill=cblank,alpha=1,size=3,pch=21)
  }

  if(addOriES){
    g <- g +
      geom_point(data=oriES,aes(x=x,y=ESCI.r),
                 color="black",fill=corange,alpha=1,size=3,pch=23)
  }

  if(addLabel){
    g <- g +
      geom_text(aes(label=study.source,hjust=0,color=fillvar),
                size= 1,
                angle = 45,
                position=position_dodge(.9))
  }

  g <- g +
    scale_y_continuous("Effect Size r", limits = c(-1,1)) +
    scale_x_continuous("", breaks = unique(df$xn),
                       labels = unique(paste(df$labels)),
                       expand = c(0, 10)) +
    scale_fill_manual("Sample",values = mypalette,  guide   = guide_legend(override.aes = list(size = 4), byrow = TRUE)) +
    gg.theme() + coord_flip()  +
    theme(legend.position = "top", legend.background=element_rect())

  return(g)

}

#' get.plotly
#'
#' Get a plotly plot.
#'
#' @param data Dataframe with ML2 testresutls and ESCI output.
#' @param analysis_url url
#'
#' @export
#'
get.plotly <- function(data,analysis_url){

  p <- plot_ly(data,
               x    = ESCI.r,
               y    = test.p.value,
               mode = "markers",
               hoverinfo = "text",
               size = ESCI.N.total,
               text = paste("<b>r</b> =",ESCI.r,"<b>p</b> <",signif(test.p.value,digits = 4),"<br>",
                            #"<i>Analysis:</i><a href=",analysis_url,">",.id,"</a><br>",
                            "<i>Sample:</i>",study.source),
               name = analysis_url) %>%
    add_trace(x=seq(-1,1),y=rep(0.05,3),  line = list(color="green"), name="p<.05") %>%
    add_trace(x=seq(-1,1),y=rep(0.000001,3), line = list(color="orange"), name="p<.000001") %>%
    add_trace(x=seq(-1,1),y=rep(0.000000000001,3), line = list(color="red"), name="p<.000000000001") %>%
    layout(yaxis = list(title = "p-value"),
           xaxis = list(title = "Effect Size r"))
  l <- plotly_build(p)
  l$data[[1]]$text <- gsub("<br>ESCI.N.total (size):"," - <b>N</b> = ",l$data[[1]]$text,fixed=T)
  l$data[[1]]$marker$size <- l$data[[1]]$marker$size/10
  return(plotly_build(l))
}



#' renderHTMLresults
#'
#' @param pageID
#'
renderHTMLresults <- function(pageID) {
  rmarkdown::render(input  = "ML2_interactive_results.Rmd",
                    params = list(
                      set_title = paste("ManyLabs2 -",pageID),
                      pageID    = pageID,
                      fName     = paste0(paste("ML2",pageID,sep='_'),".html")),
                    output_format = "html_document",
                    output_file   = paste0(paste("ML2",pageID,sep='_'),".html"),
                    output_dir    = "interActive/"
  )
}



OutputTemplate <- function(){
return(
data.frame(
 results.generated = now(),
 study.id = NA,
 study.name = NA,
 study.slate = NA,
 study.mean = NA,
 study.source = NA,
 analysis.type = NA,
 analysis.name = NA,
 stat.N = NA,
 stat.n1 = NA,
 stat.n2 = NA,
 stat.cond1.name = NA,
 stat.cond1.column = NA,
 stat.cond1.n = NA,
 stat.cond1.count = NA,
 stat.cond1.prop.cond = NA,
 stat.cond1.prop.tot = NA,
 stat.cond1.value = NA,
 stat.cond1.mean = NA,
 stat.cond1.sd = NA,
 stat.cond1.median = NA,
 stat.cond1.trimmed = NA,
 stat.cond1.mad = NA,
 stat.cond1.min = NA,
 stat.cond1.max = NA,
 stat.cond1.range = NA,
 stat.cond1.skew = NA,
 stat.cond1.kurtosis = NA,
 stat.cond1.se = NA,
 stat.cond2.name = NA,
 stat.cond2.column = NA,
 stat.cond2.n = NA,
 stat.cond2.count = NA,
 stat.cond2.prop.cond = NA,
 stat.cond2.prop.tot = NA,
 stat.cond2.value = NA,
 stat.cond2.mean = NA,
 stat.cond2.sd = NA,
 stat.cond2.median = NA,
 stat.cond2.trimmed = NA,
 stat.cond2.mad = NA,
 stat.cond2.min = NA,
 stat.cond2.max = NA,
 stat.cond2.range = NA,
 stat.cond2.skew = NA,
 stat.cond2.kurtosis = NA,
 stat.cond2.se = NA,
 stat.cond3.name = NA,
 stat.cond3.column = NA,
 stat.cond3.n = NA,
 stat.cond3.count = NA,
 stat.cond3.n = NA,
 stat.cond3.prop.cond = NA,
 stat.cond3.prop.tot = NA,
 stat.cond3.value = NA,
 stat.cond3.mean = NA,
 stat.cond3.sd = NA,
 stat.cond3.median = NA,
 stat.cond3.trimmed = NA,
 stat.cond3.mad = NA,
 stat.cond3.min = NA,
 stat.cond3.max = NA,
 stat.cond3.range = NA,
 stat.cond3.skew = NA,
 stat.cond3.kurtosis = NA,
 stat.cond3.se = NA,
 stat.cond4.name = NA,
 stat.cond4.column = NA,
 stat.cond4.n = NA,
 stat.cond4.count = NA,
 stat.cond4.n = NA,
 stat.cond4.prop.cond = NA,
 stat.cond4.prop.tot = NA,
 stat.cond4.value = NA,
 stat.cond4.mean = NA,
 stat.cond4.sd = NA,
 stat.cond4.median = NA,
 stat.cond4.trimmed = NA,
 stat.cond4.mad = NA,
 stat.cond4.min = NA,
 stat.cond4.max = NA,
 stat.cond4.range = NA,
 stat.cond4.skew = NA,
 stat.cond4.kurtosis = NA,
 stat.cond4.se = NA,
 test.type = NA,
 test.estimate = NA,
 test.estimate1 = NA,
 test.estimate2 = NA,
 test.statistic = NA,
 test.p.value = NA,
 test.parameter = NA,
 test.conf.low = NA,
 test.conf.high = NA,
 test.method = NA,
 test.alternative = NA,
 test.estype = NA,
 test.varequal = NA,
 test.ConsoleOutput = NA,
 test.ncp = NA,
 test.ncp.lo = NA,
 test.ncp.hi = NA,
 test.table = NA,
 test.parameter1 = NA,
 test.parameter2 = NA,
 test.cohensQ = NA,
 test.cohensQ.l = NA,
 test.cohensQ.u = NA,
 test.bootR1 = NA,
 test.bootR2 = NA,
 test.bootCI.l = NA,
 test.bootCI.u = NA,
 test.fZ.r = NA,
 test.fZ.l.r = NA,
 test.fZ.u.r = NA,
 ESCI.estimate = NA,
 ESCI.statistic = NA,
 ESCI.p.value = NA,
 ESCI.parameter = NA,
 ESCI.conf.low = NA,
 ESCI.conf.high = NA,
 ESCI.method = NA,
 ESCI.alternative = NA,
 ESCI.estype = NA,
 ESCI.ncp = NA,
 ESCI.ncp.lo = NA,
 ESCI.ncp.hi = NA,
 ESCI.N.total = NA,
 ESCI.n.1 = NA,
 ESCI.n.2 = NA,
 ESCI.d = NA,
 ESCI.var.d = NA,
 ESCI.l.d = NA,
 ESCI.u.d = NA,
 ESCI.U3.d = NA,
 ESCI.cl.d = NA,
 ESCI.cliffs.d = NA,
 ESCI.pval.d = NA,
 ESCI.g = NA,
 ESCI.var.g = NA,
 ESCI.l.g = NA,
 ESCI.u.g = NA,
 ESCI.U3.g = NA,
 ESCI.cl.g = NA,
 ESCI.pval.g = NA,
 ESCI.r = NA,
 ESCI.var.r = NA,
 ESCI.l.r = NA,
 ESCI.u.r = NA,
 ESCI.pval.r = NA,
 ESCI.fisher.z = NA,
 ESCI.var.z = NA,
 ESCI.l.z = NA,
 ESCI.u.z = NA,
 ESCI.OR = NA,
 ESCI.l.or = NA,
 ESCI.u.or = NA,
 ESCI.pval.or = NA,
 ESCI.lOR = NA,
 ESCI.l.lor = NA,
 ESCI.u.lor = NA,
 ESCI.pval.lor = NA,
 ESCI.cohensQ = NA,
 ESCI.cohensQ.l = NA,
 ESCI.cohensQ.u = NA,
 ESCI.bootR1 = NA,
 ESCI.bootR2 = NA,
 ESCI.bootCI.l = NA,
 ESCI.bootCI.u = NA,
 source.Source = NA,
 source.Source.Global = NA,
 source.Location = NA,
 source.ReplicationPI = NA,
 source.Filename = NA,
 source.StudyOrder = NA,
 source.IDiffOrder = NA,
 source.Country = NA,
 source.Language = NA,
 source.Weird = NA,
 source.SubjectPool = NA,
 source.Setting = NA,
 source.Tablet = NA,
 source.Pencil = NA,
 source.Execution = NA,
 source.Slate = NA,
 source.source = NA,
 source.N.sources.global = NA,
 source.N.sources.primary = NA,
 source.N.sources.secondary = NA,
 source.N.countries = NA,
 source.N.locations = NA,
 source.N.languages = NA,
 source.N.studyorders1 = NA,
 source.N.studyorders2 = NA,
 source.N.IDiffOrderN = NA,
 source.N.uIDs = NA,
 source.N.cases.included = NA,
 source.N.cases.excluded = NA,
 source.Pct.WEIRD = NA,
 source.Tbl.Execution = NA,
 source.Tbl.subjectpool = NA,
 source.Tbl.setting = NA,
 source.Tbl.Tablet = NA,
 source.Tbl.Pencil = NA,
 source.Tbl.analysistype = NA,
 source.Tbl.subset = NA,
 stat.cond5.name = NA,
 stat.cond5.column = NA,
 stat.cond5.n = NA,
 stat.cond5.mean = NA,
 stat.cond5.sd = NA,
 stat.cond5.median = NA,
 stat.cond5.trimmed = NA,
 stat.cond5.mad = NA,
 stat.cond5.min = NA,
 stat.cond5.max = NA,
 stat.cond5.range = NA,
 stat.cond5.skew = NA,
 stat.cond5.kurtosis = NA,
 stat.cond5.se = NA,
 stat.cond6.name = NA,
 stat.cond6.column = NA,
 stat.cond6.n = NA,
 stat.cond6.mean = NA,
 stat.cond6.sd = NA,
 stat.cond6.median = NA,
 stat.cond6.trimmed = NA,
 stat.cond6.mad = NA,
 stat.cond6.min = NA,
 stat.cond6.max = NA,
 stat.cond6.range = NA,
 stat.cond6.skew = NA,
 stat.cond6.kurtosis = NA,
 stat.cond6.se = NA)
)
 }

# Rmd2htmlWP <- function(infile, outfile, sup = T) {
#   require(markdown)
#   require(knitr)
#   mdOpt <- markdownHTMLOptions(default = T)
#   mdOpt <- mdOpt[mdOpt != "mathjax"]
#   mdExt <- markdownExtensions()
#   mdExt <- mdExt[mdExt != "latex_math"]
#   if (sup == T) {
#     mdExt <- mdExt[mdExt != "superscript"]
#   }
#   knit2html(input = infile, output = outfile, options = c(mdOpt), extensions = c(mdExt))
# }

#
# [copied from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ]
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multi.PLOT <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ANALYSIS functions ----------------


#' any2any
#'
#' Converts most common test statistics into most common (signed) effect sizes.
#'
#' @param st     Value(s) of a test statistic.
#' @param df1     Degrees of freedom
#' @param df2     NULL or degrees of freedom of the denominator for the f-distribution.
#' @param N     Number of data points used in calculation of test-statistic.
#' @param n1    Number of data points in sample 1.
#' @param n2    Number of data points in sample 2.
#' @param esType     Type of test statistic. One of: "t", "lm.t", "f", "lm.f", "r", "X2", "Z", "lm.Z"
#' @param CIcalc     If \code{TRUE} (default) the Confidence Interval for the test statistic in \code{x} will be calculated using the "Confidence limits for noncentral parameters" functions in package (e.g., for type - "t": \link[MBESS]{conf.limits.nct}).
#' @param CL    Confidence Limit (default: .95).
#' @param rID    Correlation among predictor values in a linear model.
#' @param q      Number of predictors in the model.
#' @param alternative     Alternative hypothesis (default = "two").
#' @param keepSign     Return effect size with sign of test statistic? (default = TRUE).
#' @param keepSignNames     Which effect sizes should keep the sign if \code{keepSign = TRUE}? Default is to keep the sign for: "r","l.r","u.r","fisher.z","l.z","u.z".
#'
#' @details The procedure to calculate a variety of effect sizes is as follows:
#'
#' \itemize{
#' \item If \code{CIcalc == FALSE}, \code{package::compute.es} will be used to convert the test statistic to a large number of effect size estimates. The confidence intervals around the effect size estimates will be based meta-analytic estimates of effect size variance (e.g., for type - "t": \link[compute.es]{tes}).
#' \item If \code{CIcalc == TRUE}, \code{package::MBESS} will be used to calculate the confidence interval for the test statistic based on its noncentral distribution (e.g., for type - "t": \link[MBESS]{conf.limits.nct}). Subsequently the test statistic, as well as its lower and upper confidence limit will each be passed to \code{compute.es} seperately.
#' \item If \code{keepSign == TRUE} the sign of the test statistic will be copied to all the effect sizes in \code{keepSignNames}.
#' }
#'
#' @note The prefix "lm" is currently disregarded, but will be implemented in future versions to indicate the test statistic is in fact a fixed factor in a linear model.
#'
#' @author
#' Fred Hasselman (inspired by RP:P function \code{any2r} by CHJ Hartgerink)
#'
#' @export
#'
#' @return The effect sizes calculated by \code{compute.es} corresponding to the test statistic(s), with either meta-analytic, or, exact CI.
#'
any2any <- function(testInfo,
                    df1 = NULL,
                    df2 = NULL,
                    N   = NULL, n1 = NULL, n2 = NULL,
                    esType  = NA,
                    var.lor = NA,
                    CIcalc  = TRUE,
                    CL = .95,
                    rID = 0, q = 1,
                    alternative   = "two", keepDirection = TRUE,
                    keepSign      = TRUE,
                    keepSignNames = c("r","l.r","u.r","fisher.z","l.z","u.z")){
  # require(MBESS)
  # require(compute.es)
  esType.cl <- NA

  ifelse(grepl("two",alternative),{alternative <<- "two"},{alternative <<- "one"})

  #  ifelse(any(grepl("estimate",colnames(testInfo))), {st <- testInfo$estimate},{st <- testInfo$statistic})
  if(any(grepl("(.r)+", esType),(esType%in%c("OR")))){
    st <- testInfo$estimate
  } else {
    st <- testInfo$statistic
  }

  if(grepl("Z.f", esType, fixed = TRUE)){
    if(length(testInfo$estimate)>1){
      esType <- esType.cl <- "Z"
      st <- testInfo$statistic
    }
  }


  if(esType%in%"Z"){
    if(alternative=="one"){
      n1<-N/2
      n2<-N/2
      alternative<-"two"
    }
  }


  if(is.null(st)){stop("No test statistic to caclulate ES-CI.")}

  # Use Cohen's dz for paired t-test
  if(esType%in%"t.p"){
    n1 <- n2 <- N/2
    st <- testInfo$statistic / sqrt(N)
  }


  # Check for model-based t-statistics
  if(grepl("(lm.t)+",esType)){
    # Make df of the predictor
    n1 <- n2 <- (df1+1)/2
  }


  # Treat model based stats as 'regular'
  esType.cl <- gsub("(lm.)","",esType)


  # Settings for model-based OR
  if(grepl("(lm.OR)+",esType)){
    st        <- exp(st)
    esType.cl <- "Asym"
    CIcalc    <- FALSE
  }

  if(is.na(esType.cl)){esType.cl<-esType}

  if(is.na(st)|is.null(st)|is.nan(st)){

    ES <- compute.es::tes(t=2, level= 95, n.1 = 100, n.2 = 100, verbose = FALSE, dig = 5)
    ES[seq_along(ES)] <- NA
    ES <- c(st,st,st,ES)

    CIcalc <- FALSE
  }


  if(CIcalc){

    getCI <- TRUE

    if(esType=="OR"){
      # Fisher exact test gives exact noncentral hypergeometric CI
      sCI <- cbind(ncp    = testInfo$estimate,
                   ncp.lo = testInfo$conf.low,
                   ncp.hi = testInfo$conf.high)
      getCI <- FALSE
    }

    if(grepl("Z.f", esType, fixed = TRUE)){
       if(testInfo$method=="Fisher r-to-Z transformed test for difference between 1 observed correlation and a hypothesized value."){
      sCI <- cbind(ncp    = testInfo$estimate,
                   ncp.lo = testInfo$conf.low,
                   ncp.hi = testInfo$conf.high)
      esType <- esType.cl <- "r"
      getCI <- FALSE
    } else {
      getCI <- FALSE
      esType <- esType.cl <- "Z"
      sCI <- cbind(ncp  = st)
    }
    }

    if(getCI){
      sCI <- get.ncpCI(st, df1, df2, N, esType.cl, CL, keepSign, alternative)
      if(esType=="f"){sCI[1,is.na(sCI)]<-1}
      if(esType%in%c("t","t.p","t.r","Z")){sCI[1,is.na(sCI)]<-0}
    }
    # no CI
  } else {
    sCI <- cbind(ncp  = st)
  }

  esComp <- list()
  cnt    <- 0

  for(cnt in seq_along(sCI)){

   # browser()

    x <- sCI[cnt]
    if(x==0|is.na(x)|is.nan(x)){
      x <- rnorm(1)*1e-12
      disp("ES converison: A test statistic of 0 (or NA, or NaN) was changed to rnorm(1) * 1e-12 in order to enable ES conversion.", header= FALSE, footer = FALSE)}

    # This effectively ignores model based stats
    esType <- gsub("lm.","",esType,fixed=TRUE)
    #esType <- gsub("Z.f","r",esType,fixed=TRUE)

    ncCI <- list()
    switch(esType,
           t.p  = esComp[[cnt]] <- compute.es::des(d   = x, n.1 = (N/2), n.2 = (N/2), level=CL*100, verbose = FALSE, dig = 5),
           t    = esComp[[cnt]] <- compute.es::tes(t   = x, level=CL*100,
                                                   n.1 = n1, n.2 = n2, verbose = FALSE, dig = 5),
           lm.t = esComp[[cnt]] <- compute.es::a.tes(t=x, level=CL*100,
                                                     n.1 = n1, n.2 = n2, R = rID, q = q,
                                                     verbose = FALSE, dig = 5),
           t.r  = esComp[[cnt]] <- compute.es::res(r = x, level=CL*100, var.r = ((1-x^2)^2)/(N-1),
                                                   n = N, verbose = FALSE, dig = 5),
           r  = esComp[[cnt]] <- compute.es::res(r = x, level=CL*100, var.r = NULL,
                                                  n = N, verbose = FALSE, dig = 5),
           #compute.es::res(r=x, level=CL, n=N, verbose = FALSE, dig = 5),
           f    = esComp[[cnt]] <- compute.es::fes(f=x, level=CL*100,
                                                   n.1 = n1, n.2 = n2, verbose = FALSE, dig = 5),
           lm.f = esComp[[cnt]] <- compute.es::a.fes(f=x, level=CL*100,
                                                     n.1 = n1, n.2 = n2, R = rID, q = q,
                                                     verbose = FALSE, dig = 5),
           X2   = esComp[[cnt]] <- compute.es::chies(chi.sq = x, level = CL*100,
                                                     n = N, verbose = FALSE, dig = 5),
           Z    = esComp[[cnt]] <- compute.es::pes(p = ifelse(pnorm(abs(x), lower.tail= FALSE)*2==0,.Machine$double.eps,pnorm(abs(x), lower.tail= FALSE)*2), level = CL*100, n.1 = n1, n.2 = n2, tail = "two", verbose = FALSE, dig = 5),
           lm.Z  = esComp[[cnt]] <- compute.es::a.pes(p = pnorm(abs(x), lower.tail= FALSE)*2, level = CL*100, n.1 = n1, n.2 = n2, R = rID, q = q, tail = alternative, verbose = FALSE, dig = 5),
           OR    = esComp[[cnt]] <- compute.es::lores(lor=log(x), n.1 = n1, n.2 = n2, var.lor = var.lor, verbose = FALSE, dig = 5, level = CL*100)
    )
  }

  # This section re-calculates CI based on the exact CI for the test statistic obtained from MBESS in function get.ncpCI
  if(cnt>1){

    if(esType%in%c("r","t.r")){

      ncp <- compute.es::tes(t=2, level= 95, n.1 = 100, n.2 = 100, verbose = FALSE, dig = 5)
      ncp[seq_along(ncp)] <- NA
      id.l <- c("l.d","l.r", "l.z", "l.or", "l.lor")
      id.u <- c("u.d","u.r", "u.z", "u.or", "u.lor")
      id.e <- c("d", "r", "fisher.z", "OR", "lOR")
      rNames <- names(compute.es::res(r=1,var.r=.5, n=100, level=95,dig=5,verbose = FALSE))
      ncp[,rNames] <- esComp[[1]][,rNames]
      ncp$N.total <- N
      ncp$n.1 <- n1
      ncp$n.2 <- n2

      if(esType=="t.r"){
        sCI <- get.ncpCI(testInfo$statistic, df1, df2, N, "t", CL, keepSign)
      }
    } else {

      ncp  <- esComp[[1]]
      id.l <- c("l.d", "l.g", "l.r", "l.z", "l.or", "l.lor")
      id.u <- c("u.d", "u.g", "u.r", "u.z", "u.or", "u.lor")
      id.e <- c("d", "g", "r", "fisher.z", "OR", "lOR")
    }

    ncp[,id.l] <- esComp[[2]][,id.e]
    ncp[,id.u] <- esComp[[3]][,id.e]

    ES <- cbind(sCI, ncp[,colnames(ncp)!="NNT"])

  } else {

    if(esType.cl%in%"Asym"){
      sCI <- cbind(ncp    = esComp[[1]]$lOR,
                   ncp.lo = esComp[[1]]$l.lor,
                   ncp.hi = esComp[[1]]$u.lor)
    }
    ncp  <- esComp[[1]]
    ES   <- cbind(sCI, ncp[,colnames(ncp)!="NNT"])

  }

  colnames(ES)[1:cnt] <- c("ncp","ncp.lo","ncp.hi")[1:cnt]

  # compute.es keeps the sign for d and related ES, but not for r if tes and des are used,.
  # If keepSign = TRUE the sign from d will be copied to r and related es.

  # unique(ML2.key$stat.type)
  #  "t"    "t.r"  "OR"   "lm.t" "Z"    "f"    "lm.Z"
  if(cnt>1){
  if(!all((sign(ES$ncp)==sign(ES[ ,c("d","r")])),(sign(ES$ncp.lo)==sign(ES[ ,c("l.d","l.r")])),(sign(ES$ncp.hi)==sign(ES[ ,c("u.d","u.r")])), na.rm = TRUE) & !esType%in%c("OR","t.r","r")){
    if(keepSign){
      if(esType%in%c("X2","f")){
        id.l <- which(colnames(ES) %in% c("l.d", "l.g", "l.r", "l.z"))
        id.u <- which(colnames(ES) %in% c("u.d", "u.g", "u.r", "u.z"))
        id.e <- which(colnames(ES) %in% c("d", "cliffs.d", "g", "r", "fisher.z"))
        col.id <- c(id.e,id.l,id.u)
      }
      if(any(esType%in%c("lm.Z","Z"))){ # esType=="Z"|esType=="lm.Z"
        col.id <-which(colnames(ES)%in%c("d","l.d","u.d",keepSignNames))
      }
      if(esType%in%c("t","lm.t")){
        col.id <-which(colnames(ES)%in%keepSignNames)
      }
      col.id <- sort(col.id)
      ES[ ,col.id] <- ES[ ,col.id] * sign(sCI)[1:cnt]
    }
  }
}
  return(ES)
}


#' get.ncpCI
#'
#' @param x  A noncentrality parameter.
#' @param df1  Degrees of freeddom.
#' @param df2  NULL or degrees of freedom of the denominator for the f-distribution.
#' @param N  Sample size
#' @param esType     Type of test statistic. One of: "t", "t.r", lm.t", "f", "lm.f", "r", "X2", "Z", "lm.Z"
#' @param CL    Confidence Limit (default: .95).
#' @param keepSign     Return effect size with sign of test statistic? (default = TRUE).
#' @param keepDirection  Use the information in \code{alternative} to decide on one-sided vs. two-sided confidence intervals. Default is \code{TRUE}. If \code{FALSE}, two-sided CIs will be calclulated irrespective of the direction of the \code{alternative}.
#' @param alternative    Alternative hypothesis (defult = "two").
#'
#'

#' @export
#'

get.ncpCI <- function(x, df1, df2, N, esType, CL=.95, keepSign = TRUE, keepDirection = TRUE, alternative = "two.sided"){
  #require(MBESS)
  esType <- gsub("lm.","",esType)
  ncCI   <- list()

  Talpha  <- 1-CL
  if(grepl("two",alternative)|keepDirection==FALSE){
    CLimsZ  <- c(Talpha/2, CL + (Talpha/2))
    CLims   <- c(NULL,NULL)
    Tsides <- 2
  }
  if(!grepl("two",alternative)&keepDirection==TRUE){
    ifelse(alternative == "greater",
           CLims <- c(-Inf,  1-Talpha),
           CLims <- c(Talpha, +Inf))
    CLimsZ <- CLims
    Tsides <- 1
    CL <- NULL
  }



  switch(esType,
         t.p = ncCI <- list(Lower.Limit = MBESS::ci.sm(sm = x, alpha.lower = CLims[1], alpha.upper = CLims[2],
                                                       conf.level=CL,N=N)$Lower.Conf.Limit.Standardized.Mean,
                            Upper.Limit = MBESS::ci.sm(sm = x, alpha.lower = CLims[1], alpha.upper = CLims[2],
                                                       conf.level=CL,N=N)$Upper.Conf.Limit.Standardized.Mean),
         t   = ncCI <- MBESS::conf.limits.nct(t.value=x, alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                              df=df1),
         f   = ncCI <- MBESS::conf.limits.ncf(F.value=x, alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                              df.1=df1, df.2=df2),
         #t.r = ncCI <- MBESS::conf.limits.nct(t.value=x, conf.level=CL, df=df1),
         t.r = ncCI <- list(Lower.Limit = MBESS::ci.R(R=abs(x), alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                                      N=N, K=1)$Lower.Conf.Limit.R,
                            Upper.Limit = MBESS::ci.R(R=abs(x), alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                                      N=N, K=1)$Upper.Conf.Limit.R),
         X2  = ncCI <- MBESS::conf.limits.nc.chisq(Chi.Square=x, alpha.lower = CLims[1], alpha.upper = CLims[2], conf.level=CL,
                                                   df=df1),
         Z   = ncCI <- list(Lower.Limit = (x + qnorm(CLimsZ[2], lower.tail = FALSE)),
                            Upper.Limit = (x + qnorm(CLimsZ[1], lower.tail = FALSE)))
  )

  #!all(sign(ES$ncp)==sign(ES[ ,c("d","r")]),na.rm = TRUE)&esType!="OR"
  if((sign(x)==-1)&keepSign&grepl("t.r",esType)){
    out <- cbind(ncp    = x,
                 ncp.lo = sign(x) * ncCI$Upper.Limit,
                 ncp.hi = sign(x) * ncCI$Lower.Limit)
  } else {
    out <- cbind(ncp    = x,
                 ncp.lo = ncCI$Lower.Limit,
                 ncp.hi = ncCI$Upper.Limit)
  }
  return(out)
}



#' Fisher Z test for correlations
#'
#' @param r1 First correlation
#' @param r2 Second correlation
#' @param n1 First sample size
#' @param n2 Second sample size
#' @param p Compute p-value? (default = TRUE)
#' @param Cohens.q Compute effect size Cohen's q (default = TRUE)
#' @param alpha Alpha evel for significance test
#' @param alternative One of "greater", "less", "two.sided" (default)
#'
#' @return An `htest.fisherz` object
#'
#' @export
#'
cor_test_fisherZ <- function(r1 = NULL,
                             r2 = NULL,
                             n1 = NULL,
                             n2 = NULL,
                             p  = TRUE,
                             Cohens.q    = TRUE,
                             conf.level  = .95,
                             alternative = "two.sided",
                             null.value  = 0,
                             cor.type = "pearson"){

  effect.size    <- NULL
  effect.size.ci <- NULL
  interp <- ""

  if(grepl("two.sided",alternative)){
    sides <- 2
  } else {
    sides <- 1
  }

  alpha <- 1-conf.level


  if(is.na(r2%00%NA)){
    if(!is.na(n2%00%NA)){
      message("Assuming 1 correlation...\n")
      n1 <- n1+n2
    }
    n2 <- NA
  }


  oneCor = FALSE
  if(all(is.null(r2),is.null(n2))){
    oneCor = TRUE
  } else {
    if(all(is.na(r2),is.na(n2))){
      oneCor = TRUE
    }
  }

  if(oneCor){
    if((dim(as.matrix(r1))[2]==2)){
      r1 <- stats::cor(r1[,1],r1[,2],use="pairwise.complete.obs", method = cor.type)
    } else{
      if(all((dim(as.matrix(r1))!=1))){
        disp(message = "r1 needs to be:", header = "cor_test_fisherZ", footer = FALSE)
        disp(message = "- Either a single numerical value representing a correlation,",
             header = FALSE, footer = FALSE)
        disp(message = "- Or a 2 column matrix from which a correlation r1 can be calculated",
             header = FALSE)
        stop
      }
    }

    z <- atanh(r1)/sqrt(1/(n1-3))
    conf.low   <- tanh(atanh(r1) - (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3)))))
    conf.high  <- tanh(atanh(r1) + (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3)))))
    if(p){p<-2*(1-pnorm(abs(z)))} else {p=NULL}
    if(Cohens.q){
      effect.size <- (atanh(r1)- 0)
      names(effect.size) <- "Cohen's q (Zr1-Zρ0)"
      conf.low.q   <- effect.size - (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))))
      conf.high.q  <- effect.size + (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))))
      effect.size.ci <- c(conf.low.q, conf.high.q)
      names(effect.size.ci) <- paste(conf.level*100,"percent effect-size confidence interval:")
      #<.1: no effect; .1 to .3: small effect; .3 to .5: intermediate effect; >.5: large effect.
      interp <- ifelse(abs(effect.size)<.1,"No effect",
                       ifelse(between(abs(effect.size),.1,.3),"Small effect",
                              ifelse(between(abs(effect.size),.3,.5),"Intermediate effect","Large effect"
                              )
                       )
      )
      names(interp) <- "Interpretation of magnitude:"
    }

    names(z)         <- "Fisher.z"
    estimate         <- r1
    names(estimate)  <- c("cor")
    parameter        <- n1
    names(parameter) <- "n1"

    method    <- "Fisher r-to-Z transformed test for difference between 1 observed correlation and a hypothesized value."
    data.name <- paste("r1 (",cor.type,")")

  } else {
    if((dim(as.matrix(r1))[2]==2)&(dim(as.matrix(r2))[2]==2)){
      r1 <- stats::cor(r1[,1],r1[,2], use = "pairwise.complete.obs", method = cor.type)
      r2 <- stats::cor(r2[,1],r2[,2], use = "pairwise.complete.obs", method = cor.type)
    } else{
      if(all((dim(as.matrix(r1))!=1))&all(dim(as.matrix(r2))!=1)){
        disp(message = "r1 and r2 each need to be:", header = "cor_test_fisherZ", footer = FALSE)
        disp(message = "- Either a single numerical value representing a correlation,", header = FALSE, footer = FALSE)
        disp(message = "- Or a 2 column matrix from which a correlation r1 and r2 can be calculated", header = FALSE)
        stop
      }
    }

    z  <- ((atanh(r1)-atanh(r2))/sqrt((1/(n1-3))+(1/(n2-3))))
    fm <- qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))+(1/(n2-3)))
    # conf.low.r1   <- tanh(atanh(r1)-fm)
    # conf.high.r1  <- tanh(atanh(r1)+fm)
    # conf.low.r2   <- tanh(atanh(r2)-fm)
    # conf.high.r2  <- tanh(atanh(r2)+fm)

    conf.low   <- paste0("cor1 [",
                         round(tanh(atanh(r1)-fm), digits = 3),",",
                         round(tanh(atanh(r1)+fm), digits = 3),"]")
    conf.high  <- paste0("cor2 [",
                         round(tanh(atanh(r2)-fm), digits = 3),",",
                         round(tanh(atanh(r2)+fm), digits = 3),"]")

    if(p){p<-2*(1-pnorm(abs(z)))} else {p=NULL}
    if(Cohens.q){
      effect.size <- (atanh(r1) - atanh(r2))
      names(effect.size) <- "Cohen's q (Zr1-Zr2-Zρ0)"
      conf.low.q   <- effect.size - (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))+(1/(n2-3))))
      conf.high.q  <- effect.size + (qnorm(1-(alpha/sides)) * sqrt((1/(n1-3))+(1/(n2-3))))
      effect.size.ci <- c(conf.low.q, conf.high.q)
      names(effect.size.ci) <- paste(conf.level*100,"percent effect-size confidence interval:")
      #<.1: no effect; .1 to .3: small effect; .3 to .5: intermediate effect; >.5: large effect.
      interp <- ifelse(abs(effect.size)<.1,"No effect",
                       ifelse(between(abs(effect.size),.1,.3),"Small effect",
                              ifelse(between(abs(effect.size),.3,.5),"Intermediate effect","Large effect"
                              )
                       )
      )
      names(interp) <- "Interpretation of magnitude:"
    }

    names(z) <- "Fisher.z"
    estimate <- c(r1, r2)
    names(estimate)<- c("cor1","cor2")
    parameter <- c(n1,n2)
    names(parameter) <- c("n1", "n2")
    method <- "Fisher r-to-Z transformed test for difference between 2 independent correlations"
    data.name <- paste("r1; r2 (",cor.type,")")
  }

  conf.int <- c(conf.low, conf.high)
  attr(conf.int,"conf.level") <- conf.level

  names(null.value) <- "correlation"

  stat.test <- structure(list(statistic = z,
                              parameter = parameter,
                              p.value   = p,
                              estimate  = estimate,
                              null.value = null.value,
                              alternative = alternative,
                              method    = method,
                              data.name = data.name,
                              conf.int  = conf.int,
                              effect.size = effect.size,
                              effect.size.ci = effect.size.ci,
                              effect.size.int = interp),
                         class = c("htest","fisherZ"))
  return(stat.test)
}



#' print fisherZ
#'
#' @param obj
#'
#' @return nothing
#' @export
#'
print.cor_test_fisherZ <- function(obj){
  class(obj) <- "htest"
  print(obj)
  cat(names(obj$effect.size), "\n")
  cat(obj$effect.size, "\n")
  cat(names(obj$effect.size.int))
  cat(obj$effect.size.int, "\n")
  cat(names(obj$effect.size.ci)[1], "\n")
  cat(obj$effect.size.ci, "\n")
}

#' z.test
#'
#' An `htest` implentation of the z test for an observed mean or proportion.
#'
#' @param x Observed value
#' @param mu Population mean under H_{0}
#' @param pi Population proportion under H_{0}
#' @param N Sample size
#' @param sigma Population standard deviation
#' @param proportion Observed proportion
#' @param alternative
#'
#' @export
#'
z.test <- function(x = 0, mu = 0, pi = NULL, N = 0, sigma = 1, proportion=FALSE, alternative = "two.sided"){
  #require(dplyr)
  if(proportion){
    if(!is.null(pi)){
      if(all(dplyr::between(x,0,1),dplyr::between(pi,0,1))){
        SE <- sqrt((pi * (1-pi))/N)
        z <- (x-pi)/SE
      } else {
        stop("Argument x and/or pi not in [0,1]")
      }
    } else {
      stop("Argument pi (expected population proportion) is NULL.")
    }
  } else {
    SE <- sigma / sqrt(N)
    z <- (x-mu)/SE
  }

  if(alternative=="two.sided"){
    p <- 2*(1-pnorm(abs(z)))
  } else {
    p <- 1-pnorm(abs(z))
  }
  names(z) <- "Z"

  if(proportion){
    null.value <- pi
    names(null.value) <- "π"
  } else {
    null.value <- mu
    names(null.value) <- "mu"
  }

  if(proportion){
    estimate <- x-pi
    names(estimate) <- "(p-π)"
  } else {
    estimate <- x-mu
    names(estimate) <- "(x-mu)"
  }

  statistic <- z
  ifelse(proportion,
         names(statistic) <- "(p-π)/sqrt(π*(1-π)/N)",
         names(statistic) <- "(x-mu)/(sigma/sqrt(N))"
  )
  parameter <- N
  names(parameter) <- "N"
  stat.test <- structure(list(statistic = z,
                              null.value = null.value,
                              estimate = estimate,
                              statistic = statistic,
                              method="Z test",
                              parameter=parameter,
                              p.value=p),
                         class = "htest")
  return(stat.test)
}


# Wilcox functions for bootstrapped CI on difference between correlations
# https://raw.githubusercontent.com/nicebread/WRS/master/pkg/R/Rallfun-v31.R

twopcor<-function(x1,y1,x2,y2,SEED=TRUE){
  #
  #   Compute a .95 confidence interval for
  #   the difference between two Pearson
  #   correlations corresponding to two independent
  #   goups.
  #
  #   This function uses an adjusted percentile bootstrap method that
  #   gives good results when the error term is heteroscedastic.
  #
  #   WARNING: If the number of boostrap samples is altered, it is
  #   unknown how to adjust the confidence interval when n1+n2 < 250.
  #
  nboot<-599  #Number of bootstrap samples
  if(SEED)set.seed(2) # set seed of random number generator so that
  #             results can be duplicated.
  X<-elimna(cbind(x1,y1))
  x1<-X[,1]
  y1<-X[,2]
  X<-elimna(cbind(x2,y2))
  x2<-X[,1]
  y2<-X[,2]
  cat("\nTaking bootstrap samples; please wait\n")
  data1<-matrix(sample(length(y1),size=length(y1)*nboot,replace=TRUE),nrow=nboot)
  bvec1<-apply(data1,1,pcorbsub,x1,y1) # A 1 by nboot matrix.
  data2<-matrix(sample(length(y2),size=length(y2)*nboot,replace=TRUE),nrow=nboot)
  bvec2<-apply(data2,1,pcorbsub,x2,y2) # A 1 by nboot matrix.
  bvec<-bvec1-bvec2
  ilow<-15
  ihi<-584
  if(length(y1)+length(y2) < 250){
    ilow<-14
    ihi<-585
  }
  if(length(y1)+length(y2) < 180){
    ilow<-11
    ihi<-588
  }
  if(length(y1)+length(y2) < 80){
    ilow<-8
    ihi<-592
  }
  if(length(y1)+length(y2) < 40){
    ilow<-7
    ihi<-593
  }
  bsort<-sort(bvec)
  r1<-stats::cor(x1,y1)
  r2<-stats::cor(x2,y2)
  ci<-c(bsort[ilow],bsort[ihi])
  list(r1=r1,r2=r2,ci=ci)
}

elimna<-function(m){
  #
  # remove any rows of data having missing values
  #
  if(is.list(m)){
    for(j in 1:length(m))m[[j]]=na.omit(m[[j]])
    elimna=m
  }
  if(!is.list(m)){
    if(is.null(dim(m)))m<-as.matrix(m)
    ikeep<-c(1:nrow(m))
    for(i in 1:nrow(m))if(sum(is.na(m[i,])>=1))ikeep[i]<-0
    elimna<-m[ikeep[ikeep>=1],]
  }
  elimna
}

pcorbsub<-function(isub, x, y)
{
  #
  #  Compute Pearson's correlation using x[isub] and y[isub]
  #  isub is a vector of length n,
  #  a bootstrap sample from the sequence of integers
  #  1, 2, 3, ..., n
  #
  pcorbsub<-stats::cor(x[isub],y[isub])
  pcorbsub
}


#' decide.Equalvar
#'
#' @param vars int
#' @param labels int
#' @param key int
#' @param alpha int
#' @param criterion int
#' @param group int
#' @param verbose  int
#'
#'
#' @export
#'
decide.EqualVar <- function(vars, labels, key, alpha=.05, criterion = 2, group, verbose = FALSE){

  if(any(names(vars) == "cleanDataFilter")){vars$cleanDataFilter <- NULL}

  vars$N  <- NULL
  longDat <- NULL

  if(length(vars)==2){

    if(any(plyr::laply(vars,is.factor))){

      longDat <- cbind.data.frame(vars)
      colnames(longDat)[plyr::laply(vars,is.factor)]    <- "group"
      colnames(longDat)[!(plyr::laply(vars,is.factor))] <- "xy"

    } else {

      if(all(lengths(vars)>1)){
        longDat <- plyr::ldply(unlist(vars))
        longDat <- data.frame(xy = longDat$V1, group = factor(c(rep(1, length(vars[[1]])),
                                                                rep(2, length(vars[[2]])))))
      }
    }

    if(!is.null(longDat)){

      t1 <- var.test(xy ~ group, data = longDat)
      t2 <- car::leveneTest(xy ~ group, data = longDat)
      t3 <- bartlett.test(xy ~ group, data = longDat)
      t4 <- fligner.test(xy ~ group, data = longDat)

      IDeq <- c(t1$p.value, t2$`Pr(>F)`[[1]], t3$p.value, t4$p.value) > alpha

      varEqual <- sum(IDeq, na.rm = TRUE) >= criterion

      if(verbose){
        cat(paste0("\n>> decide.EqualVar <<\n\n",sum(IDeq)," out of ",length(IDeq)," tests for equality of variances indicate equal population variances (p > ",alpha,"):\n\n", paste0("1. ", t1$method," (p = ",round(t1$p.value,digits = 2),")\n2. ",attributes(t2)$heading,"(p = ",round(t2$`Pr(>F)`[[1]],digits = 2),")\n3. ",t3$method," (p = ",round(t3$p.value, digits = 2),")\n4. ",t4$method," (p = ",round(t4$p.value,digits = 2),")\n")))}

    } else {
      varEqual <- NA
    }

  } else {
    varEqual <- NA
  }

  #   form <-  gsub("[():]|(summary|lm|lmer|lme4|lmerTest|t.test|anova|lmer|cor.test|chiq.test)","",key$stat.test)

  disp(paste0(group,": var.equal set to: ",varEqual),header = FALSE, footer = FALSE)
  return(varEqual)
}

tidyDF <- function(df){
  for(l in seq_along(df$labels)){
    plyr::ldply(df$labels[[l]], function(d) broom::tidy(data.frame(eval(parse(text = paste0('df$',d))))))
  }
}


#' @title try.CATCH both warnings (with value) and errors
#'
#' @description
#'  In longer simulations, aka computer experiments,
#'  you may want to
#'  1) catch all errors and warnings (and continue)
#'  2) store the error or warning messages
#'
#'  Here's a solution  (see R-help mailing list, Dec 9, 2010):
#'
#' Catch *and* save both errors and warnings, and in the case of
#' a warning, also keep the computed result.
#'
#' @export
#' @param expr an \R expression to evaluate
#' @return a list with 'value' and 'warning', where value' may be an error caught.
#' @author Martin Maechler;
#' Copyright (C) 2010-2012  The R Core Team
try.CATCH <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}


#' Rose tinted infix
#'
#'
#' @param x If \code{x} is any of \code{Inf,-Inf,NA,NaN,NULL,length(x)==0}, it will return \code{y}; otherwise it returns \code{x}.
#' @param y The value to return in case of catastrophy \code{>00<}
#'
#' @export
#' @author Fred Hasselman
#' @description When your functions wear these rose tinted glasses, the world will appear to be a nicer, fluffier place.
#' @seealso purrr::%||%
#' @examples
#' Inf %00% NA
#'
#' numeric(0) %00% ''
#'
#' NA %00% 0
#'
#' NaN %00% NA
#'
#' NULL %00% NA
`%00%` <- function(x,y){
  l0<-isna<-isnan<-isinf<-isnll<-FALSE
  if(length(x)==0){
    l0=TRUE
  } else {
    if(all(is.na(x)))       isna =TRUE
    if(all(is.nan(x)))      isnan=TRUE
    if(all(is.infinite(x))) isinf=TRUE
    if(all(is.null(x)))     isnll=TRUE
  }
  if(any(l0,isna,isnan,isinf,isnll)){x<-y}
  return(x)
}

init <- function(){
  # for testing purposes
  #srcDir <- "~/Documents/GitHub/manylabRs/manylabRs/R/"
  #source(paste0(srcDir,"C-3PR_ASCII.R"))
  #source(paste0(srcDir,'getData.R'))
  #source(paste0(srcDir,'inIT.R'))
  #source(paste0(srcDir,'ML2_variable_functions.R'))
  #source(paste0(srcDir,'fRedsRutils.R'))

  # Function inIT will load and -if necessary- install packages passed in a list (unIT will do the reverse operation).
  in.IT(c("MBESS","reshape2","plyr","tidyverse","metafor","RCurl","openxlsx","broom","httr","compute.es","downloader","car", "lme4", "lmerTest","exact2x2","ggplot2","gplots","gridExtra","lattice","latticeExtra","rio","scales","lubridate"))

}

#' disp
#'
#' @param message     A message to be displayed in the Console.
#' @param header     Print a header of '~' symbols (=\code{TRUE}), or '~' symbols with few words of text (=\code{character vector})
#' @param footer     Print a footer '~' symbols.
#'
#' @description Displays easy-to-spot text in the Console.
#'
#' @author Fred Hasselman
#'
#' @export
#'
disp <- function(message='Hello world!', header = "disp", footer = TRUE){

  ps <- "# "

  msg <- textConnection(message)
  mWidth <- max(plyr::laply(readLines(msg),nchar))
  if(!grepl(ps,message)) mWidth <- mWidth+2

  if(is.character(header)){
    hWidth <- max(plyr::laply(header,nchar))
    mWidth <- max(hWidth,mWidth)
  }

  dmessage <- list()
  for(m in 1:length(message)){
    # b <- floor((mWidth-nchar(message[m]))/2)
    e <- mWidth-nchar(message[m])
    dmessage[[m]] <- paste0(ps,message[m])
  }

  banner <- paste0(rep('~', mWidth), collapse = "")
  if(is.character(header)){
    b <- floor((nchar(banner)-nchar(header))/2)
    e <- ceiling((nchar(banner)-nchar(header))/2)
    leader <- paste0('\n\t',paste0(rep('~',b),collapse=""),header,paste0(rep('~',e),collapse=""))
  }
  if(header == TRUE){
    leader <- banner
  }
  if(header == FALSE){
    leader <- paste0(ps)
  }

  if(footer){
    cat(paste0('\n\t',leader,'\n\t',dmessage,'\n\t',banner,'\n'))
  } else {
    cat(paste0('\n\t',leader,'\n\t',dmessage))
  }
  close(msg)
  return(invisible(message))
}



#' Elastic Scaler - A Flexible Rescale Function
#'
#' @description The 'elastic scaler'will rescale numeric vectors (1D, or columns in a matrix or data.frame) to a user defined minimum and maximum, either based on the extrema in the data, or, a minimum and maximum defined by the user.
#'
#' @param x     Input vector or data frame.
#' @param mn     Minimum value of original, defaults to \code{min(x, na.rm = TRUE)}.
#' @param mx     Maximum value of original, defaults to \code{max(x, na.rm = TRUE)}.
#' @param hi     Minimum value to rescale to, defaults to \code{0}.
#' @param lo     Maximum value to rescale to, defaults to \code{1}.
#'
#'
#' @details Three uses:
#' \enumerate{
#' \item elascer(x)             - Scale x to data range: min(x.out)==0;      max(x.out)==1
#' \item elascer(x,mn,mx)       - Scale x to arg. range: min(x.out)==mn==0;  max(x.out)==mx==1
#' \item elascer(x,mn,mx,lo,hi) - Scale x to arg. range: min(x.out)==mn==lo; max(x.out)==mx==hi
#' }
#'
#' @return scaled inout
#' @export
#'
#' @examples
#' # Works on numeric objects
#' somenumbers <- cbind(c(-5,100,sqrt(2)),c(exp(1),0,-pi))
#'
#' elascer(somenumbers)
#' elascer(somenumbers,mn=-100)
#
#' # Values < mn will return < lo (default=0)
#' # Values > mx will return > hi (default=1)
#' elascer(somenumbers,mn=-1,mx=99)
#'
#' elascer(somenumbers,lo=-1,hi=1)
#' elascer(somenumbers,mn=-10,mx=101,lo=-1,hi=4)
elascer <- function(x,mn=min(x,na.rm=T),mx=max(x,na.rm=T),lo=0,hi=1){
  UNLIST = FALSE
  if(!is.data.frame(x)){UNLIST=TRUE}
  x <- as.data.frame(x)
  u <- x
  for(i in 1:NCOL(x)){
    mn=min(x[,i],na.rm=T)
    mx=max(x[,i],na.rm=T)
    if(mn>mx){warning("Minimum (mn) >= maximum (mx).")}
    if(lo>hi){warning("Lowest scale value (lo) >= highest scale value (hi).")}
    ifelse(mn==mx,{u[,i]<-rep(mx,length(x[,i]))},{
      u[,i]<-(((x[i]-mn)*(hi-lo))/(mx-mn))+lo
      id<-stats::complete.cases(u[,i])
      u[!id,i]<-0
    })
  }
  if(UNLIST){
    u <- as.numeric(u[,1])
  }
  return(u)
}


center <- function(numvec, na.rm=TRUE, type = c("mean","median")[1]){
  switch(type,
         mean   = centroid <- mean(as.numeric(numvec), na.rm=na.rm),
         median = centroid <- median(as.numeric(numvec), na.rm=na.rm),
  )
  return((as.numeric(numvec) - centroid))
}

normalise <- function(numvec, na.rm=TRUE){
  (as.numeric(numvec) - mean(as.numeric(numvec),na.rm=na.rm)) / sd(as.numeric(numvec),na.rm=na.rm)
}

# Help lme4 get a better convergence
nlopt <- function(par, fn, lower, upper, control) {
  # Add to call: control = lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE
  .nloptr <<- res <- nloptr::nloptr(par, fn, lb = lower, ub = upper,
                                    opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
                                                maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
       fval = res$objective,
       conv = if (res$status > 0) 0 else res$status,
       message = res$message
  )
}

# Convert decimal point
c2p <- function(text,N=1){
  if(!is.character(text)){text<-as.character(text)}
  if(sum(grepl("[,]",text))>=N){text <- gsub(",",".",text)}
  return(text)
}

# Count missing values in x
nmissing <- function(x){
  sum(is.na(x))
}

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data = NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval = .95, .drop=TRUE) {
  #library(plyr)

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=na.rm) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )

  # Rename the "mean" column
  #datac <- rename(datac, c("mean" = measurevar))


  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}


# ML2 VARIABLE FUNCTIONS -----
#' Huang.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#1_huang}
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis A \code{list} object containing fields \strong{High}, \strong{Low} and \strong{N}
#'
#' @section Variables:
#'
#' huan1.1_Y1: Y position of the mouse (High SES condition).
#' huan2.1_Y1: Y position of the mouse (Low SES).
#' huan1.1_R0 and huan2.1_R0 indicate for each condition whether a click was inside the map (1) or outside (0).
#'
#' For each condition a participant must have clicked inside the map (=1) to be included in the analysis.
#'
#' @export
#'
varfun.Huang.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable = c((-1*(vars$High[[1]]-238)),(-1*(vars$Low[[1]]-238))),
                                factor = c(rep(names(vars[1]),NROW(vars[[1]])), rep(names(vars[2]),NROW(vars[[2]]))))

  return(list(High = -1*(vars$High[[1]]-238),
              Low  = -1*(vars$Low[[1]]-238),
              N    = c(nrow(vars$High),nrow(vars$Low)),
              cleanDataFilter = cleanDataFilter)
  )
}

#' varfun.Kay.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#2_kay}
#'
#' @param vars      A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @export
#'
#' @return Dataset ready for analysis
#'
varfun.Kay.1 <- function(vars){
  #   require(dplyr)

  var.Order <- rowMeans(dplyr::select(vars$Order, one_of(c('kay1.5','kay1.6'))))
  # [(1) Centered Subjective value = (GP1 + GP2)]$(2)residuals + (3) mean Willingness to engage in goal pursuit
  var.windex.Order <-lm(scale(vars$Order$kay1.4,scale=F)~var.Order)$residuals + var.Order
  vars$Order$windex.Order <-var.windex.Order

  var.DisOrder <- rowMeans(dplyr::select(vars$DisOrder,one_of(c('kay2.5','kay2.6'))))
  # [(1) Centered Subjective value = (GP1 + GP2)]$(2)residuals + (3) mean Willingness to engage in goal pursuit
  var.windex.DisOrder <-lm(scale(vars$DisOrder$kay2.4,scale=F)~var.DisOrder)$residuals + var.DisOrder
  vars$DisOrder$var.windex.DisOrder <- var.windex.DisOrder

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),variable = c(vars[[1]]$windex.Order,vars[[2]]$var.windex.DisOrder), factor = c(rep(names(vars[1]),NROW(vars[[1]])), rep(names(vars[2]),NROW(vars[[2]]))))

  return(list(Order         = var.windex.Order,
              DisOrder      = var.windex.DisOrder,
              N             = c(length(var.windex.Order),length(var.windex.DisOrder)),
              cleanDataFilter = cleanDataFilter)
  )
}


#' varfun.Alter.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#3_alter}
#'
#' @param vars       A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @export
#'
#' @return Dataset ready for analysis
#'
#' @section Variables:
#'
#'   Syllogisms to include for each sample
#'   INCLUSION PERCENTAGE BASED ON
#'    FLUENT / DISFLUENT SEPERATELY: 1 5 6
#'    BOTH: 1 5 6
#'

varfun.Alter.1 <- function(vars){
  var.correct <- list(s1=c(7),
                      s2=c(8),
                      s3=c(5,6,7,8),
                      s4=c(8),
                      s5=c(3),
                      s6=c(8))

  lowP <- .25
  hiP  <- .75

  # Get correct answers
  ok.Fluent   <- sapply(seq_along(vars$Fluent[,-NCOL(vars$Fluent)]), function(c) unlist(vars$Fluent[,c])%in%var.correct[[c]])
  ok.DisFluent<- sapply(seq_along(vars$DisFluent[,-NCOL(vars$DisFluent)]), function(c) unlist(vars$DisFluent[,c])%in%var.correct[[c]])



  # Find columns
  # Syllogisms to include for each sample
  # INCLUSION PERCENTAGE BASED ON
  # FLUENT / DISFLUENT SEPERATELY: 1 5 6
  # BOTH: 1 5 6
  #     id.Fluent.cols    <- which((colSums(ok.Fluent)/nrow(ok.Fluent)>lowP)&(colSums(ok.Fluent)/nrow(ok.Fluent)<hiP))
  #     id.DisFluent.cols <- which((colSums(ok.DisFluent)/nrow(ok.DisFluent)>lowP)&(colSums(ok.DisFluent)/nrow(ok.DisFluent)<hiP))

  id.Fluent.cols    <- c(1, 5, 6)
  id.DisFluent.cols <- c(1, 5, 6)

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable = c(rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
                                              rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols]))),
                                factor = c(rep(names(vars[1]),NROW(vars[[1]])),
                                              rep(names(vars[2]),NROW(vars[[2]])))
                                )

  return(list(Fluent    = rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
              DisFluent = rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols])),
              N = c(nrow(ok.Fluent),nrow(ok.DisFluent)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Alter.2
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#3_alter}
#'
#' @param vars       A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @section Variables:
#'
#'   Syllogisms to include for each sample
#'   INCLUSION PERCENTAGE BASED ON
#'    FLUENT / DISFLUENT SEPERATELY: 1 5 6
#'    BOTH: 1 5 6
#'

varfun.Alter.2 <- function(vars){
  var.correct <- list(s1=c(7),
                      s2=c(8),
                      s3=c(5,6,7,8),
                      s4=c(8),
                      s5=c(3),
                      s6=c(8))

  # # Get correct answers
  # ok.Fluent   <- sapply(seq_along(vars$Fluent), function(c) unlist(vars$Fluent[,c])%in%var.correct[[c]])
  # ok.DisFluent<- sapply(seq_along(vars$DisFluent), function(c) unlist(vars$DisFluent[,c])%in%var.correct[[c]])

  # Get correct answers
  ok.Fluent   <- sapply(seq_along(vars$Fluent[,-NCOL(vars$Fluent)]), function(c) unlist(vars$Fluent[,c])%in%var.correct[[c]])
  ok.DisFluent<- sapply(seq_along(vars$DisFluent[,-NCOL(vars$DisFluent)]), function(c) unlist(vars$DisFluent[,c])%in%var.correct[[c]])


  # Syllogisms to include for each sample
  # First and last
  id.Fluent.cols      <- c(1,6)
  id.DisFluent.cols   <- c(1,6)

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable = c(rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
                                              rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols]))),
                                factor = c(rep(names(vars[1]),NROW(vars[[1]])),
                                              rep(names(vars[2]),NROW(vars[[2]])))
  )

  return(list(Fluent    = rowSums(ok.Fluent[,id.Fluent.cols]),
              DisFluent = rowSums(ok.DisFluent[,id.DisFluent.cols]),
              N = c(nrow(ok.Fluent),nrow(ok.DisFluent)),
              cleanDataFilter = cleanDataFilter)
  )
}

#' varfun.Alter.3
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#3_alter}
#'
#' @param vars      A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @section Variables:
#'
#'   Syllogisms to include for each sample
#'   INCLUSION PERCENTAGE BASED ON
#'    FLUENT / DISFLUENT SEPERATELY: 1 5 6
#'    BOTH: 1 5 6
varfun.Alter.3 <- function(vars){

  var.correct <- list(s1=c(7),
                      s2=c(8),
                      s3=c(5,6,7,8),
                      s4=c(8),
                      s5=c(3),
                      s6=c(8))

  # Get ids for Alter first
  id <- sapply(seq_along(vars$RawDataFilter[[1]]$.id), function(i) unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]"))[[1]] == "Alter")

  # Get correct answers for ids Alter first
  if(sum(id,na.rm=T)>0){
    # ok.Fluent   <- rbind(sapply(seq_along(vars$Fluent), function(c) unlist(vars$Fluent[id, c])%in%var.correct[[c]]))
    # ok.DisFluent<- rbind(sapply(seq_along(vars$DisFluent), function(c) unlist(vars$DisFluent[id, c])%in%var.correct[[c]]))
    # Get correct answers
    ok.Fluent   <- sapply(seq_along(vars$Fluent[,-NCOL(vars$Fluent)]), function(c) unlist(vars$Fluent[,c])%in%var.correct[[c]])
    ok.DisFluent<- sapply(seq_along(vars$DisFluent[,-NCOL(vars$DisFluent)]), function(c) unlist(vars$DisFluent[,c])%in%var.correct[[c]])
     } else {
    ok.Fluent    <- rep(FALSE,6)
    ok.DisFluent <- rep(FALSE,6)
  }

  # Use 1,5,6
  id.Fluent.cols    <- c(1,5,6)
  id.DisFluent.cols <- c(1,5,6)

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable = c(rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
                                              rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols]))),
                                factor = c(rep(names(vars[1]),NROW(vars[[1]])),
                                              rep(names(vars[2]),NROW(vars[[2]])))
  )

  return(list(Fluent    = rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
              DisFluent = rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols])),
              N = c(nrow(ok.Fluent),nrow(ok.DisFluent)),
              cleanDataFilter = cleanDataFilter))
}


#' varfun.Alter.4
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#3_alter}
#'
#' @param vars      A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @section Variables:
#'   Syllogisms to include for each sample
#'   INCLUSION PERCENTAGE BASED ON
#'    FLUENT / DISFLUENT SEPERATELY: 1 5 6
#'    BOTH: 1 5 6

varfun.Alter.4 <- function(vars){
  var.correct <- list(s1=c(7),
                      s2=c(8),
                      s3=c(5,6,7,8),
                      s4=c(8),
                      s5=c(3),
                      s6=c(8))

  # Get ids for Alter first
  id <- sapply(seq_along(vars$RawDataFilter[[1]]$.id), function(i) unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]"))[[1]] == "Alter")

  # Get correct answers for ids Alter first
  if(sum(id,na.rm=T)>0){
    # ok.Fluent   <- rbind(sapply(seq_along(vars$Fluent), function(c) unlist(vars$Fluent[id, c])%in%var.correct[[c]]))
    # ok.DisFluent<- rbind(sapply(seq_along(vars$DisFluent), function(c) unlist(vars$DisFluent[id, c])%in%var.correct[[c]]))
    # Get correct answers
    ok.Fluent   <- sapply(seq_along(vars$Fluent[,-NCOL(vars$Fluent)]), function(c) unlist(vars$Fluent[,c])%in%var.correct[[c]])
    ok.DisFluent<- sapply(seq_along(vars$DisFluent[,-NCOL(vars$DisFluent)]), function(c) unlist(vars$DisFluent[,c])%in%var.correct[[c]])
  } else {
    ok.Fluent    <- rep(FALSE,6)
    ok.DisFluent <- rep(FALSE,6)
  }

  # Syllogisms to include for each sample
  # First and last
  id.Fluent.cols      <- c(1,6)
  id.DisFluent.cols   <- c(1,6)

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable = c(rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
                                              rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols]))),
                                factor = c(rep(names(vars[1]),NROW(vars[[1]])),
                                              rep(names(vars[2]),NROW(vars[[2]])))
  )

  return(list(Fluent    = rowSums(rbind(ok.Fluent[ ,id.Fluent.cols])),
              DisFluent = rowSums(rbind(ok.DisFluent[ ,id.DisFluent.cols])),
              N = c(nrow(ok.Fluent),nrow(ok.DisFluent)),
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Graham.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#4_graham}
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#'
#' @export
#'
#' @return Dataset ready for analysis
#'

varfun.Graham.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = vars[[1]]$uID,
                                variable1 = vars$Politics$politics,
                                variable2 =rowMeans(vars$Binding,na.rm=TRUE)
                                )

  return(list(Politics = vars$Politics$politics,
              Binding  = rowMeans(vars$Binding,na.rm=TRUE),
              N        = sum(complete.cases(rowMeans(vars$Binding,na.rm=TRUE), vars$Politics$politics)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Graham.2
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#4_graham}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
varfun.Graham.2 <- function(vars){

  cleanDataFilter <- data.frame(uID = vars[[1]]$uID,
                                variable1 = vars$Politics$politics,
                                variable2 = rowMeans(vars$Individual, na.rm = TRUE)
                                )

  return(list(Politics   = vars$Politics$politics,
              Individual = rowMeans(vars$Individual, na.rm = TRUE),
              N          = sum(complete.cases(rowMeans(vars$Individual,na.rm=TRUE), vars$Politics$politics)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Rottenstreich.1
#'
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#5_rottenstreich}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#'  @export
#'
#' @return Dataset ready for analysis
#'
varfun.Rottenstreich.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable = factor(c(vars$Low[[1]],vars$Certain[[1]]),levels=c(1,2),labels=vars$labels$Response),
                                factor = factor(c(rep(1,nrow(vars$Low)),rep(2,nrow(vars$Certain))),levels=c(1,2),labels=vars$labels$Condition))

  return(list(Response  = factor(c(vars$Low[[1]],vars$Certain[[1]]),levels=c(1,2),labels=vars$labels$Response),
              Condition = factor(c(rep(1,nrow(vars$Low)),rep(2,nrow(vars$Certain))),levels=c(1,2),labels=vars$labels$Condition),
              N      = c(nrow(vars$Certain),nrow(vars$Low)),
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Bauer.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#6_bauer}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @export
#'
#' @return Dataset ready for analysis
#'
varfun.Bauer.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable = c(vars$Consumer[[2]],vars$Individual[[2]]),
                                factor =  c(rep(names(vars[1]),NROW(vars[[1]])),
                                               rep(names(vars[2]),NROW(vars[[2]]))))


  return(list(Consumer  = vars$Consumer[[2]],
              Individual= vars$Individual[[2]],
              N         = c(length(vars$Consumer[[2]]),length(vars$Individual[[2]])),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Miyamoto.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#7_miyamoto}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @export
#'
#' @return Dataset ready for analysis
#'
#' @details  \strong{Analysis plan:} An ANCOVA will compare the mean estimates of the author's true attitude across the two conditions, covarying for perceived constraint.
#'
#' @section Variables:
#' miya1.5=true attitude (pro-death condition; higher values=higher support for death penalty);
#' miya1.7=perceived constraint (pro-death condition; higher values=higher freedom);
#'
#' miya2.5=true attitude (against death penalty condition; higher values=higher support for death penalty);
#' miya2.7=perceived constraint (against death condition; higher values= higher freedom).

varfun.Miyamoto.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable1 = c(vars$CapitalCon[[1]],vars$CapitalPro[[1]]),
                                factor =  factor(c(rep(1,nrow(vars$CapitalCon)),rep(2,nrow(vars$CapitalPro))),levels=c(1,2),labels=vars$labels$Condition),
                                variable2 = scale(c(vars$CapitalCon[[2]],vars$CapitalPro[[2]]), scale = FALSE))


  return(list(Attitude  = c(vars$CapitalCon[[1]],vars$CapitalPro[[1]]),
              Condition = factor(c(rep(1,nrow(vars$CapitalCon)),rep(2,nrow(vars$CapitalPro))),levels=c(1,2),labels=vars$labels$Condition),
              Constraint= scale(c(vars$CapitalCon[[2]],vars$CapitalPro[[2]]), scale = FALSE),
              N = c(nrow(vars$CapitalCon),nrow(vars$CapitalPro)),
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Miyamoto.2
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#7_miyamoto}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @section Variables:
#' miya1.5=true attitude (pro-death condition; higher values=higher support for death penalty);
#' miya1.7=perceived constraint (pro-death condition; higher values=higher freedom);
#'
#' miya2.5=true attitude (against death penalty condition; higher values=higher support for death penalty);
#' miya2.7=perceived constraint (against death condition; higher values= higher freedom).

varfun.Miyamoto.2 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,
                                        vars[[2]]$uID),
                                variable1 = c(vars$CapitalCon[[1]],vars$CapitalPro[[1]]),
                                factor =  factor(c(rep(1,nrow(vars$CapitalCon)),rep(2,nrow(vars$CapitalPro))),levels=c(1,2),labels=vars$labels$Condition),
                                variable2 = scale(c(vars$CapitalCon[[2]],vars$CapitalPro[[2]]), scale = FALSE),
                                moderator = scale(c(vars$CapitalCon[[3]],vars$CapitalPro[[3]]), scale=FALSE))


  return(list(Attitude  = c(vars$CapitalCon[[1]],vars$CapitalPro[[1]]),
              Condition = factor(c(rep(1,nrow(vars$CapitalCon)),rep(2,nrow(vars$CapitalPro))),levels=c(1,2),labels=vars$labels$Condition),
              Constraint= scale(c(vars$CapitalCon[[2]],vars$CapitalPro[[2]]), scale=FALSE),
              Moderator = scale(c(vars$CapitalCon[[3]],vars$CapitalPro[[3]]), scale=FALSE),
              N = c(nrow(vars$CapitalCon),nrow(vars$CapitalPro)),
              cleanDataFilter = cleanDataFilter)
         )
}


#' varfun.Inbar.1
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#8_inbar}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @section Variables:
#' disg1.11,disg1.12,disg2.10,disg2.12,disg2.13;
#'
#' responses on the DS-R are scored as follows: True 1, False 0; Not disgusting 0, Slightly disgusting 0.5, Very disgusting 1
#'

varfun.Inbar.1 <- function(vars){

  vars$SameKiss$disg1.11 <- -vars$SameKiss$disg1.11
  vars$SameKiss$disg1.12 <- -vars$SameKiss$disg1.12
  vars$DiffKiss$disg1.11 <- -vars$DiffKiss$disg1.11
  vars$DiffKiss$disg1.12 <- -vars$DiffKiss$disg1.12

  vars$SameKiss <- dplyr::mutate(vars$SameKiss,
                                 DSRs = rowMeans(elascer(dplyr::select(vars$SameKiss, starts_with("disg"))), na.rm = TRUE)
  )
  vars$DiffKiss <- dplyr::mutate(vars$DiffKiss,
                                 DSRd = rowMeans(elascer(dplyr::select(vars$DiffKiss, starts_with("disg"))), na.rm = TRUE)
  )

  outcome <- c("Intent","Wrong","Encourage")[colnames(vars$SameKiss)[1]==c("inba1.3","inba1.4","inba1.5")]

  colnames(vars$SameKiss)[1] <- outcome
  colnames(vars$DiffKiss)[1] <- outcome

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable1 = c(vars$SameKiss[['DSRs']],vars$DiffKiss[['DSRd']]),
                                variable2 = c(vars$SameKiss[[outcome]],vars$DiffKiss[[outcome]]),
                                factor =  c(rep(names(vars[1]),NROW(vars[[1]])),
                                            rep(names(vars[2]),NROW(vars[[2]]))))

  return(list(r1=cbind(vars$SameKiss['DSRs'],vars$SameKiss[outcome]),
              r2=cbind(vars$DiffKiss['DSRd'],vars$DiffKiss[outcome]),
              N = c(nrow(vars$SameKiss),nrow(vars$DiffKiss)),
              cleanDataFilter = cleanDataFilter)
         )
}


#' varfun.Inbar.2
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#8_inbar}
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
varfun.Inbar.2 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable = c(vars$SameKiss[[1]],vars$DiffKiss[[1]]),
                                factor =  c(rep(names(vars[1]),NROW(vars[[1]])),
                                            rep(names(vars[2]),NROW(vars[[2]]))))

  return(list(SameKiss= vars$SameKiss[[1]],
              DiffKiss= vars$DiffKiss[[1]],
              N =  c(nrow(vars$SameKiss),nrow(vars$DiffKiss)),
              cleanDataFilter = cleanDataFilter)
         )
}


#' varfun.Critcher.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#9_critcher}
#'
#' @export
#'
#' @section Variables:
#'  crit1.1= % sold P97 in the US;
#'  crit2.1= % sold P17 in the US
#'
#'  df.P97 <- dplyr::select(tbl_df(ML2.df),which(colnames(ML2.df)%in%ML2.in$study.vars$Condition[1]))
#'  df.P97 <- slice(df.P97, which((ML2.id[[1]][,1]==T)&(ML2.id[[2]][,1]==T)))
#'
#'   df.P17 <- dplyr::select(tbl_df(ML2.df),which(colnames(ML2.df)%in%ML2.in$study.vars$Condition[2]))
#'   df.P17 <- slice(df.P17, which((ML2.id[[1]][,2]==T)&(ML2.id[[2]][,2]==T)))
#'
#'   id.P97 <- ML2.df[ML2.id[[1]][,1]&ML2.id[[2]][,1],ML2.in$study.vars$Condition[1]]
#'   id.P17 <- ML2.df[ML2.id[[1]][,2]&ML2.id[[2]][,2],ML2.in$study.vars$Condition[2]]
#'

varfun.Critcher.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable = c(as.numeric(vars$P97$crit1.1),as.numeric(vars$P17$crit2.1)),
                                factor =  c(rep(names(vars[1]),NROW(vars[[1]])),
                                            rep(names(vars[2]),NROW(vars[[2]]))))

  return(list(P97    = as.numeric(vars$P97$crit1.1),
              P17    = as.numeric(vars$P17$crit2.1),
              N      =  c(nrow(vars$P97),nrow(vars$P17)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' van.Lange.1
#'
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis A \code{list} object containing fields \strong{SVO}, \strong{Siblings} and \strong{N}
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#10_van_lange}
#'
#' @export
#'
#' @section Variables:
#'    van.p.1.2_1 TO van.p.1.2_6 are the items of SVO measure.
#'
#'   murphy et al. (2011) scoring: SVO degress=arctan [(mean Alloc other - 50)/(mean Allocation self - 50)].
#'
#'   See SVO codes (this doc) for the list of paired amounts
#'   van.p2.1_1_TEXT= # of older siblings;
#'   van.p2.1_2_TEXT= # of younger siblings.
#'

varfun.vanLange.1 <- function(vars){

  SVO <-rbind(cbind(c( 85,85 ), c( 85,76 ), c( 85,68 ), c( 85,59 ), c( 85,50 ), c( 85,41 ), c( 85,33 ), c( 85,24 ), c( 85,15 )),
              cbind(c( 85,15 ), c( 87,19 ), c( 89,24 ), c( 91,28 ), c( 93,33 ), c( 94,37 ), c( 96,41 ), c( 98,46 ), c( 100,50 )),
              cbind(c( 50,100 ), c( 54,98 ), c( 59,96 ), c( 63,94 ), c( 68,93 ), c( 72,91 ), c( 76,89 ), c( 81,87 ), c( 85,85 )),
              cbind(c( 50,100 ), c( 54,89 ), c( 59,79 ), c( 63,68 ), c( 68,58 ), c( 72,47 ), c( 76,36 ), c( 81,26 ), c( 85,15 )),
              cbind(c( 100,50 ), c( 94,56 ), c( 88,63 ), c( 81,69 ), c( 75,75 ), c( 69,81 ), c( 63,88 ), c( 56,94 ), c( 50,100 )),
              cbind(c( 100,50 ), c( 98,54 ), c( 96,59 ), c( 94,63 ), c( 93,68 ), c( 91,72 ), c( 89,76 ), c( 87,81 ), c( 85,85 )))

  SVO.self  <- SVO[seq(1,11,by=2), ]
  SVO.other <- SVO[seq(2,12,by=2), ]

  #vars$RawDataFilter
  id <- which((vars$RawDataFilter[[1]]$Included==TRUE)&(vars$RawDataFilter[[2]]$Included==TRUE))
  SVO.index <- plyr::ldply(seq_along(id), function(s){cbind(uID =  vars$RawDataFilter[[1]]$uID[id[s]], SVO = atan(
    (mean(SVO.other[array(c(1:6,unlist(vars$SVO[s, ])),dim=c(6,2))])-50)/
      (mean( SVO.self[array(c(1:6,unlist(vars$SVO[s, ])),dim=c(6,2))])-50)))}
  )

  #vars$SVO$Siblings <- vars$SVO$van.p2.1_1_TEXT+vars$SVO$van.p2.1_2_TEXT
  SVO.siblings <- cbind.data.frame(uID     = vars$RawDataFilter[[2]]$uID[id],
                                   Older   = as.numeric(vars$RawDataFilter[[2]]$van.p2.1_1_TEXT[id]),
                                   Younger = as.numeric(vars$RawDataFilter[[2]]$van.p2.1_2_TEXT[id])
  )

  if(!all(SVO.index$uID==SVO.siblings$uID)){disp(paste("van.Lange.1: uID mismatch in", vars$RawDataFilter[[2]]$.id[1]))}

  SVO.siblings$Total <- rowSums(SVO.siblings[c('Younger','Older')])

  cleanDataFilter <- data.frame(uID = SVO.siblings$uID,
                                variable1 = as.numeric(SVO.index$SVO),
                                variable2 = as.numeric(SVO.siblings$Total))

  return(list(SVO.index = SVO.index$SVO,
              Siblings  = SVO.siblings$Total,
              N         = c(nrow(SVO.index),NULL),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Hauser.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#11_hauser}
#'
#' @section Variables:
#'   haus1.1t = timing (side effect scenario);
#'   haus2.1t = timing (greater good scenario);
#'   haus1.2=previous experience (drop if 1 (yes));
#'   haus2.2=previous experience (drop if 1 (yes));
#'   haus1.1=morally permissible (side effect scenario; Yes=1);
#'   haus2.1=morally permissible (greater good scenario; Yes=1).
#'
#'
varfun.Hauser.1 <- function(vars){
  SE <- unlist(vars$SideEffect[names(vars$SideEffect)[[1]]])
  GG <- unlist(vars$GreaterGood[names(vars$GreaterGood)[[1]]])
  N  <- c(nrow(vars$SideEffect),nrow(vars$GreaterGood))

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable = factor(c(SE ,GG),levels=c(1,2),labels=vars$labels$Response),
                                factor =  factor(c(rep(1,N[1]),rep(2,N[2])),levels=c(1,2),labels=vars$labels$Condition))

  return(list(Response = factor(c(SE ,GG),levels=c(1,2),labels=vars$labels$Response),
              Condition = factor(c(rep(1,N[1]),rep(2,N[2])),levels=c(1,2),labels=vars$labels$Condition),
              N = N,
              cleanDataFilter = cleanDataFilter)
  )
}

#' varfun.Anderson.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#12_anderson}
#'
#' @section Variables:
#' and1.3=Satisfaction With Life Scale (SWLS, 5 items, Low SocioMetricStatus condition), higher numbers=higher satisfaction; and1.4=Positive And Negative Affect Scale (PANAS, Low SocioMetricStatus condition).
#' Positive items are 1,4,5,8,9,12,14,17,18,19. Negative items: 2,3,6,7,10,11,13,15,16,20;
#' Alert: recode responses to negative items before averaging.
#' and2.3=Satisfaction With Life Scale (SWLS, 5 items, High SocioMetricStatus condition), higher numbers=higher satisfaction; and2.4=Positive And Negative
#'
#' Affect Scale (PANAS, High SocioMetricStatus condition). Positive items are 1,4,5,8,9,12,14,17,18,19.
#' Negative items: 2,3,6,7,10,11,13,15,16,20;
#' Alert: recode responses to negative items before averaging.
#'
#'list(Low=c("and1.3_1", "and1.3_2", "and1.3_3", "and1.3_4", "and1.3_5", "and1.4_1", "and1.4_2", "and1.4_3", "and1.4_4", "and1.4_5", "and1.4_6", "and1.4_7", "and1.4_8", "and1.4_9", "and1.4_10", "and1.4_11", "and1.4_12", "and1.4_13", "and1.4_14", "and1.4_15", "and1.4_16", "and1.4_17", "and1.4_18", "and1.4_19", "and1.4_20"),
#' High=c("and2.3_1", "and2.3_2", "and2.3_3", "and2.3_4", "and2.3_5", "and2.4_1", "and2.4_2", "and2.4_3", "and2.4_4", "and2.4_5", "and2.4_6", "and2.4_7", "and2.4_8", "and2.4_9", "and2.4_10", "and2.4_11", "and2.4_12", "and2.4_13", "and2.4_14", "and2.4_15", "and2.4_16", "and2.4_17", "and2.4_18", "and2.4_19", "and2.4_20"))
#'
#' @export
#'

varfun.Anderson.1 <- function(vars){
  #    require(dplyr)

  # Negative
  PANASlowNA   <- dplyr::select(vars$Low,
                                one_of(unlist(strsplit(paste0("and1.4_",c(2,3,6,7,10,11,13,15,16,20),sep="|"),"|",fixed=TRUE))))
  # Negative Recode
  PANASlowNAr  <- 6-PANASlowNA
  # Positive
  PANASlowPA   <- dplyr::select(vars$Low,
                                one_of(unlist(strsplit(paste0("and1.4_",c(1,4,5,8,9,12,14,17,18,19),sep="|"),"|",fixed=TRUE))))
  # SWSL
  SWLSlow  <-  dplyr::select(vars$Low,
                             one_of(c("and1.3_1", "and1.3_2", "and1.3_3", "and1.3_4", "and1.3_5")))

  # Negative
  PANAShighNA  <- dplyr::select(vars$High,
                                one_of(unlist(strsplit(paste0("and2.4_",c(2,3,6,7,10,11,13,15,16,20),sep="|"),"|",fixed=TRUE))))
  # Negative Recode
  PANAShighNAr <- 6-PANAShighNA
  # Positive
  PANAShighPA  <- dplyr::select(vars$High,
                                one_of(unlist(strsplit(paste0("and2.4_",c(1,4,5,8,9,12,14,17,18,19),sep="|"),"|",fixed=TRUE))))
  # SWSL
  SWLShigh     <- dplyr::select(vars$High,
                                one_of(c("and2.3_1", "and2.3_2", "and2.3_3", "and2.3_4", "and2.3_5")))

  # Descriptives
  MhighlowNA  <- scale(c(rowMeans(PANAShighNA), rowMeans(PANASlowNA)))
  MhighlowNAr <- scale(c(rowMeans(PANAShighNAr), rowMeans(PANASlowNAr)))
  MhighlowPA  <- scale(c(rowMeans(PANAShighPA), rowMeans(PANASlowPA)))
  MhighlowSW  <- scale(c(rowMeans(SWLShigh),rowMeans(SWLSlow)))

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable = (MhighlowSW + MhighlowPA + MhighlowNAr)/3,
                                factor =  factor(c(rep("High",nrow(PANAShighPA)),rep("Low",nrow(PANASlowPA)))))

  return(list(SWB        = (MhighlowSW + MhighlowPA + MhighlowNAr)/3,
              Condition = factor(c(rep("High",nrow(PANAShighPA)),rep("Low",nrow(PANASlowPA)))),
              N    = c(nrow(PANAShighPA), nrow(PANASlowPA)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Ross.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#13_ross1}
#'
#' @section Variables:
#' ross.s1.1 = percentage of peers;
#' ross.s1.2 = you; values: 1=sign; 2=refuse
#'
#' @export
#'

varfun.Ross.1 <- function(vars){
  cleanDataFilter <- data.frame(uID = vars[[1]]$uID,
                                variable1 = vars$Peers[[1]],
                                variable2 = factor(vars$You[[1]],levels=c(1,2),labels=vars$labels$Response))

  return(list(Peers  = vars$Peers[[1]],
              You    = factor(vars$You[[1]],levels=c(1,2),labels=vars$labels$Response),
              N      = c(sum(vars$You[[1]]==1),sum(vars$You[[1]]==2)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Ross.2
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#14_ross2}
#'
#' @section Variables:
#' ross.s2.1=percentage of peers;
#' ross.s2.2=you; values: 1=Pay; 2=Appear in court
#'
#' @export
#'

varfun.Ross.2 <- function(vars){
  cleanDataFilter <- data.frame(uID = vars[[1]]$uID,
                                variable1 = vars$Peers[[1]],
                                variable2 = factor(vars$You[[1]],levels=c(1,2),labels=vars$labels$Response))
  return(list(Peers  = vars$Peers[[1]],
              You    = factor(vars$You[[1]],levels=c(1,2),labels=vars$labels$Response),
              N      = c(sum(vars$You[[1]]==1),sum(vars$You[[1]]==2)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Giessner.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#17_giessner}
#'
#' @section Variables:
#' geis.1.1=long line condition;
#' geis.2.1=short line condition;
#' geis.dv_1=dominant;
#' geis.dv_2=strong;
#' geis.dv_3=self-confident;
#' geis.dv_4=control;
#' geis.dv_5=status;
#' For all dvs, higher numbers=higher power.
#'
#' @export
#'

varfun.Giessner.1 <- function(vars){
  #    require(dplyr)

  Long <- vars$Long  %>% dplyr::filter(!is.na(vars$Long[1])  & rowSums(!is.na(vars$Long[-1, ]))>1)
  Short<- vars$Short %>% dplyr::filter(!is.na(vars$Short[1]) & rowSums(!is.na(vars$Short[-1, ]))>1)

  cleanDataFilter <- data.frame(uID = c(vars$Long$uID,vars$Short$uID),
                                variable = c(rowMeans(Long[-1]),rowMeans(Short[-1])),
                                factor =  c(rep("Long",NROW(vars$Long)),
                                            rep("Short",NROW(vars$Short))))

  return(list(Long   = rowMeans(Long[-1]),
              Short  = rowMeans(Short[-1]),
              N      = c(nrow(Long),nrow(Short)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Tversky.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @references    Tversky, A., Kahneman, D. (1981). The framing of decisions and the psychology of choice. Science, 211, 453-458.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#16_tversky}
#'
#' @section Variables:
#' tver1.1=choice ($250 wall hanging condition, yes=1, no=2);
#' tver2.1=choice ($30 wall hanging cond, yes=1, no=2).
#'
#' tver1.1 Imagine that you are about to purchase a ceramic vase for $30, and a wall hanging for $250. The salesman informs you that the wall hanging you wish to buy is on sale for $240 at the other branch of the store, located 20 minutes drive away. Would you make the trip to the other store?
#' m  Yes, I would go to the other branch. (1)
#' m  No, I would not go to the other branch. (2)
#'
#' tver2.1 Imagine that you are about to purchase a ceramic vase for $250, and a wall hanging for $30. The salesman informs you that the wall hanging you wish to buy is on sale for $20 at the other branch of the store, located 20 minutes drive away. Would you make the trip to the other store?
#' m  Yes, I would go to the other branch. (1)
#' m  No, I would not go to the other branch. (2)
#'
#' @export
#'
varfun.Tversky.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars$Cheap$uID,vars$Expensive$uID),
                                variable = factor(c(vars$Cheap[[1]],vars$Expensive[[1]]),levels=c(1,2),labels=vars$labels$Response),
                                factor =  factor(c(rep(1,nrow(vars$Cheap)),rep(2,nrow(vars$Expensive))),levels=c(1,2),labels=vars$labels$Condition))

  return(list(Condition = factor(c(rep(1,nrow(vars$Cheap)),rep(2,nrow(vars$Expensive))),levels=c(1,2),labels=vars$labels$Condition),
              Response  = factor(c(vars$Cheap[[1]],vars$Expensive[[1]]),levels=c(1,2),labels=vars$labels$Response),
              N         = c(nrow(vars$Cheap),nrow(vars$Expensive)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Hauser.2
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#17_hauser}
#'
#' @section Variables:
#' hauser3.1=morality judgment (greater good condition);
#' hauser4.1=morality judgment (foreseen side-effect condition; for both, yes=1, no=2.)
#' haus3.2 and haus4.2=previous experience (yes=1, no=2);
#' haus3.1t_3=timing (greater good);
#' haus4.1t_3=timing (side effect).
#'
#' @export
#'
#' @references Hauser, M., Cushman, F., Young, L., Kang-Xing Jin, R., & Mikhail, J. (2007). A dissociation between moral judgments and justifications. Mind & Language, 22, 1-21.
#'
varfun.Hauser.2 <- function(vars){
  SideEffect  <- dplyr::filter(vars$SideEffect, vars$SideEffect[vars$labels$Experience[1]] !=1 & vars$SideEffect[vars$labels$Timing[1]]>4)
  GreaterGood <- dplyr::filter(vars$GreaterGood,vars$GreaterGood[vars$labels$Experience[2]]!=1 & vars$GreaterGood[vars$labels$Timing[2]]>4)
  N <- c(nrow(SideEffect),nrow(GreaterGood))

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable = factor(c(SideEffect$haus3.1,GreaterGood$haus4.1),levels=c(1,2),labels=vars$labels$Response),
                                factor =  factor(c(rep(1,N[1]),rep(2,N[2])),levels=c(1,2),labels=vars$labels$Condition))


  return(list(Response  = factor(c(SideEffect$haus3.1,GreaterGood$haus4.1),levels=c(1,2),labels=vars$labels$Response),
              Condition = factor(c(rep(1,N[1]),rep(2,N[2])),levels=c(1,2),labels=vars$labels$Condition),
              N = N,
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Risen.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#18_risen}
#'
#' @section Variables:
#' rise1.3=likelihood that the professor will call on you (unprepared condition);
#' rise2.3=likelihood that the professor will call on you (prepared condition);
#' for both, higher numbers=higher likelihood
#' Variable = "ex.subjp" which asked if participants were recruited through a university subject pool. 1 = yes, 2 = no.
#'
#' @export
#'
#' @references  Risen, J. L., & Gilovich, T. (2008). Why people are reluctant to tempt fate. \strong{Journal of Personality and Social Psychology}, 95, 293.
#'

varfun.Risen.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars$Unprepared$uID,vars$Prepared$uID),
                                variable = c(as.numeric(vars$Unprepared[[1]]),as.numeric(vars$Prepared[[1]])),
                                factor   =  factor(c(rep("Unprepared",NROW(vars$Unprepared[[1]])),
                                                   rep("Prepared",NROW(vars$Prepared[[1]])))))

  return(list(Unprepared  = as.numeric(vars$Unprepared[[1]]),
              Prepared    = as.numeric(vars$Prepared[[1]]),
              N           = c(nrow(vars$Unprepared),nrow(vars$Prepared)),
              cleanDataFilter = cleanDataFilter)
  )
}


#' varfun.Risen.2
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#18_risen}
#'
#' @section Variables:
#' rise1.3=likelihood that the professor will call on you (unprepared condition);
#' rise2.3=likelihood that the professor will call on you (prepared condition);
#'
#' for both, higher numbers=higher likelihood   All participants that answer the dependent measure will be included in analysis.
#' The primary confirmatory test for comparing the original and replication effect size will be based on only the samples using undergraduate students.
#'
#' @export
#'
#' @references  Risen, J. L., & Gilovich, T. (2008). Why people are reluctant to tempt fate. Journal of Personality and Social Psychology, 95, 293.
#'

varfun.Risen.2 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars$Unprepared$uID,vars$Prepared$uID),
                                variable =c(vars$Unprepared$rise1.3,vars$Prepared$rise2.3),
                                factor   =  factor(c(rep(1,nrow(vars$Unprepared)),rep(2,nrow(vars$Prepared))),levels=c(1,2),labels=vars$labels$Condition),
                                gender = factor(c(vars$Unprepared$sex,vars$Prepared$sex)))

  return(list(Likelihood = c(vars$Unprepared$rise1.3,vars$Prepared$rise2.3),
              Condition  = factor(c(rep(1,nrow(vars$Unprepared)),rep(2,nrow(vars$Prepared))),levels=c(1,2),labels=vars$labels$Condition),
              Gender     = factor(c(vars$Unprepared$sex,vars$Prepared$sex)),
              N          = c(nrow(vars$Unprepared),nrow(vars$Prepared)),
              cleanDataFilter = cleanDataFilter)
  )
}

#' varfun.Savani.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#19_savani}
#'
#' @export
#'
#' @section Variables:
#' sava1.N=interpersonal actions;
#'
#' sava2.N=personal actions;
#'
#' 'sava1.4', 'sava1.5', 'sava1.9', 'sava1.10', 'sava1.15', 'sava1.16' , 'sava1.21', 'sava1.22', 'sava1.27', 'sava1.28', 'sava1.33', 'sava1.34',  'sava1.38', 'sava1.39', 'sava1.43', 'sava1.44'
#'
#' sava1.4=choice (buy a gift; 1=choice; 2=no choice);
#'
#' sava1.5=importance (buy a gift);
#'
#' sava1.9=choice (take a friend at the restaurant; 1=choice; 2=no choice);
#'
#' sava1.10=importance (restaurant);
#'
#' sava1.15=choice (trip; 1=choice, 2 and 3 = no choice);
#'
#' sava1.16=importance (trip);
#'
#' sava1.21=choice (dinner; 1=choice, 2 and 3 = no choice);
#'
#' sava1.22=importance (dinner);
#'
#' sava1.27=choice (errand; 1=choice, 2 and 3 = no choice);
#'
#' sava1.28=importance (errand);
#'
#' sava1.33=choice (help, 1=choice, 2 & 3 = no choice);
#'
#' sava1.34=importance (help);
#'
#' sava1.38=choice (advice, 1=choice, 2 & 3 = no choice);
#'
#' sava1.39=importance (advice);
#'
#' sava1.43=choice (friends, 1=choice, 2 & 3 = no choice);
#'
#' sava1.44=importance (friends);
#'
#'
#' sava2.4=choice (buy for yourself; 1=choice; 2=no choice);
#'
#' sava2.5=importance (buy for yourself);
#'
#' sava2.9=choice (at the restaurant by yourself; 1=choice; 2=no choice);
#'
#' sava2.10=importance (restaurant by yourself);
#'
#' sava2.15=choice (trip alone; 1=choice, 2 and 3 = no choice);
#'
#' sava2.16=importance (trip alone);
#'
#' sava2.21=choice (out for dinner; 1=choice, 2 and 3 = no choice);
#'
#' sava2.22=importance (out for dinner);
#'
#' sava2.27=choice (errand for yourself; 1=choice, 2 and 3 = no choice);
#'
#' sava2.28=importance (errand for yourself);
#'
#' sava2.33=choice (ask for help, 1=choice, 2 & 3 = no choice);
#'
#' sava2.34=importance (ask for help);
#'
#' sava2.38=choice (take a course, 1=choice, 2 & 3 = no choice);
#'
#' sava2.39=importance (take a course);
#'
#' sava2.43=choice (friends, 1=choice, 2 & 3 = no choice);
#'
#' sava2.44=importance (friends);
#'
#'
#' For all importance items: higher numbers=higher importance
#'
#' we will only include university data collections in the primary confirmatory analysis to be compared with the original effect sizes.
#'
#' Data for all participants will be included to examine variability across sample and setting.
#' However, participants must respond to all choice and importance of choice questions to be included in the analysis.
#'
#' @export
#'
#' @return Dataset ready for analysis
#'
varfun.Savani.1 <- function(vars){

  choice.Int     <- c('sava1.4', 'sava1.9', 'sava1.15', 'sava1.21', 'sava1.27', 'sava1.33', 'sava1.38', 'sava1.43')
  importance.Int <- c('sava1.5', 'sava1.10', 'sava1.16', 'sava1.22', 'sava1.28', 'sava1.34', 'sava1.39', 'sava1.44')

  choice.Pers     <- c('sava2.4', 'sava2.9', 'sava2.15', 'sava2.21', 'sava2.27', 'sava2.33', 'sava2.38', 'sava2.43')
  importance.Pers <- c('sava2.5','sava2.10', 'sava2.16', 'sava2.22', 'sava2.28','sava2.34', 'sava2.39', 'sava2.44')

  Response             <- rbind(dplyr::select(vars$Interpersonal,Choice=one_of(choice.Int)),
                                dplyr::select(vars$Personal,Choice=one_of(choice.Pers))
  )
  Condition    <- factor(c(rep(1,nrow(vars$Interpersonal)),rep(2,nrow(vars$Personal))),levels=c(1,2),labels=vars$labels$Condition)
  id           <- c(vars$Interpersonal$uID,vars$Personal$uID)

  Response[Response>1] <- 0
  Response$Condition   <- Condition
  Response$uID         <- id
  Response             <- reshape2::melt(Response, id = c('uID','Condition'), variable.name = 'trialID', value.name = 'Response')
  #Response$Response    <- factor(Response$Response, levels = c(0,1), labels = vars$labels$Response)
  #Response$Response    <- relevel(Response$Response, vars$labels$Response[[1]])

  Importance           <- rbind(dplyr::select(vars$Interpersonal, Importance = one_of(importance.Int)),
                                dplyr::select(vars$Personal, Importance = one_of(importance.Pers))
  )
  Importance$uID       <- id
  Importance$Condition <- Condition
  Importance           <- reshape2::melt(Importance, id = c('uID','Condition'), variable.name = 'VarLabel',
                                         value.name = 'Importance')
  # Probably superfluous
  matchID              <- Response$uID == Importance$uID
  Response$Importance[matchID] <- scale(Importance$Importance[matchID],scale=FALSE)

  cleanDataFilter <- data.frame(uID      = Response$uID,
                                trialID  = as.numeric(Response$trialID),
                                variable = Response$Importance,
                                factor   = Response$Condition)


  return(list(Response   = Response$Response,
              Condition  = Response$Condition,
              Importance = Response$Importance,
              uID        = Response$uID,
              trialID    = as.numeric(Response$trialID),
              df         = tbl_df(Response),
              N          = c(nrow(vars$Interpersonal),nrow(vars$Personal)),
              cleanDataFilter = cleanDataFilter)
  )
}



#' varfun.Norenzayan.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#20_norenzayan}
#'
#' @section Variables:
#'  nore1.1 TO nore1.20 provide choices ("belong to" condition)
#'
#'  nore2.1 to nore2.20 provide choices ("similar to" condition)
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @references  Norenzayan, A., Smith, E. E., Kim, B. J., & Nisbett, R. E. (2002). Cultural preferences for formal versus intuitive reasoning. Cognitive Science, 26, 653-684.
#'

varfun.Norenzayan.1 <- function(vars){

  cleanDataFilter <- data.frame(uID      = c(vars$Belong$uID,vars$Similar$uID),
                                variable = c(rowMeans(vars$Belong == matrix(rep(1:2,10), nrow=nrow(vars$Belong), ncol=20, byrow = TRUE), na.rm = TRUE), rowMeans(vars$Similar == matrix(rep(1:2,10), nrow=nrow(vars$Similar), ncol=20, byrow = TRUE), na.rm = TRUE)),
                                factor   = factor(c(rep("Belong",NROW(vars$Belong)),rep("Similar",NROW(vars$Similar)))))

  return(list(Belong  = rowMeans(vars$Belong == matrix(rep(1:2,10), nrow=nrow(vars$Belong), ncol=20, byrow = TRUE), na.rm = TRUE),
              Similar = rowMeans(vars$Similar == matrix(rep(1:2,10), nrow=nrow(vars$Similar), ncol=20, byrow = TRUE), na.rm = TRUE),
              N       = c(nrow(vars$Belong), nrow(vars$Similar)),
         cleanDataFilter = cleanDataFilter)
         )

  # return(list(Belong  = rowMeans(vars$Belong==rep(1:2,10),na.rm=TRUE),
  #             Similar = rowMeans(vars$Similar==rep(1:2,10),na.rm=TRUE),
  #             N       = c(nrow(vars$Belong), nrow(vars$Similar)))
  #        )
}

#' varfun.Norenzayan.2
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#20_norenzayan}
#' @note This analysis tests moderating effect of presenting Gati first or after.
#'
#' @export
#'
#' @references  Norenzayan, A., Smith, E. E., Kim, B. J., & Nisbett, R. E. (2002). Cultural preferences for formal versus intuitive reasoning. Cognitive Science, 26, 653-684.
#'
varfun.Norenzayan.2 <- function(vars){

  NafterTG.Bel <- sapply(which((vars$RawDataFilter[[1]]$Included)), function(i) which(unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]")) == "Norenzayan") > which(unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]")) == "Tversky.Gati"))
  NafterTG.Sim <- sapply(which((vars$RawDataFilter[[2]]$Included)), function(i) which(unlist(strsplit(x = vars$RawDataFilter[[2]]$StudyOrderN[i], split = "[|]")) == "Norenzayan") > which(unlist(strsplit(x = vars$RawDataFilter[[2]]$StudyOrderN[i], split = "[|]")) == "Tversky.Gati"))

  # length(rowSums(vars$Similar[,1:20]==1,na.rm=TRUE)/20)

  NafterTG.Bel <- unlist(NafterTG.Bel)
  NafterTG.Sim <- unlist(NafterTG.Sim)

  N = c(length(NafterTG.Bel),length(NafterTG.Sim))

  cleanDataFilter <- data.frame(uID      = c(vars$Belong$uID,vars$Similar$uID),
                                variable = c(rowMeans(vars$Belong == matrix(rep(1:2,10), nrow=nrow(vars$Belong), ncol=20, byrow = TRUE), na.rm = TRUE), rowMeans(vars$Similar == matrix(rep(1:2,10), nrow=nrow(vars$Similar), ncol=20, byrow = TRUE), na.rm = TRUE)),
                                factor   = factor(c(rep(1,N[1]),rep(2,N[2])),levels=c(1,2),labels=vars$labels$Condition),
                                order = factor(c(as.numeric(NafterTG.Bel), as.numeric(NafterTG.Sim)), levels=c(0,1), labels = vars$labels$Order))


  return(list(Response  = c(rowMeans(vars$Belong == matrix(rep(1:2,10), nrow = nrow(vars$Belong), ncol=20, byrow = TRUE),na.rm=TRUE),
                            rowMeans(vars$Similar== matrix(rep(1:2,10), nrow = nrow(vars$Belong), ncol=20, byrow = TRUE),na.rm=TRUE)),
              Condition = factor(c(rep(1,N[1]),rep(2,N[2])),levels=c(1,2),labels=vars$labels$Condition),
              Order     = factor(c(as.numeric(NafterTG.Bel), as.numeric(NafterTG.Sim)), levels=c(0,1), labels = vars$labels$Order),
              uID       = cleanDataFilter$uID,
              N         = N,
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Hsee.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#21_hsee}
#'
#' @section Variables:
#' hsee1.1=generosity ($90 scarf condition);
#' hsee2.1=generosity ($110 coat condition);
#' for both, higher numbers=higher generosity
#'
#' @export
#'
#' @references Hsee, C. K. (1998). Less is better: When low-value options are valued more highly than high-value options. Journal of Behavioral Decision Making, 11, 107-121.
#'
varfun.Hsee.1 <- function(vars){

  cleanDataFilter <- data.frame(uID      = c(vars$Scarf$uID,vars$Coat$uID),
                                variable = c(as.numeric(vars$Scarf[[1]]),as.numeric(vars$Coat[[1]])),
                                factor   = factor(c(rep("Scarf",NROW(vars$Scarf)),rep("Coat",NROW(vars$Coat)))))

  return(list(Scarf  = as.numeric(vars$Scarf[[1]]),
              Coat   = as.numeric(vars$Coat[[1]]),
              N      = c(length(as.numeric(vars$Scarf[[1]])),length(as.numeric(vars$Coat[[1]]))),
              cleanDataFilter = cleanDataFilter)
  )
}

#' varfun.Gray.1
#'
#' @param vars A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#22_gray}
#'
#' @references Gray, K., & Wegner, D. M. (2009). Moral typecasting: divergent perceptions of moral agents and moral patients. Journal of Personality and Social Psychology, 96, 505.
#'
#' @export
#'
#' @section Variables:
#'
#' Adult harms baby scenario; gray1.2=responsibility (adult);  gray1.4=pain (baby).
#' Baby harms adult scenario; gray2.2=responsibility (baby);  gray2.4=pain (adult)
#'
varfun.Gray.1 <- function(vars){

  cleanDataFilter <- data.frame(uID      = c(vars$adultHbaby$uID,vars$babyHadult$uID),
                                variable = c(as.numeric(vars$adultHbaby[[1]]),as.numeric(vars$babyHadult[[1]])),
                                factor   = factor(c(rep("Adult harms Baby",length(as.numeric(vars$adultHbaby[[1]]))), rep("Baby harms Adult",length(as.numeric(vars$babyHadult[[1]]))))))

  return(list(adultHbaby = as.numeric(vars$adultHbaby[[1]]),
              babyHadult = as.numeric(vars$babyHadult[[1]]),
              N     = c(length(as.numeric(vars$adultHbaby[[1]])),length(as.numeric(vars$babyHadult[[1]]))),
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Gray.2
#'
#' @param vars A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#22_gray}
#'
#' @references Gray, K., & Wegner, D. M. (2009). Moral typecasting: divergent perceptions of moral agents and moral patients. Journal of Personality and Social Psychology, 96, 505.
#'
#' @export
#'
#' @section Variables:
#' Baby harms adult scenario; gray1.3=intentionality (adult); gray1.4=pain (baby).
#' Adult harms baby scenario; gray2.3=intentionality (baby); gray2.4=pain (adult)
#'

varfun.Gray.2 <- function(vars){

  cleanDataFilter <- data.frame(uID      = c(vars$adultHbaby$uID,vars$babyHadult$uID),
                                variable = c(as.numeric(vars$adultHbaby[[1]]),as.numeric(vars$babyHadult[[1]])),
                                factor   = factor(c(rep("Adult harms Baby",length(as.numeric(vars$adultHbaby[[1]]))), rep("Baby harms Adult",length(as.numeric(vars$babyHadult[[1]]))))))

  return(list(adultHbaby = as.numeric(vars$adultHbaby[[1]]),
              babyHadult = as.numeric(vars$babyHadult[[1]]),
              N     = c(length(as.numeric(vars$adultHbaby[[1]])),length(as.numeric(vars$babyHadult[[1]]))),
         cleanDataFilter = cleanDataFilter))
}

#' varfun.Zhong.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#23_zhong}
#'
#' @export
#'
#' @section Variables:
#' zhon1.1= unethical condition;
#' zhon2.1=ethical condition;
#'
#' zhon.dv.1_1 TO zhon.dv.1_10=desirability of products (both conditions);
#' higher numbers=higher desirability;
#'
#' Products list:
#'
#' Clean:
#' Dove shower soap (zhon.dv.1_2),
#' Crest toothpaste (zhon.dv.1_3),
#' Windex glass cleaner (zhon.dv.1_7),
#' Lysol countertop disinfectant (zhon.dv.1_8),
#' Tide laundry detergent (zhon.dv.1_10)
#'
#' Not-clean:
#' Post-it notes (zhon.dv.1_1),
#' Nantucket Nectars juice (zhon.dv.1_4),
#' Energizer batteries (zhon.dv.1_5),
#' Sony cd cases (zhon.dv.1_6),
#' Snickers candy bar (zhon.dv.1_9),
#'
#' @references Zhong, C. B., & Liljenquist, K. (2006). Washing away your sins: Threatened morality and physical cleansing. Science, 313, 1451???1452.
#'
varfun.Zhong.1 <- function(vars){
  # First column is nCopied
  idClean <- c(2,3,7,8,10)+1
  idOther <- c(1,4,5,6,9)+1

  cleanDataFilter <- data.frame(uID      = c(vars$Ethical$uID,vars$Unethical$uID),
                                variable = c(rowMeans(vars$Ethical[ ,idClean],na.rm = TRUE), rowMeans(vars$Unethical[ ,idClean], na.rm = TRUE)),
                                factor   = factor(c(rep("Ethical",length(rowMeans(vars$Ethical[ ,idClean],na.rm = TRUE))), rep("Unethical",length(rowMeans(vars$Unethical[ ,idClean], na.rm = TRUE))))))


  return(list(Ethical   = rowMeans(vars$Ethical[ ,idClean],na.rm = TRUE),
              Unethical = rowMeans(vars$Unethical[ ,idClean], na.rm = TRUE),
              N         = c(length(rowMeans(vars$Ethical[ ,idClean],na.rm = TRUE)), length(rowMeans(vars$Unethical[ ,idClean], na.rm = TRUE))),
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Zhong.2
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#23_zhong}
#'
#' @export
#'
#' @section Variables:
#' zhon1.1= unethical condition;
#' zhon2.1=ethical condition;
#'
#' zhon.dv.1_1 TO zhon.dv.1_10=desirability of products (both conditions);
#' higher numbers=higher desirability;
#'
#' Products list:
#'
#' Clean:
#' Dove shower soap (zhon.dv.1_2),
#' Crest toothpaste (zhon.dv.1_3),
#' Windex glass cleaner (zhon.dv.1_7),
#' Lysol countertop disinfectant (zhon.dv.1_8),
#' Tide laundry detergent (zhon.dv.1_10)
#'
#' Not-clean:
#' Post-it notes (zhon.dv.1_1),
#' Nantucket Nectars juice (zhon.dv.1_4),
#' Energizer batteries (zhon.dv.1_5),
#' Sony cd cases (zhon.dv.1_6),
#' Snickers candy bar (zhon.dv.1_9),
#'
#'
#' @references Zhong, C. B., & Liljenquist, K. (2006). Washing away your sins: Threatened morality and physical cleansing. Science, 313, 1451--1452.
#'
varfun.Zhong.2 <- function(vars){
  # library(lme4)
  # library(lmerTest)

  idClean <- c(2,3,7,8,10)+1
  idOther <- c(1,4,5,6,9)+1

  Response  = c(rowMeans(vars$Ethical[,idClean]),
                            rowMeans(vars$Ethical[,idOther]),
                            rowMeans(vars$Unethical[,idClean]),
                            rowMeans(vars$Unethical[,idOther]))

  Condition = factor(c(rep(1,times=nrow(vars$Ethical)),
                       rep(1,times=nrow(vars$Ethical)),
                       rep(2,times=nrow(vars$Unethical)),
                       rep(2,times=nrow(vars$Unethical))),
                     levels=c(1,2),labels=vars$labels$Condition)

  Product   = factor(c(rep(1,times=nrow(vars$Ethical)),
                       rep(2,times=nrow(vars$Ethical)),
                       rep(1,times=nrow(vars$Unethical)),
                       rep(2,times=nrow(vars$Unethical))),
                     levels=c(1,2),labels=vars$labels$Product)

  uID       = c(seq_along(vars$Ethical[[1]]),
                seq_along(vars$Ethical[[1]]),
                max(seq_along(vars$Ethical[[1]])) + seq_along(vars$Unethical[[1]]),
                max(seq_along(vars$Ethical[[1]])) + seq_along(vars$Unethical[[1]]))

  N         = c(nrow(vars$Ethical),nrow(vars$Unethical))

  cleanDataFilter <- data.frame(uID = uID,
                                Response = Response,
                                Condition = Condition,
                                Product = Product)

  return(list(Response = Response,
              Condition = Condition,
              Product = Product,
              uID = uID,
              N = N,
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Zhong.3
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#23_zhong}
#'
#' @return Dataset ready for analysis
#'
#' @export
#'
#' @references Zhong, C. B., & Liljenquist, K. (2006). Washing away your sins: Threatened morality and physical cleansing. Science, 313, 1451???1452.
#'
#' @section Variables:
#' zhon1.1= unethical condition;
#' zhon2.1=ethical condition;
#'
#' zhon.dv.1_1 TO zhon.dv.1_10=desirability of products (both conditions);
#' higher numbers=higher desirability;
#'
#' Products list:
#'
#' Clean:
#' Dove shower soap (zhon.dv.1_2),
#' Crest toothpaste (zhon.dv.1_3),
#' Windex glass cleaner (zhon.dv.1_7),
#' Lysol countertop disinfectant (zhon.dv.1_8),
#' Tide laundry detergent (zhon.dv.1_10)
#'
#' Not-clean:
#' Post-it notes (zhon.dv.1_1),
#' Nantucket Nectars juice (zhon.dv.1_4),
#' Energizer batteries (zhon.dv.1_5),
#' Sony cd cases (zhon.dv.1_6),
#' Snickers candy bar (zhon.dv.1_9),
#'
varfun.Zhong.3 <- function(vars){
  idClean <- c(2,3,7,8,10)+1
  idOther <- c(1,4,5,6,9)+1

  cleanDataFilter <- data.frame(uID      = c(vars$Ethical$uID,vars$Unethical$uID),
                                variable = c(rowMeans(vars$Ethical[ ,idClean],na.rm = TRUE), rowMeans(vars$Unethical[ ,idClean], na.rm = TRUE)),
                                factor   = factor(c(rep("Ethical",length(rowMeans(vars$Ethical[ ,idClean],na.rm = TRUE))), rep("Unethical",length(rowMeans(vars$Unethical[ ,idClean], na.rm = TRUE))))))


  return(list(Ethical   = rowMeans(vars$Ethical[ ,idOther],na.rm = TRUE),
              Unethical = rowMeans(vars$Unethical[ ,idOther], na.rm = TRUE),
              N         = c(length(rowMeans(vars$Ethical[ ,idOther],na.rm = TRUE)),
                            length(rowMeans(vars$Unethical[ ,idOther], na.rm = TRUE))),
              cleanDataFilter = cleanDataFilter))
}

#' varfun.Schwarz.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#24_schwarz}
#'
#' @section Variables:
#' schw1.1=life sat (first);
#' schw1.2=partner satisfaction (second);
#' schw2.1=partner satisfaction (first);
#' schw2.2=life sat (second).
#' for all, higher numbers=higher satisfaction
#'
#' @export
#'
#' @references Schwarz, N., Strack, F., & Mai, H. P. (1991). Assimilation and contrast effects in part-whole question sequences: A conversational logic analysis. \strong{Public Opinion Quarterly, 55}, 3-23.
#'
varfun.Schwarz.1 <- function(vars){

  cleanDataFilter <- data.frame(uID = c(vars[[1]]$uID,vars[[2]]$uID),
                                variable1 = unlist(c(vars$SpecificFirst[,1],vars$GlobalFirst[,1])),
                                variable2 = unlist(c(vars$SpecificFirst[,2],vars$GlobalFirst[,2])),
                                factor =  c(rep("SpecificFirst",nrow(vars$SpecificFirst)),
                                            rep("GlobalFirst",nrow(vars$GlobalFirst))))

  return(list(r1 = cbind(vars$SpecificFirst[,1],vars$SpecificFirst[,2]),
              r2 = cbind(vars$GlobalFirst[,1],vars$GlobalFirst[,2]),
              N  = c(nrow(vars$SpecificFirst),nrow(vars$GlobalFirst)),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Schwarz.2
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#24_schwarz}
#'
#' @details \strong{Analysis plan:} We will compute the correlation between responses to the general and
#'  specific question in each item order condition, and then compare the correlations using
#'  the Fisher r-to-z transformation. Participants with valid responses to both items will
#'  be included in the analysis.
#'
#'
#'  @export
#'
#' @references Schwarz, N., Strack, F., & Mai, H. P. (1991). Assimilation and contrast effects in part-whole question sequences: A conversational logic analysis. \strong{Public Opinion Quarterly, 55}, 3-23.
#'
varfun.Schwarz.2 <- function(vars){

  id <- sapply(seq_along(vars$RawDataFilter[[1]]$.id), function(i) unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]"))[[1]] == "Schwarz")
  r1 <- na.exclude(cbind(vars$SpecificFirst[id,1], vars$SpecificFirst[id,2],vars$SpecificFirst[id,3]))
  r2 <- na.exclude(cbind(vars$GlobalFirst[id,1], vars$GlobalFirst[id,2], vars$GlobalFirst[id,3]))

  cleanDataFilter <- data.frame(uID = c(r1[,3],r2[,3]),
                                variable1 = unlist(c(r1[,1],r2[,1])),
                                variable2 = unlist(c(r1[,2],r2[,2])),
                                factor =  c(rep("SpecificFirst",nrow(r1)),
                                            rep("GlobalFirst",nrow(r2))))

  return(list(r1 = r1,
              r2 = r2,
              N  = c(nrow(r1),nrow(r2)),
              cleanDataFilter = cleanDataFilter))

}

#' varfun.Shafir.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#25_shafir}
#'
#' \strong{CHANGED} The analysis in the orignal article was likely as follows:
#'
#' - Count number of Parent B choices in both conditions
#' - Sum the proportions
#' - Divide by 2 and test against proportion = .5
#'
#' @export
#'
#' @section Variables:
#' shaf1.1=choice (award condition; Parent A=1, Parent B=2);
#' shaf2.1=choice (deny condition; Parent A=1, Parent B=2)
#'
#' @references Shafir, E. (1993). Choosing versus rejecting: Why some options are both better and worse than others. Memory & Cognition, 21, 546-556.
#'
varfun.Shafir.1 <- function(vars){
  Response = factor(c(vars$Award$shaf1.1,vars$Deny$shaf2.1), levels = c(1,2),labels = c("parent A", "Parent B"))
  N        = c(length(na.exclude(vars$Award$shaf1.1)), length(na.exclude(vars$Deny$shaf2.1)))

  cleanDataFilter = data.frame(uID =c(vars[[1]]$uID,vars[[2]]$uID),
                               variable1 = Response,
                               variable2 = factor(c(rep("Award",N[1]), rep("Deny",N[2]))),
                               parentB = rep(sum(c(sum(vars$Award$shaf1.1 == 2, na.rm = TRUE) / N[1],
                                                 sum(vars$Deny$shaf2.1  == 2, na.rm = TRUE) / N[2]),
                                               na.rm = TRUE)/2,NROW(Response)))

  return(list(Response  = Response,
              Condition = factor(c(rep("Award",N[1]), rep("Deny",N[2]))),
              ParentB   = sum(c(sum(vars$Award$shaf1.1 == 2, na.rm = TRUE) / N[1],
                                sum(vars$Deny$shaf2.1  == 2, na.rm = TRUE) / N[2]),
                              na.rm = TRUE)/2,
              N         = N, #c(nrow(vars$Award), nrow(vars$Deny))
              cleanDataFilter = cleanDataFilter)
  )
  # prop.test(x = ParentB, n=sum(N), p = .5, conf.level=stat.params[[1]], alternative = stat.params[[4]])
  # prop.test(x = .6, n=170, p = .5)
}

#' varfun.Zaval.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#26_zaval}
#' @section Variables:
#' zav1.1 TO zav1.13 provide COLD primes.
#' zav2.1 TO zav2.13 provide HEAT primes.
#' zav.dv.2=belief;
#' zav.dv.3=concern;
#' higher numbers=higher belief/concern.
#'
#' @export
#'
#' @references Zaval, L., Keenan, E. A., Johnson, E. J., & Weber, E. U. (2014). How warm days increase belief in global warming. \strong{Nature Climate Change}, 4, 143-147.
#'
varfun.Zaval.1 <- function(vars){

  idC <- vars$Cold$zav.include.strict&(vars$Cold$zav.condition==1)
  idH <- vars$Heat$zav.include.strict&(vars$Heat$zav.condition==2)

  cleanDataFilter = data.frame(uID =c(vars$Cold$uID[idC],vars$Heat$uID[idH]),
                               variable =c(as.numeric(unlist(vars$Cold[idC,1])),as.numeric(unlist(vars$Heat[idH,1]))),
                               factor = factor(c(rep("Cold",sum(idC)), rep("Heat",sum(idH))))
                               )

  return(list(Cold = as.numeric(unlist(vars$Cold[idC,1])),
              Heat = as.numeric(unlist(vars$Heat[idH,1])),
              N    = c(length(as.numeric(unlist(vars$Cold[idC,1]))),length(as.numeric(unlist(vars$Heat[idH,1])))),
              cleanDataFilter = cleanDataFilter)
         )
}


#' varfun.Knobe.1
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#27_knobe}
#'
#' @section Variables:
#'   knob1.3=intentionality (help condition);
#'   knob2.3=intentionality (harm condition);
#'   for both, higher numbers=higher intentionality
#'
#' @export
#'
#' @references Knobe, J. (2003). Intentional action and side effects in ordinary language. Analysis, 63, 190-193.
#'
varfun.Knobe.1 <- function(vars){

  cleanDataFilter = data.frame(uID =c(vars$Help$uID,vars$Harm$uID),
                               variable =c(unlist(vars$Help[,1]),unlist(vars$Harm[,1])),
                               factor = factor(c(rep("Help",length(unlist(vars$Help[,1]))), rep("Harm",length(unlist(vars$Harm[,1])))))
                               )

  return(list(Help = unlist(vars$Help[,1]),
              Harm = unlist(vars$Harm[,1]),
              N    = c(length(unlist(vars$Help[,1])), length(unlist(vars$Harm[,1]))),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Knobe.2
#'
#' @param vars    A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#27_knobe}
#'
#' @section Variables:
#'   knob1.4=intentionality (praise condition);
#'   knob2.4=intentionality (blame condition);
#'   for both, higher numbers=higher intentionality
#'
#' @export
#'
#' @references Knobe, J. (2003). Intentional action and side effects in ordinary language. Analysis, 63, 190-193.
#'

varfun.Knobe.2 <- function(vars){

  cleanDataFilter = data.frame(uID =c(vars$Praise$uID,vars$Blame$uID),
                               variable =c(unlist(vars$Praise[,1]),unlist(vars$Blame[,1])),
                               factor = factor(c(rep("Praise",length(unlist(vars$Praise[,1]))), rep("Blame",length(unlist(vars$Blame[,1])))))
  )

  return(list(Praise = unlist(vars$Praise[,1]),
              Blame  = unlist(vars$Blame[,1]),
              N    = c(length(unlist(vars$Praise[,1])), length(unlist(vars$Blame[,1]))),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Gati.1
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#28_gati}
#'
#' @export
#'
#' @references Tversky, A., & Gati, I. (1978). Studies of similarity. \strong{Cognition and categorization}, 1, 79-98.
#'

varfun.Gati.1 <- function(vars){

  same <- any(grepl("(gati(1|2)(s))+",colnames(vars[[1]])))

  CounterBalanceA <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
  CounterBalanceB <- list(P1st = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))

  dfA     <- vars[[1]]
  dfB     <- vars[[2]]

  #dfA$uID <- 1:nrow(dfA)
  dfA$CounterBalance <- 1

  #dfB$uID <- 1:nrow(dfB)+nrow(dfA)
  dfB$CounterBalance <- 2

  dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfA$itemID <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
  dfA$Condition <- 1
  dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2

  dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
  dfB$Condition <- 1
  dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2

  df <- as.data.frame(rbind(dfA,dfB))

  df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
  if(same){
    df$Condition   <-  relevel(df$Condition,ref = 'Prominent2nd')
  } else {
    df$Condition <-  relevel(df$Condition,ref = 'Prominent1st')
  }
  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))

  df <- df[order(df$uID,df$itemID), ]

  #CounterBalance = df$CounterBalance,

  cleanDataFilter = df

  return(list(DV         = df$DV,
              Condition  = df$Condition,
              uID        = df$uID,
              itemID     = df$itemID,
              N          = c(nrow(vars[[1]]),nrow(vars[[2]])),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Gati.2
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#28_gati}
#'
#' @export
#'
#' @references Tversky, A., \& Gati, I. (1978). Studies of similarity. \strong{Cognition and categorization}, 1, 79-98.
#'

varfun.Gati.2 <- function(vars){

  same <- any(grepl("(gati(1|2)(s))+",colnames(vars[[1]])))

  CounterBalanceA <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
  CounterBalanceB <- list(P1st = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))

  dfA     <- vars[[1]]
  dfB     <- vars[[2]]

  #dfA$uID <- 1:nrow(dfA)
  dfA$CounterBalance <- 1

  #dfB$uID <- 1:nrow(dfB)+nrow(dfA)
  dfB$CounterBalance <- 2

  dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfA$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
  dfA$Condition <- 1
  dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2

  dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
  dfB$Condition <- 1
  dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2

  df <- as.data.frame(rbind(dfA,dfB))

  df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
  if(same){
    df$Condition   <-  relevel(df$Condition,ref = 'Prominent2nd')
  } else {
    df$Condition <-  relevel(df$Condition,ref = 'Prominent1st')
  }
  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))


  # Subject based dataset
  df.subj <- dplyr::summarize(dplyr::group_by(df, uID, Condition, CounterBalance),
                       stimDVm = mean(DV,na.rm = TRUE),
                       # study.order = paste0(unique(study.order), collapse = "|"),
                       # Country = paste0(unique(Country), collapse = "|")
  )

  df.subj.wide  <- tidyr::spread(df.subj, key = Condition, value = stimDVm)

  if(same){
    df.subj.wide$Asymmetry <-  df.subj.wide$Prominent2nd-df.subj.wide$Prominent1st
  } else {
    df.subj.wide$Asymmetry <- df.subj.wide$Prominent1st-df.subj.wide$Prominent2nd
  }

  cleanDataFilter = df.subj.wide

  return(list(Asymmetry  = df.subj.wide$Asymmetry,
              CompareTo  = 0,
              N          = c(nrow(df.subj.wide),NULL),
              cleanDataFilter = cleanDataFilter)
         )
}

#' varfun.Gati.3
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#28_gati}
#'
#' @export
#'
#' @references Tversky, A., & Gati, I. (1978). Studies of similarity. \strong{Cognition and categorization}, 1, 79-98.
#'

varfun.Gati.3 <- function(vars){
  #    require(dplyr)

  same <- any(grepl("(gati(1|2)(s))+",colnames(vars[[1]])))

  CounterBalanceA <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
  CounterBalanceB <- list(P1st = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))

  dfA     <- vars[[1]]
  dfB     <- vars[[2]]

  #dfA$uID <- 1:nrow(dfA)
  dfA$CounterBalance <- 1

  #dfB$uID <- 1:nrow(dfB)+nrow(dfA)
  dfB$CounterBalance <- 2

  dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfA$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
  dfA$Condition <- 1
  dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2

  dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
  dfB$Condition <- 1
  dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2

  df <- as.data.frame(rbind(dfA,dfB))

  df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
  if(same){
    df$Condition   <-  relevel(df$Condition,ref = 'Prominent1st')
  } else {
    df$Condition <-  relevel(df$Condition,ref = 'Prominent2nd')
  }
  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))

  df <- df[order(df$uID,df$itemID), ]

  # Item based dataset
  df.stim <- summarize(group_by(df, itemID, Condition), # interaction(Condition, CounterBalance)),
                       stimDV = mean(DV,na.rm = TRUE)
                       )

  cleanDataFilter <- df.stim

  return(list(DV        = df.stim$stimDV,
              Condition = df.stim$Condition,
              N         = c( sum(df.stim$Condition%in%"Prominent1st"), NULL),
              cleanDataFilter = cleanDataFilter)
         )
}


#' varfun.Gati.4
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#28_gati}
#'
#' @note This analysis tests moderating effect of presenting Norenzayan first or after.
#'
#' @export
#'
#' @references Tversky, A., & Gati, I. (1978). Studies of similarity. \strong{Cognition and categorization}, 1, 79-98.
#'

varfun.Gati.4 <- function(vars){

  same <- any(grepl("(gati(1|2)(s))+",colnames(vars[[1]])))

  CounterBalanceA <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
  CounterBalanceB <- list(P1st = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))

  dfA     <- vars[[1]]
  dfB     <- vars[[2]]

  #dfA$uID <- 1:nrow(dfA)
  dfA$CounterBalance <- 1

  #dfB$uID <- 1:nrow(dfB)+nrow(dfA)
  dfB$CounterBalance <- 2

  dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfA$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
  dfA$Condition <- 1
  dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2

  dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
  dfB$Condition <- 1
  dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2

  df <- as.data.frame(rbind(dfA,dfB))

  df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
  if(same){
    df$Condition   <-  relevel(df$Condition,ref = 'Prominent1st')
  } else {
    df$Condition <-  relevel(df$Condition,ref = 'Prominent2nd')
  }
  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))

  df <- df[order(df$uID,df$itemID), ]

  # Suibject based dataset
  df.subj <- summarize(group_by(df, uID, Condition),
                       stimDV = mean(DV,na.rm = TRUE)
  )

  df.subj.wide <- tidyr::spread(df.subj, key = Condition, value = stimDV)


  TGafterN.cbA <- sapply(which((vars$RawDataFilter[[1]]$Included)), function(i) which(unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]")) == "Tversky.Gati") > which(unlist(strsplit(x = vars$RawDataFilter[[1]]$StudyOrderN[i], split = "[|]")) == "Norenzayan"))
  TGafterN.cbB <- sapply(which((vars$RawDataFilter[[2]]$Included)), function(i) which(unlist(strsplit(x = vars$RawDataFilter[[2]]$StudyOrderN[i], split = "[|]")) == "Tversky.Gati") > which(unlist(strsplit(x = vars$RawDataFilter[[2]]$StudyOrderN[i], split = "[|]")) == "Norenzayan"))

  TGafterN.cbA <- unlist(TGafterN.cbA)
  TGafterN.cbB <- unlist(TGafterN.cbB)

  Order.n <- c(as.numeric(TGafterN.cbA), as.numeric(TGafterN.cbB))

  if(same){
    df.subj.wide$Asymmetry <-  df.subj.wide$Prominent2nd-df.subj.wide$Prominent1st
  } else {
    df.subj.wide$Asymmetry <- df.subj.wide$Prominent1st-df.subj.wide$Prominent2nd
  }

  df.subj.wide$Order <- factor(Order.n, levels=c(0,1), labels = vars$labels$Order)
  cleanDataFilter <- df.subj.wide

  return(list(Asymmetry  = df.subj.wide$Asymmetry,
              Order      = factor(Order.n, levels=c(0,1), labels = vars$labels$Order),
              N          = c(sum(Order.n==0,na.rm = TRUE),sum(Order.n==1,na.rm = TRUE)),
              cleanDataFilter = cleanDataFilter)
         )

  return(list(Asymmetry  = c(dfA$asym,dfB$asym),
              Order      = factor(c(as.numeric(TGafterN.cbA), as.numeric(TGafterN.cbB)), levels=c(0,1), labels = vars$labels$Order),
              N          = c(length(dfA$asym),length(dfB$asym)),
              cleanDataFilter = cleanDataFilter)
         )
}


#' varfun.Gati.5
#'
#' @param vars     A list object generated by \code{\link{get.sourceData}} containing cleaned data and variable labels.
#'
#' @return Dataset ready for analysis
#'
#' @description \url{https://manylabsopenscience.github.io/ML2_PoPS_proposal#28_gati}
#'
#' @export
#'
#' @references Tversky, A., & Gati, I. (1978). Studies of similarity. \strong{Cognition and categorization}, 1, 79-98.
#'
varfun.Gati.5 <- function(vars){

  same <- any(grepl("(gati(1|2)(s))+",colnames(vars[[1]])))

  CounterBalanceA <- list(P1st = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22),
                          P2nd = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21))
  CounterBalanceB <- list(P1st = c(4, 6, 7, 8, 9, 10, 13, 15, 20, 21),
                          P2nd = c(2, 3, 5, 11, 12, 14, 16, 17, 18, 19, 22))

  dfA     <- vars[[1]]
  dfB     <- vars[[2]]

  #dfA$uID <- 1:nrow(dfA)
  dfA$CounterBalance <- 1

  #dfB$uID <- 1:nrow(dfB)+nrow(dfA)
  dfB$CounterBalance <- 2

  dfA <- reshape2::melt(dfA,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfA$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfA$itemID))
  dfA$Condition <- 1
  dfA$Condition[dfA$itemID%in%CounterBalanceA$P2nd] <- 2

  dfB <- reshape2::melt(dfB,id=c('uID','CounterBalance'),variable.name='itemID',value.name='DV',factorsAsStrings = FALSE)

  dfB$itemID    <- as.numeric(gsub("(gati(1|2)(s|d)[.])","",dfB$itemID))
  dfB$Condition <- 1
  dfB$Condition[dfB$itemID%in%CounterBalanceB$P2nd] <- 2

  df <- as.data.frame(rbind(dfA,dfB))

  df$Condition       <- factor(df$Condition,levels=c(1,2),labels=c('Prominent1st','Prominent2nd'))
  if(same){
    df$Condition   <-  relevel(df$Condition,ref = 'Prominent1st')
  } else {
    df$Condition <-  relevel(df$Condition,ref = 'Prominent2nd')
  }
  df$CounterBalance  <- factor(df$CounterBalance,levels=c(1,2),labels=c('CBA','CBB'))

  df <- df[order(df$uID,df$itemID), ]

  # Item based dataset
  df.stim <- summarize(group_by(df, itemID, Condition),
                       stimDV = mean(DV,na.rm = TRUE),
                       stimSD = sd(DV, na.rm = TRUE),
                       N = n()
  )

  cleanDataFilter <- df.stim

  return(list(DV        = df.stim$stimDV,
              Condition = df.stim$Condition,
              N         = c( sum(df.stim$Condition%in%"Prominent1st"), NULL),
              cleanDataFilter = cleanDataFilter)
         )
}


# Install and load packages ----

#' @title Initialise It
#' @description Load and/or install R packages
#'
#' @param need    A vector of package names to be loaded. The wrapper functions have a predefinded \code{need} list and can be used as shortcuts (see details).
#' @param inT    Logical. If \code{TRUE} (default), packages in \code{need} wil be installed if they are not available on the system.
#'
#' @details \code{in.IT} will check if the Packages in the list argument \code{need} are installed on the system and load them. If \code{inT=TRUE} (default), it will first install the packages if they are not present and then proceed to load them.
#'
#' @export
#'
#' @author Fred Hasselman
#'
#' @family initialise packages
#' @keywords internal
#'
#' @examples
#' \dontrun{in.IT(c("reshape2", "plyr", "dplyr"))}
in.IT <- function(need=NULL,inT=TRUE){
  ip <- .packages(all.available=TRUE)
  if(any((need %in% ip)==FALSE)){
    if(inT==TRUE){
      install.packages(need[!(need %in% ip)])
    } else {
      cat('Package(s):\n',paste(need[(need %in% ip)==FALSE],sep='\n'),'\nnot installed.\nUse in.IT(c("packagename1","packagename2",...),inT=TRUE)')
      need <- need[(need %in% ip)==TRUE]
    }
  }
  ok <- sapply(1:length(need),function(p) require(need[[p]],character.only=TRUE))
}


#' @title Un-initialise It
#' @description Unload and/or uninstall R packages.
#' @param loose    A vector of package names to be unloaded.
#' @param unT    Logical. If \code{TRUE}, packages in \code{loose} wil be un-installed if they are available on the system.
#'
#' @details \code{un.IT}will check if the Packages in the list argument \code{loose} are installed on the system and unload them. If \code{unT=TRUE} it will first unload the packages if they are loaded, and then proceed to uninstall them.
#'
#' @export
#' @keywords internal
#'
#' @author Fred Hasselman
#'
#' @family initialise packages
#'
#' @examples
#' \dontrun{un.IT(loose = c("reshape2", "plyr", "dplyr"), unT = FALSE)}
un.IT <- function(loose,unT=FALSE){
  dp <- .packages()
  if(any(loose %in% dp)){
    for(looseLib in loose[(loose %in% dp)]){detach(paste0("package:",looseLib), unload=TRUE,character.only=TRUE)}
  }
  rm(dp)
  if(unT==TRUE){
    dp <- .packages(all.available=TRUE)
    if(any(loose %in% dp)){remove.packages(loose[(loose %in% dp)])}
  }
}


