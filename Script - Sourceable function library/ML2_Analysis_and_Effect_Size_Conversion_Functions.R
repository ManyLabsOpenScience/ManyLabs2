
# ANALYSIS functions for MANYLABS 2  -----
#
# v.1.0 by Fred Hasselman

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
                    N   = NULL,
                    n1 = NULL, n2 = NULL,
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
           Z    = esComp[[cnt]] <- compute.es::pes(p = ifelse(pnorm(abs(x), lower.tail= FALSE)*2==0,.Machine$double.eps, pnorm(abs(x), lower.tail= FALSE)*2), level = CL*100, n.1 = n1, n.2 = n2, tail = "two", verbose = FALSE, dig = 5),
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
