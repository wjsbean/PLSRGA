  ########################################################
  # Author: Wang JinShe                                  #
  #         Gai Junyi                                    #
  # Institute: National Center for Soybean Improvement （NCSI）  #
  #            NanJing Agriculture University            #
  # Function: Genetic Improvement Plan for Hybrid Parents#
  # Date:     2019/07/26                                 #
  ########################################################


GEffect <- function(add1, add2, dom){
  # If interested locus is homozygous, then add1 = add2 = allele effect and dom = 0
  # If the locus is heterogous, then add1 = allele 1 effect, add2 = allele 2 effect and 
  #dom = dominance effect
  
  gf = 2 * add1 + 2 * add2 + dom
  return(gf)
}

gpred <- function(add, id_add, dom, id_dom, index, aname){
  # Compute the prediction of present estimation

  mrk <- NULL
  allele <- NULL
  for (i in 1:length(aname)){
    tmp <- strsplit(as.character(aname[i]), split = "-")[[1]]
    mrk <- c(mrk, tmp[1])
    allele <- c(allele, tmp[2])
  }
  nF1 <- ncol(index)
  pred_gf <- NULL
  m_a_a <- NULL
  for (i in 1:nF1){
    if (any(id_dom[, i] != 0)){
      E_dom <- dom[as.logical(id_dom[, i])]
    }
    else {
      E_dom <- 0
    }
    id1 <- id_add[, index[1, i]]
    id2 <- id_add[, index[2, i]]
    allele1 <- add[as.logical(id1)]
    allele2 <- add[as.logical(id2)]
    tmp <- matrix(c(GEffect(allele1, allele2, E_dom), 
                    GEffect(allele1, allele1, 0), 
                    GEffect(allele2, allele2, 0)), 
                  nrow = 1)
    pred_gf <- rbind(pred_gf, tmp)
    pred_homo1 <- 
    tmp <- matrix(c(mrk[as.logical(id1)], 
                    paste("P", index[1, i], sep = ""),
                    allele[as.logical(id1)], 
                    paste("P", index[2, i], sep = ""),
                    allele[as.logical(id2)]                     
                    ), 
                  nrow = 1)
    m_a_a <- rbind(m_a_a, tmp)
  }
  mrk_vale = data.frame(mrk = m_a_a[, 1], 
                        P1 = m_a_a[, 2], 
                        A1 = m_a_a[, 3], 
                        P2 = m_a_a[, 4], 
                        A2 = m_a_a[, 5], 
                        Heter = pred_gf[, 1],
                        Homo1 = pred_gf[, 2],
                        Homo2 = pred_gf[, 3]
                        )

  return(mrk_vale)
}

filt <- function(MrkVale){
  Dom_value <- unique(MrkVale$Heter)
  A1 <- NULL
  A2 <- NULL
  for (i in 1:length(Dom_value)){
    tmp <- MrkVale[which(MrkVale$Heter == Dom_value[i]), ]
    A1 <- c(A1, as.character(tmp$A1)[1])
    A2 <- c(A2, as.character(tmp$A2)[1])
  }
  AD_value <- data.frame(Value = Dom_value, A1 = A1, A2 = A2)
  return(AD_value)
}

Improv <- function(MrkValue, AD_value){
  conn <- file("c:/Improv_Plan.txt", 'a')
  cat(paste('Locus ', 
            MrkValue[1,]$mrk, 
            ' Alternative Improvement Programs are listed as follow: \n', 
            sep=''), file = conn)
  cat('**********************************************\n', file = conn)
  nF1 <- nrow(MrkValue)
  for (i in 1:nF1){
    heter_vale <- MrkValue[i, 6]
    A1 <- MrkValue[i, 3]
    A2 <- MrkValue[i, 5]
    tmp <- AD_value[AD_value$Value > heter_vale, ]
    if (length(tmp$Value) > 1){
      cat('------------------------\n', file = conn)
      cat(paste(MrkValue[i, ]$P1, '/', MrkValue[i, ]$P2, '\n', sep = ''), file = conn)
      cat(paste(MrkValue[i, ]$A1, '/', 
                  MrkValue[i, ]$A2, 
                  '(', 
                  MrkValue[i, ]$Heter, 
                  ')', ' -->> \n', 
                  sep = ''), file = conn)
      for (j in 1:length(tmp$A1)){
        cat(paste(tmp[j, ]$A1, 
                  '/', 
                  tmp[j, ]$A2, 
                  '  Value = ', 
                  tmp[j, ]$Value, 
                  '\n', 
                  sep = ''), file = conn)
      }
      cat('------------------------\n', file = conn)
    }
  }
  cat('**********************************************\n', file = conn)
  cat('***************--The End--*********************\n', file = conn)
  close(conn)
}

MainImprov <- function(add, dom){
  index <- dom[1:2, 3:ncol(dom)]
  dom <- dom[-(1:2), ]
  amrk <- NULL
  for(i in 1:nrow(add)){
    amrk <- c(amrk, strsplit(as.character(add[i, ]$ALLELE), split = '-')[[1]][1])
  }
  dmrk <- NULL
  for (j in 1:nrow(dom)){
    dmrk <- c(dmrk, strsplit(as.character(dom[j, 1]), split = '-')[[1]][1])
  }
  mrk <- unique(amrk)
  for (i in 1:length(mrk)){
    cat(paste(mrk[i],'\n'))
    id <- amrk == mrk[i]
    a_effect <- add[id, 2]
    a_index <- add[id, 3:ncol(add)]
    a_name <- add[id, 1]
    id <- dmrk == mrk[i]
    d_effect <- dom[id, 2]
    d_index <- dom[id, 3:ncol(dom)]
    geffect_pred <- gpred(a_effect, a_index, d_effect, d_index, index, a_name)
    Filt <- filt(geffect_pred)
    Improve <- Improv(geffect_pred, Filt)
  }
}