library(dplyr)
library(ggplot2)
library(reshape2)
library(gplots)
library(ggfortify)
library(data.table)
library(RTCGA)
library(RTCGA.clinical)
library(survival)
library(survminer)
library(rowr)
library(pbapply)
library(dunn.test)
library(VennDiagram)
library(gplots)

cor_test <- function(data, a, b, loga=FALSE, logb=FALSE){
  temp <- data %>%
    select(a, b)
  if (loga==TRUE){
    temp[,a] <- log(temp[,a])
  }
  if (logb==TRUE){
    temp[,b] <- log(temp[,b])
  }
  temp <- select(temp, a, b)
  temp <- temp[!is.infinite(rowSums(temp)),]
  temp2 <- tryCatch(
    {
      cor.test(x=temp[,a], y=temp[,b], method=c("pearson"))
    },
      error=function(e){
        return(data.frame("estimate"=NA, "p.value"=NA))
    },
      warning=function(w){
        return(data.frame("estimate"=NA, "p.value"=NA))
      }
  )
  return(temp2)
}
cor_mat <- function(data, gene="BRCA1"){
  # mutations=c("toA", "toT", "toG", "toC", "fromA", "fromT", "fromG", "fromC", "Transitions", "Transversions", "Silent", "Missense", "Nonsense", "Nonstop", "Splice_Site_SNP", "Insertions", "Deletions", "Indel_Total", "Frame_Shift_Ins", "Frame_Shift_Del", "In_Frame_Ins", "In_Frame_Del", "Frame_Shift_Total", "In_Frame_Total", "Splice_Site_Ins", "Splice_Site_Del", "Splice_Site_Indel_Total", "Splice_Site_Total", "Large_Ins", "Large_Del", "Large_Indel_Total", "Short_Ins", "Short_Del", "Short_Indel_Total")
  mutations=c("toA", "toT", "toG", "toC", "fromA", "fromT", "fromG", "fromC", "Transitions", "Transversions", "Silent", "Missense", "Nonsense", "Insertions", "Deletions", "Indel_Total", "Frame_Shift_Ins", "Frame_Shift_Del", "In_Frame_Ins", "In_Frame_Del", "Frame_Shift_Total", "In_Frame_Total", "Large_Ins", "Large_Del", "Large_Indel_Total", "Short_Ins", "Short_Del", "Short_Indel_Total")
  temp <- NULL
  for (i in 1:length(mutations)){
    if (max(data[,gene]) == 0){
      temp2 <- data.frame("estimate" = NA, "p.value" = NA)
      sig=c("")
      print(mutations[i])
    } else if (max(data[,mutations[i]]) == 0){
      temp2 <- data.frame("estimate" = NA, "p.value" = NA)
      sig=c("")
    } else {
      temp2 <- cor_test(data=data, a=gene, b=mutations[i], loga=T, logb=T)
      if (is.na(temp2$p.value)){
        sig=c("")
      } else if (0.05 > temp2$p.value & temp2$p.value >= 0.01){
        sig=c("*")
      } else if (0.01 > temp2$p.value & temp2$p.value >= 0.001){
        sig=c("**")
      } else if (0.001 >temp2$p.value){
        sig=c("***")
      } else {
        sig=c("")
      }
    }
    temp <- rbind(temp, cbind(mutations[i], temp2$estimate, temp2$p.value, sig))
  }
  colnames(temp) <- c("mutation", "r.value", "p.value", "sig")
  return(temp)
}
cor_mat_comb <- function(data, var){
  dnet_cor_mat <- NULL
  dnet_p_mat <- NULL
  dnet_sig_mat <- NULL
  for (i in 1:length(var)){
    print(var[i])
    temp1 <- cor_mat(data, gene=var[i])
    dnet_cor_mat <- cbind(dnet_cor_mat, as.numeric(temp1[,"r.value"]))
    dnet_p_mat <- cbind(dnet_p_mat, temp1[,"p.value"])
    dnet_sig_mat <- cbind(dnet_sig_mat, temp1[,"sig"])
    colnames(dnet_cor_mat)[i] <- var[i]
    colnames(dnet_p_mat)[i] <- var[i]
    colnames(dnet_sig_mat)[i] <- var[i]
  }
  rownames(dnet_cor_mat) <- temp1[,"mutation"]
  rownames(dnet_p_mat) <- temp1[,"mutation"]
  rownames(dnet_sig_mat) <- temp1[,"mutation"]
  return(list(dnet_cor_mat, dnet_p_mat, dnet_sig_mat))
}
hilomat <- function(data, genes, mutations){
  final_out <- NULL
  for (i in 1:length(genes)){
    inter <- data[order(data[,genes[i]]),]
    inter_lo <- inter[1:round(nrow(inter)/3),]
    inter_hi <- inter[(nrow(inter)-round(nrow(inter)/3)):nrow(inter),]
    inter2 <- NULL
    for (m in 1:length(mutations)){
      inter2 <- rbind(inter2, log2(mean(inter_hi[,mutations[m]])/mean(inter_lo[,mutations[m]])))
    }
    rownames(inter2) <- mutations
    colnames(inter2) <- c(genes[i])
    final_out <- cbind(final_out, inter2)
  }
  return(final_out)
}
hilomat2 <- function(data, mutations){
  inter_mat <- NULL
  inter_mat$gene <- data
  inter_mat$mutation <- mutations
  inter_hi <- as.data.frame(inter_mat) %>%
    top_frac(n=1/3, wt=gene) %>%
    summarise(lo=mean(mutation))

  inter_lo <- as.data.frame(inter_mat) %>%
    top_frac(n=-1/3, wt=gene) %>%
    summarise(lo=mean(mutation))
  return(log2(inter_hi/inter_lo))
}
pan_mut <- function(genes){
  tcgas = c("BRCA", "CHOL", "GBM", "LGG", "CESC", "COAD",  "KIRP",  "LIHC",  "LUAD",  "LUSC",  "MESO",  "PAAD", "PRAD", "SARC", "STAD", "THCA", "UCS", "LAML", "ACC", "BLCA", "ESCA", "HNSC", "KICH", "KIRC", "DLBC", "OV", "PCPG", "READ", "SKCM", "TGCT", "THYM", "UCEC", "UVM")
  pan_point <- NULL
  pan_ins <- NULL
  pan_del <- NULL
  pan_tmb <- NULL
  for (i in 1:length(tcgas)){
    print(tcgas[i])
    tcga_mat <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-", tcgas[i], "_mat.csv"), sep=",", stringsAsFactors=FALSE))
    inter_mat <- as.data.frame(apply(tcga_mat[,c(genes)], MARGIN=2, FUN=hilomat2, mutations=as.vector(tcga_mat$Point_Mutation_Total)))
    pan_point <- rbind(pan_point, inter_mat)
    inter_mat <- as.data.frame(apply(tcga_mat[,c(genes)], MARGIN=2, FUN=hilomat2, mutations=as.vector(tcga_mat$Insertions)))
    pan_ins <- rbind(pan_ins, inter_mat)
    inter_mat <- as.data.frame(apply(tcga_mat[,c(genes)], MARGIN=2, FUN=hilomat2, mutations=as.vector(tcga_mat$Deletions)))
    pan_del <- rbind(pan_del, inter_mat)
    inter_mat <- as.data.frame(apply(tcga_mat[,c(genes)], MARGIN=2, FUN=hilomat2, mutations=as.vector(tcga_mat$Point_Mutation_Total+tcga_mat$Insertions+tcga_mat$Deletions)))
    pan_tmb <- rbind(pan_tmb, inter_mat)
  }
  colnames(pan_point) <- genes
  colnames(pan_ins) <- genes
  colnames(pan_del) <- genes
  colnames(pan_tmb) <- genes
  rownames(pan_point) <- tcgas
  rownames(pan_ins) <- tcgas
  rownames(pan_del) <- tcgas
  rownames(pan_tmb) <- tcgas
  return(list(pan_point, pan_ins, pan_del, pan_tmb))
}
surv_fit_cust <- function(data, tcga, ids, clin){

  gene <- data[1]
  data <- data[2:length(data)]
  tcga_mat <- as.data.frame(cbind(ids, data))
  colnames(tcga_mat) <- c("barc", "gene")
  tcga_mat$gene <- as.numeric(as.character(tcga_mat$gene))

  gene_low <- data.table(tcga_mat, key="gene")[1:round((nrow(tcga_mat)/2)),]
  gene_high <- data.table(tcga_mat, key="gene")[round(nrow(tcga_mat)/2):nrow(tcga_mat),]

  b1high_ss <- as.vector(gene_high$barc)
  b1low_ss <- as.vector(gene_low$barc)
  b1high_clin <- filter(clin, bcr_patient_barcode %in% b1high_ss)
  b1low_clin <- filter(clin, bcr_patient_barcode %in% b1low_ss)

  b1high_clin$expr <- c("high")
  b1low_clin$expr <- c("low")
  brca1_clin <- rbind(b1high_clin, b1low_clin)

  exp_coeff <- exp(coxph(formula = Surv(times, patient.vital_status) ~ expr, data = as.data.frame(brca1_clin))$coefficients)
  pval <- summary(coxph(formula = Surv(times, patient.vital_status) ~ expr, data = as.data.frame(brca1_clin)))$waldtest[3]
  return(c(exp_coeff, pval))
}
single_fit <- function(gene, tcga){
  tcga_mat <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-", tcga, "_mat.csv"), sep=",", header=TRUE, stringsAsFactors=FALSE))
  id_mat <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-", tcga, "/harmonized/", "TCGA-", tcga, "_mutcases.csv"), sep=",", stringsAsFactors=FALSE))
  filt_mat <- tcga_mat[,c("case_id", gene)]
  filt_mat <- filt_mat[filt_mat$case_id %in% id_mat$case_id,]
  for (j in 1:nrow(id_mat)){
    filt_mat[(filt_mat$case_id==id_mat[j,"case_id"]),"barc"] <- id_mat[j,"submitter_id"]
  }
  filt_mat[,gene] <- as.numeric(as.character(filt_mat[,gene]))

  gene_low <- data.table(filt_mat, key=gene)[1:round((nrow(filt_mat)/2)),]
  gene_high <- data.table(filt_mat, key=gene)[round(nrow(filt_mat)/2):nrow(filt_mat),]
  b1high_ss <- as.vector(gene_high$barc)
  b1low_ss <- as.vector(gene_low$barc)

  clin <- survivalTCGA(get(paste0(tcga, ".clinical")), extract.cols="admin.disease_code")

  b1high_clin <- filter(clin, bcr_patient_barcode %in% b1high_ss)
  b1low_clin <- filter(clin, bcr_patient_barcode %in% b1low_ss)
  b1high_clin$expr <- c("high")
  b1low_clin$expr <- c("low")
  brca1_clin <- rbind(b1high_clin, b1low_clin)

  sfit <- survfit(Surv(times, patient.vital_status)~expr, data=brca1_clin)
  output <- list(brca1_clin, sfit)
  return(output)
}
pan_can_man <- function(genes){
  tcgas = c("BRCA", "CHOL", "GBM", "LGG", "CESC", "COAD",  "KIRP",  "LIHC",  "LUAD",  "LUSC",  "MESO",  "PAAD", "PRAD", "SARC", "STAD", "THCA", "UCS", "LAML", "ACC", "BLCA", "ESCA", "HNSC", "KICH", "KIRC", "DLBC", "OV", "PCPG", "READ", "SKCM", "TGCT", "THYM", "UCEC", "UVM")
  exp_coeff <- NULL
  pval_mat <- NULL
  for (i in 1:length(tcgas)){
    print(tcgas[i])
    tcga_mat <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-", tcgas[i], "_mat.csv"), sep=",", stringsAsFactors=FALSE))
    id_mat <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-", tcgas[i], "/harmonized/", "TCGA-", tcgas[i], "_mutcases.csv"), sep=",", stringsAsFactors=FALSE))

    tcga_mat <- tcga_mat[,colnames(tcga_mat) %in% c("case_id", genes)]
    tcga_mat <- tcga_mat[tcga_mat$case_id %in% id_mat$case_id,]
    for (j in 1:nrow(id_mat)){
      tcga_mat[(tcga_mat$case_id==id_mat[j,"case_id"]),"barc"] <- id_mat[j,"submitter_id"]
    }

    tcga_ids <- tcga_mat$barc
    tcga_mat <- tcga_mat[,colnames(tcga_mat)!="barc"]
    tcga_mat <- rbind(colnames(tcga_mat), tcga_mat)

    tcga_clin <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-", tcgas[i], "/harmonized/", "TCGA-", tcgas[i], "_survival.csv"), sep=",", stringsAsFactors=FALSE))
    surv_mats <- apply(tcga_mat[,2:ncol(tcga_mat)], FUN=surv_fit_cust, MARGIN=2, tcga=tcgas[i], ids=tcga_ids, clin=tcga_clin)
    exp_coeff <- rbind(exp_coeff, surv_mats[1,])
    pval_mat <- rbind(pval_mat, surv_mats[2,])
  }
  rownames(exp_coeff) <- tcgas
  rownames(pval_mat) <- tcgas
  return(list(exp_coeff, pval_mat))
}
sig_mate <- function(exp_mat, pvals, pp, pi, pd){
  sig_mat <- NULL
  for (c in 1:ncol(pvals)){
    for (r in 1:nrow(pvals)){
        sig_mat <- rbind(sig_mat, cbind(colnames(exp_mat)[c], rownames(exp_mat)[r], exp_mat[r,c], pvals[r,c]))
    }
  }
  colnames(sig_mat) <- c("Gene", "TCGA", "exp.coeff", "p.value")
  sig_mat <- as.data.frame(sig_mat)
  sig_mat <- sig_mat[sig_mat$Gene!="barc",]
  for (r in 1:nrow(sig_mat)){
    prow <- match(as.vector(sig_mat[r,"TCGA"])[1], rownames(pp))
    pcol <- as.vector(sig_mat[r,"Gene"])[1]
    out <- as.data.frame(pp)[prow,pcol]
    sig_mat[r,"PM"] <- as.data.frame(pp)[prow,pcol]
  }
  for (r in 1:nrow(sig_mat)){
    prow <- match(as.vector(sig_mat[r,"TCGA"])[1], rownames(pi))
    pcol <- as.vector(sig_mat[r,"Gene"])[1]
    out <- as.data.frame(pi)[prow,pcol]
    sig_mat[r,"Ins"] <- as.data.frame(pi)[prow,pcol]
  }
  for (r in 1:nrow(sig_mat)){
    prow <- match(as.vector(sig_mat[r,"TCGA"])[1], rownames(pd))
    pcol <- as.vector(sig_mat[r,"Gene"])[1]
    out <- as.data.frame(pd)[prow,pcol]
    sig_mat[r,"Del"] <- as.data.frame(pd)[prow,pcol]
  }

  sig_mat$exp.coeff <- as.numeric(as.character(sig_mat$exp.coeff))
  sig_mat[(log2(sig_mat$exp.coeff) <1.5),"pmsig"]<-"0"
  sig_mat[(sig_mat$PM < 1.5),"pmsig"]<-"0"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$PM <(-1.5))),"pmsig"]<-"1.5"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$PM >1.5)),"pmsig"]<-"2"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$PM <(-1.5))),"pmsig"]<-"3"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$PM >1.5)),"pmsig"]<-"4"
  sig_mat[(log2(sig_mat$exp.coeff) <1.5),"inssig"]<-"0"
  sig_mat[(sig_mat$Ins < 1.5),"inssig"]<-"0"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$Ins <(-1.5))),"inssig"]<-"1.5"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$Ins >1.5)),"inssig"]<-"2"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$Ins <(-1.5))),"inssig"]<-"3"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$Ins >1.5)),"inssig"]<-"4"
  sig_mat[(log2(sig_mat$exp.coeff) <1.5),"delsig"]<-"0"
  sig_mat[(sig_mat$Del < 1.5),"delsig"]<-"0"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$Del <(-1.5))),"delsig"]<-"1.5"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$Del >1.5)),"delsig"]<-"2"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$Del <(-1.5))),"delsig"]<-"3"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$Del >1.5)),"delsig"]<-"4"

  sig_mat <- mutate(sig_mat, TMB = (PM+Ins+Del)/3)
  sig_mat[(log2(sig_mat$exp.coeff) <1.5),"tmbsig"]<-"0"
  sig_mat[(sig_mat$TMB < 1.5),"tmbsig"]<-"0"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$TMB <(-1.5))),"tmbsig"]<-"1.5"
  sig_mat[((log2(sig_mat$exp.coeff) >1.5) & (sig_mat$TMB >1.5)),"tmbsig"]<-"2"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$TMB <(-1.5))),"tmbsig"]<-"3"
  sig_mat[((log2(sig_mat$exp.coeff) <(-1.5)) & (sig_mat$TMB >1.5)),"tmbsig"]<-"4"
  return(sig_mat)
}
scatter_cross <- function(data, file="scatter_cross_test.png", cols=c("grey", "green", "blue", "red", "purple")){
  plot_temp<-ggplot(data, aes(x=TMB, y=log2(exp.coeff), colour=as.character(tmbsig))) +
    theme(
      #background
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      #tick mark colour, length and width
      axis.ticks = element_line(colour="black"),
      axis.ticks.length = unit(4, "pt"),
      element_line(size = 1),
      #axis line
      axis.line = element_line(colour = 'white', size =1),
      #axis text
      axis.text = element_text(size=10, colour='black', face="bold")
      # axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 10, colour="black", face="bold")
    ) +
    geom_hline(aes(yintercept=0))+
    geom_vline(aes(xintercept=0))+
    geom_segment(aes(x=-1.5, xend=-1.5, y=-4.5, yend=-1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=-4.1, xend=-1.5, y=-1.5, yend=-1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=1.5, xend=1.5, y=-4.5, yend=-1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=1.5, xend=4.1, y=-1.5, yend=-1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=1.5, xend=1.5, y=4.5, yend=1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=4.1, xend=1.5, y=1.5, yend=1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=-1.5, xend=-1.5, y=4.5, yend=1.5), linetype="dashed", colour='black')+
    geom_segment(aes(x=-1.5, xend=-4.1, y=1.5, yend=1.5), linetype="dashed", colour='black')+
    # geom_text(aes(label=Gene), size=2)+
    guides(fill=FALSE, colour=FALSE)+
    scale_y_continuous(limits=c(-4.5, 4.5), breaks=c(-3, -1.5, 0, 1.5, 3)) +
    scale_x_continuous(limits=c(-4.1, 4.1), breaks=c(-3, -1.5, 0, 1.5, 3))+
    geom_point(alpha=0.6, size=2)+
    #geom_line(stat="smooth", method='lm',formula=y~x, size = 3, alpha=0.5, aes(col=variable))+
    # scale_fill_manual(values = c("red", "green2", "grey")) +
    scale_color_manual(values = cols) +
    ggsave(filename = file, width = 5, height = 5, dpi=2000, bg='white')
  return(plot_temp)
}
group_box <- function(data, file="group_box_test.png", yax="Surv", cols=c("white", "white", "white")){
  plot_temp<-ggplot(data, aes_string(y=yax), colour='black') +
    theme(
      #background
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      #tick mark colour, length and width
      axis.ticks = element_line(colour="black",),
      axis.ticks.length = unit(10, "pt"),
      element_line(size = 2),
      #axis line
      axis.line = element_line(colour = 'black', size = 2),
      #axis text
      axis.text.y = element_text(size=20, colour='black', face="bold"),
      axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 20, colour="black", face="bold")
    ) +
    guides(fill=FALSE, colour=FALSE)+
    #scale_y_continuous(limits=c(-5,5)) +
    stat_boxplot(geom ='errorbar', lwd = 2, color='black', aes(x=factor(Group, levels=c("Random", "Repair", "DNet")))) +
    geom_boxplot(color='black', lwd=2, aes(x=factor(Group, levels=c("Random", "Repair", "DNet")), fill=Group))+
    #geom_line(stat="smooth", method='lm',formula=y~x, size = 3, alpha=0.5, aes(col=variable))+
    scale_fill_manual(values = cols) +
    ggsave(filename = file, width = 5, height = 10, dpi=2000, bg='white')
  return(plot_temp)
}
group_tally <- function(data, file="group_tally_test.png", lims=c(0,255), cols=c("purple", "red", "blue", "green")){
  plot_temp<-ggplot(tally(group_by(data, Group, sig_group))) +
    theme(
      #background
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      #tick mark colour, length and width
      axis.ticks = element_line(colour="black",),
      axis.ticks.length = unit(5, "pt"),
      element_line(size = 1),
      #axis line
      axis.line = element_line(colour = 'black', size = 1),
      #axis text
      axis.text.y = element_text(size=15, colour='black', face="bold"),
      axis.text.x = element_text(size = 15, colour="black", face="bold")
      # axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 20, colour="black", face="bold")
    ) +
    guides(fill=FALSE, colour=FALSE)+
    scale_y_continuous(limits=lims, expand=c(0,0)) +
    geom_histogram(stat="identity", position="stack", colour='black',  size=1, aes(y=n, x=factor(Group, levels=c("Random", "Repair", "DNet")), fill=factor(as.character(sig_group), levels=c("4", "3", "2", "1.5"))))+
    scale_fill_manual(values = cols) +
    ggsave(filename = file, width = 4, height = 6, dpi=2000, bg='white')
  return(plot_temp)
}
surv_tally <- function(data, file="surv_tally_test.png", normal=FALSE, cols=c("grey", "blue", "green")){
  inter_data <- tally(group_by(data, TCGA, Group))
  if (normal==TRUE){
    rand <- filter(inter_data, Group=="Random")
    net <- filter(inter_data, Group=="DNet")
    repair <- filter(inter_data, Group=="Repair")
    tcgas <- as.vector(unique(inter_data$TCGA))
    for (i in 1:length(tcgas)){
      if (tcgas[i] %notin% rand$TCGA){
        rand[(nrow(rand)+1),] <- c(tcgas[i], "Random", 1)
      }
      if (tcgas[i] %notin% repair$TCGA){
        repair[(nrow(repair)+1),] <- c(tcgas[i], "Repair", 1)
      }
      if (tcgas[i] %notin% net$TCGA){
        net[(nrow(net)+1),] <- c(tcgas[i], "DNet", 1)
      }
    } 
    net$n <- as.numeric(net$n)
    repair$n <- as.numeric(repair$n)
    rand$n <- as.numeric(rand$n)
    net<-arrange(net, TCGA)
    repair<-arrange(repair, TCGA)
    rand<-arrange(rand, TCGA)

    net$n <- net$n/rand$n
    repair$n <- repair$n/rand$n
    rand$n <- rand$n/rand$n

    inter_data <- rbind(rand, net, repair)
  }
  plot_temp<-ggplot(inter_data, aes(x=reorder(TCGA, n), y=n, fill=factor(Group, levels=c("Random", "Repair", "DNet")))) +
    theme(
      #background
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      #tick mark colour, length and width
      axis.ticks = element_line(colour="black",),
      axis.ticks.length = unit(10, "pt"),
      element_line(size = 2),
      #axis line
      axis.line = element_line(colour = 'black', size = 2),
      #axis text
      axis.text.y = element_text(size=20, colour='black', face="bold"),
      axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 20, colour="black", face="bold")
    ) +
    guides(fill=FALSE, colour=FALSE)+
    scale_y_continuous(expand=c(0,0)) +
    geom_bar(stat="identity", position="dodge", color='black')+
    scale_fill_manual(values = cols) +
    ggsave(filename = file, width = 15, height = 5, dpi=2000, bg='white')
  return(plot_temp)
}
tmb_tcga_bars <- function(nets, goes, rands, file="tmb_tcga_bars_test.png", cols=c("grey", "blue", "green")){

  net_test <- apply(data.matrix(nets), MARGIN=1, FUN=mean)
  go_test <- apply(data.matrix(goes), MARGIN=1, FUN=mean)
  rand_test <- apply(data.matrix(rands), MARGIN=1, FUN=mean)
  test <- as.data.frame(rbind(cbind("DNet", as.numeric(net_test), as.character(names(net_test))), cbind("Repair", as.numeric(go_test), as.character(names(go_test))), cbind("Random", as.numeric(rand_test), as.character(names(rand_test)))))
  colnames(test) <- c("Group", "TMB", "TCGA")

  plot_temp<-ggplot(test, aes(y=as.numeric(as.character(TMB)), fill=factor(Group, levels=c("Random", "Repair", "DNet")), x=reorder(TCGA, as.numeric(as.character(TMB)))), colour='black') +
    theme(
      #background
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA),
      #tick mark colour, length and width
      axis.ticks = element_line(colour="black",),
      axis.ticks.length = unit(10, "pt"),
      element_line(size = 2),
      #axis line
      axis.line = element_line(colour = 'black', size = 2),
      #axis text
      axis.text.y = element_text(size=13, colour='black', face="bold"),
      axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 13, colour="black", face="bold")
    ) +
    geom_hline(yintercept=0)+
    guides(fill=FALSE, colour=FALSE)+
    scale_y_continuous() +
    geom_bar(stat="identity", position="dodge", color='black')+
    #geom_line(stat="smooth", method='lm',formula=y~x, size = 3, alpha=0.5, aes(col=variable))+
    scale_fill_manual(values = cols) +
    ggsave(filename = file, width = 15, height = 7, dpi=2000, bg='white')
  return(plot_temp)
}
transcor_mat <- function(data, genecol){
  cor <- cor.test(x=genecol, y=data, method=c("pearson"))$estimate
  if (is.na(cor)){
    return(0)
  } else {
    return(cor)
  }
}
pan_transcor_mat <- function(gene, df, gene_list){
  test <- apply(df[!(names(df) %in% gene_list)], MARGIN=2, FUN=transcor_mat, genecol=df[,gene])
  # test <- apply(df, MARGIN=2, FUN=transcor_mat, genecol=df[,gene])
  return(test)
}
cormat_cut <- function(data, corcut, fract){
  if (length(data[unlist(lapply(unlist(data),function(x) x>=corcut))])>length(data)*fract){
    return(data)
  }
}
hilo_barcs <- function(inter_gens, tcga){
  inter <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\TCGA-", tcga, "_mat.csv"), sep=",", header=TRUE, stringsAsFactors=FALSE))

  inter_filt <- inter[,inter_gens]
  inter_filtlog <- apply(inter_filt, 1, log2)
  inter_filtlog[!is.finite(inter_filtlog)] <- 0

  inter_filt <- inter[,c("case_id", inter_gens)]
  inter_ranked <- t(cbind(inter_filt[,1], apply(inter_filt[2:ncol(inter_filt)], 2, rank)))
  inter_sum <- as.data.frame(cbind(inter_ranked[1,], apply(inter_ranked[2:nrow(inter_ranked),], 2, function(x) sum(as.numeric(x)))))
  colnames(inter_sum) <- c("case_id", "rank")

  inter_high <- filter(inter_sum, as.numeric(as.character(rank)) > median(as.numeric(as.character(inter_sum$rank))))$case_id
  inter_low <- filter(inter_sum, as.numeric(as.character(rank)) < median(as.numeric(as.character(inter_sum$rank))))$case_id

  return(list(inter_low, inter_high))
}
hilo_mut <- function(x, mut) log2(mean(filter(x, group=="High")[,mut])/mean(filter(x, group=="Low")[,mut]))

'%notin%' <- function(x,y)!('%in%'(x,y))




















############################################################################### Group loading ###############################################################################

transcriptome <- as.vector(colnames(as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-UVM_mat.csv"), sep=",", header=TRUE, stringsAsFactors=FALSE))))

dna_go <- as.vector(fread(file="dna_repair_genes_go_0006281.txt", sep="\t", stringsAsFactors=FALSE)[[1]])
dna_go2 <- NULL
for (i in 1:length(dna_go)){
  if (toupper(dna_go[i]) %in% transcriptome){
    dna_go2 <- c(dna_go2, toupper(dna_go[i]))
  }
}

dnet_targs <- as.vector(read.csv("custom_final/ddr_mods_shared.txt", header=FALSE)$V1)
dnet_targs2 <- c()
for (i in 1:length(dnet_targs)){
  if (dnet_targs[i] %in% transcriptome & dnet_targs[i] %notin% dna_go2){
    dnet_targs2 <- c(dnet_targs2, dnet_targs[i])
  }
}

random_genes <- c()
i=0
while (i < length(dna_go2)){
  random_gene <- transcriptome[floor(runif(1, min=2, max=length(transcriptome)))]
  if (random_gene %notin% dnet_targs2 & random_gene %notin% dna_go2){
    random_genes <- c(random_genes, random_gene)
    i = i+1
  }
}




















############################################################################### pan cancer mutation analysis ###############################################################################

net_pans <- pan_mut(dnet_targs2)
go_pans <- pan_mut(dna_go2)
rand_pans <- pan_mut(random_genes)



hmcols3<- c((colorRampPalette(c("blue","#00b2ff"))(25)),(colorRampPalette(c("#00b2ff","white"))(25)),(colorRampPalette(c("white","#ff6100"))(25)),(colorRampPalette(c("#ff6100","red"))(25)))
breaking=seq(-5.3, 5.3, 0.106)

tcga_mats <- c("pan_point", "pan_ins", "pan_del")
for (i in 1:length(tcga_mats)){
  png(file = paste0(tcga_mats[i], "_net2.png"), width=6, height=2, units="in", res=3000, pointsize = 3)
  heatmap.2(data.matrix(net_pans[[i]]), trace = "none", breaks=breaking, col=hmcols3, density.info = "none", na.color="grey", dendrogram = "both")
  dev.off()
}
for (i in 1:length(tcga_mats)){
  png(file = paste0(tcga_mats[i], "_repair.png"), width=6, height=2, units="in", res=3000, pointsize = 3)
  heatmap.2(data.matrix(go_pans[[i]]), trace = "none", breaks=breaking, col=hmcols3, density.info = "none", na.color="grey", dendrogram = "both")
  dev.off()
}
for (i in 1:length(tcga_mats)){
  png(file = paste0(tcga_mats[i], "_random2.png"), width=6, height=2, units="in", res=3000, pointsize = 3)
  heatmap.2(data.matrix(rand_pans[[i]]), trace = "none", breaks=breaking, col=hmcols3, density.info = "none", na.color="grey", dendrogram = "both")
  dev.off()
}




















############################################################################### Survival vs Mutations ###############################################################################

net_surv <- pan_can_man(dnet_targs2)
go_surv <- pan_can_man(dna_go2)
rand_surv <- pan_can_man(random_genes)

net_sig <- sig_mate(as.data.frame(net_surv[1]), as.data.frame(net_surv[2]), as.data.frame(net_pans[1]), as.data.frame(net_pans[2]), as.data.frame(net_pans[3]))
go_sig <- sig_mate(as.data.frame(go_surv[1]), as.data.frame(go_surv[2]), as.data.frame(go_pans[1]), as.data.frame(go_pans[2]), as.data.frame(go_pans[3]))
rand_sig <- sig_mate(as.data.frame(rand_surv[1]), as.data.frame(rand_surv[2]), as.data.frame(rand_pans[1]), as.data.frame(rand_pans[2]), as.data.frame(rand_pans[3]))

vio_sig <- as.data.frame(rbind(cbind(rand_sig$TMB, rand_sig$exp.coeff, as.character(rand_sig$Gene), as.character(rand_sig$TCGA), "Random"), cbind(go_sig$TMB, go_sig$exp.coeff, as.character(go_sig$Gene), as.character(go_sig$TCGA), "Repair"), cbind(net_sig$TMB, net_sig$exp.coeff, as.character(net_sig$Gene), as.character(net_sig$TCGA), "DNet")))
colnames(vio_sig) <- c("TMB", "Surv", "Gene", "TCGA", "Group")
vio_sig$TMB <- as.numeric(as.character(vio_sig$TMB))
vio_sig$Surv <- log2(as.numeric(as.character(vio_sig$Surv)))

net_sig$Group <- "DNet"
go_sig$Group <- "Repair"
rand_sig$Group <- "Random"
group_tally <- rbind(rand_sig, go_sig, net_sig) %>%
  filter(log2(exp.coeff) > 1.5 | log2(exp.coeff) <(-1.5)) %>%
  filter(PM > 1.5 | PM < (-1.5) | Ins > 1.5 | Ins < (-1.5) | Del > 1.5 | Del < (-1.5))
group_tally2 <- NULL
for (r in 1:nrow(group_tally)){
  if (group_tally[r,"pmsig"]>0){
    temp <- group_tally[r,]
    temp$sig_group<-group_tally[r,"pmsig"]
    temp$tmb_group<-group_tally[r,"PM"]
    group_tally2<-rbind(group_tally2, temp)
  }
  if (group_tally[r,"delsig"]>0){
    temp <- group_tally[r,]
    temp$sig_group<-group_tally[r,"delsig"]
    temp$tmb_group<-group_tally[r,"Del"]
    group_tally2<-rbind(group_tally2, temp)
  }
  if (group_tally[r,"inssig"]>0){
    temp <- group_tally[r,]
    temp$sig_group<-group_tally[r,"inssig"]
    temp$tmb_group<-group_tally[r,"Ins"]
    group_tally2<-rbind(group_tally2, temp)
  }
}
group_tally2 <- unique(group_tally2)



plot_temp <- scatter_cross((acc_sig), file="scatter_net.png", cols=c("grey", "purple"))
plot_temp <- scatter_cross((go_sig), file="scatter_repair.png", cols=c("grey", "green", "blue", "red", "purple"))
plot_temp <- scatter_cross((rand_sig), file="scatter_random.png", cols=c("grey", "green", "blue", "red", "purple"))

plot_temp <- group_box(filter(vio_sig, TCGA=="READ"), file="group_box_surv_read.png", yax="Surv", cols= c("#00b0f0", "grey", "#ffc000"))
plot_temp <- group_box(filter(vio_sig, TCGA=="READ"), file="group_box_tmb_read.png", yax="TMB", cols= c("#00b0f0", "grey", "#ffc000"))

plot_temp <- group_tally(group_tally2, file="group_size_stacked.png")

plot_temp <- surv_tally(vio_sig, file="surv_tally.png", normal=FALSE, cols= c("grey", "#ffc000", "#00b0f0"))
plot_temp <- surv_tally(vio_sig, file="surv_tally_norm.png", normal=TRUE, cols= c("grey", "#ffc000", "#00b0f0"))

plot_temp <- tmb_tcga_bars(net_pans[[4]], go_pans[[4]], rand_pans[[4]], file="tmb_tcga_bars.png", cols= c("grey", "#ffc000", "#00b0f0"))
plot_temp <- tmb_tcga_bars(net_surv[[1]], go_surv[[1]], rand_surv[[1]], file="surv_tcga_bars.png", cols= c("grey", "#ffc000", "#00b0f0"))





wilcox.test(filter(vio_sig, TCGA=="ACC" & Group %in% c("Repair", "Random"))$TMB ~ filter(vio_sig, TCGA=="ACC" & Group %in% c("Repair", "Random"))$Group, paired = F, alternative = c("less"))
wilcox.test(filter(vio_sig, TCGA=="ACC" & Group %in% c("DNet", "Random"))$TMB ~ filter(vio_sig, TCGA=="ACC" & Group %in% c("DNet", "Random"))$Group, paired = F, alternative = c("greater"))
wilcox.test(filter(vio_sig, TCGA=="ACC" & Group %in% c("DNet", "Repair"))$TMB ~ filter(vio_sig, TCGA=="ACC" & Group %in% c("DNet", "Repair"))$Group, paired = F, alternative = c("greater"))




















############################################################################### Networking ###############################################################################

acc <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\", "TCGA-ACC_mat.csv"), sep=",", header=TRUE, stringsAsFactors=FALSE))[,2:27752]
acc_genes <- as.vector(unique(filter(filter(net_sig, TCGA=="ACC"), pmsig==4.0 | inssig==4.0 | delsig==4.0)$Gene))

acc_cut <- pbapply(acc, MARGIN=2, FUN=function(x) sum(x)>0)
acc_cut2 <- as.data.frame(acc_cut[unlist(lapply(acc_cut, FUN=function(x) x==TRUE))])
transcriptome_filt <- rownames(acc_cut2)

acc_cormat <- t(as.data.frame(pblapply(acc2_genes, FUN=pan_transcor_mat, df=acc[(names(acc) %in% c(transcriptome_filt))], gene_list=acc2_genes)))
rownames(acc_cormat) <- acc2_genes
acc2_genes <- c("RPLP0", "KIF23", "KIF4A", "MKI67", "CENPF", "STMN1", "LMNB1", "KIF2C", "TPX2", "TOP2A", "MCM10", "LMNB2", "MYBL2", "NCAPD2", "ANLN", "DNMT1", "SGO2", "PRPF40A", "HNRNPR", "NOLC1", "GNAI3", "NCL", "DDX18", "CAPRIN1", "DDX50", "DDX21", "HNRNPM", "SET", "NUP188", "TUBB", "ATAD3A", "NOC2L", "YBX1", "KNL1")
acc2_cormat<-acc_cormat
acc2_cormat <- cbind(acc2_cormat, "Nope")
colnames(acc2_cormat) <- seq(1, ncol(acc2_cormat), 1)
colnames(acc2_cormat)[ncol(acc2_cormat)] <- "group"
for (i in 1:length(acc2_genes)){
  acc2_cormat[acc2_genes[i],"group"] <- "cor"
}

# acc genes correlation matrix
hmcols3<- c((colorRampPalette(c("blue","#00b2ff"))(25)),(colorRampPalette(c("#00b2ff","white"))(25)),(colorRampPalette(c("white","#ff6100"))(25)),(colorRampPalette(c("#ff6100","red"))(25)))
breaking=seq(-1, 1, 0.02)
png(file = "hm_acc2_cormattest.png", width=2, height=2, units="in", res=3000, pointsize = 2)
plot_temp <- heatmap.2(data.matrix(acc_cormat), breaks=breaking, trace = "none", col=hmcols3, density.info = "none", na.color="grey", dendrogram = "row")
dev.off()

# acc genes correlation PCA
prcomp_acc<-data.frame(prcomp(data.matrix(acc_cormat[,acc_genes]))$x, Group=acc2_cormat[,"group"], Labels=rownames(acc2_cormat))
prcomp_read<-data.frame(prcomp(data.matrix(read_cormat[,read_genes]))$x, Group=read2_cormat[,"group"], Labels=rownames(read2_cormat))

plot_temp<-ggplot(prcomp_acc, aes(x=PC1, y=PC2, col=Group)) +
  theme(
    #background
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    #tick mark colour, length and width
    axis.ticks = element_line(colour="black"),
    axis.ticks.length = unit(7, "pt"),
    element_line(size = 1),
    #axis line
    axis.line = element_line(colour = 'black', size = 1),
    #axis text
    axis.text.y = element_text(size=10, colour='black', face="bold"),
    axis.text.x = element_text(size = 10, colour="black", face="bold")
  ) +
  #scale_y_continuous(limits=c(-3,5)) +
  geom_point(size=3)+
  stat_ellipse(geom="polygon", aes(col=Group, fill=Group), alpha=0.1, type = "norm") +
  scale_color_manual(values = c("red", "blue")) +
  scale_fill_manual(values = c("red", "blue")) +
  ggsave(filename = "pca_acc_cocor.png", width = 6, height = 5, dpi=500, bg='white')





# accnet gene dna repair percentage
acc_cut <- pbapply(acc_cormat, MARGIN=2, FUN=cormat_cut, corcut=0.5, fract=0.5)
acc_cut <- as.data.frame(acc_cut[unlist(lapply(acc_cut, FUN=function(x) length(x)>1))])

dna_go3 <- (intersect(dna_go2, colnames(acc_cormat)))

acc_overlap <- c(intersect(dna_go3, colnames(acc_cut)))
total_overlap <- c(intersect(dna_go3, colnames(acc_cormat)))

dnago_overlaps <- data.frame("group"=c("Total", "ACC"), "percent"=c(total_overlap[1], acc_overlap[1]))

plot_temp<-ggplot(dnago_overlaps, aes(y=percent, x=factor(group, levels=c("Total", "ACC")), fill=factor(group, levels=c("Total", "ACC")))) +
  theme(
    #background
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    #tick mark colour, length and width
    axis.ticks = element_line(colour="black",),
    axis.ticks.length = unit(7, "pt"),
    element_line(size = 1),
    #axis line
    axis.line = element_line(colour = 'black', size = 1),
    #axis text
    axis.text = element_text(size=20, colour='black', face="bold"),
    # axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 13, colour="black", face="bold")
  ) +
  #geom_hline(yintercept=0)+
  guides(fill=FALSE, colour=FALSE)+
  scale_y_continuous(expand=c(0,0), limits=c(0, 25.5)) +
  geom_bar(stat="identity", position="dodge", color='black', width=0.8)+
  #geom_line(stat="smooth", method='lm',formula=y~x, size = 3, alpha=0.5, aes(col=variable))+
  scale_fill_manual(values = c("grey", "purple")) +
  ggsave(filename = "percent_dnago_bar.png", width = 3, height = 6, dpi=2000, bg='white')




















############################################################################### Cumulative ACC-Net Analysis ###############################################################################

acc_filt <- acc[,acc_cut]
acc_filtlog <- apply(acc_filt, 1, log2)
acc_filtlog[!is.finite(acc_filtlog)] <- 0

hmcols3<- c((colorRampPalette(c("blue","#00b2ff"))(25)),(colorRampPalette(c("#00b2ff","white"))(25)),(colorRampPalette(c("white","#ff6100"))(25)),(colorRampPalette(c("#ff6100","red"))(25)))
breaking=seq(-1, 1, 0.02)
png(file = "hm_acc_corred_group_acc.png", width=1.3, height=3, units="in", res=3000, pointsize = 1)
plot_temp <- heatmap.2(data.matrix((acc_filtlog)), Colv=TRUE, Rowv=FALSE, scale='row', trace = "none", col=hmcols3, density.info = "none", na.color="grey", dendrogram = "col")
dev.off()

# ranking patient by all gene expression
acc_filt <- acc[,c("case_id", acc_cut)]
go_filt <- acc[,c("case_id", dna_go2)]
rand_filt <- acc[,c("case_id", random_genes)]
acc_ranked <- t(cbind(acc_filt[,1], apply(acc_filt[2:ncol(acc_filt)], 2, rank)))
go_ranked <- t(cbind(go_filt[,1], apply(go_filt[2:ncol(go_filt)], 2, rank)))
rand_ranked <- t(cbind(rand_filt[,1], apply(rand_filt[2:ncol(rand_filt)], 2, rank)))
acc_sum <- as.data.frame(cbind(acc_ranked[1,], apply(acc_ranked[2:nrow(acc_ranked),], 2, function(x) sum(as.numeric(x)))))
colnames(acc_sum) <- c("case_id", "rank")
go_sum <- as.data.frame(cbind(go_ranked[1,], apply(go_ranked[2:nrow(go_ranked),], 2, function(x) sum(as.numeric(x)))))
colnames(go_sum) <- c("case_id", "rank")
rand_sum <- as.data.frame(cbind(rand_ranked[1,], apply(rand_ranked[2:nrow(rand_ranked),], 2, function(x) sum(as.numeric(x)))))
colnames(rand_sum) <- c("case_id", "rank")

# patient bacode extraction
acc_high <- filter(acc_sum, as.numeric(as.character(rank)) > median(as.numeric(as.character(acc_sum$rank))))$case_id
acc_low <- filter(acc_sum, as.numeric(as.character(rank)) < median(as.numeric(as.character(acc_sum$rank))))$case_id
go_high <- filter(go_sum, as.numeric(as.character(rank)) > median(as.numeric(as.character(go_sum$rank))))$case_id
go_low <- filter(go_sum, as.numeric(as.character(rank)) < median(as.numeric(as.character(go_sum$rank))))$case_id
rand_high <- filter(rand_sum, as.numeric(as.character(rank)) > median(as.numeric(as.character(rand_sum$rank))))$case_id
rand_low <- filter(rand_sum, as.numeric(as.character(rank)) < median(as.numeric(as.character(rand_sum$rank))))$case_id

# clinical access and grouping
acc_id_mat <- as.data.frame(fread(paste0("Q:\\cancerdata\\newr\\GDCdata\\TCGA-ACC/harmonized/TCGA-ACC_mutcases.csv"), sep=",", stringsAsFactors=FALSE))

acc_high_barc=c()
acc_low_barc=c()
for (i in 1:length(acc_high)){
  acc_high_barc <- c(acc_high_barc, filter(acc_id_mat, case_id == acc_high[i])$submitter_id)
  acc_low_barc <- c(acc_low_barc, filter(acc_id_mat, case_id == acc_low[i])$submitter_id)
}
clin <- survivalTCGA(get("ACC.clinical"), extract.cols="admin.disease_code")
high_clin <- filter(clin, bcr_patient_barcode %in% acc_high_barc)
high_clin$group <- "High"
low_clin <- filter(clin, bcr_patient_barcode %in% acc_low_barc)
low_clin$group <- "Low"
acc_ccomb <- rbind(high_clin, low_clin)

rand_high_barc=c()
rand_low_barc=c()
for (i in 1:length(rand_high)){
  rand_high_barc <- c(rand_high_barc, filter(acc_id_mat, case_id == rand_high[i])$submitter_id)
  rand_low_barc <- c(rand_low_barc, filter(acc_id_mat, case_id == rand_low[i])$submitter_id)
}
clin <- survivalTCGA(get("ACC.clinical"), extract.cols="admin.disease_code")
high_clin <- filter(clin, bcr_patient_barcode %in% rand_high_barc)
high_clin$group <- "High"
low_clin <- filter(clin, bcr_patient_barcode %in% rand_low_barc)
low_clin$group <- "Low"
rand_ccomb <- rbind(high_clin, low_clin)

# survival analysis and plotting
acc_sfit <- survfit(Surv(times, patient.vital_status)~group, data=acc_ccomb)
rand_sfit <- survfit(Surv(times, patient.vital_status)~group, data=rand_ccomb)

theme1 <-  theme(
    #background
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    #tick mark colour, length and width
    axis.ticks = element_line(colour="black"),
    axis.ticks.length = unit(4, "pt"),
    element_line(size = 1),
    #axis line
    axis.line = element_line(colour = 'black', size =1),
    #axis text
    axis.text = element_text(size=15, colour='black', face="bold")
    # axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 10, colour="black", face="bold")
  )

png(file = "km_rand_acc.png", width=5, height=5, units="in", res=2000, pointsize = 5)
ggsurvplot(rand_sfit, conf.int=TRUE, pval=TRUE, palette=c("red", "blue"), ggtheme=theme1)
dev.off()





# cumulative mutation effect
acc_barcs <- hilo_barcs(acc_gens, "ACC")
acc_muts <- acc[,c("case_id", "Point_Mutation_Total", "Insertions", "Deletions")]
acc_muts$group[acc_muts$case_id %in% acc_barcs[[2]]] <- "High"
acc_muts$group[acc_muts$case_id %in% acc_barcs[[1]]] <- "Low"
acc_muts$samp <- "acc"

rand_barcs <- hilo_barcs(random_genes, "ACC")
rand_muts <- read[,c("case_id", "Point_Mutation_Total", "Insertions", "Deletions")]
rand_muts$group[rand_muts$case_id %in% rand_barcs[[2]]] <- "High"
rand_muts$group[rand_muts$case_id %in% rand_barcs[[1]]] <- "Low"
rand_muts$samp <- "rand"

go_barcs <- hilo_barcs(dna_go2, "ACC")
go_muts <- read[,c("case_id", "Point_Mutation_Total", "Insertions", "Deletions")]
go_muts$group[go_muts$case_id %in% go_barcs[[2]]] <- "High"
go_muts$group[go_muts$case_id %in% go_barcs[[1]]] <- "Low"
go_muts$samp <- "go"

tot_muts <- rbind(rand_muts, acc_muts, go_muts)

muts <- c("Point_Mutation_Total", "Insertions", "Deletions")
tot_muts <- NULL
for (i in 1:length(muts)){
  tot_muts <- rbind(tot_muts, cbind("rand", muts[i], hilo_mut(rand_muts, muts[i])))
  tot_muts <- rbind(tot_muts, cbind("go", muts[i], hilo_mut(go_muts, muts[i])))
  tot_muts <- rbind(tot_muts, cbind("acc", muts[i], hilo_mut(acc_muts, muts[i])))
}
tot_muts <- as.data.frame(tot_muts)
colnames(tot_muts) <- c("samp", "mut", "log2")
tot_muts$log2 <- as.numeric(as.character(tot_muts$log2))

plot_temp<-ggplot(tot_muts, colour='black') +
  theme(
    #background
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    #tick mark colour, length and width
    axis.ticks = element_line(colour="black",),
    axis.ticks.length = unit(10, "pt"),
    element_line(size = 2),
    #axis line
    axis.line.y = element_line(colour = 'black', size = 2),
    axis.line.x = element_line(colour = 'white', size = 2),
    #axis text
    axis.text.y = element_text(size=20, colour='black', face="bold"),
    axis.text.x = element_text(angle = -45, hjust =-0.2, vjust = 2, size = 20, colour="black", face="bold")
  ) +
  guides(fill=FALSE, colour=FALSE)+
  #scale_y_continuous(expand=c(0,0)) +
  geom_histogram(stat="identity", position="stack", colour='black',  size=1, aes(y=log2, x=factor(samp, levels=c("rand", "go", "acc")), fill=factor(mut, levels=c("Point_Mutation_Total", "Insertions", "Deletions"))))+
  geom_hline(aes(yintercept=0), size=2)+
  #geom_line(stat="smooth", method='lm',formula=y~x, size = 3, alpha=0.5, aes(col=variable))+
  scale_fill_manual(values =c("chartreuse2", "steelblue1", "orangered2")) +
  ggsave(filename = "mut_bar_acc.png", width = 5, height = 7, dpi=2000, bg='white')





# stats

acc_pans <- pan_mut(acc_cut)
acc_surv <- pan_can_man(acc_cut)
acc_sig <- sig_mate(as.data.frame(acc_surv[1]), as.data.frame(acc_surv[2]), as.data.frame(acc_pans[1]), as.data.frame(acc_pans[2]), as.data.frame(acc_pans[3]))

vio_accnet <- as.data.frame(rbind(cbind(rand_sig$TMB, rand_sig$exp.coeff, as.character(rand_sig$Gene), as.character(rand_sig$TCGA), "Random"), cbind(go_sig$TMB, go_sig$exp.coeff, as.character(go_sig$Gene), as.character(go_sig$TCGA), "Repair"), cbind(acc_sig$TMB, acc_sig$exp.coeff, as.character(acc_sig$Gene), as.character(acc_sig$TCGA), "ACC-Net")))
colnames(vio_accnet) <- c("TMB", "Surv", "Gene", "TCGA", "Group")
vio_accnet$TMB <- as.numeric(as.character(vio_accnet$TMB))
vio_accnet$Surv <- log2(as.numeric(as.character(vio_accnet$Surv)))

wilcox.test(filter(vio_accnet, TCGA=="ACC" & Group %in% c("Repair", "Random"))$Surv ~ filter(vio_accnet, TCGA=="ACC" & Group %in% c("Repair", "Random"))$Group, paired = F, alternative = c("greater"))
wilcox.test(filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Random"))$Surv ~ filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Random"))$Group, paired = F, alternative = c("less"))
wilcox.test(filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Repair"))$Surv ~ filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Repair"))$Group, paired = F, alternative = c("less"))

wilcox.test(filter(vio_accnet, TCGA=="ACC" & Group %in% c("Repair", "Random"))$TMB ~ filter(vio_accnet, TCGA=="ACC" & Group %in% c("Repair", "Random"))$Group, paired = F, alternative = c("less"))
wilcox.test(filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Random"))$TMB ~ filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Random"))$Group, paired = F, alternative = c("greater"))
wilcox.test(filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Repair"))$TMB ~ filter(vio_accnet, TCGA=="ACC" & Group %in% c("ACC-Net", "Repair"))$Group, paired = F, alternative = c("greater"))


