setwd("/Users/zhangyuan/Google Drive/Files_PNC_Analysis")
library(car)

#-----read in subject info----------
sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)

#-----supporting function----
findsig <- function(data,threshold=0.05,effname){
  set <- subset(data, fdrp<threshold)
  if(nrow(set)>0){
    set$eff <- effname
  } else{
    set <- set[0, ]
    set$eff <- numeric(nrow(set))
  }
  return(set)
}

cons <- c("fear","anger","sad","happy") 

deg_df <- data.frame(fear=numeric(50),anger=numeric(50),sad=numeric(50),happy=numeric(50))

for(c in 1:length(cons)){ # for each condition
  #--read in connectivity--
  fname <- paste("BS_Conn_50ROIs_2018/Original Data/bs_conns_",cons[c],".txt",sep="")
  conns <- read.table(fname,header = TRUE, sep = '\t',
                      colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
  conns$X <- NULL
  
  #--Combine data--
  cdata <- cbind(sub, conns[,-1])
  cdata$Sex <- as.factor(cdata$Sex)
  cdata$Race <- as.factor(cdata$Race)
  cdata$Agebin <- as.factor(cdata$Agebin) 
  cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
  cdata$GGroup <- as.factor(cdata$GGroup) 
  cdata$DGroup <- as.factor(cdata$DGroup) 
  
  #--Statistical Analyses--
  num_conn <- ncol(conns)-1
  conn.names <- names(conns)[-1]
  
  # examine age and group main effect
  res.group <- data.frame()
  res.age <- data.frame()
  # model: Connectivity ~ Age + Group + Sex + Race + Motion (scale continuous variables)
  for(i in 1:num_conn){ # for each connection
      print(i)
      if(cons[c]=='fear' | cons[c] == 'anger'){
          f <- paste("scale(",conn.names[i],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
      } else if(cons[c]=='sad' | cons[c] == 'happy'){
          f <- paste("scale(",conn.names[i],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep=" ")
      }
      fit <- lm(as.formula(f), data = cdata)
      #print(summary(fit))
    
      stats <- Anova(fit,type="III")
      tmp.group <- data.frame(stats[3, 3], stats[3, 2],stats["Residuals", 2], stats[3, 4],
                              summary(fit)$adj.r.squared,fit$coefficients[3])
      colnames(tmp.group) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef")
      res.group <- rbind(res.group, tmp.group)
      
      tmp.age <- data.frame(stats["scale(Age)", 3], stats["scale(Age)", 2],stats["Residuals", 2], stats["scale(Age)", 4],
                            summary(fit)$adj.r.squared, fit$coefficients['scale(Age)'])
      colnames(tmp.age) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef")
      res.age <- rbind(res.age, tmp.age)
  } # connection
  
  res.group <- cbind(conn.names, res.group)
  res.age <- cbind(conn.names, res.age)
  # FDR correction
  res.group$fdrp <- p.adjust(res.group$p, method="fdr", n=length(res.group$p))
  res.age$fdrp <- p.adjust(res.age$p, method="fdr", n=length(res.age$p))
  res.group$fdrsig <- res.group$fdrp < 0.05
  res.age$fdrsig <- res.age$fdrp < 0.05
  
  res.group.sig <- findsig(res.group, 0.05, "group")
  res.age.sig <- findsig(res.age, 0.05, "age")
  
  res.sig <- rbind(res.group.sig, res.age.sig)
  
  # plot
  roi_names <- read.table('Beta_50ROIs/roiname_list.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
  num_roi <- length(roi_names$V1)
  roi.names <- roi_names$V1
  
  coef_mat_all <- matrix(rep(0,num_roi*num_roi), nrow=num_roi,ncol=num_roi) 
  dim(coef_mat_all) <- c(num_roi,num_roi)
  coef_mat <- matrix(rep(0,num_roi*num_roi), nrow=num_roi,ncol=num_roi) 
  dim(coef_mat) <- c(num_roi,num_roi)
  deg_mat <- matrix(rep(0,num_roi*num_roi), nrow=num_roi,ncol=num_roi) 
  
  ss <- sapply(conn.names, strsplit, split="_")
  for(i in 1:num_conn){
    ind <- which(roi_names$V1 %in% ss[[i]])
    
    coef_mat_all[ind[1],ind[2]] <- res.age$coef[i]
    coef_mat_all[ind[2],ind[1]] <- res.age$coef[i]
    
    if(res.age$fdrsig[i]){
      coef_mat[ind[1],ind[2]] <- res.age$coef[i]
      coef_mat[ind[2],ind[1]] <- res.age$coef[i]
      deg_mat[ind[1],ind[2]] <- 1
      deg_mat[ind[2],ind[1]] <- 1
    }
  }
  dimnames(coef_mat_all) <- list(roi_names$V1, roi_names$V1)
  dimnames(coef_mat) <- list(roi_names$V1, roi_names$V1)
  dimnames(deg_mat) <- list(roi_names$V1, roi_names$V1)
  
  net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
  rownames(deg_df) <- roi_names$V1
  deg_df[,c] <- rowSums(deg_mat)

  library(corrplot)
  col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
                             "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
  corrplot(coef_mat[net,net], method="square", type="lower",
           diag = TRUE, col=col0(50),
           tl.col="black", tl.srt=20, tl.cex=1, tl.pos='n',
           cl.cex = 1, is.corr=FALSE, cl.lim = c(-0.3,0.3), cl.length=7,
           outline = FALSE)
  
  corrplot(deg_mat[net,net], method="square", #type="lower",
           diag = TRUE, col=col0(50),
           tl.col="black", tl.srt=20, tl.cex=1, #tl.pos='n',
           cl.cex = 1, is.corr=FALSE,  cl.length=7,
           outline = FALSE)

  #outputfile <- paste("BS_Conn_50ROIs_2018/Model1/bs_stats_",cons[c],"_model_mainEff.txt",sep="")
  #write.table(res.sig, outputfile, sep="\t", row.names=FALSE, quote = FALSE)
  outputfile <- paste("BS_Conn_50ROIs_2018/Model1/age_coef_",cons[c],"_model_mainEff.RData",sep="")
  save(coef_mat,coef_mat_all,file=outputfile)
  
  #rm(cdata, conns)
  
  # # for those links that showing a significant, test if there is an interaction
  # res.inter <- data.frame()
  # res.group <- data.frame()
  # fname <- paste("BS_Conn_50ROIs_2018/Model1/bs_stats_",cons[c],"_model_mainEff.txt",sep="")
  # links <- read.table(fname,header = TRUE, sep = '\t',stringsAsFactors=FALSE)
  # links <- unique(links$conn.names)
  # for(i in 1:length(links)){
  #     if(cons[c]=='fear' | cons[c] == 'anger'){
  #         f <- paste("scale(",links[i],") ~ ","scale(Age)*GGroup + Sex + Race + scale(Motion)", sep="")
  #     } else if(cons[c]=='sad' | cons[c] == 'happy'){
  #         f <- paste("scale(",links[i],") ~ ","scale(Age)*DGroup + Sex + Race + scale(Motion)", sep=" ")
  #     }
  #     fit <- lm(as.formula(f), data = cdata)
  #   
  #     stats <- Anova(fit,type="III")
  #     tmp.inter <- data.frame(stats["scale(Age):", 3], stats["scale(Age):", 2],stats["Residuals", 2], stats["scale(Age):", 4],
  #                             summary(fit)$adj.r.squared, fit$coefficients[7])
  #     colnames(tmp.inter) <- c("F", "Df", "Df.res", "p", "adj.r2","coef")
  #     res.inter <- rbind(res.inter, tmp.inter)
  #     
  #     tmp.group <- data.frame(stats[3, 3], stats[3, 2],stats["Residuals", 2], stats[3, 4],
  #                             summary(fit)$adj.r.squared,fit$coefficients[3])
  #     colnames(tmp.group) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef")
  #     res.group <- rbind(res.group, tmp.group)
  # } # links 
  # res.inter <- cbind(links, res.inter)
  # res.group <- cbind(links, res.group)
  # 
  # # FDR correction
  # res.group$fdrp <- p.adjust(res.group$p, method="fdr", n=length(res.group$p))
  # res.inter$fdrp <- p.adjust(res.inter$p, method="fdr", n=length(res.inter$p))
  # res.group$fdrsig <- res.group$fdrp < 0.05
  # res.inter$fdrsig <- res.inter$fdrp < 0.05
  # 
  # res.group.sig <- findsig(res.group, 0.05, "group")
  # res.inter.sig <- findsig(res.inter, 0.05, "group*age")
  # 
  # res.sig <- rbind(res.inter.sig, res.group.sig)
  # outputfile <- paste("BS_Conn_50ROIs_2018/Model1/bs_stats_",cons[c],"_model_interaction.txt",sep="")
  # write.table(res.sig, outputfile, sep="\t", row.names=FALSE, quote = FALSE)
  
} # conditions
deg <- deg_df[net,]
save(deg, file='BS_Conn_50ROIs_2018/Model1/degree_age_eff.RData')

outputfile <- paste("BS_Conn_50ROIs_2018/Model1/degree_age_eff.txt",sep="")
write.table(deg, outputfile, sep="\t", row.names=FALSE, quote = FALSE)
