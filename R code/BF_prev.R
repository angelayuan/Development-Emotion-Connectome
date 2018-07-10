setwd("/Users/yuanzh/Google Drive/Files_PNC_Analysis")
library(BayesFactor)

# # calculate BF for ROI activity with coordinates from previous studies
# #-----read in subject info----------
# sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
#                   colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
# cons <- c("neutral","fear","anger","sad","happy")
# 
# # load data
# roi_beta <- read.table('roi_prev_beta_stats_25/roi_beta_average.tsv',header = TRUE, sep = '\t',
#                        colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
# roi_beta$X <- NULL
# roi_names <- read.table('roi_prev_beta_stats_25/roiname_list.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
# 
# cnames <- c()
# for(i in c(1:length(roi_names$V1))){
#   for(j in c(1:length(cons))){
#     cnames <- c(cnames, paste(roi_names$V1[i],cons[j],sep="_") )
#   }
# }
# colnames(roi_beta) <- c("Subj",cnames)
# 
# include = c('Gee_R_AMY','L_AMY_AAL','R_AMY_AAL','Wolf_L_AMY_Talairach','Wolf_R_AMY_Talairach',
#             'Kujawa_L_AMY_TD','Kujawa_R_AMY_TD','Kujawa_L_AMY_AD','Kujawa_R_AMY_AD')
# inname <- c()
# for(i in c(1:length(include))){
#   for(j in c(1:length(cons))){
#     inname <- c(inname, paste(include[i],cons[j],sep="_") )
#   }
# }
# 
# roi_beta = roi_beta[,inname]
# 
# #--Combine data--
# cdata <- cbind(sub, roi_beta)
# cdata$Sex <- as.factor(cdata$Sex)
# cdata$Race <- as.factor(cdata$Race)
# cdata$Agebin <- as.factor(cdata$Agebin)
# cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
# cdata$GGroup <- as.factor(cdata$GGroup)
# cdata$DGroup <- as.factor(cdata$DGroup)
# 
# #-----Statistical Analysis---------
# num_roi <- length(include)
# roi.names <- include
# cons <- c("fear","anger","sad","happy")
# BF_mat <- matrix(rep(0,num_roi*4), nrow=4,ncol=num_roi) # condition*roi
# dim(BF_mat) <- c(length(cons),num_roi)
# dimnames(BF_mat) <- list(cons, roi.names)
# 
# for(c in 1:length(cons)){
#     ss = cdata[,c(1:10, grep(cons[c],names(cdata)))]
# 
#     for(i in 1:length(roi.names)){
#       if(cons[c]=='fear' | cons[c] == 'anger'){
#         f1 <- paste(roi.names[i],"_",cons[c]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
#         f0 <- paste(roi.names[i],"_",cons[c]," ~ ","GGroup + Sex + Race + Motion", sep="")
#       } else if(cons[c]=='sad' | cons[c] == 'happy'){
#         f1 <- paste(roi.names[i],"_",cons[c]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
#         f0 <- paste(roi.names[i],"_",cons[c]," ~ ","DGroup + Sex + Race + Motion", sep="")
#       }
#       
#       bf_f1 = lmBF(as.formula(f1), data = ss)
#       bf_f0 = lmBF(as.formula(f0), data = ss)
#       BF10 = bf_f1/bf_f0
#       BF_mat[c,i] <- exp(BF10@bayesFactor$bf)
#    } # ROI
# }
# 
# # save
# outputfile <- 'roi_prev_beta_stats_25/BF_roi_stats_allconds_maineffmodel.txt'
# write.table(BF_mat, outputfile, sep="\t", row.names=FALSE, quote = FALSE)


# calculate BF for connectivity with coordinates from previous studies
sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
cons <- c("fear","anger","sad","happy")

num_conn = 18
BF_mat <- matrix(rep(0,num_conn*4), nrow=4,ncol=num_conn) # condition*connectivity

for(c in 1:length(cons)){ # for each condition
  #--read in connectivity--
  fname <- paste("BS_Conn_prev_25_2018/bs_conns_",cons[c],".txt",sep="")
  conns <- read.table(fname,header = TRUE, sep = '\t',
                      colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
  conns$X <- NULL

  col_of_interest = c('Gee_R_AMY_Gee_R_VMPFC',
                      'Vink_L_AMY_Neg_Vink_L_mOFC_AAL','Vink_L_AMY_Neg_Vink_R_mOFC_AAL',
                      'Vink_R_AMY_Neg_Vink_L_mOFC_AAL','Vink_R_AMY_Neg_Vink_R_mOFC_AAL',
                      'Vink_L_AMY_Pos_Vink_L_mOFC_AAL','Vink_L_AMY_Pos_Vink_R_mOFC_AAL',
                      'Vink_R_AMY_Pos_Vink_L_mOFC_AAL','Vink_R_AMY_Pos_Vink_R_mOFC_AAL',
                      'Wolf_L_AMY_Talairach_Wolf_L_VMPFC','Wolf_L_AMY_Talairach_Wolf_R_VMPFC',
                      'Wolf_R_AMY_Talairach_Wolf_R_DMPFC',
                      'Kujawa_L_dACC_L_AMY_AAL','Kujawa_R_dACC_R_AMY_AAL',
                      'L_AMY_AAL_Wu_L_mPFC.1','R_AMY_AAL_Wu_L_mPFC',
                      'L_AMY_AAL_Wu_L_ACC','R_AMY_AAL_Wu_R_ACC')
  conns = conns[,col_of_interest]

  #--Combine data--
  cdata <- cbind(sub, conns)
  cdata$Sex <- as.factor(cdata$Sex)
  cdata$Race <- as.factor(cdata$Race)
  cdata$Agebin <- as.factor(cdata$Agebin)
  cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
  cdata$GGroup <- as.factor(cdata$GGroup)
  cdata$DGroup <- as.factor(cdata$DGroup)

  #--Statistical Analyses--
  num_conn <- ncol(conns)
  conn.names <- names(conns)

  for(i in 1:num_conn){ # for each connection
    if(cons[c]=='fear' | cons[c] == 'anger'){
      f1 <- paste(conn.names[i]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
      f0 <- paste(conn.names[i]," ~ ","GGroup + Sex + Race + Motion", sep="")
    } else if(cons[c]=='sad' | cons[c] == 'happy'){
      f1 <- paste(conn.names[i]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
      f0 <- paste(conn.names[i]," ~ ","DGroup + Sex + Race + Motion", sep="")
    }
    bf_f1 = lmBF(as.formula(f1), data = cdata)
    bf_f0 = lmBF(as.formula(f0), data = cdata)
    BF10 = bf_f1/bf_f0
    BF_mat[c,i] <- exp(BF10@bayesFactor$bf)
  } # connectivity
} # conditions

dimnames(BF_mat) <- list(cons, conn.names)
outputfile <- 'BS_Conn_prev_25_2018/BF_bs_stats_allconds_mainmodel.txt'
write.table(BF_mat, outputfile, sep="\t", row.names=FALSE, quote = FALSE)




# # network connectivity & roi/connectivity with coordinates from previous studies
# rm(list=ls())
# cons = c('Fear','Anger','Sad','Happy')
# # load data
# data = read.table('For_BF_Analysis_connectivity_2018.txt',header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# data$Age <- scale(data$Age)
# data$Motion <- scale(data$Motion)
# data$GGroup <- as.factor(data$GGroup)
# data$DGroup <- as.factor(data$DGroup)
# 
# # #M1: subcortical, M2: DMN, M3: FPN
# # c=1
# # seq = c(14, 13, 17, 16)
# #
# # c=2
# # seq = c(14, 18)
# #
# #
# # c=4
# # seq = c(14, 12, 16)
# 
# c=3 
# 
# ss = data[,c(1:11, grep(cons[c],names(data)))]
# for(i in 18:dim(data)[2]){
#   if(cons[c]=='Fear' | cons[c] == 'Anger'){
#       f1 <- paste(names(ss)[i]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
#       f0 <- paste(names(ss)[i]," ~ ","GGroup + Sex + Race + Motion", sep="")
#   } else if(cons[c]=='Sad' | cons[c] == 'Happy'){
#     f1 <- paste(names(ss)[i]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
#     f0 <- paste(names(ss)[i]," ~ ","DGroup + Sex + Race + Motion", sep="")
#   }
# 
#   bf_f1 = lmBF(as.formula(f1), data = ss)
#   bf_f0 = lmBF(as.formula(f0), data = ss)
#   BF10 = bf_f1/bf_f0
#   BF10
# }
# 
# # for Anger: BLA.R - VMPFC.R
# lmBF(BLA.R_VMPFC.L_Anger ~ Age + GGroup + Sex + Race + Motion, data=ss) / lmBF(BLA.R_VMPFC.L_Anger ~ Age + Sex + Race + Motion, data=ss)
# 
# 
# #-----large sample ROI activity----------
# rm(list=ls())
# sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
#                   colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
# cons <- c("neutral","fear","anger","sad","happy") 
# 
# #------read in ROI betas (759 subjects)--------
# # ROIs * conditions
# roi_beta <- read.table('Beta_50ROIs/Original Data/roi_beta_average.tsv',header = TRUE, sep = '\t',
#                        colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
# roi_beta$X <- NULL
# 
# roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
# 
# cnames <- c()
# for(i in c(1:length(roi_names$V1))){
#   for(j in c(1:length(cons))){
#     cnames <- c(cnames, paste(roi_names$V1[i],cons[j],sep="_") )
#   }
# }
# colnames(roi_beta) <- c("Subj",cnames)
# 
# 
# #--Combine data--
# cdata <- cbind(sub, roi_beta[,-1])
# cdata$Sex <- as.factor(cdata$Sex)
# cdata$Race <- as.factor(cdata$Race)
# cdata$Agebin <- as.factor(cdata$Agebin) 
# cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
# cdata$GGroup <- as.factor(cdata$GGroup) 
# cdata$DGroup <- as.factor(cdata$DGroup) 
# cdata$Age <- scale(cdata$Age)
# cdata$Motion <- scale(cdata$Motion)
# 
# #-----Statistical Analysis---------
# num_roi <- length(roi_names$V1)
# roi.names <- roi_names$V1
# cons <- c("fear","anger","sad","happy") 
# BF_mat <- matrix(rep(0,num_roi*4), nrow=4,ncol=num_roi) # condition*roi
# dim(BF_mat) <- c(length(cons),num_roi)
# 
# for(c in 1:length(cons)){
#   ss = cdata[,c(1:10, grep(cons[c],names(cdata)))]
#   
#   for(i in 1:length(roi.names)){ 
#     if(cons[c]=='fear' | cons[c] == 'anger'){
#        f1 <- paste(names(ss)[10+i]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
#        f0 <- paste(names(ss)[10+i]," ~ ","GGroup + Sex + Race + Motion", sep="")
#     } else if(cons[c]=='sad' | cons[c] == 'happy'){
#        f1 <- paste(names(ss)[10+i]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
#        f0 <- paste(names(ss)[10+i]," ~ ","DGroup + Sex + Race + Motion", sep="")
#     }
#     
#     ss2 = ss[,c(1:10,10+i)]
#     ss2 = na.omit(ss2)
#     bf_f1 = lmBF(as.formula(f1), data = ss2)
#     bf_f0 = lmBF(as.formula(f0), data = ss2)
#     BF10 = bf_f1/bf_f0
#     BF_mat[c,i] <- exp(BF10@bayesFactor$bf)
#   } # ROI
# }
# dimnames(BF_mat) <- list(cons, roi.names)
# 
# # plot results
# library(corrplot)
# col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
#                            "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
# reord <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
# res <- log(BF_mat[,reord])
# corrplot(res, method="square",
#          col=col0(50), 
#          tl.col="black", tl.srt=90, tl.cex=1, #tl.pos='n',
#          cl.cex = 1, is.corr=FALSE, cl.lim = c(floor(min(res)),ceiling(max(res))), cl.length=5, 
#          outline = FALSE) 
# 
# res <- BF_mat[,reord]>10
# res[res==0] = -1
# res[res==1] = 1
# corrplot(res, method="pie",
#          col=col0(50), 
#          tl.col="black", tl.srt=90, tl.cex=1, #tl.pos='n',
#          cl.cex = 1, is.corr=FALSE, cl.lim = c(-1,1), cl.length=3, 
#          outline = FALSE) 
# 
# 
# 
# #-----large sample connectivity----------
# rm(list=ls())
# sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
#                   colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
# 
# cons <- c("fear","anger","sad","happy") 
# 
# num_conn = 50*49/2
# BF_mat <- matrix(rep(0,num_conn*4), nrow=4,ncol=num_conn) # condition*connectivity
# dim(BF_mat) <- c(length(cons),num_conn)
# 
# for(c in 1:length(cons)){ # for each condition
#   #--read in connectivity--
#   fname <- paste("BS_Conn_50ROIs_2018/Original Data/bs_conns_",cons[c],".txt",sep="")
#   conns <- read.table(fname,header = TRUE, sep = '\t',
#                       colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
#   conns$X <- NULL
#   
#   #--Combine data--
#   cdata <- cbind(sub, conns[,-1])
#   cdata$Sex <- as.factor(cdata$Sex)
#   cdata$Race <- as.factor(cdata$Race)
#   cdata$Agebin <- as.factor(cdata$Agebin) 
#   cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
#   cdata$GGroup <- as.factor(cdata$GGroup) 
#   cdata$DGroup <- as.factor(cdata$DGroup) 
#   cdata$Age <- scale(cdata$Age)
#   cdata$Motion <- scale(cdata$Motion)
#   
#   #--Statistical Analyses--
#   num_conn <- ncol(conns)-1
#   conn.names <- names(conns)[-1]
#   
#   print(c)
#   for(i in 1:num_conn){ # for each connection
#     print(i)
#     if(cons[c]=='fear' | cons[c] == 'anger'){
#        f1 <- paste(names(cdata)[10+i]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
#        f0 <- paste(names(cdata)[10+i]," ~ ","GGroup + Sex + Race + Motion", sep="")
#     } else if(cons[c]=='sad' | cons[c] == 'happy'){
#        f1 <- paste(names(cdata)[10+i]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
#        f0 <- paste(names(cdata)[10+i]," ~ ","DGroup + Sex + Race + Motion", sep="")
#     }
#     
#     ss = cdata[,c(1:10,10+i)]
#     ss = na.omit(ss)
#     bf_f1 = lmBF(as.formula(f1), data = ss)
#     bf_f0 = lmBF(as.formula(f0), data = ss)
#     BF10 = bf_f1/bf_f0
#     BF_mat[c,i] <- exp(BF10@bayesFactor$bf)
#   }
# } # conditions
# dimnames(BF_mat) <- list(cons, conn.names)
# 
# output <- "BS_Conn_50ROIs_2018/BF_large_sample.RData"
# save(BF_mat, file = output)
# 
# 
# load("BS_Conn_50ROIs_2018/BF_large_sample.RData")
# # plot
# library(corrplot)
# col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
#                            "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
# 
# cons = c('fear','anger','sad','happy')
# roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
# num_rois <- length(roi_names$V1)
# num_conn = 50*49/2
#   
# for(c in 1:length(cons)){
#   # split each connectivity into two nodes name
#   ss <- sapply(colnames(BF_mat), strsplit, split="_")
#   bf_mat <- matrix(rep(0,num_rois), nrow=num_rois,ncol=num_rois) # for matrix plot purpose
#   for(i in 1:num_conn){
#     ind <- which(roi_names$V1 %in% ss[[i]])
#     bf_mat[ind[1],ind[2]] <- BF_mat[c,i]
#     bf_mat[ind[2],ind[1]] <- BF_mat[c,i]
#   }
#   
#   dimnames(bf_mat) <- list(roi_names$V1, roi_names$V1)
#   
#   res= log(bf_mat)
#   diag(res) <- 0
#   # plot matrix: frontoparietal, limbic, subcortical networks
#   net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
#   corrplot(res[net,net], method="square", type="lower",
#            diag = TRUE, col=col0(200),
#            tl.col="black", tl.srt=20, tl.cex=1, tl.pos='n',
#            cl.cex = 1, is.corr=FALSE, cl.lim = c(floor(min(res)),ceiling(max(res))), cl.length=5,
#            outline = FALSE)
# 
#   corrplot(res[net,net], method="square", type="lower",
#            diag = TRUE, col=col0(200),
#            tl.col="black", tl.srt=20, tl.cex=1, tl.pos='n',
#            cl.cex = 1, is.corr=FALSE, cl.lim = c(-10,10), cl.length=5,
#            outline = FALSE)
#   
#   
#   res = bf_mat>10
#   net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
#   corrplot(res[net,net], method="square", type="lower", 
#            diag = TRUE, col=col0(200), 
#            tl.col="black", tl.srt=20, tl.cex=1, tl.pos='n',
#            cl.cex = 1, is.corr=FALSE, cl.lim = c(-1,1), cl.length=3, 
#            outline = FALSE) 
#   
#    # corrplot(bf_mat[net,net], method="square", type="lower", 
#    #          diag = TRUE, col=col0(50), 
#    #          tl.col="black", tl.srt=20, tl.cex=1, tl.pos='n',
#    #          cl.cex = 1, is.corr=FALSE, cl.lim = c(floor(min(bf_mat)),ceiling(max(bf_mat))), cl.length=5, 
#    #          outline = FALSE) 
#   
#   
#   readline(prompt="Press [enter] to continue")
# }
