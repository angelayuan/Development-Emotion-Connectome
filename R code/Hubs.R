setwd('/Users/yuanzh/Google Drive/Files_PNC_Analysis')

sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
roi_names <- read.table('Beta_50ROIs/roiname_list.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
roi.name = roi_names$V1
num_roi = length(roi.name)
agey = c(8:23)
ageyear = length(agey)

cons = c("fear","anger","sad","happy")

library(R.matlab)
library(reshape2)
library(ggplot2)

for(c in 1:length(cons)){
  f = paste("BS_Conn_50ROIs_2018/ZDegree/Degree",cons[c],"759s.mat",sep="_")
  cmat <- readMat(f)
  cmat <- cmat$Node.Deg.auc

  hubness_mn = rowMeans(cmat)
  names(hubness_mn) = roi.name
  ord <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
  hubness_mn = hubness_mn[ord]
  hubness_mn = round(hubness_mn^2)

  
  outputfile <- paste("BS_Conn_50ROIs_2018/ZDegree/hubness_",cons[c],".txt",sep="")
  write.table(hubness_mn, outputfile, sep="\t", row.names=FALSE, quote = FALSE) 
}




for(c in 1:length(cons)){
   f = paste("BS_Conn_50ROIs_2018/ZDegree/ZDegAUC",cons[c],"759s.mat",sep="_")
   cmat <- readMat(f)
   cmat <- cmat$degz.sorted
   age <- sort(sub$Age)

   pmat = matrix(rep(0,num_roi*ageyear), nrow=length(roi.name),ncol=ageyear)
   for(i in 8:23){
     ind = which(age >= i & age < i+1)

     mat = cmat[,ind]
     bmat = mat > 1
     if(length(ind) > 1){
       p = rowSums(bmat)/length(ind)
     }else{
       p = bmat/length(ind)
     }
     pmat[,i-7] = p
   }

   dimnames(pmat) = list(roi.name, agey)

   # ord <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
   # pmat2 = pmat[ord,-ageyear]
   # 
   # tf = melt(t(pmat2))
   # colnames(tf) = c('Age','ROIs','Probability')
   # roi_crucial = c('preSMA.L','preSMA.R',
   #                 'pgACC.L','pgACC.R','PCC.L','PCC.R',
   #                 'VMPFC.L','VMPFC.R','rmOFC.L','rmOFC.R','mOFC.L','mOFC.R',
   #                 'DMPFC.L','DMPFC.R')
   # tf2 = subset(tf, ROIs %in% roi_crucial)
   # ggplot(data=tf2, aes(x=Age, y=Probability, colour = ROIs)) +
   #        geom_line() +
   #        geom_point( size=2, shape=21, fill="white")
            
#   L = seq(1,22,2)
#   R = seq(2,22,2)
#   lfpn = colMeans(pmat2[L,])
#   rfpn = colMeans(pmat2[R,])
#
#   L = seq(23,36,2)
#   R = seq(24,36,2)
#   ldmn = colMeans(pmat2[L,])
#   rdmn = colMeans(pmat2[R,])
#
#   L = seq(37,50,2)
#   R = seq(38,50,2)
#   lspin = colMeans(pmat2[L,])
#   rspin = colMeans(pmat2[R,])
#
#   nets = rbind(lfpn,rfpn,ldmn,rdmn,lspin,rspin)
#   tf = melt(nets, id.vars="age", value.name="probability")
#   colnames(tf) = c('ROIs','Age','Probability')
#   ggplot(data=tf, aes(x=Age, y=Probability, colour = ROIs)) +
#     geom_line() +
#     geom_point( size=2, shape=21, fill="white") +
#     scale_y_continuous(breaks=seq(0, 0.5,0.1), limits=c(0, 0.5))
#   fname = paste("BS_Conn_50ROIs_2018/ZDegree/hubs_",cons[c],".eps",sep="")
#   ggsave(file=fname, width = 6.5, height = 6.5)
}


# for(c in 1:length(cons)){
#   f = paste("BS_Conn_50ROIs_2018/ZDegree/ZDegAUC",cons[c],"759s.mat",sep="_")
#   cmat <- readMat(f)
#   cmat <- cmat$degz.sorted
#   age <- sort(sub$Age)
#   dimnames(cmat) = list(roi.name, age)
#   ord <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
#   cmat2 = cmat[ord,]
#
#   dat = cbind(age,t(cmat2))
#
#   roi_crucial = c('preSMA.L','preSMA.R',
#                   'pgACC.L','pgACC.R','PCC.L','PCC.R',
#                   'VMPFC.L','VMPFC.R','rmOFC.L','rmOFC.R','mOFC.L','mOFC.R',
#                   'DMPFC.L','DMPFC.R')
#
#   dat2 = dat[,c(roi_crucial)]
#   plot(age,cmat2['DMPFC.L',])
#
#   tf = melt(dat2, id.vars="age")
#   colnames(tf) = c("Age","ROIs","Degree")
#   ggplot(data=tf[tf$ROIs=="DMPFC.L",], aes(x=Age, y=Degree, colour = ROIs)) +
#     geom_line() +
#     geom_point( size=2, shape=21, fill="white") #+
#     scale_y_continuous(breaks=seq(0, 0.5,0.1), limits=c(0, 0.5))
#   fname = paste("BS_Conn_50ROIs_2018/ZDegree/hubs_",cons[c],".eps",sep="")
#   ggsave(file=fname, width = 6.5, height = 6.5)
# }



# another way to show 'significant' individual hubs
library(gtools)
iteration = 1000

for(c in 1:length(cons)){
    print(cons[c])
    f = paste("BS_Conn_50ROIs_2018/ZDegree/ZDegAUC",cons[c],"759s.mat",sep="_")
    cmat <- readMat(f) 
    cmat <- cmat$degz.sorted
    age <- sort(sub$Age)
    
    bmat = (cmat > 1)*1 # a binary matrix indicating if a roi is a hub
    
    # probability based on observed data
    pmat_real = matrix(rep(0,num_roi*ageyear), nrow=length(roi.name),ncol=ageyear)
    for(i in 8:23){
        ind = which(age >= i & age < i+1)

        mat = bmat[,ind]
        if(length(ind) > 1){
          p = rowSums(mat)/length(ind)
        }else{
          p = mat/length(ind)
        }
        pmat_real[,i-7] = p
    }
    
    
    # probability based on permuted data
    iter1000 <- array(rep(0, num_roi*ageyear*iteration), c(num_roi,ageyear,iteration)); 
    for(iter in 1:iteration){
        perm_bmat = matrix(rep(0,dim(bmat)[1]*dim(bmat)[2]), nrow=dim(bmat)[1],ncol=dim(bmat)[2])
        for(i in 1:dim(bmat)[2]){ #permute if a roi is a hub within each individual
          perm_bmat[,i] = permute(bmat[,i])
        }
      
        # calculate probability
        pmat = matrix(rep(0,num_roi*ageyear), nrow=length(roi.name),ncol=ageyear)
        for(i in 8:23){
          print(i)
          ind = which(age >= i & age < i+1)
          ss = perm_bmat[,ind]
          
          if(length(ind) > 1){
            p = rowSums(ss)/length(ind)
          }else{
            p = ss/length(ind)
          }
          pmat[,i-7] = p
        }
        
        dimnames(pmat) = list(roi.name, agey)
        iter1000[,,iter] = pmat
    }
  
    # find 95th percential based on 1000 permutation
    mat95 = matrix(rep(0,num_roi*ageyear), nrow=length(roi.name),ncol=ageyear)
    for(i in 1:dim(iter1000)[1]){
      for(j in 1:dim(iter1000)[2]){
        mat95[i,j] = unname(quantile(iter1000[i,j,],0.95))
      }
    }
    
    # use the 95th percentile as a cutoff
    mask = 1*(pmat_real > mat95)
    res = mask*pmat_real
    dimnames(res) = list(roi.name, agey)
    
    # plot probability matrix of the observed data with 95th percentile as mask
    library(corrplot)
    col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
                               "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
    # plot: frontoparietal, limbic, subcortical networks
    net5 <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
    corrplot(res[net5,-ageyear], method="square", type="full",
             diag = TRUE, col=col0(50),
             tl.col="black", tl.srt=20, tl.cex=1, #tl.pos = "n",
             cl.cex = 1, is.corr=FALSE, cl.lim = c(0,1),
             cl.length=3, cl.pos = "b",
             outline = FALSE)
    
} #condition