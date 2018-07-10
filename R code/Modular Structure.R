setwd('/Users/zhangyuan/Google Drive/Files_PNC_Analysis')

sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
library(corrplot)
library(R.matlab)
library(ade4)

cons = c('fear','anger','sad','happy')

age <- data.frame(coef=numeric(length(cons)), p=numeric(length(cons)), 
                  cond=character(length(cons)),stringsAsFactors=FALSE)
# for(c in 1:length(cons)){
#   f = paste('BS_Conn_50ROIs_2018/Modularity/Q_',cons[c],'_759s.mat',sep="")
#   cmat <- readMat(f)
#   Q <- cmat$all.Qs
#   modules <- cmat$all.mod
#   
#   cdata <- cbind(sub, Q)
#   cdata$Sex <- as.factor(cdata$Sex)
#   cdata$Race <- as.factor(cdata$Race)
#   cdata$Agebin <- as.factor(cdata$Agebin) 
#   cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
#   cdata$GGroup <- as.factor(cdata$GGroup) 
#   cdata$DGroup <- as.factor(cdata$DGroup) 
# 
#   if(cons[c]=='fear' | cons[c] == 'anger'){
#     f <- paste("scale(Q) ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#     
#   }else if(cons[c]=='sad' | cons[c] == 'happy'){
#     f <- paste("scale(Q) ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#   }
#   fit <- lm(as.formula(f), data = cdata)
#   
#   age$coef[c] =  summary(fit)$coefficients['scale(Age)',1] 
#   age$p[c] = summary(fit)$coefficients['scale(Age)',4] 
#   age$cond[c] = cons[c]
# }
# 
# save(age,file='BS_Conn_50ROIs_2018/Modularity/Q_vs_Age.RData')


# co-occurence matrix
template_mat = matrix(0,nrow=dim(modules)[2],ncol=dim(modules)[2])
template_mat = matrix(0,nrow=dim(modules)[2],ncol=dim(modules)[2])
template_mat[c(5:6,17:18,23:24,27:34,41:42,45:50),c(5:6,17:18,23:24,27:34,41:42,45:50)] = 1
template_mat[c(19:22,25:26,35:40,43:44),c(19:22,25:26,35:40,43:44)] = 1
template_mat[c(1:4,7:16),c(1:4,7:16)] = 1
# roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
# dimnames(template_mat) =  list(roi_names$V1,roi_names$V1)
# net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
# corrplot(template_mat[net,net], method="square", type="full", 
#          tl.col="black", tl.srt=20, tl.cex=1,outline = FALSE) 


dist = data.frame(fear_dice=numeric(dim(modules)[1]),anger_dice=numeric(dim(modules)[1]),
                  sad_dice=numeric(dim(modules)[1]),happy_dice=numeric(dim(modules)[1]))

for(c in 1:length(cons)){
  f = paste('BS_Conn_50ROIs_2018/Modularity/Q_',cons[c],'_759s.mat',sep="")
  cmat <- readMat(f)
  modules <- cmat$all.mod
  
  for(s in 1:dim(modules)[1]){
    v = modules[s,]
    co = matrix(0,nrow=dim(modules)[2],ncol=dim(modules)[2])
    for(i in 1:dim(modules)[2]){
      m = v[i]
      ind = which(v==m)
      co[i,ind] = 1
    } 
    
    A = template_mat[lower.tri(template_mat,diag=FALSE)]
    B = co[lower.tri(co,diag=FALSE)]
    dist[s,c] = as.numeric(dist.binary(rbind(A,B),method=5))
    
    # # plot 
    # col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
    #                            "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
    # roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
    # num_rois <- length(roi_names$V1)
    # dimnames(co) =  list(roi_names$V1,roi_names$V1)
    # net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
    # corrplot(co[net,net], method="square", type="full", 
    #          diag = TRUE, col=col0(50), 
    #          tl.col="black", tl.srt=20, tl.cex=1, #tl.pos='n',
    #          cl.cex = 1, is.corr=FALSE, cl.lim = c(0, 1), cl.length=5, 
    #          outline = FALSE) 
    
    # order_v = order(v)
    # corrplot(co[order_v,order_v], method="square", type="full",
    #          diag = TRUE, col=col0(50),
    #          tl.col="black", tl.srt=20, tl.cex=1, #tl.pos='n',
    #          cl.cex = 1, is.corr=FALSE, cl.lim = c(0, 1), cl.length=5,
    #          outline = FALSE)
  }
}

# check distance vs. age
age <- data.frame(coef=numeric(length(cons)), p=numeric(length(cons)), 
                  cond=character(length(cons)),stringsAsFactors=FALSE)
for(c in 1:length(cons)){
  cdata <- cbind(sub, dist)
  cdata$Sex <- as.factor(cdata$Sex)
  cdata$Race <- as.factor(cdata$Race)
  cdata$Agebin <- as.factor(cdata$Agebin)
  cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
  cdata$GGroup <- as.factor(cdata$GGroup)
  cdata$DGroup <- as.factor(cdata$DGroup)

  if(cons[c]=='fear' | cons[c] == 'anger'){
    f <- paste("scale(",colnames(cdata)[10+c],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")

  }else if(cons[c]=='sad' | cons[c] == 'happy'){
    f <- paste("scale(",colnames(cdata)[10+c],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
  }
  fit <- lm(as.formula(f), data = cdata)

  age$coef[c] =  summary(fit)$coefficients['scale(Age)',1]
  age$p[c] = summary(fit)$coefficients['scale(Age)',4]
  age$cond[c] = cons[c]
}
save(age,file='BS_Conn_50ROIs_2018/Modularity/Dissimilarity_vs_Age.RData')
  
# plot template co-occurence matrix
template_mat = matrix(0,nrow=50,ncol=50)
template_mat[c(5:6,17:18,23:24,27:34,41:42,45:50),c(5:6,17:18,23:24,27:34,41:42,45:50)] = 1
template_mat[c(19:22,25:26,35:40,43:44),c(19:22,25:26,35:40,43:44)] = 2
template_mat[c(1:4,7:16),c(1:4,7:16)] = 3

net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
corrplot(template_mat[net,net], method="square", type="full", 
         is.corr = FALSE,
          tl.col="black", tl.srt=20, tl.cex=1,outline = FALSE) 

# plot CH fear and anger
roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
ch_mat = matrix(0,nrow=50,ncol=50)
ch_mat[c(5:6,17:18,23:24,27:34,41:42,45:50),c(5:6,17:18,23:24,27:34,41:42,45:50)] = 1
ch_mat[c(13,14,19:22,25:26,35:40,43:44),c(13,14,19:22,25:26,35:40,43:44)] = 2
ch_mat[c(1:4,7:12,15,16),c(1:4,7:12,15,16)] = 3

net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
corrplot(ch_mat[net,net], method="square", type="full", 
         is.corr = FALSE,
         tl.col="black", tl.srt=20, tl.cex=1,outline = FALSE) 

# plot CH sad and happy
roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
ch_mat = matrix(0,nrow=50,ncol=50)
ch_mat[c(5:6,17:18,23:24,27:33,41:42,45:50),c(5:6,17:18,23:24,27:33,41:42,45:50)] = 1
ch_mat[c(13,14,19:22,25:26,34:40,43:44),c(13,14,19:22,25:26,34:40,43:44)] = 2
ch_mat[c(1:4,7:12,15,16),c(1:4,7:12,15,16)] = 3

net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
corrplot(ch_mat[net,net], method="square", type="full", 
         is.corr = FALSE,
         tl.col="black", tl.srt=20, tl.cex=1,outline = FALSE) 

# plot ADU sad
roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
adu_mat = matrix(0,nrow=50,ncol=50)
adu_mat[c(5:6,11,12,17:18,23:24,27:34,41:42,45:50),c(5:6,11,12,17:18,23:24,27:34,41:42,45:50)] = 1
adu_mat[c(13,14,19:22,25:26,35:40,43:44),c(13,14,19:22,25:26,35:40,43:44)] = 2
adu_mat[c(1:4,7:10,15,16),c(1:4,7:10,15,16)] = 3

net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
corrplot(adu_mat[net,net], method="square", type="full", 
         is.corr = FALSE,
         tl.col="black", tl.srt=20, tl.cex=1,outline = FALSE) 
