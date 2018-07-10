setwd("/Users/zhangyuan/Google Drive/Files_PNC_Analysis")
rm(list=ls())

# library(car)
# library(gtools)
# # Randomly draw 50 subjects from 759 subjects and run regression model (e.g.,
# # vmpfc-amygdala connectivity = age + sex + race? + motion) on
# # the 50 subjects and then extracted the beta value of age term
# # (positive value indicates age-related increase in the
# # connectivity whereas negative value indicates age-related
# # decrease in the connectivity)
# # Repeat above step for e.g. 5000 times and
# # we will get a distribution of beta (age term)
# # which inform us about how likely there is a positive/negative
# # value of the effect
# 
# #-----read in subject info----------
# sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
#                   colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
# cons <- c("fear","anger","sad","happy")
# iter = 1000
# sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
# 
# #--supporting function: draw subjects-----
# # data: data that contains age and other information
# # n: how many subjects to draw
# # ratio: how many percent in the draw comes from children,
# # how many from adolescents, and how many from adults
# draw_subs <- function(data=cdata,n=50){
#   tmp = table(cdata$Agebin)
#   ratio = tmp/sum(tmp)
#   #nums = round(n*ratio) # #CH, #ADO, and #ADU to draw
#   nums = trunc(round(n*ratio,1) + 0.5) #CH, #ADO, and #ADU to draw
#   ch = cdata[cdata$Agebin=="Children",]$Subj
#   ado = cdata[cdata$Agebin=="Adolescents",]$Subj
#   adu = cdata[cdata$Agebin=="Adults",]$Subj
# 
#   int_ch = sort(sample(1:length(ch),nums[1]))
#   int_ado = sort(sample(1:length(ado),nums[2]))
#   int_adu = sort(sample(1:length(adu),nums[3]))
# 
#   set = c(ch[int_ch],ado[int_ado],adu[int_adu])
#   return(set)
# }
# 
# for(c in 1:length(cons)){ # for each condition
#     print(c)
#     #--read in connectivity--
#     fname <- paste("BS_Conn_50ROIs_2018/Original Data/bs_conns_",cons[c],".txt",sep="")
#     conns <- read.table(fname,header = TRUE, sep = '\t',
#                         colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
#     conns$X <- NULL
#     
#     #--Combine data--
#     cdata <- cbind(sub, conns[,-1])
#     cdata$Sex <- as.factor(cdata$Sex)
#     cdata$Race <- as.factor(cdata$Race)
#     cdata$Agebin <- as.factor(cdata$Agebin)
#     cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
#     cdata$GGroup <- as.factor(cdata$GGroup)
#     cdata$DGroup <- as.factor(cdata$DGroup)
#     
#     # statistical analyses
#     num_conn <- ncol(conns)-1
#     conn.names <- names(conns)[-1]
#     
#     library(foreach)
#     library(doMC)
#     registerDoMC(2) # cores
#     
#     for(s in 1:length(sample_size)){ 
#       print(sample_size[s])
#       
#       start_time <- Sys.time()
#       age_list <- foreach(i=1:iter) %dopar% {
#           print(i)
#           age <- data.frame(coef=numeric(num_conn), p=numeric(num_conn),
#                             iter=numeric(num_conn), conn=character(num_conn),stringsAsFactors=FALSE)
#           
#           # draw sample_size subjects from 759 subjects
#           selected_subj <- draw_subs(data=cdata,n=sample_size[s])
#           #print(length(selected_subj))
#           ss <- subset(cdata, Subj %in% selected_subj)
#           ss$Age = permute(ss$Age)
#           
#           # for each link, run the model and extract age coeff
#           for(j in 1:num_conn){
#             #print(j)
#             if(cons[c]=='fear' | cons[c] == 'anger'){
#               f <- paste("scale(",conn.names[j],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#               
#             }else if(cons[c]=='sad' | cons[c] == 'happy'){
#               f <- paste("scale(",conn.names[j],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#             }
#             fit <- lm(as.formula(f), data = ss)
#             age$coef[j] =  summary(fit)$coefficients['scale(Age)',1]
#             age$p[j] = summary(fit)$coefficients['scale(Age)',4]
#             age$iter[j] = i
#             age$conn[j] = conn.names[j]
#           }
#           return(age)
#       } # iteration
#       end_time <- Sys.time()
#       end_time - start_time
#       
#       # save age coefficients
#       outputfile <- paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/permute_stats_list_",cons[c],"_allconns_mainmodel_n",sample_size[s],".RData",sep="")
#       save(age_list, file=outputfile)
#       rm(age_list)
#     } # sample size
# } # condition



# load data, count #significance for each link
#rm(list=ls())
library(ade4)
cons <- c("fear","anger","sad","happy") 
sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
cor_len = length(sample_size)*length(cons)
cor_stats = data.frame(j_95=numeric(cor_len),cor_95=numeric(cor_len),
                       cond=character(cor_len),stringsAsFactors=FALSE)

for(c in 1:length(cons)){
  print(c)
  f = paste("BS_Conn_50ROIs_2018/Model1/age_coef_",cons[c],"_model_mainEff.RData",sep="")
  load(f)
  fullsample_all = coef_mat_all
  fullsample_all_vec = fullsample_all[lower.tri(fullsample_all, diag = FALSE)]
  
  fullsample_sig = coef_mat
  fullsample_bin = fullsample_sig !=0
  fullsample_bin_vec = fullsample_bin[lower.tri(fullsample_bin, diag = FALSE)]
  
  for(sz in 1:length(sample_size)){
    print(sample_size[sz])
    f <- paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/permute_stats_list_",cons[c],"_allconns_mainmodel_n",sample_size[sz],".RData",sep="")
    load(f)
    
    iter = length(age_list)
    conn.names = age_list[[1]]$conn
    num_conn = length(conn.names)
    
    p_mat <- matrix(rep(0,num_conn*iter), nrow=num_conn,ncol=iter) # num_conn * num_iter
    age_coef <- matrix(rep(0,num_conn*iter), nrow=num_conn,ncol=iter) # num_conn * num_iter
    
    for(i in 1:iter){
      pp = p.adjust(age_list[[i]]$p, method = "fdr", n=length(age_list[[i]]$p))
      for(j in 1:num_conn){
        p_mat[j,i] = pp[j]
        age_coef[j,i] = age_list[[i]]$coef[j]
      } 
    }
    dimnames(p_mat) <- list(conn.names, c(1:iter))
    dimnames(age_coef) <- list(conn.names, c(1:iter))
    sig_mat = (p_mat<0.05)*age_coef 
    sig_mat_bin = sig_mat != 0 
    
    j_vec = c()
    cor_vec = c()
    for(kk in 1:iter){
      dist = dist.binary(rbind(1*fullsample_bin_vec,1*sig_mat_bin[,kk]),method=1)
      jac = 1-dist^2
      j_vec = c(j_vec,jac)
      
      cor_vec = c(cor_vec, cor(fullsample_all_vec,age_coef[,kk]))
    }
    
    sss = length(sample_size)*(c-1)+sz

    cor_stats$j_95[sss] = unname(quantile(j_vec,0.95))
    cor_stats$cor_95[sss] = unname(quantile(cor_vec,0.95))
    cor_stats$cond[sss] = cons[c]
    cor_stats$sample_size[sss] = sz
    
    
    #count = rowSums(sig_mat_bin)
    #freq = count/iter
    
    
    # # plot
    # library(corrplot)
    # col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
    #                            "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
    # roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
    # num_rois <- length(roi_names$V1)
    # # split each connectivity into two nodes name
    # ss <- sapply(conn.names, strsplit, split="_")
    # freq_mat <- matrix(rep(0,num_rois), nrow=num_rois,ncol=num_rois) # for matrix plot purpose
    # 
    # for(i in 1:num_conn){
    #   ind <- which(roi_names$V1 %in% ss[[i]])
    #   freq_mat[ind[1],ind[2]] <- freq[i]
    #   freq_mat[ind[2],ind[1]] <- freq[i]
    # }
    # dimnames(freq_mat) <- list(roi_names$V1, roi_names$V1)
    # 
    # # plot matrix: frontoparietal, limbic, subcortical networks
    # fname = paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/Stability_",cons[c],"_freq_mainmodel_n",sample_size[sz],".eps",sep="")
    # postscript(file = fname)
    # net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
    # corrplot(freq_mat[net,net], method="square", type="lower", 
    #          diag = TRUE, col=col0(50), 
    #          tl.col="black", tl.srt=20, tl.cex=1, tl.pos='n',
    #          cl.cex = 1, is.corr=FALSE, cl.lim = c(0, 100), cl.length=5, 
    #          outline = FALSE) 
    # dev.off()
  }
}

f = "BS_Conn_50ROIs_2018/Age_Coef_Distribution/reliatbility_corr_jaccard_overall_permute1000.RData"
save(cor_stats,file=f)


#---------------plot---------------
# load corr & j from real data (i.e., not permuted)
f = "BS_Conn_50ROIs_2018/Age_Coef_Distribution/reliatbility_corr_jaccard_overall.RData"
load(f)
real_dat = cor_stats
real_dat = real_dat[,c(1,3,5,6)]
real_dat$type = 'real'
colnames(real_dat) = c('jaccard','cor','cond','sample_size','type')
rm(cor_stats)

# load corr & j from permuted data
f = "BS_Conn_50ROIs_2018/Age_Coef_Distribution/reliatbility_corr_jaccard_overall_permute1000.RData"
load(f)
perm_dat = cor_stats
perm_dat$type = 'permute'
colnames(perm_dat) = c('jaccard','cor','cond','sample_size','type')
rm(cor_stats)

# combine data
cor_stats = rbind(real_dat,perm_dat)

# plot
cor_stats$cond = as.factor(cor_stats$cond)
cor_stats$cond = factor(cor_stats$cond,levels(cor_stats$cond)[c(2,1,4,3)])
cor_stats$type = as.factor(cor_stats$type)
cor_stats$type = factor(cor_stats$type,levels(cor_stats$type)[c(2,1)])

ggplot(data=cor_stats, aes(x=sample_size, y=cor, colour = cond, linetype = type)) +
  geom_line() +
  geom_point(size=2, shape=21, fill="white") +
  scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
fname = "BS_Conn_50ROIs_2018/Age_Coef_Distribution/Reliability_conn_lineplot_cor.eps"
ggsave(file=fname)

ggplot(data=cor_stats, aes(x=sample_size, y=jaccard, colour = cond, linetype = type)) +
  geom_line() +
  geom_point(size=2, shape=21, fill="white") +
  scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
fname = "BS_Conn_50ROIs_2018/Age_Coef_Distribution/Reliability_conn_lineplot_jaccard.eps"
ggsave(file=fname)
#ggsave(file=fname,width = 6.06, height = 6.06,units="in")




library(ggplot2)
# plot overall reliability
ggplot(data=cor_stats, aes(x=sample_size, y=j_95, colour = cond)) +
  geom_line() +
  geom_point( size=2, shape=21, fill="white") 
#fname = paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/Reliability_lineplot_jaccard.eps",sep="")
#ggsave(file=fname)

ggplot(data=cor_stats, aes(x=sample_size, y=cor_95, colour = cond)) +
  geom_line() +
  geom_point( size=2, shape=21, fill="white") 
#fname = paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/Reliability_lineplot_cor.eps",sep="")
#ggsave(file=fname)


#------Previously used (not used now)----------- 
# # for each link, plot frequency of significance vs. sample size
# rm(list=ls())
# sample_size = c(20,30,40,50,60,70,80,90,100)
# cons <- c("fear","anger","sad","happy") 
# 
# c = 4
# fout = paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/stability_",cons[c],"_allsamplesize_mainmodel.txt",sep="")
# res = data.frame(n20 = numeric(1225), n30 = numeric(1225), n40 = numeric(1225),
#                  n50 = numeric(1225), n60 = numeric(1225), n70 = numeric(1225),
#                  n80 = numeric(1225), n90 = numeric(1225), n100 = numeric(1225),
#                  conn.name=character(1225), stringsAsFactors = FALSE)
# fout2 = paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/stability_",cons[c],"_allsamplesize_mainmodel_forplot.txt",sep="")
# forplot = as.data.frame(matrix(0, ncol=1225, nrow=length(sample_size)))
# 
# for(s in 1:length(sample_size)){
#   f <- paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/stats_list_",cons[c],"_allconns_mainmodel_n",sample_size[s],".RData",sep="")
#   load(f)
#   
#   iter = length(age_list)
#   conn.names = age_list[[1]]$conn
#   num_conn = length(conn.names)
#   
#   p_mat <- matrix(rep(0,num_conn*iter), nrow=num_conn,ncol=iter) # num_conn * num_iter
#   for(i in 1:iter){
#     for(j in 1:num_conn){
#       p_mat[j,i] = age_list[[i]]$p[j]
#     } 
#   }
#   dimnames(p_mat) <- list(conn.names, c(1:iter))
#   sig_mat = p_mat <= 0.05
#   count = rowSums(sig_mat)
#   freq = (count/iter)*100
#   
#   res[,s] = freq
#   forplot[s,] = freq
# }
# 
# res$conn.name = conn.names
# res = res[,c(10,1:9)]
# res$mask = 0
# fmask = paste("BS_Conn_50ROIs_2018/Model1/bs_stats_",cons[c],"_model_mainEff.txt",sep="")
# mask = read.table(fmask,header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# for(i in 1:length(mask$conn.names)){
#   ind = grep(paste("^",mask$conn.names[i], sep=""),res$conn.name)
#   res$mask[ind] = 1
# }
# 
# dimnames(forplot) = list(sample_size,conn.names)
# forplot$samplesize = sample_size
# mdf = melt(forplot, id.vars="samplesize", value.name="freq", variable="conn.name")
# mdf$mask=0
# for(i in 1:length(mask$conn.names)){
#   ind = grep(paste("^",mask$conn.names[i], sep=""),mdf$conn.name)
#   mdf$mask[ind] = 1
# }
# 
# write.table(res, file=fout, quote=FALSE, row.names = FALSE, sep="\t")
# write.table(mdf, file=fout2, quote=FALSE, row.names = FALSE, sep="\t")
# 
# # library(reshape2)
# # library(ggplot2)
# # tf = mdf[mdf$mask==1,]
# # ggplot(data=tf, aes(x=samplesize, y=freq, group = conn.name, colour = conn.name)) +
# #   geom_line() +
# #   geom_point( size=2, shape=21, fill="white")
# 
# library(corrplot)
# col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
#                            "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
# f = paste("BS_Conn_50ROIs_2018/Age_Coef_Distribution/stability_",cons[c],"_allsamplesize_mainmodel.txt",sep="")
# df = read.table(f,header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# #net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
# mat = as.matrix(df[df$mask==1,c(2:10)])
# rownames(mat) = df$conn.name[df$mask==1]
# 
# corrplot(mat[51:100,], method="square", type="full", 
#          diag = TRUE, col=col0(50), 
#          tl.col="black", tl.srt=20, tl.cex=1, #tl.pos='n',
#          cl.cex = 1, is.corr=FALSE, cl.lim = c(0, 62), cl.length=5, cl.pos = 'n',
#          outline = FALSE) 
