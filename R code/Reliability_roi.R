setwd("/Users/yuanzh/Google Drive/Files_PNC_Analysis")

# library(car)
# # Randomly draw 50 subjects from 759 subjects and run regression model (e.g., 
# # activity = age + sex + race + motion) on 
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
# cons <- c("neutral","fear","anger","sad","happy")
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
# # load data
# roi_beta <- read.table('Beta_50ROIs/Original Data/roi_beta_average.tsv',header = TRUE, sep = '\t',
#                        colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
# roi_beta$X <- NULL
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
# #--Combine data--
# cdata <- cbind(sub, roi_beta[,-1])
# cdata$Sex <- as.factor(cdata$Sex)
# cdata$Race <- as.factor(cdata$Race)
# cdata$Agebin <- as.factor(cdata$Agebin) 
# cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
# cdata$GGroup <- as.factor(cdata$GGroup) 
# cdata$DGroup <- as.factor(cdata$DGroup) 
# 
# #-----Statistical Analysis---------
# num_roi <- length(roi_names$V1)
# roi.names <- roi_names$V1
# cons <- c("fear","anger","sad","happy") 
# 
# iter = 1000
# #sample_size = 20
# sample_size = c(20,30,40,50,60,70,80,90,100)
# for(s in 1:length(sample_size)){
#   print(sample_size[s])
#   for(c in 1:length(cons)){
#     print(c)
#     ss = cdata[,c(1:10, grep(cons[c],names(cdata)))]
#     
#     library(foreach)
#     library(doMC)
#     registerDoMC(2) # 2 cores
#     
#     age_list <- foreach(i=1:iter) %dopar% { # repeat the following analyses for iter times
#       print(i)
#       age <- data.frame(coef=numeric(num_roi), p=numeric(num_roi), 
#                         iter=numeric(num_roi), roi=character(num_roi),stringsAsFactors=FALSE)
#       
#       # draw sample_size subjects from 759 subjects
#       selected_subj <- draw_subs(data=ss,n=sample_size[s])
#       ss2 <- subset(ss, Subj %in% selected_subj)
#       # for each region, run the model and extract age coeff
#       # model: Activity = Age + Sex + Race + Motion
#       for(j in 1:num_roi){
#         if(cons[c]=='fear' | cons[c] == 'anger'){
#           f <- paste("scale(",roi.names[j],"_",cons[c],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#         }else if(cons[c]=='sad' | cons[c] == 'happy'){
#           f <- paste("scale(",roi.names[j],"_",cons[c],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#         }
#         #f <- paste("scale(",roi.names[j],"_",cons[c],") ~ ","scale(Age) + Sex + Race + scale(Motion)", sep="")
#         fit <- lm(as.formula(f), data = ss2)
#         #age.coef[i,roi.names[j]] <- fit$coefficients['scale(Age)']
#         age$coef[j] =  summary(fit)$coefficients['scale(Age)',1] 
#         age$p[j] = summary(fit)$coefficients['scale(Age)',4] 
#         age$iter[j] = i
#         age$roi[j] = roi.names[j]
#       }
#       return(age)
#     }
#     # save age coefficients
#     outputfile <- paste("Beta_50ROIs/Age_Coef_Distribution/stats_list_",cons[c],"_allrois_mainmodel_n",sample_size[s],".RData",sep="")
#     save(age_list, file=outputfile)
#     
#     rm(age_list)
#   }
# }


# load data, count #significance for each roi, calculate binomial p
roi_names <- read.table('Beta_50ROIs/roiname_list_old.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
num_roi <- length(roi_names$V1)
roi.names <- roi_names$V1

cons <- c("fear","anger","sad","happy") 
sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)

# load age_coef of the full sample
load("Beta_50ROIs/age_coef_full_sample.RData")
age_coef_fullsample = coef_mat_full
age_coef_fullsample_sig = coef_mat
cor_len = length(sample_size)*length(cons)
cor_stats = data.frame(cor_avg=numeric(cor_len ),cor_std=numeric(cor_len),
                       j_avg=numeric(cor_len ),j_std=numeric(cor_len),
                       cond=character(cor_len),stringsAsFactors=FALSE)

for(c in 1:length(cons)){
  freq_mat <- matrix(rep(0,num_roi*length(sample_size)), nrow=num_roi,ncol=length(sample_size)) 
  
  for(s in 1:length(sample_size)){
    f <- paste("Beta_50ROIs/Age_Coef_Distribution/stats_list_",cons[c],"_allrois_mainmodel_n",sample_size[s],".RData",sep="")
    load(f)
    
    iter = length(age_list)
    roi.names = age_list[[1]]$roi
    num_roi = length(roi.names)
    
    p_mat <- matrix(rep(0,num_roi*iter), nrow=num_roi,ncol=iter) # num_roi * num_iter
    age_coef <- matrix(rep(0,num_roi*iter), nrow=num_roi,ncol=iter) # num_roi * num_iter

    for(i in 1:iter){
      pp = p.adjust(age_list[[i]]$p, method = "fdr", length(age_list[[i]]$p))
      for(j in 1:num_roi){
        p_mat[j,i] = pp[j]
        age_coef[j,i] = age_list[[i]]$coef[j]
      } 
    }
    dimnames(p_mat) <- list(roi.names, c(1:iter))
    dimnames(age_coef) <- list(roi.names, c(1:iter))
    
    age_coef_sig = (p_mat<0.05)*age_coef 
    age_coef_bin = age_coef_sig !=0 
    age_coef_fullsample_bin = age_coef_fullsample_sig !=0
    # Correlation or Jaccard between age_effect from the subsample and that from the full sample
    library(ade4)
    cor_vec = c()
    j_vec = c()
    for(kk in 1:iter){
      cor_vec = c(cor_vec, cor(age_coef_fullsample[c,],age_coef[,kk]))
      dist = dist.binary(rbind(1*age_coef_fullsample_bin[c,],1*age_coef_bin[,kk]),method=1)
      j_vec = c(j_vec,1-dist^2)
    }
    sss = length(sample_size)*(c-1)+s
    cor_stats$cor_avg[sss] = mean(cor_vec)
    cor_stats$cor_std[sss] = sd(cor_vec)
    cor_stats$j_avg[sss] = mean(j_vec)
    cor_stats$j_std[sss] = sd(j_vec)
    cor_stats$cond[sss] = cons[c]
    cor_stats$sample_size[sss] = s
      
    full_mat = matrix(age_coef_fullsample[c,], nrow=dim(p_mat)[1], ncol=dim(p_mat)[2])
    sig_mat = (p_mat < 0.05) & (sign(full_mat)==sign(age_coef))
    count = rowSums(sig_mat)
    freq = count/iter #(count/iter)*100
    # bp = numeric(num_roi)
    # for(i in 1:num_roi){
    #   bp[i] = binom.test(count[i], n=iter, p = 0.5, alternative="greater")$p.value
    # }
    # plot(bp)

    freq_mat[,s] = freq
  }
  
  # # plot
  # library(corrplot)
  # col0 <- colorRampPalette(c("#053061","#2166AC", "#4393C3","#92C5DE","#D1E5F0","#FFFFFF",
  #                            "#FDDBC7","#F4A582","#D6604D","#B2182B","#67001F"))
  # dimnames(freq_mat) <- list(roi.names, sample_size)
  # #fname = paste("Beta_50ROIs/Age_Coef_Distribution/Stability_",cons[c],"_freq_mainmodel_allsz.eps",sep="")
  # #postscript(file = fname)
  # net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
  # corrplot(freq_mat[net,], method="square", type="full",
  #          diag = TRUE, col=col0(50),
  #          tl.col="black", tl.srt=20, tl.cex=1, #tl.pos='n',
  #          cl.cex = 1, is.corr=FALSE, cl.lim = c(0, ceiling(max(freq_mat))), cl.length=5, cl.pos = 'b',
  #          outline = FALSE)
  # #dev.off()
  
  # line plot for significant regions
  library(ggplot2)
  library(reshape2)
  dimnames(freq_mat) <- list(roi.names, sample_size)
  net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
  freq_mat = freq_mat[net,]

  roi = c('dACC.L','dACC.R','preSMA.L','preSMA.L','BLA.L','CMA.L','CMA.R')
  # if(cons[c]=='fear'){
  #   roi = c('dACC.L','dACC.R','preSMA.L','preSMA.L','BLA.L','CMA.L','CMA.R')
  # }else if(cons[c]=='anger'){
  #   roi = c('dACC.L','dACC.R','preSMA.L','preSMA.L','DLPFC.R','CMA.L')
  # }else if(cons[c]=='sad'){
  #   roi = c('LOFC.L','preSMA.L','VLPFC.L','CMA.L','CMA.R')
  # }else if(cons[c]=='happy'){
  #   roi = c('FFG.R','SPL.L','preSMA.L','preSMA.L','DLPFC.R','CMA.L')
  # }

  tf = melt(freq_mat[roi,])
  colnames(tf) = c('Region','Sample_Size','Probability')
  fname = paste("Beta_50ROIs/Age_Coef_Distribution/Reliability_lineplot_",cons[c],"_freq_mainmodel_allsz_onlyfearrois.eps",sep="")
  ggplot(data=tf, aes(x=Sample_Size, y=Probability, group = Region, colour = Region)) +
    geom_line() +
    geom_point( size=2, shape=21, fill="white")
  ggsave(file=fname)
}  

fname = "Beta_50ROIs/Age_Coef_Distribution/corr_jaccard.RData"
save(cor_stats, freq_mat, file=fname)

# plot overall reliability
cor_stats$cond = as.factor(cor_stats$cond)
cor_stats$cond = factor(cor_stats$cond,levels(cor_stats$cond)[c(2,1,4,3)])
ggplot(data=cor_stats, aes(x=sample_size, y=cor_avg, group = cond, colour = cond)) +
  #geom_errorbar(aes(ymin=cor_avg-cor_std, ymax=cor_avg+cor_std), width=.1, position=position_dodge(0)) +
  geom_line() +
  geom_point( size=2, shape=21, fill="white") +
  scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
  #geom_ribbon(aes(ymin=cor_avg-cor_std, ymax=cor_avg+cor_std, fill=cond), linetype=2, alpha=0.1)
fname = paste("Beta_50ROIs/Age_Coef_Distribution/Reliability_lineplot_cor.eps",sep="")
ggsave(file=fname)

ggplot(data=cor_stats, aes(x=sample_size, y=j_avg, group = cond, colour = cond)) +
  #geom_errorbar(aes(ymin=cor_avg-cor_std, ymax=cor_avg+cor_std), width=.1, position=position_dodge(0)) +
  geom_line() +
  geom_point( size=2, shape=21, fill="white") +
  scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
#geom_ribbon(aes(ymin=cor_avg-cor_std, ymax=cor_avg+cor_std, fill=cond), linetype=2, alpha=0.1)
fname = paste("Beta_50ROIs/Age_Coef_Distribution/Reliability_lineplot_jaccard.eps",sep="")
ggsave(file=fname)


# mat = as.data.frame(freq_mat)
# mat = t(mat)
# rownames(mat) = sample_size
# colnames(mat) = roi.names

# library(reshape2)
# library(ggplot2)
# tf = melt(mat, id.vars="sample_size", value.name="percent_freq", variable="roi")
# colnames(tf) = c('sample_size','ROIs','percent_freq')
# ggplot(data=tf, aes(x=sample_size, y=percent_freq, group = ROIs, colour = ROIs)) +
#   geom_line() +
#   geom_point( size=2, shape=21, fill="white")