# ROI beta is extracted from individual stats using the script under
# the path /oak/stanford/groups/menon/projects/yuanzh/2017_fconn_EI/scripts/taskfmri/fconn/ExtractROIBeta
# on Sherlock

setwd("/Users/zhangyuan/Google Drive/Files_PNC_Analysis")
# library(car)
# 
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
# 
# #------first calculate the observed value using the full sample-----
# res.age <- data.frame()
# res.group <- data.frame()
# for(c in 1:length(cons)){
#   ss = cdata[,c(1:10, grep(cons[c],names(cdata)))]
#   # model: Activation ~ Age + Group + Sex + Race + Motion (scale continuous variables)
#   for(i in 1:length(roi.names)){ 
#     if(cons[c]=='fear' | cons[c] == 'anger'){
#       f <- paste("scale(",roi.names[i],"_",cons[c],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#     } else if(cons[c]=='sad' | cons[c] == 'happy'){
#       f <- paste("scale(",roi.names[i],"_",cons[c],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#     }
#     fit <- lm(as.formula(f), data = cdata)
#     #print(summary(fit))
#     
#     stats <- Anova(fit,type="III")
#     tmp.age <- data.frame(stats["scale(Age)", 3], stats["scale(Age)", 2],stats["Residuals", 2], stats["scale(Age)", 4],
#                           summary(fit)$adj.r.squared, fit$coefficients['scale(Age)'],
#                           "Age",cons[c])
#     colnames(tmp.age) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","effect","condition")
#     res.age <- rbind(res.age, tmp.age)
#     
#     tmp.group <- data.frame(stats[3, 3], stats[3, 2],stats["Residuals", 2], stats[3, 4],
#                             summary(fit)$adj.r.squared,fit$coefficients[3],
#                             "Group",cons[c])
#     colnames(tmp.group) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","effect","condition")
#     res.group <- rbind(res.group, tmp.group)
#   } # ROI
# }
# res.age <- cbind(roi.names, res.age)
# res.group <- cbind(roi.names, res.group)
# res.all <- rbind(res.group, res.age)
# # save 
# outputfile <- 'roi_prev_beta_stats_25/roi_stats_allconds_maineffmodel.txt'
# write.table(res.all, outputfile, sep="\t", row.names=FALSE, quote = FALSE)
# 
# #-----Next calculate the observed value across a wide range of sample size----
# #-----for 1000 times, and then plot correlation/jaccard vs. sample size---------
# 
# # Randomly draw 50 subjects from 759 subjects and run regression model (e.g., 
# # activity = age + sex + race + motion) on 
# # the 50 subjects and then extracted the beta value of age term 
# # (positive value indicates age-related increase in the 
# # connectivity whereas negative value indicates age-related 
# # decrease in the connectivity)
# # Repeat above step for e.g. 1000 times and 
# # we will get a distribution of beta (age term) 
# # which inform us about how likely there is a positive/negative 
# # value of the effect
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
#   #print(c(nums[1],nums[2],nums[3]))
#   set = c(ch[int_ch],ado[int_ado],adu[int_adu])
#   return(set)
# }
# 
# iter = 1000
# sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
# for(sz in 1:length(sample_size)){
#   print(sample_size[sz])
#   
#   for(c in 1:length(cons)){
#     print(c)
#     ss = cdata[,c(1:10, grep(cons[c],names(cdata)))]
#     
#     library(foreach)
#     library(doMC)
#     registerDoMC(4) # 4 cores
# 
#     age_list <- foreach(i=1:iter) %dopar% { # repeat the following analyses for iter times
#       print(i)
#       age <- data.frame(coef=numeric(num_roi), p=numeric(num_roi), 
#                         iter=numeric(num_roi), roi=character(num_roi),stringsAsFactors=FALSE)
#       # draw sample_size subjects from 759 subjects
#       selected_subj <- draw_subs(data=ss,n=sample_size[sz])
#       ss2 <- subset(ss, Subj %in% selected_subj)
#       # for each region, run the model and extract age coeff
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
#     outputfile <- paste("roi_prev_beta_stats_25/Age_Coef_Distribution/stats_list_",cons[c],"_allrois_mainmodel_n",sample_size[sz],".RData",sep="")
#     save(age_list, file=outputfile)
#     
#     rm(age_list)
#   }
# }


# load data, count #significance for each roi, calculate binomial p
roi_names <- read.table('roi_prev_beta_stats_25/roiname_list.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
include = c('Gee_R_AMY','L_AMY_AAL','R_AMY_AAL','Wolf_L_AMY_Talairach','Wolf_R_AMY_Talairach',
             'Kujawa_L_AMY_TD','Kujawa_R_AMY_TD','Kujawa_L_AMY_AD','Kujawa_R_AMY_AD')
num_roi <- length(include)
roi.names <- include

cons <- c("fear","anger","sad","happy") 
iter=1000
sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)

# load age_coef of the full sample
fullsample = read.table('roi_prev_beta_stats_25/roi_stats_allconds_maineffmodel.txt',header = TRUE, sep = '\t',stringsAsFactors=FALSE)


for(c in 1:length(cons)){
  freq_mat <- matrix(rep(0,num_roi*length(sample_size)), nrow=num_roi,ncol=length(sample_size)) 
  age_coef_fullsample = subset(fullsample,condition==cons[c] & effect=="Age")
  age_coef_fullsample = age_coef_fullsample$coef  
  
  for(sz in 1:length(sample_size)){
    f <- paste("roi_prev_beta_stats_25/Age_Coef_Distribution/stats_list_",cons[c],"_allrois_mainmodel_n",sample_size[sz],".RData",sep="")
    load(f)
    
    iter = length(age_list)
    roi.names = age_list[[1]]$roi
    num_roi = length(roi.names)
    
    p_mat <- matrix(rep(0,num_roi*iter), nrow=num_roi,ncol=iter) # num_roi * num_iter
    age_coef <- matrix(rep(0,num_roi*iter), nrow=num_roi,ncol=iter) # num_roi * num_iter
    
    for(i in 1:iter){
      for(j in 1:num_roi){
        p_mat[j,i] = age_list[[i]]$p[j]
        age_coef[j,i] = age_list[[i]]$coef[j]
      } 
    }
    dimnames(p_mat) <- list(roi.names, c(1:iter))
    dimnames(age_coef) <- list(roi.names, c(1:iter))
    
    full_mat = matrix(age_coef_fullsample, nrow=dim(p_mat)[1], ncol=dim(p_mat)[2])
    sig_mat = (p_mat < 0.05) & (sign(full_mat)==sign(age_coef))
    count = rowSums(sig_mat)
    freq = count/iter
    freq_mat[,sz] = freq
  } # sample size
  
  # line plot
  library(ggplot2)
  library(reshape2)
  dimnames(freq_mat) <- list(roi.names, sample_size)
  tf = melt(freq_mat)
  colnames(tf) = c('Region','Sample_Size','Probability')
  fname = paste("roi_prev_beta_stats_25/Age_Coef_Distribution/Reliability_lineplot_",cons[c],"_freq_mainmodel_allsz.eps",sep="")
  ggplot(data=tf, aes(x=Sample_Size, y=Probability, colour = Region)) +
    geom_line() +
    geom_point( size=2, shape=21, fill="white") + 
    scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
  ggsave(file=fname, width=8, height=6)
  
  # plot just "facial emotion" studies: Gee, Kujawa, Wu
  tf = melt(freq_mat[-c(4,5),])
  colnames(tf) = c('Region','Sample_Size','Probability')
  fname = paste("roi_prev_beta_stats_25/Age_Coef_Distribution/Reliability_lineplot_",cons[c],"_freq_mainmodel_allsz_face.eps",sep="")
  ggplot(data=tf, aes(x=Sample_Size, y=Probability, colour = Region)) +
    geom_line() +
    geom_point( size=2, shape=21, fill="white") + 
    scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
  ggsave(file=fname,width=8, height=6)

} # condition
