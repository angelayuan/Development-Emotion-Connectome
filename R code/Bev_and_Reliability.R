setwd("/Users/yuanzh/Google Drive/Files_PNC_Analysis")
library(car)
#-----read in subject info----------
sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)

#--read in performance data----
perf <- read.table('perfsummary.txt',header = TRUE, sep = '\t',
                   colClasses=c("SubID"="character"),stringsAsFactors=FALSE)
perf <- subset(perf, SubID %in% sub$Subj)

perf <- perf[,c("pctcorrfear", "pctcorrangry", "pctcorrsad", "pctcorrhappy",
                "rtmdfearcorr", "rtmdangrycorr", "rtmdsadcorr", "rtmdhappycorr")]

# #-----supporting function----
# findsig <- function(data,threshold=0.05,effname){
#   set <- subset(data, fdrp<threshold)
#   if(nrow(set)>0){
#     set$eff <- effname
#   } else{
#     set <- set[0, ]
#     set$eff <- numeric(nrow(set))
#   }
#   return(set)
# }

#--Combine data--
cdata <- cbind(sub, perf)
cdata$Sex <- as.factor(cdata$Sex)
cdata$Race <- as.factor(cdata$Race)
cdata$Agebin <- as.factor(cdata$Agebin) 
cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
cdata$GGroup <- as.factor(cdata$GGroup) 
cdata$DGroup <- as.factor(cdata$DGroup) 

# cons <- c("fear","anger","sad","happy") 
# res.group <- data.frame()
# res.age <- data.frame()
# for(t in 1:2){
#   if (t==1) {
#     offset = 0 # for accuracy, = 4 for RT
#     type = "accuracy"
#   }else{
#     offset = 4
#     type = "mdRT"
#   }
# 
#   for(c in 1:length(cons)){ # for each condition
#     
#     # model: accuracy ~ Age + Group + Sex + Race + Motion (scale continuous variables)
#     f <- paste("scale(",colnames(cdata)[10 + offset + c],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#     fit <- lm(as.formula(f), data = cdata)
#   
#     
#     stats <- Anova(fit,type="III")
#     tmp.group <- data.frame(stats[3, 3], stats[3, 2],stats["Residuals", 2], stats[3, 4],
#                             summary(fit)$adj.r.squared,fit$coefficients[3],'GGroup',colnames(cdata)[10 + offset + c],'Group')
#     colnames(tmp.group) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","whoisgroup","type","Effect")
#     res.group <- rbind(res.group, tmp.group)
#     
#     tmp.age <- data.frame(stats["scale(Age)", 3], stats["scale(Age)", 2],stats["Residuals", 2], stats["scale(Age)", 4],
#                           summary(fit)$adj.r.squared, fit$coefficients['scale(Age)'],'GGroup',colnames(cdata)[10 + offset + c],'Age')
#     colnames(tmp.age) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","whoisgroup","type","Effect")
#     res.age <- rbind(res.age, tmp.age)
#     
#     # DGroup
#     f <- paste("scale(",colnames(cdata)[10 + offset + c],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#     fit <- lm(as.formula(f), data = cdata)
#   
#     stats <- Anova(fit,type="III")
#     tmp.group <- data.frame(stats[3, 3], stats[3, 2],stats["Residuals", 2], stats[3, 4],
#                             summary(fit)$adj.r.squared,fit$coefficients[3],'DGroup',colnames(cdata)[10 + offset + c],'Group')
#     colnames(tmp.group) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","whoisgroup","type","Effect")
#     res.group <- rbind(res.group, tmp.group)
#     
#     tmp.age <- data.frame(stats["scale(Age)", 3], stats["scale(Age)", 2],stats["Residuals", 2], stats["scale(Age)", 4],
#                           summary(fit)$adj.r.squared, fit$coefficients['scale(Age)'],'DGroup',colnames(cdata)[10 + offset + c],'Age')
#     colnames(tmp.age) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","whoisgroup","type","Effect")
#     res.age <- rbind(res.age, tmp.age)
#   }
# }
# res.all <- rbind(res.group, res.age)
# outputfile <- paste("Behavior_Stability/beh_stats_model_mainEff.txt",sep="")
# write.table(res.all, outputfile, sep="\t", row.names=FALSE, quote = FALSE)


# # calculate BF for group effect
# library(BayesFactor)
# cons <- c("fear","anger","sad","happy") 
# BF_mat <- matrix(rep(0,4*4), nrow=4,ncol=4) # condition*(acc GGroup + acc DGroup + rt GGroup + rt DGroup)
# 
# for(t in 1:2){
#   if (t==1) {
#     offset = 0 # for accuracy, = 4 for RT
#     type = "accuracy"
#   }else{
#     offset = 4
#     type = "mdRT"
#   }
#   
#   for(c in 1:length(cons)){ # for each condition
#     
#     # model: accuracy ~ Age + Group + Sex + Race + Motion (scale continuous variables)
#     f1 <- paste(colnames(cdata)[10 + offset + c]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
#     f0 <- paste(colnames(cdata)[10 + offset + c]," ~ ","Age + Sex + Race + Motion", sep="")
#     bf_f1 = lmBF(as.formula(f1), data = cdata)
#     bf_f0 = lmBF(as.formula(f0), data = cdata)
#     BF10 = bf_f1/bf_f0
#     BF_mat[c,2*t-1] <- exp(BF10@bayesFactor$bf)
#    
#     
#     # DGroup
#     f1 <- paste(colnames(cdata)[10 + offset + c]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
#     f0 <- paste(colnames(cdata)[10 + offset + c]," ~ ","Age + Sex + Race + Motion", sep="")
#     bf_f1 = lmBF(as.formula(f1), data = cdata)
#     bf_f0 = lmBF(as.formula(f0), data = cdata)
#     BF10 = bf_f1/bf_f0
#     BF_mat[c,2*t] <- exp(BF10@bayesFactor$bf)
#   }
# }
# dimnames(BF_mat) <- list(cons, c('acc_ggroup','acc_dgroup','rt_ggroup','rt_dgroup'))
# 
# outputfile <- paste("Behavior_Stability/BF_group_beh_stats_model_mainEff.txt",sep="")
# write.table(BF_mat, outputfile, sep="\t", row.names=FALSE, quote = FALSE)

# # calculate BF for age effect
# library(BayesFactor)
# cons <- c("fear","anger","sad","happy") 
# BF_mat <- matrix(rep(0,4*4), nrow=4,ncol=4) # condition*(acc GGroup + acc DGroup + rt GGroup + rt DGroup)
# 
# for(t in 1:2){
#   if (t==1) {
#     offset = 0 # for accuracy, = 4 for RT
#     type = "accuracy"
#   }else{
#     offset = 4
#     type = "mdRT"
#   }
#   
#   for(c in 1:length(cons)){ # for each condition
#     
#     # model: accuracy ~ Age + Group + Sex + Race + Motion (scale continuous variables)
#     f1 <- paste(colnames(cdata)[10 + offset + c]," ~ ","Age + GGroup + Sex + Race + Motion", sep="")
#     f0 <- paste(colnames(cdata)[10 + offset + c]," ~ ","GGroup + Sex + Race + Motion", sep="")
#     bf_f1 = lmBF(as.formula(f1), data = cdata)
#     bf_f0 = lmBF(as.formula(f0), data = cdata)
#     BF10 = bf_f1/bf_f0
#     BF_mat[c,2*t-1] <- exp(BF10@bayesFactor$bf)
#     
#     
#     # DGroup
#     f1 <- paste(colnames(cdata)[10 + offset + c]," ~ ","Age + DGroup + Sex + Race + Motion", sep="")
#     f0 <- paste(colnames(cdata)[10 + offset + c]," ~ ","DGroup + Sex + Race + Motion", sep="")
#     bf_f1 = lmBF(as.formula(f1), data = cdata)
#     bf_f0 = lmBF(as.formula(f0), data = cdata)
#     BF10 = bf_f1/bf_f0
#     BF_mat[c,2*t] <- exp(BF10@bayesFactor$bf)
#   }
# }
# dimnames(BF_mat) <- list(cons, c('acc_ggroup','acc_dgroup','rt_ggroup','rt_dgroup'))
# 
# outputfile <- paste("Behavior_Stability/BF_age_beh_stats_model_mainEff.txt",sep="")
# write.table(BF_mat, outputfile, sep="\t", row.names=FALSE, quote = FALSE)

# plot accuracy vs. age
library(reshape2)
library(ggplot2)
df1 = cdata[,c(2,11:14)]
df2 = cdata[,c(2,15:18)]
  
tf1 = melt(df1, id.vars="Age")
tf2 = melt(df2, id.vars="Age")
colnames(tf1) = c("Age","Condition","Accuracy")
colnames(tf2) = c("Age","Condition","RT")

ggplot(data=tf1, aes(x=Age, y=Accuracy, colour = Condition)) +
  geom_point(size=1) +
  geom_smooth(method = "lm",se = FALSE, linetype=1)
  #geom_smooth(method = "lm",se = TRUE) +
  #facet_wrap(~Condition, ncol=2, scale="fixed")
fname = paste("Behavior_Stability/acc_vs_age.eps",sep="")
ggsave(fname)

ggplot(data=tf2, aes(x=Age, y=RT, colour = Condition)) +
  geom_point(size=1) +
  geom_smooth(method = "lm",se = FALSE, linetype=1)
#geom_smooth(method = "lm",se = TRUE) +
#facet_wrap(~Condition, ncol=2, scale="fixed")
fname = paste("Behavior_Stability/RT_vs_age.eps",sep="")
ggsave(fname)

# #-------Reliability-----------------------
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
#   #print(c(nums[1],nums[2],nums[3]))
#   #print(c(length(ch),length(ado),length(adu)))
#   
#   int_ch = sort(sample(1:length(ch),nums['Children']))
#   int_ado = sort(sample(1:length(ado),nums['Adolescents']))
#   int_adu = sort(sample(1:length(adu),nums['Adults']))
#   
#   #print(c(length(int_ch),length(int_ado),length(int_adu)))
#   set = c(ch[int_ch],ado[int_ado],adu[int_adu])
#   return(set)
# }
# 
# cons <- c("fear","anger","sad","happy") 
# iter = 1000
# sample_size = 50 #c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
# 
# perf.names = c('acc_fear','acc_anger','acc_sad','acc_happy',
#                'rt_fear','rt_anger','rt_sad','rt_happy')
# num_perf = length(perf.names) 
# 
# for(s in 1:length(sample_size)){
#     print(sample_size[s])
#   
#     library(foreach)
#     library(doMC)
#     registerDoMC(4) # 4 cores
# 
#     age_list <- foreach(i=1:iter) %dopar% { # repeat the following analyses for iter times
#       age <- data.frame(coef=numeric(num_perf), p=numeric(num_perf),
#                         iter=numeric(num_perf), perf=character(num_perf),stringsAsFactors=FALSE)
# 
#       repeat{
#         # draw sample_size subjects from 759 subjects
#         selected_subj <- draw_subs(data=cdata,n=sample_size[s])
#         ss <- subset(cdata, Subj %in% selected_subj)
#         
#         if(length(unique(ss$GGroup))==2 & length(unique(ss$DGroup))==2 & length(unique(ss$Sex))==2 & length(unique(ss$Race))==2){
#           break
#         }
#       }
#    
#       
#       # model: perf = Age + Group + Sex + Race + Motion
#       for(j in 1:num_perf){
#         if(j %in% c(1,2,5,6)){
#           f <- paste("scale(",names(ss)[10+j],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#         }else if(j %in% c(3,4,7,8)){
#           f <- paste("scale(",names(ss)[10+j],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#         }
#         
#         fit <- lm(as.formula(f), data = ss)
#         age$coef[j] =  summary(fit)$coefficients['scale(Age)',1]
#         age$p[j] = summary(fit)$coefficients['scale(Age)',4]
#         age$iter[j] = i
#         age$perf[j] = perf.names[j]
#       }
#       return(age)
#     }
#     # save age coefficients
#     outputfile <- paste("Behavior_Stability/reliability_n",sample_size[s],".RData",sep="")
#     save(age_list, file=outputfile)
# 
#     rm(age_list)
# }
# 
# 
# calculate frequency of significance & plot
iter = 1000
sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
freq = data.frame(sample_size = numeric(length(sample_size)),
                  fear = numeric(length(sample_size)), anger = numeric(length(sample_size)),
                  sad = numeric(length(sample_size)), happy = numeric(length(sample_size)))

freq_mat <- matrix(rep(0,8*length(sample_size)), nrow=8,ncol=length(sample_size))

for(sz in 1:length(sample_size)){
  f=paste("Behavior_Stability/Reliability_n",sample_size[sz],".RData",sep="")
  load(f)

  iter = length(age_list)
  perf.names = age_list[[1]]$perf
  num_perf = length(perf.names)

  p_mat <- matrix(rep(0,num_perf*iter), nrow=num_perf,ncol=iter) # num_perf * num_iter
  age_coef <- matrix(rep(0,num_perf*iter), nrow=num_perf,ncol=iter) # num_perf * num_iter

  for(i in 1:iter){
      pp = p.adjust(age_list[[i]]$p, method = "fdr", length(age_list[[i]]$p))
      for(j in 1:num_perf){
          p_mat[j,i] = pp[j]
          age_coef[j,i] = age_list[[i]]$coef[j]
      }
  }
  dimnames(p_mat) <- list(perf.names, c(1:iter))
  dimnames(age_coef) <- list(perf.names, c(1:iter))

  full_mat = rep(1,iter) %*% t(c(1,1,0,0,-1,-1,-1,-1))
  full_mat = t(full_mat)
  sig_mat = (p_mat < 0.05) & (sign(full_mat)==sign(age_coef))
  count = rowSums(sig_mat)
  freq = count/iter
  freq_mat[,sz] = freq
}

# line plot for significant regions
library(ggplot2)
library(reshape2)
dimnames(freq_mat) <- list(perf.names, sample_size)

tf = melt(freq_mat)
colnames(tf) = c('Condition','Sample_Size','Probability')

tf1 = subset(tf, Condition %in% c('acc_fear','acc_anger','acc_sad','acc_happy'))
tf2 = subset(tf, Condition %in% c('rt_fear','rt_anger','rt_sad','rt_happy'))

fname = paste("Behavior_Stability/Acc_Reliability_lineplot_probability_mainmodel_allsz.eps",sep="")
ggplot(data=tf1, aes(x=Sample_Size, y=Probability, colour = Condition)) +
      geom_line() +
      geom_point( size=2, shape=21, fill="white")
ggsave(file=fname)

fname = paste("Behavior_Stability/RT_Reliability_lineplot_probability_mainmodel_allsz.eps",sep="")
ggplot(data=tf2, aes(x=Sample_Size, y=Probability, colour = Condition)) +
  geom_line() +
  geom_point( size=2, shape=21, fill="white")
ggsave(file=fname)

  
  
