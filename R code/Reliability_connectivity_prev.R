# ROI-ROI connectivity (based on beta series) is extracted using scripts under the path 
# /oak/stanford/groups/menon/projects/yuanzh/2017_fconn_EI/scripts/taskfmri/fconn/Beta_Series_Yuan
# on Sherlock

setwd("/Users/zhangyuan/Google Drive/Files_PNC_Analysis")
# library(car)
# #-----read in subject info----------
# sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
#                   colClasses=c("Subj"="character"),stringsAsFactors=FALSE)
# cons <- c("fear","anger","sad","happy") 
# 
# #------first calculate the observed value using the full sample-----
# res.age <- data.frame()
# for(c in 1:length(cons)){ # for each condition
#   print(cons[c])
#   #--read in connectivity--
#   fname <- paste("BS_Conn_prev_25_2018/bs_conns_",cons[c],".txt",sep="")
#   conns <- read.table(fname,header = TRUE, sep = '\t',
#                       colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
#   conns$X <- NULL
#   
#   col_of_interest = c('Gee_R_AMY_Gee_R_VMPFC',
#                       'Vink_L_AMY_Neg_Vink_L_mOFC_AAL','Vink_L_AMY_Neg_Vink_R_mOFC_AAL',
#                       'Vink_R_AMY_Neg_Vink_L_mOFC_AAL','Vink_R_AMY_Neg_Vink_R_mOFC_AAL',
#                       'Vink_L_AMY_Pos_Vink_L_mOFC_AAL','Vink_L_AMY_Pos_Vink_R_mOFC_AAL',
#                       'Vink_R_AMY_Pos_Vink_L_mOFC_AAL','Vink_R_AMY_Pos_Vink_R_mOFC_AAL',
#                       'Wolf_L_AMY_Talairach_Wolf_L_VMPFC','Wolf_L_AMY_Talairach_Wolf_R_VMPFC',
#                       'Wolf_R_AMY_Talairach_Wolf_R_DMPFC',
#                       'Kujawa_L_dACC_L_AMY_AAL','Kujawa_R_dACC_R_AMY_AAL',
#                       'L_AMY_AAL_Wu_L_mPFC.1','R_AMY_AAL_Wu_L_mPFC',
#                       'L_AMY_AAL_Wu_L_ACC','R_AMY_AAL_Wu_R_ACC')
#   conns = conns[,col_of_interest] 
#   
#   #--Combine data--
#   cdata <- cbind(sub, conns)
#   cdata$Sex <- as.factor(cdata$Sex)
#   cdata$Race <- as.factor(cdata$Race)
#   cdata$Agebin <- as.factor(cdata$Agebin) 
#   cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
#   cdata$GGroup <- as.factor(cdata$GGroup) 
#   cdata$DGroup <- as.factor(cdata$DGroup) 
#   
#   #--Statistical Analyses--
#   num_conn <- ncol(conns)
#   conn.names <- names(conns)
#   
#   # model: Connectivity ~ Age + Group + Sex + Race + Motion (scale continuous variables)
#   for(i in 1:num_conn){ # for each connection
#     if(cons[c]=='fear' | cons[c] == 'anger'){
#       f <- paste("scale(",conn.names[i],") ~ ","scale(Age)+GGroup + Sex + Race + scale(Motion)", sep="")
#     } else if(cons[c]=='sad' | cons[c] == 'happy'){
#       f <- paste("scale(",conn.names[i],") ~ ","scale(Age)+DGroup + Sex + Race + scale(Motion)", sep=" ")
#     }
#     fit <- lm(as.formula(f), data = cdata)
#     # print(summary(fit))
#     stats <- Anova(fit,type="III")
#     tmp.age <- data.frame(stats["scale(Age)", 3], stats["scale(Age)", 2],stats["Residuals", 2], stats["scale(Age)", 4],
#                           summary(fit)$adj.r.squared, fit$coefficients['scale(Age)'],
#                           conn.names[i], "main effect model",cons[c])
#     colnames(tmp.age) <- c("F", "Df", "Df.res", "p", "adj.r2", "coef","connectivity","model","condition")
#     res.age <- rbind(res.age, tmp.age)
#   } # connection
#   
# } # conditions
# 
# outputfile <- 'BS_Conn_prev_25_2018/bs_stats_allconds_mainmodel.txt'
# write.table(res.age, outputfile, sep="\t", row.names=FALSE, quote = FALSE)
# 
# 
# #-----Next calculate the observed value across a wide range of sample size----
# #-----for 1000 times, and then plot correlation/jaccard vs. sample size---------
# 
# # Randomly draw 50 subjects from 759 subjects and run regression model (e.g., 
# # vmpfc-amygdala connectivity = age + sex + race? + motion) on 
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
#   set = c(ch[int_ch],ado[int_ado],adu[int_adu])
#   return(set)
# }
# 
# 
# iter = 1000
# sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
# 
# for(sz in 1:length(sample_size)){
#   print(sample_size[sz])
#   
#   for(c in 1:length(cons)){ # for each condition
#     print(c)
#     #--read in connectivity--
#     fname <- paste("BS_Conn_prev_25_2018/bs_conns_",cons[c],".txt",sep="")
#     conns <- read.table(fname,header = TRUE, sep = '\t',
#                         colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
#     conns$X <- NULL
#     
#     col_of_interest = c('Gee_R_AMY_Gee_R_VMPFC',
#                         'Vink_L_AMY_Neg_Vink_L_mOFC_AAL','Vink_L_AMY_Neg_Vink_R_mOFC_AAL',
#                         'Vink_R_AMY_Neg_Vink_L_mOFC_AAL','Vink_R_AMY_Neg_Vink_R_mOFC_AAL',
#                         'Vink_L_AMY_Pos_Vink_L_mOFC_AAL','Vink_L_AMY_Pos_Vink_R_mOFC_AAL',
#                         'Vink_R_AMY_Pos_Vink_L_mOFC_AAL','Vink_R_AMY_Pos_Vink_R_mOFC_AAL',
#                         'Wolf_L_AMY_Talairach_Wolf_L_VMPFC','Wolf_L_AMY_Talairach_Wolf_R_VMPFC',
#                         'Wolf_R_AMY_Talairach_Wolf_R_DMPFC',
#                         'Kujawa_L_dACC_L_AMY_AAL','Kujawa_R_dACC_R_AMY_AAL',
#                         'L_AMY_AAL_Wu_L_mPFC.1','R_AMY_AAL_Wu_L_mPFC',
#                         'L_AMY_AAL_Wu_L_ACC','R_AMY_AAL_Wu_R_ACC')
#     conns = conns[,col_of_interest] 
#     
#     #--Combine data--
#     cdata <- cbind(sub, conns)
#     cdata$Sex <- as.factor(cdata$Sex)
#     cdata$Race <- as.factor(cdata$Race)
#     cdata$Agebin <- as.factor(cdata$Agebin) 
#     cdata$Agebin <- factor(cdata$Agebin,levels(cdata$Agebin)[c(3,1,2)])
#     cdata$GGroup <- as.factor(cdata$GGroup) 
#     cdata$DGroup <- as.factor(cdata$DGroup) 
#     
#     # statistical analyses
#     num_conn <- ncol(conns)
#     conn.names <- names(conns)
#     
#     library(foreach)
#     library(doMC)
#     registerDoMC(4) # 4 cores
#   
#     age_list <- foreach(i=1:iter) %dopar% { # repeat the following analyses for iter times
#       age <- data.frame(coef=numeric(num_conn), p=numeric(num_conn), 
#                         iter=numeric(num_conn), conn=character(num_conn),stringsAsFactors=FALSE)
#       
#       # draw sample_size subjects from 759 subjects
#       selected_subj <- draw_subs(data=cdata,n=sample_size[sz])
#       ss <- subset(cdata, Subj %in% selected_subj)
#       # for each link, run the model and extract age coeff
#       # model: connectivity = Age + Sex + Race + Motion
#       for(j in 1:num_conn){
#         #print(j)
#         if(cons[c]=='fear' | cons[c] == 'anger'){
#           f <- paste("scale(",conn.names[j],") ~ ","scale(Age) + GGroup + Sex + Race + scale(Motion)", sep="")
#           
#         }else if(cons[c]=='sad' | cons[c] == 'happy'){
#           f <- paste("scale(",conn.names[j],") ~ ","scale(Age) + DGroup + Sex + Race + scale(Motion)", sep="")
#         }
#         fit <- lm(as.formula(f), data = ss)
#         age$coef[j] =  summary(fit)$coefficients['scale(Age)',1] 
#         age$p[j] = summary(fit)$coefficients['scale(Age)',4] 
#         age$iter[j] = i
#         age$conn[j] = conn.names[j]
#       }
#       return(age)
#     }
#     
#     # save age coefficients
#     outputfile <- paste("BS_Conn_prev_25_2018/Age_Coef_Distribution/stats_list_",cons[c],"_allconns_mainmodel_n",sample_size[sz],".RData",sep="")
#     save(age_list, file=outputfile)
#     
#     rm(age_list)
#   }
# }


# load data, count #significance for each link, calculate binomial p
cons <- c("fear","anger","sad","happy") 
iter = 1000
sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)

# load age_coef of the full sample
fullsample = read.table('BS_Conn_prev_25_2018/bs_stats_allconds_mainmodel.txt',header = TRUE, sep = '\t',stringsAsFactors=FALSE)
num_conn = 18

for(c in 1:length(cons)){
  freq_mat <- matrix(rep(0,num_conn*length(sample_size)), nrow=num_conn,ncol=length(sample_size)) 
  age_coef_fullsample = subset(fullsample,condition==cons[c])
  age_coef_fullsample = age_coef_fullsample$coef  
  
  for(sz in 1:length(sample_size)){
    f <- paste("BS_Conn_prev_25_2018/Age_Coef_Distribution/stats_list_",cons[c],"_allconns_mainmodel_n",sample_size[sz],".RData",sep="")
    load(f)
  
    iter = length(age_list)
    conn.names = age_list[[1]]$conn
    num_conn = length(conn.names)
  
    p_mat <- matrix(rep(0,num_conn*iter), nrow=num_conn,ncol=iter) # num_conn * num_iter
    age_coef <- matrix(rep(0,num_conn*iter), nrow=num_conn,ncol=iter) # num_conn * num_iter
    
    for(i in 1:iter){
      for(j in 1:num_conn){
        p_mat[j,i] = age_list[[i]]$p[j]
        age_coef[j,i] = age_list[[i]]$coef[j]
      } 
    }
    dimnames(p_mat) <- list(conn.names, c(1:iter))
    dimnames(age_coef) <- list(conn.names, c(1:iter))
    
    full_mat = matrix(age_coef_fullsample, nrow=dim(p_mat)[1], ncol=dim(p_mat)[2])
    sig_mat = (p_mat < 0.05) & (sign(full_mat)==sign(age_coef))
    count = rowSums(sig_mat)
    freq = count/iter
    freq_mat[,sz] = freq
  } # sample size
  
  # line plot
  library(ggplot2)
  library(reshape2)
  dimnames(freq_mat) <- list(conn.names, sample_size)
  # tf = melt(freq_mat)
  # colnames(tf) = c('Connectivity','Sample_Size','Probability')
  # fname = paste("BS_Conn_prev_25_2018/Age_Coef_Distribution/Reliability_lineplot_",cons[c],"_freq_mainmodel_allsz.eps",sep="")
  # ggplot(data=tf, aes(x=Sample_Size, y=Probability, colour = Connectivity)) +
  #   geom_line() +
  #   geom_point( size=2, shape=21, fill="white") + 
  #   scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
  # ggsave(file=fname, width=8, height=6)
  
  # plot just "facial emotion" studies: Gee, Kujawa, Wu
  tf = melt(freq_mat[-c(2:12),])
  colnames(tf) = c('Connectivity','Sample_Size','Probability')
  fname = paste("BS_Conn_prev_25_2018/Age_Coef_Distribution/Reliability_lineplot_connectivity_",cons[c],"_freq_mainmodel_allsz_face.eps",sep="")
  ggplot(data=tf, aes(x=Sample_Size, y=Probability, colour = Connectivity)) +
    geom_line() +
    geom_point( size=2, shape=21, fill="white") + 
    scale_y_continuous(breaks=seq(0, 1,0.25), limits=c(0, 1))
  ggsave(file=fname,width=8, height=6)

} # condition