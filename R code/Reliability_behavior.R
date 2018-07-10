setwd("/Users/yuanzh/Google Drive/Files_PNC_Analysis")
#-----read in subject info----------
sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)

#--read in performance data----
perf <- read.table('perfsummary.txt',header = TRUE, sep = '\t',
                   colClasses=c("SubID"="character"),stringsAsFactors=FALSE)
perf <- subset(perf, SubID %in% sub$Subj)

perf <- perf[,c("pctcorr", "pctcorrfear", "pctcorrangry", "pctcorrsad", "pctcorrhappy",
                "rtmnallcorr", "rtmnfearcorr", "rtmnangrycorr", "rtmnsadcorr", "rtmnhappycorr")]
perf_acc <- perf[,c(2,3,4,5)]

#--supporting function: draw subjects-----
# data: data that contains age and other information
# n: how many subjects to draw
# ratio: how many percent in the draw comes from children,
# how many from adolescents, and how many from adults
draw_subs <- function(data=cdata,n=50){
  tmp = table(cdata$Agebin)
  ratio = tmp/sum(tmp)
  #nums = round(n*ratio) # #CH, #ADO, and #ADU to draw
  nums = trunc(round(n*ratio,1) + 0.5) #CH, #ADO, and #ADU to draw
  ch = cdata[cdata$Agebin=="Children",]$Subj
  ado = cdata[cdata$Agebin=="Adolescents",]$Subj
  adu = cdata[cdata$Agebin=="Adults",]$Subj
  
  #print(c(nums[1],nums[2],nums[3]))
  #print(c(length(ch),length(ado),length(adu)))
  
  int_ch = sort(sample(1:length(ch),nums['Children']))
  int_ado = sort(sample(1:length(ado),nums['Adolescents']))
  int_adu = sort(sample(1:length(adu),nums['Adults']))
  
  print(c(length(int_ch),length(int_ado),length(int_adu)))
  set = c(ch[int_ch],ado[int_ado],adu[int_adu])
  return(set)
}

# combine data
fear = data.frame(Subj = c(1:dim(sub)[1]), Acc = perf_acc$pctcorrfear, Cond = rep(1,dim(sub)[1]), 
                  Age = sub$Age, Sex = sub$Sex, Race =  sub$Race, Motion = sub$Motion, Agebin = sub$Agebin)
anger = data.frame(Subj = c(1:dim(sub)[1]), Acc = perf_acc$pctcorrangry, Cond = rep(2,dim(sub)[1]), 
                   Age = sub$Age, Sex = sub$Sex, Race =  sub$Race, Motion = sub$Motion, Agebin = sub$Agebin)
sad = data.frame(Subj = c(1:dim(sub)[1]), Acc = perf_acc$pctcorrsad, Cond = rep(3,dim(sub)[1]), 
                 Age = sub$Age, Sex = sub$Sex, Race =  sub$Race, Motion = sub$Motion, Agebin = sub$Agebin)
happy = data.frame(Subj = c(1:dim(sub)[1]), Acc = perf_acc$pctcorrhappy, Cond = rep(4,dim(sub)[1]), 
                   Age = sub$Age, Sex = sub$Sex, Race =  sub$Race, Motion = sub$Motion, Agebin = sub$Agebin)
df = rbind(fear, anger, sad, happy)
df$Cond = factor(df$Cond)

library(lme4)
library(car)

iter = 1000
sample_size = 200 #c(20,30,40,50,60,70,80,90,100)

for(sz in 1:length(sample_size)){
  print(sample_size[sz])
  age_cond = data.frame(inter_p = numeric(iter), fear_cor = numeric(iter), fear_p = numeric(iter),
                        anger_cor = numeric(iter), anger_p = numeric(iter),
                        sad_cor = numeric(iter), sad_p = numeric(iter),
                        happy_cor = numeric(iter), happy_p = numeric(iter))
  inter = numeric(iter)
  for(i in 1:iter){
    print(i)
    cdata = fear
    selected_subj <- draw_subs(data=cdata,n=sample_size[sz])
    ss = subset(df, Subj %in% selected_subj)
    
    fit = lmer(scale(Acc) ~ scale(Age)*Cond + (1|Subj), data=ss)
    stats = Anova(fit, type="III",test.statistic=c("F"),  vcov.=vcov(fit))
    age_cond$inter_p[i] = stats['scale(Age):Cond','Pr(>F)']
    
    f = cor.test(ss$Age[ss$Cond==1], ss$Acc[ss$Cond==1])
    a = cor.test(ss$Age[ss$Cond==2], ss$Acc[ss$Cond==2])
    s = cor.test(ss$Age[ss$Cond==3], ss$Acc[ss$Cond==3])
    h = cor.test(ss$Age[ss$Cond==4], ss$Acc[ss$Cond==4])
    
    age_cond$fear_cor[i] = f$estimate
    age_cond$fear_p[i] = f$p.value
    
    age_cond$anger_cor[i] = a$estimate
    age_cond$anger_p[i] = a$p.value
    
    age_cond$sad_cor[i] = s$estimate
    age_cond$sad_p[i] = s$p.value
    
    age_cond$happy_cor[i] = h$estimate
    age_cond$happy_p[i] = h$p.value
  }
  
  fout=paste("Behavior_Stability/stability_behavior_n",sample_size[sz],".RData",sep="")
  save(age_cond, file=fout)
}


# calculate frequency of significance & plot
iter = 1000
sample_size = c(50,100,150,200,250,300,350,400,450,500,550,600,650,700,750)
freq = data.frame(sample_size = numeric(length(sample_size)), inter_sig = numeric(length(sample_size)), 
                  fear = numeric(length(sample_size)), anger = numeric(length(sample_size)),
                  sad = numeric(length(sample_size)), happy = numeric(length(sample_size)))

for(sz in 1:length(sample_size)){
  f=paste("Behavior_Stability/stability_behavior_n",sample_size[sz],".RData",sep="")
  load(f)
  sig = subset(age_cond, inter_p<0.05)

  freq$sample_size[sz] = sample_size[sz]
  freq$inter_sig[sz] = (dim(sig)[1]/iter)*100
  freq$fear[sz] = 100*sum(sig$fear_p < 0.05)/dim(sig)[1]
  freq$anger[sz] = 100*sum(sig$anger_p < 0.05)/dim(sig)[1]
  freq$sad[sz] = 100*sum(sig$sad_p < 0.05)/dim(sig)[1] 
  freq$happy[sz] = 100*sum(sig$happy_p < 0.05,na.rm=TRUE)/dim(sig)[1]
}

# plot
library(reshape2)
library(ggplot2)
tf = melt(freq, id.vars="sample_size", value.name="percent_freq", variable="condition")
ggplot(data=tf[-c(1:15),], aes(x=sample_size, y=percent_freq, group = condition, colour = condition)) +
   geom_line() +
   geom_point( size=2, shape=21, fill="white")

