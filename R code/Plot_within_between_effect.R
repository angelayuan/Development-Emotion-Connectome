setwd("/Users/zhangyuan/Google Drive/Files_PNC_Analysis")

#-----read in subject info----------
sub <- read.table('SubInfo_2018.txt',header = TRUE, sep = '\t',
                  colClasses=c("Subj"="character"),stringsAsFactors=FALSE)

cons <- c("fear","anger","sad","happy") 

# M1: subcortical, M2: limbic, M3: frontoparietal
cdata <- data.frame(Subject=character(), M1=numeric(), M2=numeric(), M3=numeric(), 
                    M12=numeric(), M13=numeric(), M23=numeric(), Cond=character())
for(c in 1:length(cons)){ # for each condition
  #------read in data--------
  # within and between module connecivity (M1,M2,M3,M12,M13,M23)
  filename <- paste('BS_Conn_50ROIs_2018/Original Data/within_between_mc_',cons[c],'_weighted_avg.txt',sep="")
  intra_inter <- read.table(filename,header = TRUE, sep = '\t',colClasses=c("Subject"="character"),stringsAsFactors=FALSE)
  intra_inter$X <- NULL
  
  intra_inter$Cond <- cons[c]
  intra_inter$Age <- sub$Age
  intra_inter$Motion <- sub$Motion
    
  cdata <- rbind(cdata,intra_inter)
}
cdata$Cond <- as.factor(cdata$Cond)
cdata$Cond <- factor(cdata$Cond, levels(cdata$Cond)[c(2,1,4,3)])

# cdata$M1resid <- resid(lm(M1 ~ Motion, data=cdata))
# cdata$M2resid <- resid(lm(M2 ~ Motion, data=cdata))
# cdata$M3resid <- resid(lm(M3 ~ Motion, data=cdata))
# cdata$M12resid <- resid(lm(M12 ~ Motion, data=cdata))
# cdata$M13resid <- resid(lm(M13 ~ Motion, data=cdata))
# cdata$M23resid <- resid(lm(M23 ~ Motion, data=cdata))

library(ggplot2)
Ms <- c('Within SPIN','Within DMN','Within FPN',
       'DMN-SPIN','FPN-SPIN','FPN-DMN')
#---plot: age vs. network connectivity for each condition---
# i=3
# #for(i in 1:length(Ms)){
#   ggplot(cdata, aes_string(x='Age', y=names(cdata)[1+i], color='Cond')) + 
#     geom_point(size=2, alpha=0.5) + 
#     geom_smooth(method=lm) + 
#     facet_wrap(~Cond) + 
#     labs(x = "Age", y = paste(Ms[i],'Interaction')) +
#     theme(axis.text=element_text(size=12),
#           axis.title=element_text(size=12,face="bold"),
#           plot.title = element_text(size = 15,face="bold"))
# #}
  # library(colorspace) 
  # colors_border= rainbow_hcl(4)
  cons = c("fear","anger","sad","happy")
  c=4
  i=5
  ggplot(cdata[cdata$Cond==cons[c],], aes_string(x='Age', y=names(cdata)[1+i])) + 
    geom_point(size=2) + #, alpha=0.8,color=colors_border[1]) + 
    geom_smooth(method=lm) + 
    labs(x = "Age", y = paste(Ms[i],'interaction')) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          plot.title = element_text(size = 15,face="bold"))
  fname = paste("/Users/zhangyuan/Desktop/",cons[c],"_",Ms[i],".eps",sep="")
  ggsave(file=fname)
  