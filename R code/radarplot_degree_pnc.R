#setwd("/Users/zhangyuan/Google Drive/Manuscript/Figures_0612/New version 0612")
setwd("/Users/yuanzh/Google Drive/Files_PNC_Analysis")
library(fmsb)

roi_names <- read.table('Beta_50ROIs/roiname_list.txt',header = FALSE, sep = '\t',stringsAsFactors=FALSE)
roi.names <- roi_names$V1
net <- c(5:6,17:18,23:24,27:34,41:42,45:50,   19:22,25:26,35:40,43:44,  1:4,7:16)
roi.names = roi.names[net]
#deg <- read.table('50nodes_AgeEffect_Hubs_plot.txt',header = TRUE, sep = '\t',
#                  stringsAsFactors=FALSE)
deg <- read.table('BS_Conn_50ROIs_2018/Model1/degree_age_eff.txt',header = TRUE, sep = '\t',
                  stringsAsFactors=FALSE)

#data.plot <- rbind(deg$Fear,deg$Anger)
data.plot <- rbind(deg$fear,deg$anger,deg$sad,deg$happy)
colnames(data.plot) <- roi.names
data.plot <- rbind(rep(25,50),rep(0,50),data.plot)
data.plot = as.data.frame(data.plot)
#rownames(data.plot) <- c("max","min","fear","anger")
#rownames(data.plot) <- c("max","min","sad","happy")
library(colorspace) 
colors_border= rainbow_hcl(4)#c('orange','lightblue','pink','green') #c( rgb(0.8,0.2,0.5,0.9), rgb(0.2,0.5,0.5,0.9))
#colors_in=c( rgb(0.8,0.2,0.5,0.4), rgb(0.2,0.5,0.5,0.4))
#reorder = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,
#            2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50)

reorder = c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,
            50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,18,16,14,12,10,8,6,4,2)
data.plot = data.plot[,reorder]
radarchart(data.plot, axistype=1, seg = 5, 
           pcol=colors_border, plwd=1 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,25,5), cglwd=0.8, vlcex=0.5) #caxislabels=seq(0,24,8)
#legend(x=0.7, y=1, legend = rownames(data.plot[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

# 
# #--------Examples----------------
# # Library
# library(fmsb)
# # Create data: note in High school for several students
# set.seed(99)
# data=as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
# colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
# rownames(data)=paste("mister" , letters[1:3] , sep="-")
# 
# # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
# data=rbind(rep(20,5) , rep(0,5) , data)
# 
# #==================
# # Plot 1: Default radar chart proposed by the library:
# radarchart(data)
# 
# #==================
# # Plot 2: Same plot with custom features
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# radarchart( data  , axistype=1 , 
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#             #custom labels
#             vlcex=0.8 
# )
# legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
# 
# #=================
# # Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
# colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
# colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
#             #custom polygon
#             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
#             #custom the grid
#             cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
#             #custom labels
#             vlcex=0.8 
# )


mn = rowMeans(data.plot)
std = c(sd(data.plot[1,]),sd(data.plot[2,]),sd(data.plot[3,]),sd(data.plot[4,]))
cutoff = mn+std
which(data.plot[1,] > cutoff[1])
which(data.plot[2,] > cutoff[2])
which(data.plot[3,] > cutoff[3])
which(data.plot[4,] > cutoff[4])
