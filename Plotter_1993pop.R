#!/usr/bin/Rscript
# Title     : TODO
# Objective : TODO
# Created by: Sam
# Created on: 2019-01-31
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


library(ggplot2)

#df1 <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_try3_R1000_FP450_HF0_OPDmean8.txt",header=F,sep="\t")

#df1 <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_ShannonE_R1000_FP450_HF0_OPDmean8.txt",header=F,sep="\t")

#df1 <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_AlleleNum_R1000_FP450_HF0_OPDmean5.txt",header=F,sep="\t")

df1 <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_Recal_R1000_FP450_HF0_OPDmean5.txt",header=F,sep="\t")

EmpH <- read.table("~/PycharmProjects/simuPOP/Boot_empirical_H.tsv",header=T,sep="\t")

EmpH$Gene2 <- factor(EmpH$Gene, levels = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))

Shan <- read.table("~/PycharmProjects/simuPOP/Shannon_empirical.txt",header=T,sep="\t")


 #
 # df1$C2745 <- (df1$V3-df1[1,3])/df1[1,3]
 # df1$Kettin <- (df1$V4-df1[1,4])/df1[1,4]
 # df1$Tpi <- (df1$V5-df1[1,5])/df1[1,5]
 # df1$Catalase <- (df1$V6-df1[1,6])/df1[1,6]
 # df1$C5197 <- (df1$V7-df1[1,7])/df1[1,7]
 # df1$Masc <- (df1$V8-df1[1,8])/df1[1,8]
 # df1$Shaker <- (df1$V9-df1[1,9])/df1[1,9]
 # df1$Ldh <- (df1$V10-df1[1,10])/df1[1,10]

 df1$C2745 <- df1$V3/df1[1,3]
 df1$Kettin <- df1$V4/df1[1,4]
 df1$Tpi <- df1$V5/df1[1,5]
 df1$Catalase <- df1$V6/df1[1,6]
 df1$C5197 <- df1$V7/df1[1,7]
 df1$Masc <- df1$V8/df1[1,8]
 df1$Shaker <- df1$V9/df1[1,9]
 df1$Ldh <- df1$V10/df1[1,10]

# df1$C2745 <- df1$V3
# df1$Kettin <- df1$V4
# df1$Tpi <- df1$V5
# df1$Catalase <- df1$V6
# df1$C5197 <- df1$V7
# df1$Masc <- df1$V8
# df1$Shaker <- df1$V9
# df1$Ldh <- df1$V10

#stderror <- function(x) sqrt(var(x)/length(x))

df1_melt <- melt(data = df1, id.vars = c("V1","V2"), measure.vars = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))

decline <- read.table("~/PycharmProjects/simuPOP/Decline_H_250-100Ne",header=F,sep="\t")

#df1_meanSE <- aggregate(df1_melt$value,by=list(df1_melt$V2,df1_melt$variable), FUN = function(x) c(mean = mean(x), varia = var(x)))

#ggplot(data=df1_meanSE) + geom_line(aes(x=Group.1,y=x[,1])) +  geom_ribbon(aes(x=Group.1,ymin=x[,1]-x[,2],ymax=x[,1]+x[,2]),alpha=0.3)   + facet_wrap(~Group.2,nrow=1) + coord_cartesian(ylim=c(0,1.2))
## Median and confidence intervals
ggplot() + geom_line(data=decline,aes(x=V1,y=V2),linetype="dotted") +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.05, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .95)) +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.1, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .75)) +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.15, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .5)) +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.2, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .25)) +
    facet_wrap(~variable,nrow=1) +
    labs(x="Generations",y="Hd decline") +
    ylim(c(0,1.2)) + theme_bw()



my_ci <- function(x) data.frame(
  y=mean(x),
  ymin=mean(x) - 2 * sd(x),
  ymax=mean(x) + 2 * sd(x)
)

ggplot() + #geom_line(data=decline,aes(x=V1,y=V2),linetype="dotted") +
    stat_summary(data=df1_melt, aes(x=V2,y=value),fun.data="my_ci", geom="smooth") +
    facet_wrap(~variable,nrow=1) +
    labs(x="Generations",y="Shannon E") +
    ylim(c(0,20)) + theme_bw()


#Number of alleles
ggplot(data=df1_melt, aes(x=V2,y=value))+
      stat_summary(fun.data = mean_sdl, geom = "ribbon", aes(x=V2,y=value), linetype = "dashed",alpha = 0.3)+
    stat_summary(geom="line", fun.y=mean) +
    facet_wrap(~variable,nrow=1) +
    labs(x="Generations",y="Number of alleles") +
     theme_bw()
#Boot

ggplot() + geom_line(data=decline,aes(x=V1,y=V2),linetype="dotted") +
    stat_summary(data=df1_melt, aes(x=V2,y=value),fun.data="mean_cl_boot", geom="errorbar",width=0.1,colour = "red")  +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.1, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .75)) +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.15, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .5)) +
    stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.2, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .25)) +
    facet_wrap(~variable,nrow=1) +
    labs(x="Generations",y="Decline in Hd") +
    ylim(c(0,1.2)) + theme_bw()



Shan <- read.table("~/PycharmProjects/simuPOP/Shannon_empirical.txt",header=T,sep="\t")

Shan$X2006_CI <- sqrt(Shan$X2006_SV)*2
Shan$X1993_CI <- sqrt(Shan$X1993_SV)*2

shan_melt1 <- melt(Shan,measure.vars=c("X1993_S","X2006_S"))
shan_melt2 <- melt(Shan,measure.vars=c("X1993_CI","X2006_CI"))

shan_melt3 <- merge(shan_melt1,shan_melt2,by="Gene")

shan_melt1$Gene2 <- factor(shan_melt1$Gene, levels = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))
shan_melt2$Gene2 <- factor(shan_melt2$Gene, levels = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))

shan_melt3$CI <- (sqrt(shan_melt3$value.y)*2)

ggplot() +geom_point(data=shan_melt1,aes(x=variable,y=value)) + geom_errorbar(data=shan_melt1,aes(x=variable, ymin=value-shan_melt2$value, ymax=value+shan_melt2$value), width=.2,position=position_dodge(.9)) + facet_wrap(~Gene2,nrow=1)

###Real_Shan



#ggplot() + geom_line(data=decline,aes(x=V1,y=V2),linetype="dotted") + stat_summary(data=df1_melt, aes(x=V2,y=value),geom = 'smooth', alpha = 0.2, fill = 'black', color = 'black',fun.data = median_hilow, fun.args = list(conf.int = .95)) +facet_wrap(~variable,nrow=1) + labs(x="Generations",y="Decline in Hd") + ylim(c(0,1.2)) + theme_bw()




##Empirical bootstrapped Hd

library(boot)

df1 <- read.table("~/PycharmProjects/simuPOP/Bicyclus_Z.genepop",header=TRUE,colClasses = "character")
df1[df1 == "0000"] <- NA

pop1 <- subset(df1, df1$Pop=="pop1,")
pop2 <- subset(df1, df1$Pop=="pop2,")

#1-sum(sapply(t(table(df1$C2745)), function(x) (x/sum(t(table(df1$C2745))))^2))
run_DiD <- function(my_data, indices){
    d <- my_data[indices,]
    #temp2 <- t(table(d[,c(1,2)]))
    temp1 <- t(table(d[,c(1,2)]))
    H1 <- 1-sum(sapply(temp1, function(x) (x/sum(temp1))^2))
    #H2 <- 1-sum(sapply(temp2, function(x) (x/sum(temp2))^2))
    #Hd <- (H2 - H1)/H1

    return(
        H1

    )
}

Z_decline_boots <- matrix(0, ncol = 8, nrow = 1000)

for (i in c(2,3,4,5,6,7,8,9)){
    pop1_est <- boot(pop1[,c(1,i)], run_DiD, R=1000)
    pop2_est <- boot(pop2[,c(1,i)], run_DiD, R=1000)
    Boot_pops <- as.data.frame(cbind(pop1_est$t,pop2_est$t))
    Z_decline_boots[,i-1] <- (Boot_pops$V2-Boot_pops$V1)/Boot_pops$V1
}

Z_decline_boots_df <- as.data.frame(Z_decline_boots)

colnames(Z_decline_boots_df) <- c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh")

Z_decline_boots_df$V1 <- as.numeric(row.names(Z_decline_boots_df)) -1
Z_decline_boots_df$Data <- "Empirical"


df1 <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_Recal_R1000_FP450_HF0_OPDmean5.txt",header=F,sep="\t")

 df1$C2745 <- (df1$V3-df1[1,3])/df1[1,3]
 df1$Kettin <- (df1$V4-df1[1,4])/df1[1,4]
 df1$Tpi <- (df1$V5-df1[1,5])/df1[1,5]
 df1$Catalase <- (df1$V6-df1[1,6])/df1[1,6]
 df1$C5197 <- (df1$V7-df1[1,7])/df1[1,7]
 df1$Masc <- (df1$V8-df1[1,8])/df1[1,8]
 df1$Shaker <- (df1$V9-df1[1,9])/df1[1,9]
 df1$Ldh <- (df1$V10-df1[1,10])/df1[1,10]

df1_70 <- subset(df1, df1$V2 == 69)

Z_decline_Sim <- df1_70[,c(1,11:18)]
Z_decline_Sim$Data <- "Simulated"
Z_decline_Sim2 <- data.frame(V1=Z_decline_Sim$V1,C2745=Z_decline_Sim$C2745,Kettin=Z_decline_Sim$Kettin,Tpi=Z_decline_Sim$Tpi,Catalase=Z_decline_Sim$Catalase,C5197=Z_decline_Sim$C5197,Masc=Z_decline_Sim$Masc,Shaker=Z_decline_Sim$Shaker,Ldh=Z_decline_Sim$Ldh,Data=Z_decline_Sim$Data)

Z_all <- rbind(Z_decline_Sim2,Z_decline_boots_df)
Z_all_melt <- melt(data = Z_all, id.vars = c("V1","Data"), measure.vars = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))

gt3 <- ggplot(data=Z_all_melt,aes(value,colour=Data))+ geom_line(stat='density') + facet_wrap(~variable,ncol=1,switch="both") + theme_bw() + guides(colour=FALSE) + geom_vline(xintercept=-0.264701535,linetype="dotted") + theme(strip.background = element_blank(),strip.text.y = element_blank(),plot.margin=unit(c(10,10,15.5,-2),unit="pt")) + labs(x="Decline in Hd",y="Density") + scale_color_manual(values = c("salmon", "goldenrod"))




#
#
#
#
#
#
#
#
#



df_H <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_Recal_R1000_FP450_HF0_OPDmean5.txt",header=F,sep="\t")

colnames(df_H) <- c("rep","gen","C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh")

df_N <- read.table("~/PycharmProjects/simuPOP/simuPop_1993start_2pop_AlleleNum_R1000_FP450_HF0_OPDmean5.txt",header=F,sep="\t")

colnames(df_N) <- c("rep","gen","C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh")


df_N_melt <- melt(data = df_N, id.vars = c("rep","gen"), measure.vars = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))
df_H_melt <- melt(data = df_H, id.vars = c("rep","gen"), measure.vars = c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"))



Merged_HN<- merge(df_H_melt,df_N_melt,by=c("rep","gen","variable"))


Merged_HN_70 <- subset(Merged_HN,Merged_HN$gen==69)

Emp_dat <- data.frame(variable= c("C2745", "Kettin","Tpi","Catalase","C5197","Masc","Shaker","Ldh"),value.x=c(0.82,0.59,0.43,0.65,0.83,0.82,0.77,0.78),value.y=c(6,3,3,5,7,7,6,5))

#cairo_pdf("Hd_Hn_heatmap.pdf",width=20,height=5)
#ggplot(data=Merged_HN_70,aes(x=factor(value.y),y=value.x)) + geom_bin2d(binwidth = c(1, 0.05)) + facet_wrap(~variable,nrow=1) + scale_fill_gradient2(high="red",mid="yellow",  low="darkgreen", name="Simulations",midpoint=100) +theme_bw() + geom_point(data=Emp_dat)

ggplot(data=Merged_HN_70,aes(x=factor(value.y),y=value.x)) + geom_bin2d(binwidth = c(1, 0.05)) + facet_wrap(~variable,nrow=1) + scale_fill_gradient2(high="red",mid="yellow",  low="darkgreen", name="Simulations",midpoint=50) +theme_bw() + geom_point(data=Emp_dat)


length(subset(Merged_HN_70,Merged_HN_70$variable=="C2745" & Merged_HN_70$value.x>=0.82 & Merged_HN_70$value.y >=6)[,1])
#[1] 1, OD [1] 1
length(subset(Merged_HN_70,Merged_HN_70$variable=="Kettin" & Merged_HN_70$value.x>=0.59 & Merged_HN_70$value.y >=3)[,1])
#[1] 332, OD [1] 351
length(subset(Merged_HN_70,Merged_HN_70$variable=="Tpi" & Merged_HN_70$value.x>=0.43 & Merged_HN_70$value.y >=3)[,1])
#[1] 809, OD [1] 778
length(subset(Merged_HN_70,Merged_HN_70$variable=="Catalase" & Merged_HN_70$value.x>=0.65 & Merged_HN_70$value.y >=5)[,1])
#[1] 13, OD [1] 19
length(subset(Merged_HN_70,Merged_HN_70$variable=="C5197" & Merged_HN_70$value.x>=0.83 & Merged_HN_70$value.y >=7)[,1])
#[1] 0, OD [1] 168
length(subset(Merged_HN_70,Merged_HN_70$variable=="Masc" & Merged_HN_70$value.x>=0.82 & Merged_HN_70$value.y >=7)[,1])
#[1] 0, OD [1] 67
length(subset(Merged_HN_70,Merged_HN_70$variable=="Shaker" & Merged_HN_70$value.x>=0.77 & Merged_HN_70$value.y >=6)[,1])
#[1] 8, OD [1] 7
length(subset(Merged_HN_70,Merged_HN_70$variable=="Ldh" & Merged_HN_70$value.x>=0.78 & Merged_HN_70$value.y >=5)[,1])
#[1] 13, OD [1] 11


#dev.off


#
# df2 <- read.table("~/PycharmProjects/simuPOP/simuPop_Xchrom_R500_P50_FP50_HF1_OPDmean8_R0.01.txt",header=F,sep="\t")
#
# df2$Loc1HR <- df2$V3/df2[1,3]
# df2$Loc2HR <- df2$V4/df2[1,4]
#
# decline <- read.table("~/PycharmProjects/simuPOP/Decline_H_250Ne",header=F,sep="\t")
#
#
# p1 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df1, aes(x=V2,y=Loc1HR),geom = 'smooth', alpha = 0.2, fill = 'red', color = 'red',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc1 OD") + ylim(c(0,1)) + theme_bw()
# p2 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df1, aes(x=V2,y=Loc2HR),geom = 'smooth', alpha = 0.2, fill = 'red', color = 'red',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc2 r=0.01") + ylim(c(0.,1)) + theme_bw()
#
# p3 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df2, aes(x=V2,y=Loc1HR),geom = 'smooth', alpha = 0.2, fill = 'blue', color = 'blue',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc1") + ylim(c(0,1)) + theme_bw()
# p4 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df2, aes(x=V2,y=Loc2HR),geom = 'smooth', alpha = 0.2, fill = 'blue', color = 'blue',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc2 r=0.01") + ylim(c(0,1)) + theme_bw()
#
#
# df3 <- read.table("~/PycharmProjects/simuPOP/simuPop_Xchrom_R500_P50_FP50_HF0_OPDmean8_R0.1.txt",header=F,sep="\t")
#
# df3$Loc1HR <- df3$V3/df3[1,3]
# df3$Loc2HR <- df3$V4/df3[1,4]
#
# df4 <- read.table("~/PycharmProjects/simuPOP/simuPop_Xchrom_R500_P50_FP50_HF1_OPDmean8_R0.1.txt",header=F,sep="\t")
#
# df4$Loc1HR <- df4$V3/df4[1,3]
# df4$Loc2HR <- df4$V4/df4[1,4]
#
# decline <- read.table("~/PycharmProjects/simuPOP/Decline_H_250Ne",header=F,sep="\t")
#
#
# p5 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df3, aes(x=V2,y=Loc1HR),geom = 'smooth', alpha = 0.2, fill = 'red', color = 'red',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc1 OD") + ylim(c(0,1)) + theme_bw()
# p6 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df3, aes(x=V2,y=Loc2HR),geom = 'smooth', alpha = 0.2, fill = 'red', color = 'red',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc2 r=0.1") + ylim(c(0.,1)) + theme_bw()
#
# p7 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df4, aes(x=V2,y=Loc1HR),geom = 'smooth', alpha = 0.2, fill = 'blue', color = 'blue',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc1") + ylim(c(0,1)) + theme_bw()
# p8 <- ggplot() + geom_line(data=decline,aes(x=V1,y=V2)) + stat_summary(data=df4, aes(x=V2,y=Loc2HR),geom = 'smooth', alpha = 0.2, fill = 'blue', color = 'blue',fun.data = median_hilow, fun.args = list(conf.int = .95)) + labs(x="Generations",y="Decline in Hd",title="Loc2 r=0.1") + ylim(c(0,1)) + theme_bw()
#
#
#
# multiplot(p1, p2, p3, p4, p5, p6, p7, p8, cols=4)
