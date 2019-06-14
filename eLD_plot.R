# Title     : TODO
# Objective : TODO
# Created by: Sam
# Created on: 2019-02-12
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(lattice)


eLD <- read.table("~/PycharmProjects/simuPOP/eLD/eLD_values.tsv",header=TRUE)

eLDSim <- read.table("~/PycharmProjects/simuPOP/eLD/Sim_eLD_values.tsv",header=TRUE)

eLDSim$Both <- paste(round(eLDSim$x.mean,2),round(eLDSim$x.sd,2),sep="±")
Matr <- dcast(eLDSim,factor(Group.1,levels=c("C2745", "Kettin", "Tpi", "Catalase", "C5197", "Masc", "Shaker", "Ldh"))~factor(Group.2,levels=c("C2745", "Kettin", "Tpi", "Catalase", "C5197", "Masc", "Shaker", "Ldh")),value.var="Both")[,-1]
Matr[upper.tri(Matr)] <- "0±0"
Matr <- cbind(N=dcast(eLDSim,factor(Group.1,levels=c("C2745", "Kettin", "Tpi", "Catalase", "C5197", "Masc", "Shaker", "Ldh"))~factor(Group.2,levels=c("C2745", "Kettin", "Tpi", "Catalase", "C5197", "Masc", "Shaker", "Ldh")),value.var="Both")[,1],Matr)

write.table(Matr, file="~/PycharmProjects/simuPOP/eLD/eLD_SIM_mat.tsv", sep="\t", quote=F, row.names=F,col.names=T)



eLDSim2 <- read.table("~/PycharmProjects/simuPOP/eLD/eLD_SIM_mat.tsv",header=TRUE)


eLDSim2_melt <- melt(eLDSim2,id="N")
eLDSim2_melt$V1 <- factor(eLDSim2_melt$N, levels = rev(c("C2745", "Kettin", "Tpi", "Catalase", "C5197", "Masc", "Shaker", "Ldh")))



eLD_melt <- melt(eLD)

eLD_melt$V1 <- factor(eLD_melt$N, levels = rev(c("C2745", "Kettin", "Tpi", "Catalase", "C5197", "Masc", "Shaker", "Ldh")))



p <- ggplot(data = eLD_melt, aes(x=variable, y=V1, fill=ifelse(value==1,NA,value))) +
    geom_tile() + scale_fill_gradient2(high="green4",mid="white",  low="white", name="ε",midpoint=0.08,guide='none') +
    geom_text(aes(variable, V1, label = ifelse(value==1,NA,formatC(round(value,digits=2),format='f',digits=2))), color = "black", size = 2.5) +
    geom_text(inherit.aes=FALSE,data=eLDSim2_melt, aes(variable,V1, label = ifelse(value=="0±0",NA,paste("(",value,")",sep=""))), color = "black", size = 2,vjust=2.5) +
    theme(plot.title = element_text(vjust=-15,hjust=-0.1,size=12),axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.background = element_rect(fill="white"),plot.margin=unit(c(5.5,0,5.5,-2),unit="pt")) +
    labs(x="",y="",title="C")
    #annotation_custom(grob = textGrob(label = "C", hjust = 0, gp = gpar(cex = 1)), ymin = 8.5, ymax = 8.5, xmin = -0.4, xmax = -0.4)

    #annotation_custom(grob = textGrob(label = "2006", hjust = 0, gp = gpar(cex = 1)), ymin = 0.2, ymax = 0.2, xmin = -0.3, xmax = -0.3) +
    #annotation_custom(grob = textGrob(label = "1993", hjust = 0, gp = gpar(cex = 1)), ymin = 8.5, ymax = 8.5, xmin = 8.6, xmax = 8.6)




gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
#grid.draw(gt)


posL <- data.frame (labpos= c(0,9,	18,	27,	36,	45,	54,	63), position = c(0, 10, 17.9, 27.8, 44.6,44.6, 55.6, 63), Names = c("C2745","Kettin","Tpi","Catalase","C5197",  "Masc","Shaker","Ldh"))

#p2 <- ggplot() + geom_rect(aes(xmin=0,xmax=10,ymin=0,ymax=63))  + coord_cartesian(xlim=c(0,60)) + scale_y_reverse(breaks=posL$position) + theme(plot.margin=unit(c(0.6,2,0.9,0),"cm"),panel.background = element_rect(fill="white"),axis.ticks.x=element_blank(),axis.text.x=element_blank()) + geom_text(aes(x=60,y=posL$labpos,label=posL$Names)) + geom_segment(aes(x=0,xend=50,y=posL$position,yend=posL$labpos)) + labs(x="",y="")
p2 <- ggplot() + geom_rect(aes(xmin=0,xmax=10,ymin=0,ymax=63))  +
    coord_cartesian(xlim=c(0,60)) + scale_y_reverse(breaks=posL$position) +
    theme(plot.title = element_text(vjust=20,hjust=-0.3,size=12),plot.margin=unit(c(0.5,-2,0.8,0.2),"cm"),panel.background = element_rect(fill="white"),axis.ticks.x=element_blank(),axis.text.x=element_blank())  +
    geom_segment(aes(x=10,xend=30,y=posL$position,yend=posL$labpos)) +
    geom_segment(aes(x=0,xend=10,y=posL$position,yend=posL$position)) +
    labs(x="",y="cM",title="B")

    #annotation_custom(grob = textGrob(label = "B", hjust = 0, gp = gpar(cex = 1)), ymin = 4.4, ymax = 4.4, xmin = -24, xmax = -24)

gt2 <- ggplot_gtable(ggplot_build(p2))
gt2$layout$clip[gt2$layout$name == "panel"] <- "off"
#grid.draw(gt2)

dat_text <- data.frame(
  variable = c("Masc")
)

gt3 <- ggplot(data=Z_all_melt,aes(value,colour=Data))+
    geom_line(stat='density') + facet_wrap(~variable,ncol=1,switch="both") + theme_bw() + guides(colour=FALSE) +
    geom_vline(xintercept=-0.264701535,linetype="dotted") +
    theme(plot.title = element_text(hjust=-0.16,size=12),panel.grid.minor=element_blank(), strip.background = element_blank(),strip.text.y = element_blank(),plot.margin=unit(c(10,0,9,2),unit="pt")) +
    labs(x="Decline in Hd",y="Density",title="A") + scale_color_manual(values = c("limegreen", "goldenrod"))

    #annotation_custom(data=dat_text,grob = textGrob(label = "A", hjust = 0, gp = gpar(cex = 1)), ymin = 4, ymax = 4, xmin = -0, xmax = -0)


cairo_pdf("eLD_plot_extended.pdf",width=9,height=4.5)
grid.arrange(gt3,gt2,gt,ncol=3,widths = c(0.8,0.4,1.6))
dev.off()


#####
#
####
#
###
#
##
#
#####
#
####
#
###
#
##
#
#
#####
#
####
#
###
#
##
#

#Simulated eLD

frbind = function(df1, df2) {
    colnames(df2) = colnames(df1)
    rbind(df1, df2)
}

EPS_total <- data.frame(Var1=character(),Var2=character(),value=numeric())



for (File in 0:99){

Sim1 <- read.table(paste("~/PycharmProjects/simuPOP/Simulation_output_pops/pop",File,".pop",sep=""),header=FALSE,skip=11)

Sim2 <- frbind(Sim1[,3:10],Sim1[,12:19])

Sim2$C2745 <- ifelse(as.numeric(Sim1$V3)>=10,paste("C2745",Sim1$V3,sep="_"),paste("C2745",Sim1$V3,sep="_0"))
Sim2$Kettin <- ifelse(as.numeric(Sim1$V4)>=10,paste("Kettin",Sim1$V4,sep="_"),paste("Kettin",Sim1$V4,sep="_0"))
Sim2$Tpi <- ifelse(as.numeric(Sim1$V5)>=10,paste("Tpi",Sim1$V5,sep="_"),paste("Tpi",Sim1$V5,sep="_0"))
Sim2$Catalase <- ifelse(as.numeric(Sim1$V6)>=10,paste("Catalase",Sim1$V6,sep="_"),paste("Catalase",Sim1$V6,sep="_0"))
Sim2$C5197 <- ifelse(as.numeric(Sim1$V7)>=10,paste("C5197",Sim1$V7,sep="_"),paste("C5197",Sim1$V7,sep="_0"))
Sim2$Masc <- ifelse(as.numeric(Sim1$V8)>=10,paste("Masc",Sim1$V8,sep="_"),paste("Masc",Sim1$V8,sep="_0"))
Sim2$Shaker <- ifelse(as.numeric(Sim1$V9)>=10,paste("Shaker",Sim1$V9,sep="_"),paste("Shaker",Sim1$V9,sep="_0"))
Sim2$Ldh <- ifelse(as.numeric(Sim1$V10)>=10,paste("Ldh",Sim1$V10,sep="_"),paste("Ldh",Sim1$V10,sep="_0"))

Sim2$id <- row.names(Sim2)

Sim3 <- Sim2[,9:17]

Sim3_melt <- melt(Sim3, id.var = "id")
Sim3_bin <-  t(with(Sim3_melt, table(id, value)))
Sim3_bin[Sim3_bin==1] <- "P"
Sim3_bin[Sim3_bin==0] <- "A"

Sim4 <- cbind("M",rownames(Sim3_bin),Sim3_bin)



Sim5 <- rbind(c("P","Pedigree",colnames(Sim4)[-1:-2]),c("I","id",colnames(Sim4)[-1:-2]),c("fID","Father",rep(0,ncol(Sim4))[-1:-2]),c("mID","Mother",rep(0,ncol(Sim4))[-1:-2]),c("C","gender",rep(1,ncol(Sim4))[-1:-2]),Sim4)
colnames(Sim5) <- NULL
rownames(Sim5) <- NULL

Data <-as.data.frame(Sim5)




#rm(list = ls(all = TRUE));

## Settings ##
setwd("./"); ## set your working directory ##
#head <- "Z_haps06.v1"; ## prefix of the input HLA allele data (in the SNP2HLA imputation reference format) ##
Thres <- 0.05; ## MAF threshold for combining the rare alleles ##
Iter <- 10000; ## No. iterations to estimate null distributin of epsilon ##

#Data <-read.table(paste(head, ".bgl.phased", sep=""), header=F, sep="\t");prefix <- paste(head, ".bgl.phased.eps", sep="");
Fix <- (length(Data[1,])-2)/2;
HLA <- c("C2745_", "Kettin_", "Tpi_", "Catalase_", "C5197_", "Masc_", "Shaker_", "Ldh_");numHLA <- length(HLA);

PermOrd<- function(Fix) {
    flagR <- rep(F, Fix*2);
    Seq <- 1:Fix;
    ord <-  order(runif(Fix));
    flagR[Seq*2-1] <- ord*2-1;
    flagR[Seq*2] <- ord*2;
    return(flagR)
}

#outfile <- paste(prefix, "_FreqThres", Thres, "_N", Fix, "_Iter", Iter, ".txt", sep="");
#outfile1 <- paste(prefix, "_FreqThres", Thres, "_N", Fix, "_Iter", Iter, ".Matrix.txt", sep="");
#outfile2 <- paste(prefix, "_FreqThres", Thres, "_N", Fix, "_Iter", Iter, "_Haplo.txt", sep="");

Geno <- Data[6:length(Data[,1]),3:length(Data[1,])]=="P";Geno <- Geno[, PermOrd(length(Geno[1,])/2)[1:(Fix*2)]];numSample <- length(Geno[1,]);
Allele <- Data[6:length(Data[,1]),2];N <- length(Allele);
Freq <- rowSums(Geno==T)/length(Geno[1,]);

flagHLA <-matrix(rep(F, length(Allele)*numHLA), ncol=numHLA);
flagFreq <- Freq>=Thres;
for (i in 1:numHLA) {
    flagHLA[substr(Allele, 1, nchar(HLA[i]))==HLA[i],i] <- T;
}

NQC <- sum(flagFreq)+numHLA;
GenoQC <- matrix(rep(F, NQC*numSample), ncol=numSample);
AlleleQC <- rep(0, NQC);
flagHLAQC <-matrix(rep(F, length(AlleleQC)*numHLA), ncol=numHLA);

GenoQC[1:sum(flagFreq), ] <- Geno[flagFreq,]
AlleleQC[1:sum(flagFreq)] <- as.vector(Allele[flagFreq]);

counter <- sum(flagFreq);
for (i in 1:numHLA) {
    flagT <- flagHLA[,i] & !flagFreq;
    if (sum(flagT)>=2) {
        GenoQC[counter+i,] <- apply(Geno[flagT,], 2, any);
        AlleleQC[counter+i] <- paste(HLA[i], "rare", sep="");
    } else if (sum(flagT)==1) {
        GenoQC[counter+i,] <- Geno[flagT,];
        AlleleQC[counter+i] <- paste(HLA[i], "rare", sep="");
    } else {
        AlleleQC[counter+i] <- "-";
    }

    if (sum(GenoQC[counter+i,])==0) {
        AlleleQC[counter+i] <- "-";
    }
}

flagHLAQC <-matrix(rep(F, length(AlleleQC)*numHLA), ncol=numHLA);
FreqQC <- rowSums(GenoQC==T)/length(GenoQC[1,]);
for (i in 1:numHLA) {
    flagHLAQC[substr(AlleleQC, 1, nchar(HLA[i]))==HLA[i],i] <- T;
}

CalcEps<- function(Num1, Num2, Geno1, Geno2, numSample) {
    S_Exp <- 0;
    S_Obs <- 0;

    for (k in 1:Num1) {
        for (l in 1:Num2) {

            FreqExp <- sum(Geno1[k,])*sum(Geno2[l,])/numSample^2;
            FreqObs <- sum(Geno1[k,] & Geno2[l,])/numSample;

            S_Exp <- S_Exp + -1*FreqExp*log(FreqExp, 2);
            if (FreqObs>0) {
                S_Obs <- S_Obs + -1*FreqObs*log(FreqObs, 2);
            }
        }
    }
    eps <- 1-S_Obs/S_Exp;
    if (Num1==1 | Num2==1) {
        eps = 0;
    }
    return(eps)
}

EPS <- matrix(numeric(choose(numHLA, 2)*5), ncol=5);
tmpEPS <- matrix(numeric(choose(numHLA,2)*Iter), ncol=choose(numHLA,2));
EPSmat <- matrix(numeric(numHLA^2), ncol=numHLA);

out <- paste("HLA1", "NumAllele", "HLA2", "NumAllele", "Epsilon", "Epsilon_mean_in_null", "Epsilon_sd_mean_in_null", "Z-score", sep="\t");
#write(out, file=outfile, append=F);

counter <- 1;
out <- "Iter";
for (i in 1:(numHLA-1)) {
    for (j in (i+1):numHLA) {
        EPS[counter,1] <- HLA[i];
        EPS[counter,2] <- sum(flagHLAQC[,i]);
        EPS[counter,3] <- HLA[j];
        EPS[counter,4] <- sum(flagHLAQC[,j]);

        Geno1 <- GenoQC[flagHLAQC[,i],];
        Geno2 <- GenoQC[flagHLAQC[,j],];
        Num1 <- sum(flagHLAQC[,i]);
        Num2 <- sum(flagHLAQC[,j]);

        if(Num1==1) {
            Geno1 <- matrix(as.vector(Geno1), ncol=length(Geno1));
        }
        if(Num2==1) {
            Geno2 <- matrix(as.vector(Geno2), ncol=length(Geno2));
        }

        tmpEPSval <- CalcEps(Num1, Num2, Geno1, Geno2, numSample);
        EPS[counter,5] <- tmpEPSval;
        EPSmat[i,j] <- as.numeric(tmpEPSval);
        EPSmat[j,i] <- as.numeric(tmpEPSval);

        for (k in 1:Iter) {
            tmpGeno2 <-  Geno2[, PermOrd(Fix)];
            if(Num2==1) {
                tmpGeno2 <- matrix(as.vector(tmpGeno2), ncol=length(tmpGeno2));
            }
            tmpEPS[k, counter] <- CalcEps(Num1, Num2, Geno1, tmpGeno2, numSample);
        }

        out <- paste(out, paste(HLA[i], "-", HLA[j], sep=""), sep="\t");

        counter <- counter+1;
    }
}

colnames(EPSmat) <- HLA
rownames(EPSmat) <- HLA

EPSmat_melt<- melt(EPSmat)
EPS_total <- rbind(EPS_total,EPSmat_melt)

}


g <- aggregate(EPS_total$value, by=list(EPS_total$Var1,EPS_total$Var2), function(x) c(mean = mean(x), sd = sd(x)))

#write.table(file=outfile, cbind(EPS, apply(tmpEPS, 2, mean), apply(tmpEPS, 2, sd), (as.numeric(EPS[,5])-apply(tmpEPS, 2, mean))/apply(tmpEPS, 2, sd)), quote=F, col.names=F, row.names=F, sep="\t", append=T);
#write.table(EPSmat, file=outfile1, sep="\t", quote=F, row.names=F, col.names=HLA)

# DataQC <- matrix(rep("-", Fix*(numHLA)), ncol=numHLA);
# DataQC2 <- matrix(rep("-", Fix*(numHLA)*2), ncol=numHLA);
# HapQC <- numeric(Fix*2);
#
# for (i in 1:numHLA) {
#     tmpAllele <- AlleleQC[flagHLAQC[,i]];
#
#     for (j in 1:Fix) {
#         A1 <- "NA";
#         A2 <- "NA";
#         tmpA1 <- tmpAllele[GenoQC[flagHLAQC[,i],j*2-1]];
#         tmpA2 <- tmpAllele[GenoQC[flagHLAQC[,i],j*2]];
#         if (length(tmpA1)==1) {
#             A1 <- substr(tmpA1, nchar(tmpA1)-4, nchar(tmpA1));
#         }
#         if (length(tmpA2)==1) {
#             A2 <- substr(tmpA2, nchar(tmpA2)-4, nchar(tmpA2));
#         }
#         DataQC[j, i] <- paste(A1, A2, sep=" ");
#         DataQC2[j*2-1, i] <- A1;
#         DataQC2[j*2, i] <- A2;
#
#     }
# }
#
# for (i in 1:(Fix*2)) {
#     HapQC[i] <- paste(DataQC2[i,], collapse="-");
# }
# UniHapQC <- unique(HapQC);
# UniHapQCFreq <- numeric(length(UniHapQC));
# for (i in 1:length(UniHapQC)) {
#     UniHapQCFreq[i] <- sum(HapQC==UniHapQC[i])/length(HapQC);
# }

#write.table( cbind(UniHapQC, UniHapQCFreq), file=outfile2, sep="\t", col.names=F, row.names=F, quote=F);
