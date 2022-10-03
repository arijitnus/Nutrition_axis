###Analyzing the positive mode UPLC QTOF root exudates data using xcms
library(xcms)
pos_MS1<-list.files()
pos_MS1<-pos_MS1[c(1,2,6,7,8,9,10,11)]
xset<-xcmsSet(method="centWave",ppm =15, peakwidth=c(5,20), prefilter=c(0,0),snthresh=10)
xset2<-retcor(xset,method="obiwarp",plottype="none",profStep=0.91)#profStep 1 throws error
xset3<-group(xset2,bw=2,minsamp=3,mzwid=0.025)
xset4<-fillPeaks(xset3)
peaklist<-peakTable(xset4,filebase="posMS1_15ppm")

#repeat the same for negative MS 1 samples
neg_MS1<-list.files()
neg_MS1
neg_MS1<-neg_MS1[-c(7,8)]
xset_neg<-xcmsSet(method="centWave",ppm =15, peakwidth=c(5,20), prefilter=c(0,0),snthresh=10)
xset2_neg<-retcor(xset_neg,method="obiwarp",plottype="none",profStep=0.91)
xset3_neg<-group(xset2_neg,bw=2,minsamp=3,mzwid=0.025)
xset4_neg<-fillPeaks(xset3_neg)
peaklist<-peakTable(xset4_neg,filebase="negMS1_15ppm")

#this gives the peaktable with features (rows) and samples (columns)
library(readxl)
#read in peak table
pos_tab<-read_excel("/Users/arijitmukherjee/Documents/Root_exudates_Aug2022/mzML16082022/positive MS1_new/Feature_table.xlsx",sheet="15ppm_pos",col_names = T,
                    skip = 0)
head(pos_tab)
neg_tab<-read_excel("/Users/arijitmukherjee/Documents/Root_exudates_Aug2022/mzML16082022/positive MS1_new/Feature_table.xlsx",sheet="15ppm_neg",col_names = T,
                    skip = 0)
head(neg_tab)
dim(neg_tab)
names(pos_tab)
# we can replace the values of 0 with half minimum intensity for each of the features
#create separate df based on all metadata and use them only for retrieving later
replacezero<-function(x)"[<-"(x,!x|is.na(x),min(x[x>0],na.rm=TRUE)/2)
pos_tab2<-pos_tab[,c(1,4,9:17)]
neg_tab2<-neg_tab[,c(1,4,9:17)]
head(pos_tab2)
pos_tab2$features<-paste(pos_tab2$mz,pos_tab2$rt,sep = "_")
class(pos_tab2$features)
neg_tab2$features<-paste(neg_tab2$mz,neg_tab2$rt,sep = "_")

pos_tab2<-pos_tab2[,-c(1,2)]
neg_tab2<-neg_tab2[,-c(1,2)]
head(neg_tab2)
class(pos_tab2)
pos_tab2<-as.data.frame(pos_tab2)
neg_tab2<-as.data.frame(neg_tab2)
###Convert the features to rownames
rownames(pos_tab2)<-pos_tab2$features
rownames(neg_tab2)<-neg_tab2$features
pos_tab2<-pos_tab2[,-10]
neg_tab2<-neg_tab2[,-10]
#rownmaes as features and columns are samples
head(pos_tab2[,1:2])

#Now perform log transformation and pareto scaling
class(pos_tab2)
log_pos_tab2<-log2(pos_tab2+1)
log_neg_tab2<-log2(neg_tab2+1)

paretoscale<-function(z){
  rowmean<-apply(z, 1, mean)
  rowsd<-apply(z, 1, sd)
  rowsqrtsd<-sqrt(rowsd)
  rv<-sweep(z,1,rowmean,"-")
  rv<-sweep(z,1,rowsd,"/")
  return(rv)
}
pareto.log_pos<-paretoscale(log_pos_tab2)
pareto.log_neg<-paretoscale(log_neg_tab2)

pca_pos<-prcomp(t(pareto.log_pos),center = F,scale. = F)
pca_neg<-prcomp(t(pareto.log_neg),center = F,scale. = F)


pca_pos_df<-pca_pos$x
pca_pos_df<-as.data.frame(pca_pos_df)
head(pca_pos_df)
pca_pos_df$genotypes<-factor(c(rep("MSA",3),rep("SDI",3),rep("Col-0",3)))
head(pca_pos_df)

names(pca_pos_df)
library(ggplot2)

eigs_neg<-pca_neg$sdev^2
eigs_neg[1]/sum(eigs_neg)
eigs_neg[2]/sum(eigs_neg)

eigs_neg


p<-ggplot(pca_neg_df,aes(x=PC1,y=PC2,col=as.factor(genotypes)))+
  geom_point()+
  geom_polygon(alpha=0.1)+
  theme_classic()+
  xlab("PC1 (99.8%)")+
  ylab("PC2(1%)")

p


pca_neg_df<-pca_neg$x
pca_neg_df<-as.data.frame(pca_neg_df)
head(pca_neg_df)

pca_neg_df$genotypes<-factor(c(rep("Col-0",3),rep("MSA",3),rep("SDI",3)))



#No apparent differences in PCA plot for the genotypes
#perform t-test and find out differential features
pvals_pos<-apply(log_pos_tab2, 1, function(x){t.test(x[1:3],x[7:9])$p.value})
pval_pos_BHcorr<-p.adjust(pvals_pos,method = "BH")

pvals_pos
sum(pval_pos_BHcorr<0.1)

sum(pvals_pos<0.01)#31 features <0.01 p-value
log_pos_tab2$pval<-pvals_pos
log_pos_tab2$pval_BH<-pval_pos_BHcorr



head(log_neg_tab2)
pvals_neg<-apply(log_neg_tab2, 1, function(x){t.test(x[1:3],x[4:6])$p.value})
pval_neg_BHcorr<-p.adjust(pvals_neg,method = "BH")

sum(pval_neg_BHcorr<0.05)#102; <0.05= 82 features
sum(pvals_neg<0.01)#233

pvals_neg<0.01

log_neg_tab2$pval<-pvals_neg
log_neg_tab2$pvals_BH<-pval_neg_BHcorr

write.table(log_neg_tab2,"negativeMS1_stats_15ppm.tsv",sep = "\t")
write.table(log_pos_tab2,"positiveMS1_stats_15ppm.tsv",sep = "\t")














