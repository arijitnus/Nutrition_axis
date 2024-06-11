#Clear data
rm(list = ls())
graphics.off()


#Set back when done
#setwd("D:/AR_Sulf_MM_network")
setwd("/Users/ooiqien/Desktop/R Projects/AR_Sulf_MM_network")



#https://rdrr.io/cran/microeco/man/trans_network.html


####################################################################

#Preparation of data:

#Usual necessary packages
library(dada2)
library(dplyr)
library(ggplot2) 
library(phyloseq)
library(microbiome)
library(tidyverse)
library(RColorBrewer)

#Recommended for networks
library(microeco)
#install.packages("file2meco")
#install.packages("rgexf")
library(file2meco)
library(rgexf)



#Import rds first time. Note: updated with new rds file
#NPhylo.Sulf <- readRDS("AR_phyloseq_obj.rds")

#After adding missforest data in excel/ tidy data
SulfMet <- read.csv("AR_Sulf_Met.csv")

#Recompile Phyloseq
OTU <- otu_table(NPhylo.Main)
TX <- tax_table(NPhylo.Main)
MET = sample_data(SulfMet)

#Ensure sample names are variables
row.names(MET) <- MET$Samples

#New phyloseq
NPhylo.Sulf <- phyloseq(OTU, TX, MET)

#inspect phyloseq
NPhylo.Sulf

#Update rds object
#saveRDS(NPhylo.Sulf, "AR_phyloseq_updated.rds")

#Analysis starts here with updated rds
NPhylo.Sulf <- readRDS("AR_phyloseq_updated.rds")

#filter away usual contaminants, inspect
NPhylo.Filt <- subset_taxa(NPhylo.Sulf, Order != "Chloroplast" | is.na(Order))
NPhylo.Filt <- subset_taxa(NPhylo.Filt, Family != "Mitochondria" | is.na(Family))
NPhylo.Filt <- subset_taxa(NPhylo.Filt, Kingdom != "NA")
NPhylo.Filt <- filter_taxa(NPhylo.Filt, function (x) {sum(x > 0) > 0}, prune=TRUE)
NPhylo.Filt

#metadata table check
Sulf <- sample_data(NPhylo.Filt)

#Sub-object with normal + high levels
NPhylo.nhigh <- subset_samples(NPhylo.Filt, sulfate_levels != "low") #not low
NPhylo.nhigh <- filter_taxa(NPhylo.nhigh, function (x) {sum(x > 0) > 0}, prune=TRUE)
NPhylo.nhigh

#Sub-object with normal + low levels
NPhylo.nlow <- subset_samples(NPhylo.Filt, sulfate_levels != "high") #not high
NPhylo.nlow <- filter_taxa(NPhylo.nlow, function (x) {sum(x > 0) > 0}, prune=TRUE)
NPhylo.nlow

#conversion into microeco objects
nhM <- phyloseq2meco(NPhylo.nhigh)
nlM <- phyloseq2meco(NPhylo.nlow)

#Create a trans-network object in subset
nhM.t <- trans_network$new(dataset = nhM, cor_method = "spearman", filter_thres = 0.0005)
#322 

#Formula for relative abundance used in calculations:
minTotRelAbun = 0.0005
x = taxa_sums(NPhylo.nhigh)
keepTaxa = (x / sum(x)) > minTotRelAbun
prunedSet = prune_taxa(keepTaxa, NPhylo.nhigh)
prunedSet
#322

#Create a trans-network object in subset
nlM.t <- trans_network$new(dataset = nlM, cor_method = "spearman", filter_thres = 0.0005)
#315

#Formula for relative abundance used in calculations:
minTotRelAbun = 0.0005
x = taxa_sums(NPhylo.nlow)
keepTaxa = (x / sum(x)) > minTotRelAbun
prunedSet2 = prune_taxa(keepTaxa, NPhylo.nlow)
prunedSet2
#315

# construct network; require igraph package
nhM.t$cal_network(COR_p_thres = 0.01, COR_optimization = TRUE)
nlM.t$cal_network(COR_p_thres = 0.01, COR_optimization = TRUE)
#0.13 for nh 
#0.66 for nl

#Check status
nhM.t$res_network
nlM.t$res_network

# invoke igraph cluster_fast_greedy function for this undirected network 
nhM.t$cal_module(method = "cluster_fast_greedy")
nlM.t$cal_module(method = "cluster_fast_greedy")

#gephi export
#install.packages("rgexf")
library(rgexf)
# require rgexf package to be installed
nhM.t$save_network(filepath = "AR_SPman_high_110424.gexf")
nlM.t$save_network(filepath = "AR_SPman_low_110424.gexf")


#calculate and display network attributes
nhM.t$cal_network_attr()
nhM.t$res_network_attr

# get node properties
nhM.t$get_node_table(node_roles = TRUE)
nhM.t$res_node_table

# get edge properties
nhM.t$get_edge_table()
# return t1$res_edge_table 
nhM.t$get_adjacency_matrix()
# return t1$res_adjacency_matrix

# add_label = TRUE can be used to directly add text label for points
nhM.t$plot_taxa_roles(use_type = 1) + aes(add_label = TRUE) + ggtitle("normal to high")

ggsave(filename = "AR_ZiPi_nhigh_SPman_110424.png", width = 6, height = 5)

#Redo the same for normal-low
nlM.t$cal_network_attr()
nlM.t$res_network_attr

# get node properties
nlM.t$get_node_table(node_roles = TRUE)
nlM.t$res_node_table

# get edge properties
nlM.t$get_edge_table()
# return t1$res_edge_table 

nlM.t$get_adjacency_matrix()
# return t1$res_adjacency_matrix

# add_label = TRUE can be used to directly add text label for points
nlM.t$plot_taxa_roles(use_type = 1) + aes(add_label = TRUE) + ggtitle("normal to low")

ggsave(filename = "AR_ZiPi_nlow_SPman_110424.png", width = 6, height = 5)

#Random bootstrapped network stats

nhM.t$random_network(runs = 100, output_sim = FALSE)
#RM: 0.246

nlM.t$random_network(runs = 100, output_sim = FALSE)
#RM: 0.139


#What if: we use the same cutoff
nhM.t2 <- trans_network$new(dataset = nhM, cor_method = "spearman", filter_thres = 0.0005)
#322 
nhM.t2$cal_network(COR_p_thres = 0.66)
#Forced
nhM.t2$cal_module(method = "cluster_fast_greedy")
nhM.t2$save_network(filepath = "AR_SPman_experimental_110424.gexf")

nhM.t2$random_network(runs = 100, output_sim = FALSE)
#RM: 0.23115

#Worse RM score, more spurious edges

#Other options:

#Random subsampling till equal sample count as n-low group? 
#Check stability of random subsampled networks?


#Multiple comparisons across various networks

#install.packages("meconetcomp")
library(meconetcomp)
library(magrittr)

#Convert for Networks
AR.MEco <- phyloseq2meco(NPhylo.AR)
# first create a list
AR_network <- list()
# select samples of high group
# use clone to get a deep copy of soil_amp (R6 object)
tmp <- clone(AR.MEco)
# change sample_table directly
tmp$sample_table %<>% subset(sulfate_levels != "low")
# trim all files in the object
tmp$tidy_dataset()
# use filter_thres parameter to filter the feature with low relative abundance
tmp <- trans_network$new(dataset = tmp, cor_method = "spearman", filter_thres = 0.0005)
#322
# COR_p_thres represents the p value threshold
# COR_cut denotes the correlation coefficient threshold
tmp$cal_network(COR_p_thres = 0.01, COR_optimization = TRUE)
#0.13
# put the network into the list
AR_network$normhigh <- tmp


# select samples of normal +low group
tmp <- clone(AR.MEco)
tmp$sample_table %<>% subset(sulfate_levels != "high")
tmp$tidy_dataset()
tmp <- trans_network$new(dataset = tmp, cor_method = "spearman", filter_thres = 0.0005)
#315
tmp$cal_network(COR_p_thres = 0.01, COR_optimization = TRUE)
#0.66
AR_network$normlow <- tmp


#Calculate modules together
AR_network %<>% cal_module(undirected_method = "cluster_fast_greedy")
tmp <- cal_network_attr(AR_network)
# tmp is a data.frame object

AR_network %<>% get_node_table(node_roles = TRUE) %>% get_edge_table

# obtain the node distributions by searching the res_node_table in the object
tmp <- node_comp(AR_network, property = "name")
# obtain nodes intersection
tmp1 <- trans_venn$new(tmp, ratio = "numratio")
g1 <- tmp1$plot_venn(fill_color = FALSE)
g1
ggsave("AM_node_overlap_110424.pdf", g1, width = 7, height = 6)
# calculate jaccard distance to reflect the overall differences of networks
tmp$cal_betadiv(method = "jaccard")
tmp$beta_diversity$jaccard

# get the edge distributions across networks
tmp <- edge_comp(AR_network)
# obtain edges intersection
tmp1 <- trans_venn$new(tmp, ratio = "numratio")
g1 <- tmp1$plot_venn(fill_color = FALSE)
g1
ggsave("AM_edge_overlap_110424.pdf", g1, width = 7, height = 6)
# calculate jaccard distance
tmp$cal_betadiv(method = "jaccard")
tmp$beta_diversity$jaccard

# first obtain edges distribution and intersection
tmp <- edge_comp(AR_network)
tmp1 <- trans_venn$new(tmp)
# convert intersection result to a microtable object
tmp2 <- tmp1$trans_comm()
# extract the intersection of all the networks
# please use colnames(tmp2$otu_table) to find the required name
colnames(tmp2$otu_table)
Intersec_all <- subset_network(AR_network, venn = tmp2, name = "normhigh&normlow")
# Intersec_all is a trans_network object
# for example, save Intersec_all as gexf format
Intersec_all$save_network("AR_Intersec_all_110424.gexf")


#most influential nodes
install.packages("pheatmap")
library(pheatmap)
soil_amp_network_edgetax <- edge_tax_comp(AR_network, taxrank = "Phylum", label = "+", rel = TRUE)
# filter the features with small number
soil_amp_network_edgetax <- soil_amp_network_edgetax[apply(soil_amp_network_edgetax, 1, mean) > 0.01, ]
# visualization
g1 <- pheatmap::pheatmap(soil_amp_network_edgetax, display_numbers = TRUE)



#robustness analysis - efficiency and resilience
?robustness
#Eff - Efficiency, number of links to move from one end to another
#Eigen - Measure of Resilience

tmp <- robustness$new(AR_network, remove_strategy = c("node_rand", "node_hub", "node_degree_high"), 
                      remove_ratio = seq(0, 0.99, 0.1), measure = c("Eff", "Eigen"), run = 10)
View(tmp$res_table)
tmp$plot(linewidth = 1) + ggtitle("Network stability under differing Sulfate treatments")

ggsave(filename = "AR_SPman_no_vs_wt_Sulf_110424.png", width = 8, height = 5)

#https://www.nature.com/articles/s41598-023-45218-9

#Network vulnerability
vul_table <- vulnerability(AR_network)
View(vul_table)

write.csv(vul_table, "AR_SPman_vul_no_vs_wt_Sulf_test_110424.csv")








