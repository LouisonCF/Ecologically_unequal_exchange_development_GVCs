rm(list=ls(all=TRUE))#Erase from R environment all variables created previously
cat("\014") #Clean the terminal
library(FactoMineR)
library(FactoInvestigate)
library(dplyr)
library(readxl)
library(tidyverse)
library(openxlsx)
library(countrycode)
library(ggplot2)
library(EFAtools)
library(RcmdrMisc)
library(plotly)
library(htmlwidgets)
library(fmsb)

#options(device="RStudioGD")
options(ggrepel.max.overlaps = Inf)

#This code performs the statistical analysis for the article "Ecologically unequal exchange and uneven development patterns along global value chains" published in World Development. It reproduces the clustering, export the file with the clustering data and generates figures 2 (radar graph) and 3 (3D graph of cluster) in the paper

#Authors: Jeff Althouse, Louison Cahen-Fourot, Bruno Carballa-Smichowski, Cédric Durand, Steven Knauss

#Questions on the code can be addressed to Louison Cahen-Fourot (lcahenfo@ruc.dk) or Steven Knauss for the 3D graph part (steven.knauss@wu.ac.at)

#Make sure you have the data set "Data_EUE_GVC_index_variables.xlsx" that you find in the same folder. This file contains the index variables upon which the clustering is performed.

#Make sure your R version and the packages are up-to-date! This code was tested and ran fine under R 4.2.2.

#Dataset preparation----
eue_gvc <- read_excel("Data_EUE_GVC_index_variables.xlsx") #importing data set with index variables
eue_gvc <- eue_gvc %>% data.frame(row.names="ctr") %>% select(-ctr_full) #making ctr the column with row names and removing ctr_full to keep only numeric variables in the datafile.

#######Correlations matrix between synthetic dimension variables, and Keyser-Meyer-Ohlin and Bartlett tests of data suitability for factor analysis----
eue_gvc_corr <- rcorr.adjust(eue_gvc[,c("GVCpart_index", "GVCvalcap_index", "proddev_index", "socecon_index", "dom_ecol_index", "rela_ecol_index")], type="pearson", use="complete")
print(eue_gvc_corr)
eue_gvc_corr_pearsons <- as.data.frame(eue_gvc_corr$R$r)
eue_gvc_corr_pvalues <- as.data.frame(eue_gvc_corr$P.unadj)
eue_gvc_corr_adjustpvalues <- as.data.frame(eue_gvc_corr$P)

kmo_res <- KMO(eue_gvc,use=c("everything"),cor_method = c("pearson"))
#cor_method = c("pearson", "spearman", "kendall")
print(kmo_res, stats = c("both"), vars = "all", sort = FALSE, show = "all", digits = getOption("digits"))

bartlett_res <- BARTLETT(eue_gvc,cor_method = c("pearson"))
#cor_method = c("pearson", "spearman", "kendall")
print(bartlett_res, stats = c("both"), vars = "all", sort = FALSE, show = "all", digits = getOption("digits"))

kmo_bartlett_res <- rbind(data.frame(KMO=kmo_res$KMO_i),data.frame(KMO=kmo_res$KMO))
kmo_bartlett_res <- cbind(kmo_bartlett_res,data.frame(Bartlett_Chi2 = bartlett_res$chisq))
kmo_bartlett_res <- cbind(kmo_bartlett_res,data.frame(Bartlett_pvalue = bartlett_res$p_value))
kmo_bartlett_res <- cbind(kmo_bartlett_res,data.frame(Bartlett_df = bartlett_res$df))
row.names(kmo_bartlett_res) <- c("GVC_participation_kmo","GVC_valuecapture_kmo","Productive_dev_kmo","Socecon_dev_kmo","Dom_ecol_impact_kmo","External_ecol_balance_kmo","Overall_kmo")
kmo_bartlett_res <- rownames_to_column(kmo_bartlett_res,"Variable")

write.xlsx(kmo_bartlett_res,file="Keyser-Meyer-Ohlin_Bartlett_tests.xlsx", overwrite=TRUE)

####PCA----
eue_gvc_pca<-PCA(eue_gvc, scale.unit=TRUE, ncp=NULL, graph = FALSE)#Performing a 1st PCA to determine how many axes to keep for clustering.

out<-outliers(eue_gvc_pca, file = "", Vselec = "cos2", Vcoef = 1, nmax = 10)#detecting possible outliers
outN<-out$N
outID<-out$ID

nb <- inertiaDistrib(eue_gvc_pca, file = "", ncp = NULL, q = 0.90, time = "100000L",parallel = TRUE, figure.title = "", graph = TRUE, options = NULL)#computing statistically significant axes to interpret and to keep for the clustering and storing into "nb" for further use
nb

Investigate(eue_gvc_pca,file="automatic_analysis_eue_gvc_pca", document = c("html_document"),parallel=TRUE)#generates an automatic analysis providing first elements of understanding. Ease the analysis of the PCA.

eue_gvc_pca<-PCA(eue_gvc, scale.unit=TRUE, ncp=nb+2, graph = FALSE,)#We run the PCA again to keep the number of significant axes determined above. Since the significant information is just above 50%, we can keep nb+1 axes to keep a bit more information while still removing most of the statistical noise. 

#Plotting PCA----
pdf(file="eue_gvc_pca_countries_12.pdf",paper="a4")
plot(eue_gvc_pca, axes=c(1, 2), choix="ind", habillage="none", col.ind="black", 
     col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
     new.plot=TRUE, title="Countries cloud, axes 1 and 2",auto="y",shadow=TRUE,cex=0.45,graph.type = c("classic"))
dev.off()

pdf(file="eue_gvc_pca_variables_12.pdf",paper="a4")
plot.PCA(eue_gvc_pca, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="black", 
                       col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0, 
                       title="Variables projection, axes 1 and 2",auto="y",shadow=TRUE,cex=0.7,graph.type = c("classic"))
dev.off()

pdf(file="eue_gvc_pca_countries_23.pdf",paper="a4")#before generating a graph with other dimensions than 1:2, check that you have kept more than two dimensions, which is the case here. Otherwise you will get the error message "subscript out of bounds" because there are no other dimensions to build the graph.
plot.PCA(eue_gvc_pca, axes=c(2, 3), choix="ind", habillage="none", col.ind="black", 
                     col.ind.sup="blue", col.quali="magenta", label=c("ind", "ind.sup", "quali"),
                    new.plot=TRUE, title="Countries cloud, axes 2 and 3",auto="y",shadow=TRUE,cex=0.45,graph.type = c("classic"))
dev.off()

pdf(file="eue_gvc_pca_variables_23.pdf",paper="a4")
plot.PCA(eue_gvc_pca, axes=c(2, 3), choix="var", new.plot=TRUE, col.var="black", 
                     col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0, 
                    title="Variables projection, axes 2 and 3",auto="y",shadow=TRUE,cex=0.7,graph.type = c("classic"))
dev.off()

#Printing and exporting PCA results----
summary(eue_gvc_pca, nb.dec = 3, nbelements=10, nbind = 10, ncp = NULL)

countries <- as.data.frame(eue_gvc_pca$ind)
countries <- rownames_to_column(countries,"ctr")
eigenvalues <- as.data.frame(eue_gvc_pca$eig)
eigenvalues <- rownames_to_column(eigenvalues,"Axis")
variables <- as.data.frame(eue_gvc_pca$var)
variables <- variables %>% rownames_to_column("var_index")
dim_desc <- dimdesc(eue_gvc_pca, axes=(1:(nb+2)), proba=1)#generating description of axes in terms of variables correlated to the axes. In dim_desc the "axes" argument needs to be set so it matches with the number of dimensions kept in the analysis as decided when calling PCA again after InertiaDistrib.

dim_desc_dim1 <- as.data.frame(dim_desc$Dim.1)
dim_desc_dim1 <- dim_desc_dim1 %>% rename(correlation_axis_1 = "quanti.correlation",p_value_axis_1="quanti.p.value") %>% rownames_to_column("var_index")
dim_desc_dim2 <- as.data.frame(dim_desc$Dim.2)
dim_desc_dim2 <- dim_desc_dim2 %>% rename(correlation_axis_2 = "quanti.correlation",p_value_axis_2="quanti.p.value") %>% rownames_to_column("var_index")
dim_desc_dim3 <- as.data.frame(dim_desc$Dim.3)
dim_desc_dim3 <- dim_desc_dim3 %>% rename(correlation_axis_3 = "quanti.correlation",p_value_axis_3="quanti.p.value") %>% rownames_to_column("var_index")

eue_gvc_pca_variables <- merge(dim_desc_dim1,dim_desc_dim2,by="var_index")
eue_gvc_pca_variables <- merge(eue_gvc_pca_variables,dim_desc_dim3,by="var_index")
eue_gvc_pca_variables <- merge(eue_gvc_pca_variables,variables,by = "var_index") 
eue_gvc_pca_variables <- select(eue_gvc_pca_variables, -contains("cor."))

list_eue_gvc_pca <- list("countries"=countries, "variables"=eue_gvc_pca_variables, "eigenvalues"=eigenvalues)
write.xlsx(list_eue_gvc_pca,file="eue_gvc_pca.xlsx",overwrite = TRUE)

#Clustering----

eue_gvc.hcpc<-HCPC(eue_gvc_pca,nb.clust=-1,consol=FALSE,min=3,max=10,nb.par=Inf,graph=TRUE,proba=1,iter.max=100000)#With option consol=FALSE to do a strict hierarchical clustering without k-means consolidation so to keep the hierarchical nature of the clustering. BEWARE: on some computers it can happen that the number of clusters generated does not match with the level of the bar on the plot. We could not figure out why: it seems to be a computer-specific issue (maybe an issue with the screen settings). In that case, turn the choice of number of clusters to automatic: set argument nb.clust on -1 for automatic choice of clusters (0 for manual choice).

#Plotting clustering----

pdf(file="eue_gvc_clustering_tree.pdf",paper="a4r")
plot.HCPC(eue_gvc.hcpc, choice="tree", rect=TRUE, ind.names=TRUE, title=NULL,
          new.plot=FALSE, tree.barplot=TRUE,cex=0.3)
dev.off()

pdf(file="eue_gvc_clustering_bar.pdf",paper="a4")
plot.HCPC(eue_gvc.hcpc, choice="bar", ind.names=TRUE, title=NULL,
          new.plot=FALSE, max.plot=15)
dev.off()

pdf(file="eue_gvc_clustering_clusters_12.pdf",paper="a4")
plot.HCPC(eue_gvc.hcpc, axes=c(1,2), choice="map",draw.tree=FALSE, ind.names=TRUE, title=NULL,
                   new.plot=FALSE,auto="y",shadow=TRUE)
dev.off()

pdf(file="eue_gvc_clustering_3D_12.pdf",paper="a4")
plot.HCPC(eue_gvc.hcpc, axes=c(1,2), choice="3D.map", ind.names=TRUE, title=NULL,
                   new.plot=FALSE, centers.plot=FALSE)
dev.off()

pdf(file="eue_gvc_clustering_clusters_23.pdf",paper="a4")
plot.HCPC(eue_gvc.hcpc, axes=c(2,3), choice="map",draw.tree=FALSE, ind.names=TRUE, title=NULL,
                  new.plot=FALSE, auto="y",shadow=TRUE)
dev.off()

pdf(file="eue_gvc_clustering_3D_23.pdf",paper="a4")
plot.HCPC(eue_gvc.hcpc, axes=c(2,3), choice="3D.map", ind.names=TRUE, title=NULL,new.plot=FALSE, centers.plot=FALSE)
dev.off()

#Printing and exporting clustering results----
countries_clust <- eue_gvc.hcpc$data.clust
countries_clust <- countries_clust %>% rownames_to_column("ctr") %>% rename(cluster = "clust")
countries_clust$ctr_full <- countrycode(countries_clust$ctr, origin="iso3c", destination = "country.name.en", warn = FALSE)

clusters_1 <- countries_clust %>% select(c(ctr, ctr_full, cluster)) %>% filter(cluster == 1)
clusters_1 <- as.data.frame(paste(clusters_1$ctr_full, collapse =", "))
clusters_1 <- rename(clusters_1, cluster_1= contains("paste"))
clusters_1 <- rownames_to_column(clusters_1,"Clusters")
clusters_2 <- countries_clust %>% select(c(ctr, ctr_full, cluster)) %>% filter(cluster == 2)
clusters_2 <- as.data.frame(paste(clusters_2$ctr_full, collapse =", "))
clusters_2 <- rename(clusters_2, cluster_2= contains("paste"))
clusters_2 <- rownames_to_column(clusters_2,"Clusters")
clusters_3 <- countries_clust %>% select(c(ctr, ctr_full, cluster)) %>% filter(cluster == 3)
clusters_3 <- as.data.frame(paste(clusters_3$ctr_full, collapse =", "))
clusters_3 <- rename(clusters_3, cluster_3= contains("paste"))
clusters_3 <- rownames_to_column(clusters_3,"Clusters")


clusters <- clusters_1 %>% left_join(clusters_2,by="Clusters") %>% left_join(clusters_3,by="Clusters")

variables_clust <- eue_gvc.hcpc$desc.var
print(variables_clust)
eta_var <- as.data.frame(variables_clust$quanti.var)
eta_var <- rownames_to_column(eta_var,"variable")
var_clust_1 <- as.data.frame(variables_clust$quanti$"1")#BEWARE:variables by cluster need first to be put in a distinct object for each cluster. Otherwise values will be merged according to the order of variables for the 1st cluster but variables order varies from one cluster to the other, therefore, the values for clusters other than the 1st will be wrong!
var_clust_1<- rownames_to_column(var_clust_1,"index variable")
colnames(var_clust_1)[2:7] <- paste(colnames(var_clust_1)[2:7], "1", sep = "_")
var_clust_2 <- as.data.frame(variables_clust$quanti$"2")
var_clust_2<- rownames_to_column(var_clust_2,"index variable")
colnames(var_clust_2)[2:7] <- paste(colnames(var_clust_2)[2:7], "2", sep = "_")
var_clust_3 <- as.data.frame(variables_clust$quanti$"3")
var_clust_3<- rownames_to_column(var_clust_3,"index variable")
colnames(var_clust_3)[2:7] <- paste(colnames(var_clust_3)[2:7], "3", sep = "_")

var_clust <- var_clust_1 %>% 
  left_join(var_clust_2,by="index variable") %>% 
  left_join(var_clust_3,by="index variable")

axes_clust <- eue_gvc.hcpc$desc.axes
print(axes_clust)
eta_axes <- as.data.frame(axes_clust$quanti.var)
eta_axes <- rownames_to_column(eta_axes,"axes")
axes_clust_1 <- as.data.frame(axes_clust$quanti$"1")
axes_clust_1 <- rownames_to_column(axes_clust_1,"axes")
colnames(axes_clust_1)[2:7] <- paste(colnames(axes_clust_1)[2:7], "1", sep = "_")
axes_clust_2 <- as.data.frame(axes_clust$quanti$"2")
axes_clust_2 <- rownames_to_column(axes_clust_2,"axes")
colnames(axes_clust_2)[2:7] <- paste(colnames(axes_clust_2)[2:7], "2", sep = "_")
axes_clust_3 <- as.data.frame(axes_clust$quanti$"3")
axes_clust_3 <- rownames_to_column(axes_clust_3,"axes")
colnames(axes_clust_3)[2:7] <- paste(colnames(axes_clust_3)[2:7], "3", sep = "_")

axes_clust <- axes_clust_1 %>% left_join(axes_clust_2,by="axes") %>% left_join(axes_clust_3,by="axes")

para_dist_clust <- eue_gvc.hcpc$desc.ind
print(para_dist_clust)

para_clust1 <- as.data.frame(para_dist_clust$para$"1")
para_clust1 <- para_clust1 %>% rownames_to_column("ctr") %>% rename(parangon_cluster_1=contains("para_dist"))
para_clust2 <- as.data.frame(para_dist_clust$para$"2")
para_clust2 <- para_clust2 %>% rownames_to_column("ctr") %>% rename(parangon_cluster_2=contains("para_dist"))
para_clust3 <- as.data.frame(para_dist_clust$para$"3")
para_clust3 <- para_clust3 %>% rownames_to_column("ctr") %>% rename(parangon_cluster_3=contains("para_dist"))

para_clust <- rownames_to_column(eue_gvc,"ctr")
para_clust <- para_clust %>% left_join(para_clust1,by="ctr") %>% left_join(para_clust2,by="ctr") %>% left_join(para_clust3,by="ctr") %>% select(-contains("_index"))

dist_clust1 <- as.data.frame(para_dist_clust$dist$"1")
dist_clust1 <- dist_clust1 %>% rownames_to_column("ctr") %>% rename(dist_cluster_1=contains("dist_"))
dist_clust2 <- as.data.frame(para_dist_clust$dist$"2")
dist_clust2 <- dist_clust2 %>% rownames_to_column("ctr") %>% rename(dist_cluster_2=contains("dist_"))
dist_clust3 <- as.data.frame(para_dist_clust$dist$"3")
dist_clust3 <- dist_clust3 %>% rownames_to_column("ctr") %>% rename(dist_cluster_3=contains("dist_"))

dist_clust <- rownames_to_column(eue_gvc,"ctr")
dist_clust <- dist_clust %>% left_join(dist_clust1,by="ctr") %>% left_join(dist_clust2,by="ctr") %>% left_join(dist_clust3,by="ctr") %>% select(-contains("_index"))

list_eue_gvc_clustering <- list("countries_clust"=countries_clust, "clusters"=clusters, "eta_var"=eta_var, "variables_clust"=var_clust,"axes_clust"=axes_clust,"parangons"=para_clust,"most_distinct"=dist_clust)
write.xlsx(list_eue_gvc_clustering,file="eue_gvc_clustering.xlsx",overwrite = TRUE)

#Radar graph (figure 2 in the article)----
##Radar graph in the article was generated with excel. The code below generates an R version.
var_clust_radar <- var_clust
var_clust_radar <- column_to_rownames(var_clust_radar,"index variable")
var_clust_radar <- var_clust_radar %>% 
  select(contains("Mean in category"))
var_clust_radar_transposed <- as.data.frame(t(var_clust_radar))

var_clust_radar_transposed <- rbind(
  
  c(.8,.8,.8,.8,.8,.8),
  
  c(-.6,-.6,-.6,-.6,-.6,-.6)
  
  ,var_clust_radar_transposed,
  c(0,0,0,0,0,0))

var_clust_radar_transposed <- var_clust_radar_transposed %>%
  select(dom_ecol_index,socecon_index,proddev_index,GVCpart_index,GVCvalcap_index,rela_ecol_index)

row.names(var_clust_radar_transposed) <- c("Minimum","Maximum","Cluster 1", "Cluster 2", "Cluster 3","Sample average")

radar <- radarchart(var_clust_radar_transposed, axistype = 4, seg = 7, pcol = c("gold","red","blue","black"),cglcol = "black", vlabels = c("Domestic ecological impact","Socio-economic development","Productive development","GVC participation","GVC value capture","External ecological balance"), vlcex = .6,axislabcol = "black",caxislabels = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8))
legend(
  x = -1.3, y = 1.35, legend = c("Cluster 1 - Curse of GVC marginalization","Cluster 2 - Ecologically-perverse upgrading", "Cluster 3 - Reproduction of the core", "Sample average"), horiz = TRUE,  bty = "n", pch = 16, col = c("gold", "red", "blue","black"),cex = 0.5, pt.cex = 2,text.width=c(.6,.65,.55,.5))




#3D graph of clusters (figure 3 in the article)----

datafor3dgraph <- merge(countries,countries_clust,by = "ctr")

datafor3dgraph <- select(datafor3dgraph, "ctr", contains("coord"), "cluster")

datafor3dgraph <- datafor3dgraph %>%
  mutate(itemlabels = ifelse(cluster==1, "Class 1", ifelse(cluster==2, "Class 2", "Class 3")))

t <- list(
  family = "sans serif",
  size = 10)

axx <- list(
  title = "<b>PC 1</b>",
  tickvals = c(-3, -2, -1, 0, 1, 2, 3),
  ticktext = c("<i>low GVC participation <br> high domestic ecol. impact</i>", "-2", "-1", "0", "1", "<i>high GVC participation <br> low domestic ecol. impact</i>", "3"),
  tickfont = list(size = 11)
)


axy <- list(
  title = "<b>PC 2</b>",
  tickvals = c(-3, -2, -1, 0, 1, 2, 3),
  ticktext = c("-3", "<i>low productive <br> development </i>", "-1", "0", "1", "2", "<i>high productive <br> development</i>"),
  tickfont = list(size = 11)
)

axz <- list(
  title = "<b>PC 3</b>",
  tickvals = c(-4, -3, -2, -1, 0, 1, 2, 3, 4),
  ticktext = c("<i>high GVC value capture <br> low external ecol. balance</i>", "-3", "-2", "-1", "0", "1", "2", "<i>low GVC value capture <br> high external ecol. balance</i>", "4"),
  tickfont = list(size = 11)
)


figure3 <- plot_ly(datafor3dgraph, x=~coord.Dim.1, y=~coord.Dim.2, z=~coord.Dim.3, color=~itemlabels, text=~ctr) %>%
  add_text(textfont = t, textposition = "top right") %>%
  layout(title = list(text = "Country coordinates on the retained axes", xanchor = "center", yanchor = "center", x = 0.5, y = 0.95), font=list(color="black", size = 15), scene = list(xaxis=axx,yaxis=axy,zaxis=axz), legend = list(traceorder = "normal", orientation = "v", xanchor = "center", yanchor = "center", x = 0.9, y = 0.6, title = list(text ="      Country<br>       cluster"), font=list(color="black", size = 11)))

figure3

