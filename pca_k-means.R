#By Reid Doctor

require(matrixStats)
library(cluster)


masculinity.data <- read.csv("/Users/rdoctor/Desktop/cogs\ 109\ hw/one_hot_responses.csv")
masculinity.data = masculinity.data[2:263]

masc.clust <- kmeans(masculinity.data, centers = 3)
dissE <- daisy(masculinity.data)
dE2 <- dissE^2
clust.silhouette <- silhouette(masc.clust$cluster, dE2)
plot(clust.silhouette)

#looking for especially large differences between clusters
cluster.1 = masculinity.data[pca5.clust$cluster==1,]
cluster.2 = masculinity.data[pca5.clust$cluster==2,]
cluster.3 = masculinity.data[pca5.clust$cluster==3,]
for (i in 1:262) {
  clust1.diff = mean(cluster.1[,i]) - mean(masculinity.data[,i])
  clust2.diff = mean(cluster.2[,i]) - mean(masculinity.data[,i])
  clust3.diff = mean(cluster.3[,i]) - mean(masculinity.data[,i])
  if (clust1.diff >= 0.3 | clust2.diff >= 0.3 | clust3.diff >= 0.3) {
    print(colnames(masculinity.data)[i])
    print(paste0("cluster 1 mean = ", mean(cluster.1[,i])))
    print(paste0("cluster 2 mean = ", mean(cluster.2[,i])))
    print(paste0("cluster 3 mean = ", mean(cluster.3[,i])))
  }
}


clust.cov <- cov(masculinity.data, use = "pairwise.complete.obs")
#pca5.princomp <- princomp(~., data = masculinity.data, covmat = clust.cov, )
pca5.prcomp <- prcomp(masculinity.data, scale. = TRUE, rank. = 5)

pca5.clust <- kmeans(pca5.prcomp$x, centers = 3)
plot(pca5.clust$cluster)

for (pcaweight in pca5.prcomp$rotation[,1]) {
  if(pcaweight <= -0.03) {
    
    weight.index = which(pcaweight == pca5.prcomp$rotation[,1])[[1]]
    print(colnames(masculinity.data)[weight.index])
    #print(masculinity.data[1, weight.index])
    #match(pca5.prcomp$rotation[,1], pcaweight)
    print(pcaweight)
  }
}

#scrub the data and make PCA, cluster analysis, and plot
masculinity.scrubbed <- masculinity.data[pca5.clust$cluster != 3,]
pca5.scrubbed <- prcomp(masculinity.scrubbed, scale. = TRUE, rank. = 5)
for (i in 1:262) {
  if (mean(masculinity.scrubbed[,i]) == 0) {
    print(colnames(masculinity.scrubbed)[i])
    print(i)
  }
}
masculinity.scrubbed.2 <- cbind(masculinity.scrubbed[,1:117], masculinity.scrubbed[,119], masculinity.scrubbed[,121], masculinity.scrubbed[,123:262])
pca5.scrubbed <- prcomp(masculinity.scrubbed.2, scale. = TRUE, rank. = 5)
pca5.scrubbed.clust <- kmeans(pca5.scrubbed$x, centers = 3)
pairs(pca5.scrubbed$x, col = c(1:3)[pca5.scrubbed.clust$cluster])

#looking for especially large differences between clusters
cluster.1.scrubbed = masculinity.data[pca5.scrubbed.clust$cluster==1,]
cluster.2.scrubbed = masculinity.data[pca5.scrubbed.clust$cluster==2,]
cluster.3.scrubbed = masculinity.data[pca5.scrubbed.clust$cluster==3,]
for (i in 1:262) {
  clust1.scrubbed.diff = mean(cluster.1.scrubbed[,i]) - mean(masculinity.data[,i])
  clust2.scrubbed.diff = mean(cluster.2.scrubbed[,i]) - mean(masculinity.data[,i])
  clust3.scrubbed.diff = mean(cluster.3.scrubbed[,i]) - mean(masculinity.data[,i])
  if (clust1.scrubbed.diff >= 0.04 | clust2.scrubbed.diff >= 0.04 | clust3.scrubbed.diff >= 0.04) {
    print(colnames(masculinity.data)[i])
    print(paste0("cluster 1 mean = ", mean(cluster.1.scrubbed[,i])))
    print(paste0("cluster 2 mean = ", mean(cluster.2.scrubbed[,i])))
    print(paste0("cluster 3 mean = ", mean(cluster.3.scrubbed[,i])))
  }
}

#find big contributors in the scrubbed pca
for (pcaweight in pca5.scrubbed$rotation[,1]) {
  if(pcaweight >= 0.1) {
    
    weight.index = which(pcaweight == pca5.scrubbed$rotation[,1])[[1]]
    print(colnames(masculinity.data)[weight.index])
    #print(masculinity.data[1, weight.index])
    #match(pca5.prcomp$rotation[,1], pcaweight)
    print(pcaweight)
  }
}
