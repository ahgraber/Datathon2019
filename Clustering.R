# test k iteratively
ggdf <- data.frame()
for (k in 3:9) {
  clust <- kmeans(train, k, iter.max=100, nstart=100)
  ggdf <- rbind(ggdf, c(k, 
                        clust$toss, 
                        clust$withinss,
                        clust$tot.withinss,
                        clust$betweenss))
  names(ggdf) <- c('k','toss','withinss','tot.withinss','betweenss')
}

# plot
ggplot(ggdf) + 
  geom_point(aes(k, toss), color = '#56B4E9') +
  geom_line(aes(k, toss), color = '#56B4E9') +
  ggtitle("Total Sum Square") +
  xlab("k") + ylab("Total Sum Square") +
  theme_bw()

# ggplot(ggdf) + 
#   geom_point(aes(k, withinss), color = '#56B4E9') +
#   geom_line(aes(k, withinss), color = '#56B4E9') +
#   ggtitle("Within Sum Square") +
#   xlab("k") + ylab("Within Sum Square") +
#   theme_bw()
  
ggplot(ggdf) + 
  geom_point(aes(k, tot.withinss), color = '#56B4E9') +
  geom_line(aes(k, tot.withinss), color = '#56B4E9') +
  ggtitle("Total Within Sum Square") +
  xlab("k") + ylab("Total Within Sum Square") +
  theme_bw()
  
ggplot(ggdf) + 
  geom_point(aes(k, betweenss), color = '#56B4E9') +
  geom_line(aes(k, betweenss), color = '#56B4E9') +
  ggtitle("Between Sum Square") +
  xlab("k") + ylab("Between Sum Square") +
  theme_bw()

# final cluster solution
set.seed(1234)
k <- 3
clust <- kmeans(train, k, iter.max=100, nstart=100)
cluster <- clust$cluster
centroids <- clust$centers
clustData3 <- cbind(clustData, cluster)


# --- KNN ---
classes <- class:knn(centroids, test, 
                     classes, # factor vector of true classifications of training set
                     k = 1)






# --- plotting ---
# Colorblind-friendly palette with grey:
cbGray <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbBlack <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Step 2 - Plot clusters
p1 <- ggplot(clustData3, aes(F1, fill=factor(cluster))) + 
  geom_density(alpha=.33) +
  scale_fill_manual(name = "Cluster", values = cbGray) +
  theme_bw() + theme(legend.background = element_rect(fill=NA), 
                     legend.justification=c(1,1), legend.position=c(1,1))

p2 <- ggplot(clustData3, mapping=aes(F2, F1, color=factor(cluster))) + 
  geom_point(alpha=.66) +
  scale_colour_manual(name = "Cluster", values = cbGray, guide=FALSE) +
  theme_bw()

p3 <- ggplot(clustData3, mapping=aes(F3, F1, color=factor(cluster))) + 
  geom_point(alpha=.66) +
  scale_colour_manual(name = "Cluster", values = cbGray, guide=FALSE) +
  theme_bw()

p4 <- ggplot(clustData3, mapping=aes(F1, F2, color=factor(cluster))) + 
  geom_point(alpha=.66) +
  scale_colour_manual(name = "Cluster", values = cbGray, guide=FALSE) +
  theme_bw()

p5 <- ggplot(clustData3, aes(F2, fill=factor(cluster))) + 
  geom_density(alpha=.33) +
  scale_fill_manual(name = "Cluster", values = cbGray) +
  theme_bw() + theme(legend.background = element_rect(fill=NA), 
                     legend.justification=c(1,1), legend.position=c(1,1))

p6 <- ggplot(clustData3, mapping=aes(F3, F2, color=factor(cluster))) + 
  geom_point(alpha=.66) +
  scale_colour_manual(name = "Cluster", values = cbGray, guide=FALSE) +
  theme_bw()

p7 <- ggplot(clustData3, mapping=aes(F1, F3, color=factor(cluster))) + 
  geom_point(alpha=.66) +
  scale_colour_manual(name = "Cluster", values = cbGray, guide=FALSE) +
  theme_bw()

p8 <- ggplot(clustData3, mapping=aes(F2, F3, color=factor(cluster))) + 
  geom_point(alpha=.66) +
  scale_colour_manual(name = "Cluster", values = cbGray, guide=FALSE) +
  theme_bw()

p9 <- ggplot(clustData3, aes(F3, fill=factor(cluster))) + 
  geom_density(alpha=.33) +
  scale_fill_manual(name = "Cluster", values = cbGray) +
  theme_bw() + theme(legend.background = element_rect(fill=NA), 
                     legend.justification=c(0,1), legend.position=c(0,1))

install.packages(c('grid','gridExtra'))
library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3,
             p4, p5, p6,
             p7, p8, p9,
             ncol=3)