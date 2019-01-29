library(tidyverse)

location <- '~/PATH/TO/DATA'
data <- read.csv(paste(location,'CLEANED_DATA.csv',sep='/'),stringsAsFactors = F)

# --------------------------
pca <- prcomp(CKDdata, scale=T)
pca$sdev     # std deviation of the principal components (square-root of the eigenvalues)
pca$rotation # the matrix of variable loadings (columns are eigenvectors)
pca$x        # The coordinates of the individuals (observations) on the principal components.

var <- pca$sdev^2
# scree plot (looking at first n PCs):
n <- 20
ggdf <- as.data.frame(cbind(c(1:n), var[1:n]))
ggplot(ggdf) + 
  geom_point(aes(V1, V2), color = '#56B4E9') + 
  geom_line(aes(V1, V2), color = '#56B4E9') + 
  geom_hline(yintercept = 1, color = 'red') +
  ggtitle("Scree Plot on Standardized Data") +
  xlab("Principal Components") +
  ylab("Variance") +
  theme_bw()
# blue is variance explained

cumlvar <- cumsum( (var/sum(var)) )
# scree plot (looking at first n PCs):
n <- 20
threshold <- 0.8
ggdf <- as.data.frame(cbind(c(1:n), cumlvar[1:n]))
ggplot(ggdf) + 
  geom_point(aes(V1, V2), color = '#E69F00') + 
  geom_line(aes(V1, V2), color = '#E69F00') + 
  geom_hline(yintercept = threshold, color = 'red') +
  ggtitle("Scree Plot on Standardized Data") +
  xlab("Principal Components") +
  ylab("Variance") +
  theme_bw()
# orange is cumulative variance

### RULES:
  # 1. Pick PCs with variance > 1
  # 2. Find the elbow
  # 3. Pick n pcs accounting for > 80% cumulative variance (or 90% or 95%...)

# or use automated viz
install.packages('factoextra')
library(factoextra)

# automated scree plot
fviz_eig(pca, ncp = n)

# automated plot
fviz_pca_var(pca,
             axes=c(1,2), # PCs to plot
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_ind(pca, 
             axes=c(1,2),
             geom.ind = "point", pointshape = 21, pointsize = 2, 
             fill.ind = factor(CKDdata$CKD), # label by class
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5))
