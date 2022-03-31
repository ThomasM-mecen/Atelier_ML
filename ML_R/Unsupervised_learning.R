rm(list = ls())
# Import packages
library(tidyverse)
library(FactoMineR)
library(factoextra)

# Load dataset
data = iris
# Perform PCA
res.pca = PCA(data[,-5], graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 80))
fviz_pca_var(res.pca, col.var = "black")
var = get_pca_var(res.pca)
corrplot::corrplot(var$cos2, is.corr=FALSE)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

# Représentation des individus
fviz_pca_ind(res.pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = iris$Species, # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Groups"
)

kc = kmeans(res.pca$ind$coord, 3)
data = cbind(data, as.factor(kc$cluster))
par(mfrow = c(1, 2))
plot(res.pca$ind$coord[,1:2],col=factor(kc$cluster))
plot(res.pca$ind$coord[,1:2],col=factor(data$Species))
table(data$Species, data$`as.factor(kc$cluster)`)

# HCPC 
res.hcpc = HCPC(res.pca, graph = FALSE, nb.clust = 3)
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
)
fviz_cluster(res.hcpc,
             geom = "point",
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
data = cbind(data, as.factor(res.hcpc$data.clust[,5]))
table(data$Species, data$`as.factor(res.hcpc$data.clust[, 5])`)
table(data$`as.factor(kc$cluster)`, data$`as.factor(res.hcpc$data.clust[, 5])`)
