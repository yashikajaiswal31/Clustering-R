#Installation of required packages
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggthemes")
install.packages("gganimate")
install.packages("cluster")
install.packages("Rtsne")
install.packages("knitr")

#Load Libraries
library(tidyverse)# for data cleaning
library(dplyr)  
library(reshape2)
library(ggthemes)  
library(gganimate)
library(cluster)#for cluster analysis 
library(Rtsne)  
library(knitr)
library(Rtsne)
library(dbscan)

#Import the dataset
population_data = read.csv("Downloads/world_population.csv")
#Display structure of the dataset
glimpse(population_data)

#Rename coloumns for clarity
names(population_data)<-c('Rank','CCA3','Country','Capital','Continent','Population_2022', 'Population_2020', 'Population_2015','Population_2010', 'Population_2000', 'Population_1990', 'Population_1980', 'Population_1970', 'Area', 'Density','Growth_Rate','World_population_perc') 
#Display first few rows of the dataset
(head(population_data))

# Plot world population percentage by continent using ggplot
population_data%>% 
  select(Continent,World_population_perc)%>% 
  group_by(Continent)%>% 
  dplyr::summarise(World_population_perc = sum(World_population_perc))%>% 
  ggplot( aes(x="", y=World_population_perc, fill=Continent)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+  
  scale_fill_brewer(palette = "Paired")+
  theme_void() 

# Prepare data for clustering
cluster_data <- population_data%>% 
  mutate_if(sapply(population_data, is.character), as.factor)%>%
  select(Country,Area,Density, Growth_Rate,World_population_perc)
#Display first few rows of cluster_data
(head(cluster_data,10))

# Compute Gower distance matrix
gower_dist <- daisy(cluster_data[,-1],
                    metric = "gower",
                    type = list(logratio = 3))

# Calculate silhouette width for different number of clusters
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
} 
sil_width[is.na(sil_width)] <- 0

# Create data frame for silhouette width
dfs <-data.frame(Cluster_num= 1:10,Silhouette_width=sil_width)

# Plot silhouette width
ggplot(dfs, aes(x=Cluster_num,y=Silhouette_width)) + 
  geom_line()+  
  scale_x_continuous(breaks=seq(1, 10, 1))+
  theme_bw()

# Perform clustering with k = 3
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

# Summarize clustering results
pam_results <- cluster_data %>% 
  select(-Country) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.)) 

# Display cluster summary
pam_results$the_summary


set.seed(123)
# Perform t-SNE dimensionality reduction
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

# Extract t-SNE coordinates and assign cluster labels and country names
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = cluster_data$Country)

# Plot distribution of t-SNE clusters
tsne_data %>% group_by(cluster)%>% count() %>% 
  ggplot(aes(x=cluster, y = n, fill= cluster)) + 
  geom_bar(stat='Identity')+  
  coord_flip()+  
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()

# Plot t-SNE visualization with cluster labels
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +theme(legend.position="none") + 
  geom_text( 
    data=tsne_data %>% group_by(cluster) %>%
      filter(X == max(X) | X==min(X) | Y==max(Y)| Y == min(Y)) %>%
      arrange(cluster), 
    aes(label=name),vjust = 0, nudge_y =1
  ) +scale_color_brewer(palette = "Dark2")+ 
  theme_bw()

# Perform hierarchical clustering
hc_fit <- hclust(gower_dist)

# Cut the dendrogram to get cluster assignments
hierarchical_clusters <- cutree(hc_fit, k = 3)  

# Add hierarchical cluster information to tsne_data
tsne_data$hierarchical_cluster <- factor(hierarchical_clusters)

# Plot the distribution of hierarchical clusters
tsne_data %>% 
  group_by(hierarchical_cluster) %>% 
  count() %>% 
  ggplot(aes(x = hierarchical_cluster, y = n, fill = hierarchical_cluster)) + 
  geom_bar(stat = 'Identity') +  
  coord_flip() +  
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

# Plot the t-SNE visualization with hierarchical cluster labels
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = hierarchical_cluster)) +
  theme(legend.position = "none") + 
  geom_text( 
    data = tsne_data %>% 
      group_by(hierarchical_cluster) %>% 
      filter(X == max(X) | X == min(X) | Y == max(Y) | Y == min(Y)) %>% 
      arrange(hierarchical_cluster), 
    aes(label = name), vjust = 0, nudge_y = 1
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

# Perform DBSCAN clustering
dbscan_fit <- dbscan(gower_dist, eps = 0.1, minPts = 5)  

# Extract cluster assignments and noise points
dbscan_clusters <- dbscan_fit$cluster
dbscan_noise <- as.logical(dbscan_clusters == 0)

# Add DBSCAN cluster information to tsne_data
tsne_data$dbscan_cluster <- factor(dbscan_clusters)

# Plot the distribution of DBSCAN clusters
tsne_data %>% 
  filter(!dbscan_noise) %>%  # Exclude noise points
  group_by(dbscan_cluster) %>% 
  count() %>% 
  ggplot(aes(x = dbscan_cluster, y = n, fill = dbscan_cluster)) + 
  geom_bar(stat = 'Identity') +  
  coord_flip() +  
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

# Plot the t-SNE visualization with DBSCAN cluster labels
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = dbscan_cluster)) +
  theme(legend.position = "none") + 
  geom_text( 
    data = tsne_data %>% 
      filter(!dbscan_noise) %>%  # Exclude noise points
      group_by(dbscan_cluster) %>% 
      filter(X == max(X) | X == min(X) | Y == max(Y) | Y == min(Y)) %>% 
      arrange(dbscan_cluster), 
    aes(label = name), vjust = 0, nudge_y = 1
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

# Perform K-means clustering
kmeans_fit <- kmeans(gower_dist, centers = 3)

# Extract cluster assignments
kmeans_clusters <- kmeans_fit$cluster

# Add K-means cluster information to tsne_data
tsne_data$kmeans_cluster <- factor(kmeans_clusters)

# Plot the distribution of K-means clusters
tsne_data %>% 
  group_by(kmeans_cluster) %>% 
  count() %>% 
  ggplot(aes(x = kmeans_cluster, y = n, fill = kmeans_cluster)) + 
  geom_bar(stat = 'Identity') +  
  coord_flip() +  
  scale_fill_brewer(palette = "Dark2") +
  theme_bw()

# Plot the t-SNE visualization with K-means cluster labels
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = kmeans_cluster)) +
  theme(legend.position = "none") + 
  geom_text( 
    data = tsne_data %>% 
      group_by(kmeans_cluster) %>% 
      filter(X == max(X) | X == min(X) | Y == max(Y) | Y == min(Y)) %>% 
      arrange(kmeans_cluster), 
    aes(label = name), vjust = 0, nudge_y = 1
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()


