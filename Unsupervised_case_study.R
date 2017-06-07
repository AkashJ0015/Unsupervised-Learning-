url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"
# Download the data: wisc.df
wisc.df<-read.csv(url)

# Convert the features of the data: wisc.data
wisc.data<-as.matrix(wisc.df[,3:32])

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == 'M')

#Exploring dataset
t<-as.data.frame(wisc.data)
names(t)
length(grep('_mean',names(t),value=TRUE))
sum(diagnosis)
dim(wisc.data)
colMeans(wisc.data)
apply(wisc.data,2,sd)

# Execute PCA, scaling if appropriate
wisc.pr<-prcomp(wisc.data,scale=TRUE,center=TRUE)
summary(wisc.pr)

# Create a biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1),
xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[,c(1,3)], col = (diagnosis + 1),
xlab = "PC1", ylab = "PC3")

# Calculate variability of each component
pr.var<-wisc.pr$sdev^2

# Variance explained by each principal component: pve
pve<-pr.var/sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
ylab = "Proportion of Variance Explained",
ylim = c(0, 1), type = "b")

#Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
ylab = "Cummulative Proportion of Variance Explained",
ylim = c(0, 1), type = "b")

#summary(wisc.pr)

Scale the wisc.data data: data.scaled
data.scaled<-scale(wisc.data)

# Calculate the (Euclidean) distances: data.dist
data.dist<-dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust<-hclust(data.dist,method='complete')

# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters<-cutree(wisc.hclust,k=4)

# Compare cluster membership to actual diagnoses
table(diagnosis,wisc.hclust.clusters)

# Create a k-means model on wisc.data: wisc.km
set.seed(1)

wisc.km<-kmeans(x=scale(wisc.data),center=2,nstart=20)

# Compare k-means to actual diagnoses
table(diagnosis,wisc.km$cluster)

# Compare k-means to hierarchical clustering
table(wisc.hclust.clusters,wisc.km$cluster)

# Create a hierarchical clustering model: wisc.pr.hclust
pr.var<-wisc.pr$sdev^2
pve<-pr.var/sum(pr.var)
cum_pve<-cumsum(pve)
t<-which(cum_pve>0.9)[1]
wisc.pr.hclust <- hclust(dist(wisc.pr$x[,1:7]), method = 'complete')

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters<-cutree(wisc.pr.hclust,k=4)

# Compare to actual diagnoses
table(diagnosis,wisc.pr.hclust.clusters)

# Compare to k-means and hierarchical
table(diagnosis,wisc.km$cluster)
table(diagnosis,wisc.hclust.clusters)