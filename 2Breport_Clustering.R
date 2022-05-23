# modified-V5

#### Table 15.2

library(readxl)
Fata.df <- read.csv("R/Final_Fata_Clustering_report2b.csv")
View(Fata.df)
Fata.df=unique(Fata.df, incomparables = FALSE, fromLast = FALSE, nmax = NA)

# set row names to the fata column
row.names(Fata.df) <- Fata.df[,1]

# remove the fata column
Fata.df <- Fata.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(Fata.df, method = "euclidean")


#15.4
# normalize input variables
Fata.df.norm <- sapply(Fata.df, scale)

# add row names
row.names(Fata.df.norm) <- row.names(Fata.df) # compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(Fata.df.norm[,c(2,3,4)], method = "euclidean")  
# Remember to try different methods for computing distance:
# "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"



#### Figure 15.3
# compute normalized DISTANCE paramneter based on all 8 variables
d.norm <- dist(Fata.df.norm, method = "euclidean") 

# in hclust() set LINKAGE parameter to "ward.D", "single", "complete", "average", "median", or "centroid"
# Note: hang -1 means do not hang labels off the leaves; make them level; ann is for plot annotation

hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)

hc3 <- hclust(d.norm, method = "median")
plot(hc3, hang = -1, ann = FALSE)

hc4 <- hclust(d.norm, method = "complete")
plot(hc4, hang = -1, ann = FALSE)

hc5 <- hclust(d.norm, method = "centroid")
plot(hc5, hang = -1, ann = FALSE)


# Why is Ward appealing?


#### Table 15.6

memb2 <- cutree(hc2, k = 4) #average
memb2

memb3 <- cutree(hc3, k = 4) #median
memb3

memb4 <- cutree(hc4, k = 4) #complete
memb4

memb5 <- cutree(hc5, k = 4) #centroid
memb5

# ward.D
memb6 <- cutree(hc4, k = 4) #ward.D
memb6


memb6 <- cutree(hc4, h = 7) # should be equivalent to k = 4
memb6
cat(memb6)

memb6 <- cutree(hc3, k = 4) # should be equivalent to h = 7
memb6
cat(memb6)


# Tip: Use cat() function to capture the cluster numbers without the row names and without line numbers:
# It's more concise and easy to visually check
cat(memb2)
cat(memb3)
cat(memb4)
cat(memb5)
cat(memb6)


hist(memb2)
hist(memb3)
hist(memb4)
hist(memb5)
hist(memb6)


### Figure 15.4

# set labels as cluster membership number : utility name
#row.names(utilities.df.norm) <- paste(memb1, ": ", row.names(utilities.df), sep = "")
#row.names(utilities.df.norm) <- paste(memb2, ": ", row.names(utilities.df), sep = "")
#row.names(utilities.df.norm) <- paste(memb3, ": ", row.names(utilities.df), sep = "")
#row.names(utilities.df.norm) <- paste(memb4, ": ", row.names(utilities.df), sep = "")
#row.names(utilities.df.norm) <- paste(memb5, ": ", row.names(utilities.df), sep = "")
row.names(Fata.df.norm) <- paste(memb6, ": ", row.names(Fata.df), sep = "")
row.names(Fata.df.norm)

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(Fata.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("grey",1:99,sep="")))


#### Table 15.9

# load and preprocess data 
Fata.df <- read.csv("R/Final_Fata_Clustering_report2b.csv")
Fata.df=unique(Fata.df, incomparables = FALSE, fromLast = FALSE, nmax = NA)
row.names(Fata.df) <- Fata.df[,1]
Fata.df <- Fata.df[,-1]

# normalized distance:
Fata.df.norm <- sapply(Fata.df, scale)
row.names(Fata.df.norm) <- row.names(Fata.df) 

# Remember to try other distance metrics besides Euclidean on line 27:
# "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"


########################################################################

# Run k-means algorithm if you know what k should be; you do not have to choose distance or linkage parameters.
set.seed(2)
km <- kmeans(Fata.df.norm,2)   # k = 4 (we know it from domain expertise)

# With what utilities is Boston utility clustering?

# show cluster membership
km$cluster

# How can we get just the cluster numbers (no rownames, no line numbers in output)?
cat(km$cluster)

#### Table 15.10
# centroids
km$centers

#### Figure 15.5

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(Fata.df))

# plot centroids
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                       "black", "dark grey"))     

# how about more colors?                                                                                                           
# plot centroids 
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = switch(i, "black", "red", 
                                                       "green", "purple"))


# name clusters
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:4)))

# What do the profile plot tell you?


#### Table 15.11

dist(km$centers)

# Hierarchical clustering is better at identifying large clusters
# K-means clustering is better at identifying smaller clusters 
# Can we get both?

# Another method: combination of divisive (top-down) AND k-means to get both 


library(dclust)
hybrid <- dclust(d.norm, method = "kmeans", stand = TRUE)
plot(hybrid, ann = FALSE)


