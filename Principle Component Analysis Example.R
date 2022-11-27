# Supervised VS Unsupervised Learning

---------------------------------------------------------------------------------
  # PRINCIPAL COMPONENTS ANALYSIS for supervised vs unsupervised
  ---------------------------------------------------------------------------------
  # Load in data for states
  states <- row.names(USArrests)
  states
  
  # look at names
  names (USArrests)
  
  apply(USArrests, 2, mean)
  
  apply(USArrests, 2, var)
  
  pr.out <- prcomp (USArrests , scale = TRUE)
  
  names(pr.out)
  
  pr.out$center
  
  pr.out$scale
  
  pr.out$rotation
  
  dim(pr.out$x)
  
  biplot (pr.out , scale = 0)
  
  # first biplot
  pr.out$rotation = -pr.out$rotation
  pr.out$x = -pr.out$x
  biplot (pr.out , scale = 0)
  
  pr.out$sdev
  
  pr.var <- pr.out$sdev^2
  pr.var
  
  pve <- pr.var / sum (pr.var)
  
  pve
  
  # graphing the principal component analysis
  par (mfrow = c(1, 2))
  plot (pve , xlab = " Principal Component ",
        ylab = " Proportion of Variance Explained ", ylim = c(0, 1),
        type = "b")
  plot ( cumsum (pve), xlab = " Principal Component ",
         ylab = " Cumulative Proportion of Variance Explained ",
         ylim = c(0, 1), type = "b")
  
  
  a <- c(1, 2, 8, -3)
  cumsum (a)
  ---------------------------------------------------------------------------------
    # K Means Clustering
    ---------------------------------------------------------------------------------
    # setting the seed
    set.seed (2)
  x <- matrix ( rnorm (50 * 2), ncol = 2)
  x[1:25, 1] <- x[1:25, 1] + 3
  x[1:25, 2] <- x[1:25, 2] - 4
  
  km.out <- kmeans (x, 2, nstart = 20)
  # looking at your mean
  km.out$cluster
  # graphing cluster and plotting cluster
  par (mfrow = c(1, 2))
  plot (x, col = (km.out$cluster + 1),
        main = "K- Means Clustering Results with K = 2",
        xlab = "", ylab = "", pch = 20, cex = 2)
  
  # When K = 3 ...........
  set.seed (4)
  km.out <- kmeans (x, 3, nstart = 20)
  km.out
  
  plot (x, col = (km.out$cluster + 1),
        main = "K- Means Clustering Results with K = 3",
        xlab = "", ylab = "", pch = 20, cex = 2)
  
  set.seed (4)
  km.out <- kmeans (x, 3, nstart = 1)
  km.out$tot.withinss
  
  km.out <- kmeans (x, 3, nstart = 20)
  km.out$tot.withinss
  [1] 97.9793
  ----------------------------------------------------------------
    # Hierarchical Clustering
    
    hc.complete <- hclust ( dist (x), method = "complete")
  # average
  hc.average <- hclust ( dist (x), method = "average")
  # single
  hc.single <- hclust ( dist (x), method = "single")
  # plotting
  par (mfrow = c(1, 3))
  plot (hc.complete, main = "Complete Linkage",
        xlab = "", sub = "", cex = .9)
  plot (hc.average , main = "Average Linkage",
        xlab = "", sub = "", cex = .9)
  plot (hc.single, main = "Single Linkage",
        xlab = "", sub = "", cex = .9)
  # cutrees 
  cutree (hc.complete, 2)
  # average
  cutree (hc.average , 2)
  # complete 
  cutree (hc.single, 2)
  # single
  cutree (hc.single, 4)
  # single
  xsc <- scale (x)
  plot ( hclust ( dist (xsc), method = "complete") ,
         main = "Hierarchical Clustering with Scaled Features")
  
  x <- matrix ( rnorm (30 * 3), ncol = 3)
  dd <- as.dist (1 - cor (t(x)))
  plot ( hclust (dd, method = "complete") ,
         main = "Complete Linkage with Correlation - Based Distance",
         xlab = "", sub = "")
  