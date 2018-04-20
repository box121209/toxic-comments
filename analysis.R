ubuntu <- TRUE

if(ubuntu){
  setwd("~/projects/toxic-comments/toxic-comments")
  HOMEPATH <- "/home/bill/projects/wikiwords"
} else{
  setwd("~/Kaggle/2018-toxic-comments/toxic-comments")
  HOMEPATH <- "/Users/bill/Projects/201801-wikiwords/wikiwords"
}

source(sprintf("%s/tda_functions.R", HOMEPATH))

inpath <- "./data"

# TEMP FOR COMPARISON:
#inpath <- "~/Projects/201801-wikiwords/wikipedia" 

# read input data:

freqfile <- sprintf("%s/wcounts.txt", inpath)
vecfile <- sprintf("%s/wvectors.txt", inpath)

dat1 <- read.table(freqfile, row.names=NULL)
names(dat1) <- c("freq", "word")
dat1$freq <- as.numeric(dat1$freq)

dat2 <- read.table(vecfile)
names(dat2)[1] <- "word"

# NOTE that the next merge() removes rows not common to both 
# data frames:
dat <- merge(dat1, dat2)
attach(dat)

vecsize <- ncol(dat) - 2


# frequency distribution:
tab <- table(freq)
xx <- as.numeric(dimnames(tab)$freq)
yy <- as.numeric(tab)
plot(yy ~ xx, log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")

# we see there are two distinct distributions:
rng <- 1:33
word.rare <- word[freq %in% rng]
freq.rare <- freq[freq %in% rng]
tab <- table(freq.rare)
xx <- as.numeric(dimnames(tab)$freq.rare)
yy <- as.numeric(tab)
plot(yy ~ xx, log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")
cat("Nr rare words:", length(unique(word.rare)))
sample(word.rare, 100)


rng <- (34:max(freq))
word.common <- word[freq %in% rng]
freq.common <- freq[freq %in% rng]
tab <- table(freq.common)
xx <- as.numeric(dimnames(tab)$freq.common)
yy <- as.numeric(tab)
plot(yy ~ xx, log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")
cat("Nr common words:", length(unique(word.common)))
sample(word.common, 100)


# choose data set:

dat <- dat[freq %in% rng, ]

################################################################
# continuity of frequency

ssize <- 1000
sampled <- dat[sample(2:nrow(dat), ssize),]
subdat <- sampled
x <- subdat[,3:(vecsize+2)]

# similarity matrix:
word.dist <- dist(x, method="euclidean")
dmat <- as.matrix(word.dist)
hist(dmat, col='lightgrey', xlab="Pairwise distance in word2vec space", main="")

# look at some neighbourhoods:
i <- sample(1:ssize,1); cat(sprintf("%s", subdat$word[i]))

# how discontinuous is frequency as a function on words?
# look at points close together: do they have similar frequency?
# call vectors 'close' if distance is <= [e.g. 10]:

threshold <- 1.2
res <- matrix(nrow=0, ncol=2)
for(i in 1:(ssize-1))
  for(j in (i+1):ssize)
    if(dmat[i,j] < threshold){
      y <-  abs(subdat$freq[i] - subdat$freq[j])
      res <- rbind(res, c(dmat[i,j], y))
    }
plot(res, col='blue', frame.plot=0, 
     xlab='Vector separation', 
     ylab='Frequency separation')

################################################################
# fit power law:

cond <- (xx < 1.7e03) # for wikipedia data

cond <- (5e01 < xx & xx < 1e03)

model <- lm(log(yy[cond]) ~ log(xx[cond]))
( coef <- model$coefficients )
f <- function(x) exp(coef[1] + coef[2]*log(x))
xxx <- exp(0:10)
yyy <- sapply(xxx, f)

main=sprintf("Power law coefficients (%g, %g)", 
             coef[1], coef[2])
plot(yy ~ xx, log='xy', col='blue', cex=0.5, frame.plot=0, 
     main=main,
     xlab="Word count", ylab="Nr words")
points( yyy~xxx, type='l', col='red', lwd=2)


################################################################
# exponential binning of frequency using the power law model:

makebins <- function(nbins, coef, condition=TRUE){
  
  epsilon <- 1/nbins/5
  left <- (0:(nbins-1))/nbins - epsilon
  right <- (1:nbins)/nbins + epsilon
  # aa <- exp(coef[1]) # don't need
  bb <- -coef[2]
  mn <- min(freq[condition])^(1-bb)
  mx <- max(freq[condition])^(1-bb)
  func <- function(x) x^(1/(1-bb))
  left <- func( mn + (mx - mn)*left )
  right <- func( mn + (mx - mn)*right )
  right[nbins] <- 1e09
  
  bins <- list()
  for(i in 1:nbins)
    bins[[i]] <- which(freq[condition] > left[i] & freq[condition] < right[i]) 
  # prune:
  empties <- c()
  for(i in 1:nbins) if(length(bins[[i]])==0) empties <- c(empties, i)
  for(e in rev(empties)){ bins[[e]] <- NULL; nbins <<- nbins-1 }
  # return 
  bins
}

nbins <- 20                # PARAMETER for mapper construction
coarseness <- 32            # PARAMETER target average cluster size

bins <- makebins(nbins, coef, condition=cond)
sapply(bins, length)

################################################################
# clustering 

makeclusters_centroids <- function(bins, coarseness){
  
  cluster.set <- list()
  ccount <- 0

  recurse.func <- function(hc, bin.nr, ccount){
    
    for(child in 1:2){
      set <- unique(as.numeric(labels(hc[[child]])))
      if(length(set) < coarseness){
        # update global variables:
        ccount <- ccount + 1
        centroid <- colSums( dat[set, 3:(vecsize+2)] )/length(set)
        cluster.set[[ccount]] <<- list(cluster=set, centroid=centroid, height=bin.nr)
        cat(sprintf("%d %d       \r", length(set), ccount))
      } else {
        ccount <- recurse.func( hc[[child]], bin.nr, ccount )
      }
    }
    return(ccount)
  }
  
  nbins <- length(bins)
  for(nr in 1:nbins){
    
    bin <- bins[[nr]]
    cat(sprintf("Bin %d                           \n", nr))
    if(length(bin) > 1){
      bindist <- dist(dat[bin, 3:(vecsize+2)], method="euclidean") 
      hc <- as.dendrogram( hclust(bindist) )
      ccount <- recurse.func(hc, nr, ccount)
    } else {
      ccount <- ccount + 1
      set <- as.numeric(bin)
      centroid <- dat[set, 3:(vecsize+2)]
      print(centroid)
      cluster.set[[ccount]] <<- list(cluster=set, centroid=centroid, height=nr)
      cat(sprintf("%d %d       \r", length(set), ccount))
    }
    cat("\r")
  }  
  
  # return deduped set:
  unique(cluster.set)
}


# hierarchical clustering within bins:

options(expressions=10000)
cluster.set <- makeclusters_centroids(bins, coarseness)
cat(sprintf("Found %d clusters\n", length(cluster.set)))

################################################################
# compute graph edges:

edgelist <- function(cluster.set){
  
  ccount <- length(cluster.set)
  edges <- matrix(nrow=0, ncol=2)
  for(i in 1:(ccount-1))
    for(j in (i+1):ccount){
      si <- cluster.set[[i]]$cluster
      sj <- cluster.set[[j]]$cluster
      wt <- length(si) + length(sj) - length(unique(c(si,sj)))
      if(wt > 0)
        edges <- rbind(edges, c(i,j))
    }
  mx <- max(edges)
  if(mx < ccount)
    for(i in (mx+1):ccount)
      edges <- rbind(edges, c(i,i))
  # return:
  edges
}


library(igraph)

cat("Computing mapper graph...\n")
edges <- edgelist(cluster.set)

mg <- graph_from_edgelist(edges, directed=FALSE)
cc <- clusters(mg)
cat(sprintf("Found %d graph edges, %d components, sizes:\n", 
            length(unique(as.numeric(edges))),
            cc$no))
table(cc$csize)

################################################################
# 2-dim layout using t-SNE:

library(Rtsne)

cat("Computing t-SNE layout...\n")
centroid.mat <- do.call(rbind, lapply(cluster.set, function(x) x$centroid))
lout <- Rtsne(centroid.mat, 
              theta=0.5,
              perplexity=30,
              verbose=TRUE, 
              initial_dims=100,
              check_duplicates=FALSE, 
              max_iter=2000)
lout <- data.frame(lout$Y)
names(lout) <- c("X","Y")


################################################################
# write output:

path <- "shiny/wordrep/data"
dat <- dat[,1:2]

save(dat, file=sprintf("%s/w2v_freq.Rds", path))
save(cluster.set, file=sprintf("%s/w2v_cset.Rds", path))
save(mg, file=sprintf("%s/w2v_graph.Rds", path))
save(lout, file=sprintf("%s/w2v_layout.Rds", path))

