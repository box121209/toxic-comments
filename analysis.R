ubuntu <- FALSE

if(ubuntu){
  setwd("~/projects/toxic-comments/toxic-comments")
  HOMEPATH <- "/home/bill/projects/wikiwords"
} else{
  setwd("~/Kaggle/2018-toxic-comments/toxic-comments")
  HOMEPATH <- "/Users/bill/Projects/201801-wikiwords/wikiwords"
}

source(sprintf("%s/tda_functions.R", HOMEPATH))

inpath <- "./data"

# read input data:

dictfile <- sprintf("%s/wdict.txt", inpath)
freqfile <- sprintf("%s/wcounts.txt", inpath)
vecfile <- sprintf("%s/wvectors.txt", inpath)

dat1 <- read.table(dictfile)
names(dat1) <- c("word", "class")
dat2 <- read.table(freqfile, row.names=NULL)
names(dat2) <- c("freq", "word")
dat2$freq <- as.numeric(dat2$freq)
dat3 <- read.table(vecfile)
names(dat3)[1] <- "word"
dat4 <- merge(dat1, dat2)
alldat <- merge(dat3, dat4)
attach(alldat)

# frequency distribution:
tab <- table(freq)
xx <- as.numeric(dimnames(tab)$freq)
yy <- as.numeric(tab)
plot(yy ~ xx, log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")

# we see there are two distinct distributions:
rng <- 1:33
plot(yy[rng] ~ xx[rng], log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")
cat("Nr words:", length(word[freq %in% rng]))
sample(word[freq %in% rng], 100)


rng <- (34:length(xx))
plot(yy[rng] ~ xx[rng], log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")
cat("Nr words:", length(word[freq %in% rng]))
sample(word[freq %in% rng], 100)


################################################################
# fit power law:

cond <- (33 < xx & xx < 1e03)
cond <- (xx <= 33)

model <- lm(log(yy[cond]) ~ log(xx[cond]))
( coef <- model$coefficients )
f <- function(x) exp(coef[1] + coef[2]*log(x))
xxx <- exp(0:10)
yyy <- sapply(xxx, f)
points( yyy~xxx, type='l', col='red', lwd=2)
text(1e03, 100,
     labels=sprintf("Power law coefficients (%g, %g)", 
                    coef[1], coef[2]), col='red')


################################################################
# exponential binning of frequency using the power law model:

makebins <- function(nbins, coef, condition=TRUE){
  
  epsilon <- 1/nbins/5
  left <- (0:(nbins-1))/nbins - epsilon
  right <- (1:nbins)/nbins + epsilon
  # aa <- exp(coef[1]) # don't need
  bb <- -coef[2]
  mn <- min(freq)^(1-bb)
  mx <- max(freq)^(1-bb)
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

nbins <- 20                 # PARAMETER for mapper construction
coarseness <- 32            # PARAMETER target average cluster size

bins <- makebins(nbins, coef, condition=cond)
sapply(bins, length)

################################################################
# clustering 

makeclusters_centroids <- function(bins, coarseness){
  
  cluster.set <- list()
  ccount <- 0
  nc <- ncol(dat3)
  
  recurse.func <- function(hc, bin.nr, ccount){
    
    for(child in 1:2){
      set <- unique(as.numeric(labels(hc[[child]])))
      if(length(set) < coarseness){
        # update global variables:
        ccount <- ccount + 1
        centroid <- colSums( alldat[set, 2:nc] )/length(set)
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
    bindist <- dist(alldat[bin, 2:nc], method="euclidean") 
    hc <- as.dendrogram( hclust(bindist) )
    ccount <- recurse.func(hc, nr, ccount)
    cat("\r")
  }  
  
  # return deduped set:
  unique(cluster.set)
}


# hierarchical clustering within bins:

cluster.set <- makeclusters_centroids(bins, coarseness)
cat(sprintf("Found %d clusters\n", length(cluster.set)))

################################################################
# compute graph edges:

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
              theta=0.1,
              verbose=TRUE, 
              initial_dims=100,
              check_duplicates=FALSE, 
              max_iter=2000)
lout <- data.frame(lout$Y)
names(lout) <- c("X","Y")


################################################################
# write output:

cat("Writing output...\n")

path <- sprintf("%s/data", outpath)
dat <- dat2

save(dat, file=sprintf("%s/w2v_freq.Rds", path))
save(cluster.set, file=sprintf("%s/w2v_cset.Rds", path))
save(mg, file=sprintf("%s/w2v_graph.Rds", path))
save(lout, file=sprintf("%s/w2v_layout.Rds", path))

system(sprintf("cp ./shiny/*.R %s", args[2]))

cat("Done.\n")
