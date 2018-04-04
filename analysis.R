setwd("~/projects/toxic-comments/toxic-comments")

HOMEPATH <- "/home/bill/projects/wikiwords"
source(sprintf("%s/tda_functions.R", HOMEPATH))

inpath <- "./data"

# read input data:

cat("Reading data...\n")

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

cat("Fitting frequency distribution...\n")

# frequency distribution:
tab <- table(freq)
xx <- as.numeric(dimnames(tab)$freq)
yy <- as.numeric(tab)
plot(yy ~ xx, log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")

# we see there are two distinct distributions:
rng <- 1:35
plot(yy[rng] ~ xx[rng], log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")

rng <- (36:length(xx))
plot(yy[rng] ~ xx[rng], log='xy', col='blue', cex=0.5, frame.plot=0, 
     xlab="Word count", ylab="Nr words")

# fit power law:
cond <- (35 < xx & xx < 1e03)
model <- lm(log(yy[cond]) ~ log(xx[cond]))
( coef <- model$coefficients )
f <- function(x) exp(coef[1] + coef[2]*log(x))
xxx <- exp(0:10)
yyy <- sapply(xxx, f)
points( yyy~xxx, type='l', col='red', lwd=2)
text(1e03, 100,
     labels=sprintf("Power law coefficients (%g, %g)", coef[1], coef[2]), col='red')


################################################################
# exponential binning of frequency using the power law model:

nbins <- 20                 # PARAMETER for mapper construction
coarseness <- 32            # PARAMETER target average cluster size

bins <- makebins(nbins, coef)

################################################################
# clustering 

# hierarchical clustering within bins:

cat(sprintf("Building clusters (using %d bins and average cluster size %d)...\n", 
            nbins, coarseness))
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
