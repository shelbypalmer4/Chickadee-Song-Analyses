#### Quantifying the variation in song of chickadees with mixed ancastry ####

songWDlaptop <- "C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses"
songWDmac <- "/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses"

setwd(songWDmac)
setwd(songWDlaptop)
allsongs <- read.csv("HZCH_song-level-measurements_2.csv")
which(allsongs$number_notes>12)
# let's go ahead and exclude the superlong songs from Mr. O
allsongs <- allsongs[-which(allsongs$number_notes>12),]
# also going to retroactively exclude a few mis-measured songs
allsongs <- allsongs[-which(allsongs$file_name=="Poecile.sp_Ym.NG_Mar192022_SparrowfootPUA.HenryCO.MO_SMP_e_0.59.wav"),]
allsongs <- allsongs[-which(allsongs$file_name=="Poecile.sp_Gm.RB_Mar042023_SparrowfootPUA.HenryCO.MO_SMP_c_0.55.wav"),]
allsongs <- allsongs[-which(allsongs$file_name=="Poecile.sp_Gm.RB_Mar062023_SparrowfootPUA.HenryCO.MO_SMP_a_1.27.wav"),]


allsongs$nickname <- rep(NA)
allsongs$nickname[which(allsongs$ind_ID=="Gm.GO")] <- "Eliud"
allsongs$nickname[which(allsongs$ind_ID=="Gm.RB")] <- "Doc Holliday"
allsongs$nickname[which(allsongs$ind_ID=="Nm.GR")] <- "JAY-Z"
allsongs$nickname[which(allsongs$ind_ID=="Nm.RY")] <- "Johnny Ringo"
allsongs$nickname[which(allsongs$ind_ID=="Om.NO")] <- "Mr. Orange"
allsongs$nickname[which(allsongs$ind_ID=="Om.YR")] <- "Pre"
allsongs$nickname[which(allsongs$ind_ID=="Rm.RR")] <- "Remy"
allsongs$nickname[which(allsongs$ind_ID=="Rm.YB")] <- "Kafka"
allsongs$nickname[which(allsongs$ind_ID=="Ym.NG")] <- "Nick Dunne"
allsongs$nickname[which(allsongs$ind_ID=="Ym.WG")] <- "Houdini"

# For starters, PCA to see how the variation in songs falls out
#### Song-level PCA ####
colnames(allsongs)
pca1 <- prcomp(allsongs[,c(3:8,10:14)], 
               center = T,
               scale. = T)

# looking at the variable loadings

pca1$rotation
pca1scores <- as.data.frame(pca1$x)
# eigenvalues
pca1$sdev^2

# make a plot
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca1, 
         groups = allsongs$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

## I want to make a biplot for this PCA, but with just some individuals' points. Let's separate by Q-score...
# individuals with Q<0.9
CaroLike <- c("Mr. Orange", "JAY-Z", "Kafka", "Pre", "Nick Dunne")
HybridLike <- c("Remy", "Houdini", "Doc Holliday", "Eliud", "Johnny Ringo")

ggbiplot(pca1, 
         groups = allsongs$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = ifelse(allsongs$nickname %in% HybridLike, 0.75, 0),
         varname.size = 3,
         varname.adjust = 1)
# individuals with Q>0.9
ggbiplot(pca1, 
         groups = allsongs$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = ifelse(allsongs$nickname %in% CaroLike, 0.75, 0),
         varname.size = 3,
         varname.adjust = 1)

#### Life is short; lets's make a note-level plot!! ####
allnotes <- read.csv("HZCH_note-level-measurements_3.csv")
for (i in 1:length(allnotes$file_name)) {
  allnotes$ind_ID[i] <- unlist(strsplit(allnotes$file_name[i],
                                        split = "_"))[2]
}
allnotes$nickname <- rep(NA)
allnotes$nickname[which(allnotes$ind_ID=="Gm.GO")] <- "Eliud"
allnotes$nickname[which(allnotes$ind_ID=="Gm.RB")] <- "Doc Holliday"
allnotes$nickname[which(allnotes$ind_ID=="Nm.GR")] <- "JAY-Z"
allnotes$nickname[which(allnotes$ind_ID=="Nm.RY")] <- "Johnny Ringo"
allnotes$nickname[which(allnotes$ind_ID=="Om.NO")] <- "Mr. Orange"
allnotes$nickname[which(allnotes$ind_ID=="Om.YR")] <- "Pre"
allnotes$nickname[which(allnotes$ind_ID=="Rm.RR")] <- "Remy"
allnotes$nickname[which(allnotes$ind_ID=="Rm.YB")] <- "Kafka"
allnotes$nickname[which(allnotes$ind_ID=="Ym.NG")] <- "Nick Dunne"
allnotes$nickname[which(allnotes$ind_ID=="Ym.WG")] <- "Houdini"

colnames(allnotes)
pca2 <- prcomp(allnotes[,4:10], 
               center = T,
               scale. = T)
pca2$rotation
pca2scores <- as.data.frame(pca2$x)

ggbiplot(pca2, 
         groups = allnotes$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

# individuals with Q<0.9
ggbiplot(pca2, 
         groups = allnotes$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = ifelse(allnotes$nickname %in% HybridLike, 0.75, 0),
         varname.size = 3,
         varname.adjust = 1)
# individuals with Q>0.9
ggbiplot(pca2, 
         groups = allnotes$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = ifelse(allnotes$nickname %in% CaroLike, 0.75, 0),
         varname.size = 3,
         varname.adjust = 1)

#### Can't install PhenotypeSpace ####
## ERROR: dependency 'spatstat.core' is not available for package 'PhenotypeSpace'
# library(remotes)
# install_github("maRce10/PhenotypeSpace")
# install.packages("https://cran.r-project.org/src/contrib/Archive/spatstat.core/spatstat.core_2.4-4.tar.gz", repos=NULL, type="source")
# No luck with any of this









# looking at the measurements for songs with PC2 scores > 4; all seem ok (mainly truncated Sparrowfoot classic) except ones which I removed upstream
which(pca1scores$PC2 > 4)
allsongs$file_name[which(pca1scores$PC2 > 4)]

# with PC2 and PC3. Duration loads heavily onto PC3
ggbiplot(pca2,
         choices = c(1,3),
         groups = allnotes$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)


# let's try some preliminary interpretation
ggbiplot(pca1, 
         groups = allsongs$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)
pca1$rotation
# eigenvalues
pca1$sdev^2
paran(allsongs[,c(4:9,11:15)], 
      center = T,
      scale. = T)

# looks to me like PC1 explains the classic BCCH-CACH song split: note-level duration measurements load positively. Overall song duration and note number, plus basically all the measurements implicating higher overall frequency and dominant frequency slope, load positively.
    # If this were a PCA of BCCH and CACH songs, min_freq would probably load   positively along with the rest of the frequency measurements...but as we've   noticed, the Sparrowfoot Classic is usually sung with BCCH-like low notes.
# PC2 is trickier to interpret, but it might be where the heat is for me. Minimum frequency, dominant frequency slope, and between-note slope variation load positively, while song duration, note number, and between-note duration variation load negatively. It seems like PC2 separates traditionally CACH songs (+) from more Sparrowfoot classic-y songs (-) on the left, and on the right, BCCH-like songs whose amplitude-modulated terminal notes got separated by timer() (-) and those that did not (+). Notice in the middle of these two clusters, there are some songs by Mr. O and Nick which are probably BCCH-like songs with extra notes rather than a classic BCCH song with a split note

# library(geometry)
# # area of convex hull of points for one individual...
# pca1scores_Eliud <- pca1scores[which(allsongs$nickname=="Eliud"),1:2]
# # points on the border of the convex hull
# chull(pca1scores_Eliud)
# # [1]  46  85   3 117  48  40  42 103 101
# pca1scores_Eliud[chull(pca1scores_Eliud),]
# 
# area_ind <- function(z) { 
#   xy <- z[chull(z), ]; polyarea(xy[,1], xy[,2]) }
# area_ind(pca1scores_Eliud)
# # [1] 8.363258
# 
# # return areas of convex hulls of all individual groups
# CHareas <- data.frame(names = unique(allsongs$nickname),
#                     area = rep(NA))
# for (i in 1:length(unique(allsongs$nickname))) {
#   CHareas$area[i] <- area_ind(pca1scores[which(allsongs$nickname==unique(allsongs$nickname)[i]),1:2])
# }
# CHareas
# 
# install.packages("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/spatstat.core_2.4-4.tar", repos=NULL, type="source")
# remotes::install_github("maRce10/PhenotypeSpace")
# library(PhenotypeSpace)
# remotes::install_github("maRce10/warbleR")

# mess around with the PhenotypeSpace functions
data(example_space)
View(example_space)

song_pca_scores <- cbind(pca1scores, allsongs$ind_ID)
colnames(song_pca_scores)[12] <- "ind_ID"

space_size(song_pca_scores,
           dimensions = c("PC1", "PC2"),
           group = "ind_ID",
           type = "mst")

# rarefying to 100 samples iteratively (x30)
rarefact_space_size(song_pca_scores,
                    dimensions = c("PC1", "PC2"),
                    group = "ind_ID",
                    iterations = 30,
                    type = "mst")

# testmatrix <- as.matrix(song_pca_scores$PC1[which(song_pca_scores$ind_ID=="Om.NO")],
#                         song_pca_scores$PC2[which(song_pca_scores$ind_ID=="Om.NO")])
# library(vegan)
# dissim1 <- vegdist(x = testmatrix,
#                    method = "euclidean")
# testtree <- spantree(dissim1)
# plot(testtree,
#      col = "red")


ggbiplot(pca1, 
         groups = allsongs$nickname,
         obs.scale = 1,
         var.scale = 1,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

# try mst-knn with one individual
testdist <- song_pca_scores[which(song_pca_scores$ind_ID=="Rm.RR"),1:2]
testmatrix <- dist(testdist,
                   diag = T,
                   upper = T)
testmatrix <- as.matrix(testmatrix)
library(mstknnclust)
testMST <- mst.knn(testmatrix) 

igraph::V(testMST$network)$label.cex <- seq(0.6,0.6,length.out=vcount(testMST$network))

plot(testMST$network, 
     vertex.size=8, 
     vertex.color=igraph::clusters(testMST$network)$membership, 
     layout=igraph::layout.fruchterman.reingold(testMST$network, 
                                                niter=10000),
     main=paste("MST-kNN (Rm.RR) \n Clustering solution \n Number of clusters=", 
                testMST$cnumber,sep="" ))

# with all
testmatrix2 <- as.matrix(dist(song_pca_scores[1:2],
                              diag = T,
                              upper = T))
realtest <- mst.knn(testmatrix2)

igraph::V(realtest$network)$label.cex <- seq(0.6,0.6,length.out=vcount(realtest$network))
plot(realtest$network, 
     vertex.size=3, 
     vertex.color=igraph::clusters(realtest$network)$membership, 
     layout=igraph::layout.kamada.kawai(realtest$network),
     main=paste("MST-kNN (HZCH) \n Clustering solution \n Number of clusters=", 
                realtest$cnumber,sep="" ))

#
#
#
# 24 May 2023
# running the mst space size estimation again, but including outlier points...does not change the numbers at all.
MST_dists_1 <- rarefact_space_size(song_pca_scores,
                    dimensions = c("PC1", "PC2"),
                    group = "ind_ID",
                    iterations = 30,
                    type = "mst")
write.csv(MST_dists_1, "MST_dists_1.csv")

# checking other methods of space size estimation: kernel density (KD) and minimum convex polygon (MCP)
KD_dists_1 <- rarefact_space_size(song_pca_scores,
                                   dimensions = c("PC1", "PC2"),
                                   group = "ind_ID",
                                   iterations = 30,
                                   type = "density")
write.csv(KD_dists_1, "KD_dists_1.csv")

MCP_dists_1 <- rarefact_space_size(song_pca_scores,
                                  dimensions = c("PC1", "PC2"),
                                  group = "ind_ID",
                                  iterations = 30,
                                  type = "mcp")

# are the estimations consistent across methods?
plot(MST_dists_1$mean.size, 
     MCP_dists_1$mean.size)

plot(KD_dists_1$mean.size, 
     MCP_dists_1$mean.size)

plot(MST_dists_1$mean.size, 
     KD_dists_1$mean.size)
# No.
# What could be going on?

# fuck it
# plot all the birds separately
for (i in 1:length(unique(allsongs$ind_ID))) {
  a<-ggbiplot(pca1, 
           groups = allsongs$nickname,
           obs.scale = 1,
           var.scale = 1,
           alpha = ifelse(allsongs$ind_ID==unique(allsongs$ind_ID)[i], 1, 0),
           varname.size = 3,
           varname.adjust = 1)
  print(a)
}

# checking the kernel density images
library(adehabitatHR)
KD_test <- SpatialPointsDataFrame(coords = song_pca_scores[,1:2],
                         data = song_pca_scores)
head(as.data.frame(KD_test))
kern <- kernelUD(KD_test[,12])
image(kern)
# it appears that KD underestimates space sizes for individuals with variable, but less-clustered songs
# considering that MCP inflates space sizes of individuals with variable but highly-clustered songs, it seems like MST is the way to go.

setwd(genWDmac)
setwd(genWDlaptop)

list.files()
genotypes <- read.csv("STRUCTURE_data_with_localities.csv")
setwd(songWDmac)
setwd(songWDlaptop)
MST_dists_1 <- read.csv("MST_dists_1.csv")
singer_data <- genotypes[which(genotypes$ind_ID %in% c("HZ3", "HZ16", "HZ20", "HZ22", "HZ24", "HZ25", "HZ27", "HZ28", "HZ30", "HZ36")),]
MST_dists_1 <- cbind(MST_dists_1,
                    ind_ID = c("HZ24", "HZ28", "HZ22", "HZ36", "HZ16", "HZ25", "HZ3", "HZ30", "HZ20", "HZ27"))
# dataframe with song and genotype data
alldata <- merge(singer_data, MST_dists_1, by = "ind_ID")
View(alldata)
# setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
write.csv(alldata, "All_Data_1.csv")

# get recording dates for each individual
# first, write a csv of the finalized song-level measurements...for version control purposes
write.csv(allsongs, "SPPUA_song_level_measurements_final.csv")

for (i in 1:length(allsongs$file_name)) {
  allsongs$date_recorded[i] <- unlist(strsplit(allsongs$file_name[i], split = "_"))[3]
}

datesnames <- c(rep(NA, length(unique(allsongs$ind_ID))))
for (i in 1:length(unique(allsongs$ind_ID))) {
datesnames[i] <- length(unique(allsongs$date_recorded[which(allsongs$ind_ID==unique(allsongs$ind_ID)[i])]))
}

datesnames <- as.data.frame(cbind(datesnames, unique(allsongs$ind_ID)))[,1:2]
colnames(datesnames) <- c("number_days_recorded", "group")
alldata <- merge(alldata, datesnames, by = "group")
# need to clean up columns and column names
colnames(alldata)
alldata <- alldata[,-c(3,4,12)]
colnames(alldata)[10] <- "number_songs_used"
for (i in 11:14) {
  colnames(alldata)[i] <- paste(colnames(alldata)[i], "songrep", sep = "_")
}
colnames(alldata)
write.csv(alldata, "All_Data_1.csv")



#
#
#
#
#
#
#### Rarefaction Analysis & Viz ####
alldata <- read.csv("All_Data_1.csv")
allsongs <- read.csv("SPPUA_song_level_measurements_final.csv")
allsongs <- allsongs[,3:length(colnames((allsongs)))]
# try to plot a minimum spanning tree with simulated data
simdata <- data.frame(x = c(0, 1.6, 3.4, -0.7, 2, -4, 0.3, 1.1, -3, 2.2),
                      y = c(-1, 3, 2.5, 0.9, -2, 2, 0.1, 0, 1.5, -0.6))

simdist <- (dist(simdata))

library(vegan)
simtree <- spantree(simdist)

plot(simtree, 
     ord = simdata,
     pch = 19,
     col = "red") # done

# do it with real data
# get one individual's PC scores 
# sample to 100 and make a MST
Eliud_pca_scores <- song_pca_scores[which(song_pca_scores$ind_ID=="Gm.GO"),]
Eliud_rare <- Eliud_pca_scores[sample(1:nrow(Eliud_pca_scores), 100), ]
Eliud_dist <- dist(Eliud_rare[,1:2])
Eliud_tree <- spantree(Eliud_dist)
dev.off()
plot(Eliud_tree, 
     ord = Eliud_rare[,1:2],
     pch = 19,
     col = "red")
rm(Eliud_tree)

# make a function that will create spantree objects and rarefied PC score dataframes using n songs for each individual 
gettrees <- function(x, n) {
              score <- song_pca_scores[which(song_pca_scores$ind_ID==x),]
              rare <- score[sample(1:nrow(score), n), ]
              distmat <- dist(rare[,1:2])
              assign(paste(x, "tree", sep = "_"),
                     spantree(distmat),
                     envir = .GlobalEnv)
              assign(paste(x, "PCscore", sep = "_"),
                     rare,
                     envir = .GlobalEnv)
}
lapply(unique(allsongs$ind_ID), n=100, gettrees)

# pull them together into a list
treelist <- mget(ls(pattern = "_tree" ))
PCscorelist <- mget(ls(pattern = "_PCscore" ))

# plot the first item 
plot(treelist[[1]], 
     ord = PCscorelist[[1]][,1:2],
     pch = 19,
     col = "red")

#### Euclidean Minimum Spanning Tree with more than 2 principal components ####
library(emstreeR)
tryem <- ComputeMST(Eliud_rare[,1:3])
plotMST3D(treelist3D[[4]],
          pch = 19,
          col.pts = rgb(0,0,0.5,0.5))
# do it with all individuals
get3Dtrees <- function(x, n) {
  score <- song_pca_scores[which(song_pca_scores$ind_ID==x),]
  rare <- score[sample(1:nrow(score), n), 1:3]
  assign(paste(x, "3Dtree", sep = "_"),
         ComputeMST(rare),
         envir = .GlobalEnv)
}
lapply(unique(allsongs$ind_ID), n=100, get3Dtrees)
treelist3D <- mget(ls(pattern = "_3Dtree" ))

for (i in 1:length(treelist3D)) {
  alldata$size_songrep_3D[i] <- sum(treelist3D[[i]][["distance"]])
}

# now do it with 4 PC's
get4Dtrees <- function(x, n) {
  score <- song_pca_scores[which(song_pca_scores$ind_ID==x),]
  rare <- score[sample(1:nrow(score), n), 1:4]
  assign(paste(x, "4Dtree", sep = "_"),
         ComputeMST(rare),
         envir = .GlobalEnv)
}
lapply(unique(allsongs$ind_ID), n=100, get4Dtrees)
treelist4D <- mget(ls(pattern = "_4Dtree" ))

for (i in 1:length(treelist4D)) {
  alldata$size_songrep_4D[i] <- sum(treelist4D[[i]][["distance"]])
}


# get mean PC scores 
for (i in 1:length(PCscorelist)) {
  alldata$mean_PC1[i] <- mean(PCscorelist[[i]][["PC1"]])
}
for (i in 1:length(PCscorelist)) {
  alldata$mean_PC2[i] <- mean(PCscorelist[[i]][["PC2"]])
}
for (i in 1:length(PCscorelist)) {
  alldata$mean_PC3[i] <- mean(PCscorelist[[i]][["PC3"]])
}
for (i in 1:length(PCscorelist)) {
  alldata$mean_PC4[i] <- mean(PCscorelist[[i]][["PC4"]])
}

# get mean branch lengths for 2D, 3D, and 4D MSTs
for (i in 1:length(treelist)) {
  alldata$mean_MST_branchlength_2D[i] <- mean(treelist[[i]]$dist)
}
for (i in 1:length(treelist3D)) {
  alldata$mean_MST_branchlength_3D[i] <- mean(treelist3D[[i]]$distance)
}
for (i in 1:length(treelist4D)) {
  alldata$mean_MST_branchlength_4D[i] <- mean(treelist4D[[i]]$distance)
}

# hacked scatterplot with axis ranges encompassing the whole acoustic space - see file in repo "hack_scatterplot_plotMST3D.R"
# scatter3Dhack()
# plotMSThack
library(scatterplot3d)

# as a loop: plot all images separately
for (i in 1:length(treelist3D)) {
  plotMSThack(treelist3D[[i]],
              pch = 19,
              xlab = "PC1",
              ylab = "",
              zlab = "PC3",
              main = alldata$ind_ID[i],
              xlim = c(-6,6.25),
              ylim = c(-4.5,5.5),
              zlim = c(-5,4.25),
              col.segts = "black",
              highlight.3d = T,
              cex.symbols = 1.5,
              cex.axis = 0.5)
  dims <- par("usr")
  par(mar=c(5,6,4,1))
  x <- dims[1]+ 0.85*diff(dims[1:2])
  y <- dims[3]+ 0.08*diff(dims[3:4])
  text(x,y,"PC2",srt=30)
}

# graphical params to plot all images in 1 figure
par(mfrow = c(5,2), oma = c(2,1.5,0,0))
par(mar = c(3,3,1,1))

# generate figure
for (i in 1:length(treelist3D)) {
  plotMSThack(treelist3D[[i]],
              pch = 19,
              xlab = "",
              ylab = "",
              zlab = "",
              main = alldata$ind_ID[i],
              xlim = c(-6,6.25),
              ylim = c(-4.5,5.5),
              zlim = c(-5,4.25),
              col.segts = "black",
              highlight.3d = T,
              cex.symbols = 1.5,
              cex.axis = 0.5)
  par(las = 0)
  mtext(text = "PC3", 
        side = 2, 
        outer = TRUE, 
        line = 0.3, 
        padj = 1, 
        cex = 0.75)
  mtext(text = "PC1", 
        side = 1, 
        outer = TRUE, 
        line = 0, 
        padj = 1, 
        cex = 0.75)
}
# still need PC2 axis label and something to explain the color value change

#
#
#
#
plot(alldata$prob_CA, alldata$mean.size_songrep)
plot(alldata$prob_CA, alldata$size_songrep_3D)
plot(alldata$prob_CA, alldata$size_songrep_4D)

# get number of songs data
for (i in 1:length(unique(allsongs$ind_ID))) {
  alldata$number_songs_recorded[i] <- length(allsongs$ind_ID[which(allsongs$ind_ID==unique(allsongs$ind_ID)[i])])
}

#### linear regression ####
# for every model, the model without the total song counts predictor explains more variation.

# LM with MST PC1-2
LM2D_Ancestry_Counts <- glm(mean.size_songrep ~ prob_CA + number_songs_recorded,
              data = alldata)
LM2D_Ancestry <- glm(mean.size_songrep ~ prob_CA,
                           data = alldata)
summary(LM2D_Ancestry_Counts)
summary(LM2D_Ancestry)
AIC(LM2D_Ancestry_Counts) # 71.70871
AIC(LM2D_Ancestry) # 69.89889

# LM with PC1-3
LM3D_Ancestry_Counts <- lm(size_songrep_3D ~ prob_CA + number_songs_recorded,
                           data = alldata)
LM3D_Ancestry <- lm(size_songrep_3D ~ prob_CA,
                    data = alldata)
summary(LM3D_Ancestry_Counts)
summary(LM3D_Ancestry)
AIC(LM3D_Ancestry_Counts) # 84.53885
AIC(LM3D_Ancestry) # 82.55541

# # LM with PC1-4
# LM4D_Ancestry_Counts <- lm(size_songrep_4D ~ prob_CA + number_songs_recorded,
#                            data = alldata)
# LM4D_Ancestry <- lm(size_songrep_4D ~ prob_CA,
#                     data = alldata)
# summary(LM4D_Ancestry_Counts)
# summary(LM4D_Ancestry)
# AIC(LM4D_Ancestry_Counts) # 89.89574
# AIC(LM4D_Ancestry) # 87.89931

# checking the redisuals
plot(LM2D_Ancestry)
plot(LM3D_Ancestry)




hist(alldata$size_songrep_3D)

