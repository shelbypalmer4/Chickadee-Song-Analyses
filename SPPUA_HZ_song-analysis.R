#### Quantifying the variation in song of chickadees with mixed ancastry ####

setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses")
# setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
allsongs <- read.csv("HZCH_song-level-measurements_1.csv")
which(allsongs$number_notes>12)
# let's go ahead and exclude the superlong songs from Mr. O
allsongs <- allsongs[-which(allsongs$number_notes>12),]
# also going to retroactively exclude a mis-measured noisy song from Nick
allsongs <- allsongs[-which(allsongs$file_name=="Poecile.sp_Ym.NG_Mar192022_SparrowfootPUA.HenryCO.MO_SMP_e_0.59.wav"),]
allnotes <- allnotes[-which(allnotes$file_name=="Poecile.sp_Ym.NG_Mar192022_SparrowfootPUA.HenryCO.MO_SMP_e_0.59.wav"),]

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
pca1 <- prcomp(allsongs[,4:15], 
               center = T,
               scale. = T)

# looking at the variable loadings

pca1$rotation
pca1scores <- as.data.frame(pca1$x)

# remove signal pause ratio; probably too noisy of a measurement, sadly
pca1 <- prcomp(allsongs[,c(4:9,11:15)], 
               center = T,
               scale. = T)

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
allnotes <- read.csv("HZCH_note-level-measurements_2.csv")
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









# looking at the measurements for songs with PC2 scores > 4; all seem ok (mainly truncated Sparrowfoot classic) except one which I removed upstream
which(pca1scores$PC2 > 4)
allsongs[which(pca1scores$PC2 > 4),]

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
# PC2 is trickier to interpret, but it might be where the heat is for me. Minimum frequency, dominant frequency slope, and between-note slope variation load positively, while song duration, note number, and between-note duration variation load negatively. It seems like PC2 separates traditionally CACH songs (+) from more Sparrowfoot classic-y songs (-) on the left, and on the right, BCCH-like songs whose amplitude-modulated terminal notes got separated by timer() (-) and those that did not (+). Notice in the middle of these two clusters, there are some songs by Mr. O and Nick which are probably BCCH-like songs with extra notes rather than a classic BCCH song with a split note.

#
#
#
#### Workflow going forward ####
# Chop the songs from Doc Holliday that I got on my last day out recording (After quality checks, Doc Holliday is currently at 99 songs, so I will have to use these after all)
# Take note-level measurements of the newly-chopped songs and check the raw measurements alongside spectrograms (should be doable since it's not very many songs)
# Add note-level measurements to HZCH, re-order, and re-export csv
# Add the new file names to HZCH_QC_Y, re-order, and re-export csv
# When all note-level measurements look ok, take song-level measurements
# Add song-level measurements to HZsongs, re-order, and re-export

#
#
#
#### Once all song measurements are finalized ####
# From each individual, sample 100 songs without replacement and create a new dataframe with these measurements
# Re-run PCA on these measurements
# Since PhenotypeSpace won't load, figure out a way to quantify the total area occupied by each individual as a proxy for repertoire size
# What is the best clustering method to use? How will points be grouped?

library(geometry)
# area of convex hull of points for one individual...
pca1scores_Eliud <- pca1scores[which(allsongs$nickname=="Eliud"),1:2]
# points on the border of the convex hull
chull(pca1scores_Eliud)
# [1]  46  85   3 117  48  40  42 103 101
pca1scores_Eliud[chull(pca1scores_Eliud),]

area_ind <- function(z) { 
  xy <- z[chull(z), ]; polyarea(xy[,1], xy[,2]) }
area_ind(pca1scores_Eliud)
# [1] 8.363258

# return areas of convex hulls of all individual groups
CHareas <- data.frame(names = unique(allsongs$nickname),
                    area = rep(NA))
for (i in 1:length(unique(allsongs$nickname))) {
  CHareas$area[i] <- area_ind(pca1scores[which(allsongs$nickname==unique(allsongs$nickname)[i]),1:2])
}
CHareas

