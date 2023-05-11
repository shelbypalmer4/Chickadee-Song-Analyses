#### Quantifying the variation in song of chickadees with mixed ancastry ####

setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses")
allsongs <- read.csv("HZCH_song-level-measurements_1.csv")
which(allsongs$number_notes>12)
# let's go ahead and exclude the superlong songs from Mr. O
allsongs <- allsongs[-which(allsongs$number_notes>12),]

# For starters, a PCA to see how the variation in songs falls out
colnames(allsongs)
pca1 <- prcomp(allsongs[,4:15], 
               center = T,
               scale. = T)

# looking at the variable loadings

pca1$rotation

# make a plot
# library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(pca1, 
         groups = allsongs$ind_ID,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

# Life is short; lets's make a note-level plot!!
allnotes <- read.csv("HZCH_note-level-measurements_2.csv")
for (i in 1:length(allnotes$file_name)) {
  allnotes$ind_ID[i] <- unlist(strsplit(allnotes$file_name[i],
                                        split = "_"))[2]
}

colnames(allnotes)
pca2 <- prcomp(allnotes[,4:10], 
               center = T,
               scale. = T)
pca2$rotation
ggbiplot(pca2, 
         groups = allnotes$ind_ID,
         obs.scale = 1,
         var.scale = 1,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)
