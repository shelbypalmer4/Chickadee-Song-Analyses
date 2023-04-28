#### Extracting measurements from wav files of HZCH song ####

# setwd("C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/AllSongsSPPUA")
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
library(tuneR)
library(seewave)

length(list.files())
# okay, I have 1347 chopped recordings. The first thing I need to do is quality control & exception handling, which will have to be done visually by looking at spectrograms.

# AllSPPUA <- data.frame(file_names = list.files(),
#                        include = rep(NA),
#                        threshold = rep(NA))
# write.csv(AllSPPUA, "C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses/SPPUA_Hz_Quality-and-exceptions.csv")
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
AllSPPUA <- read.csv("SPPUA_Hz_Quality-and-exceptions.csv")

# generate spectrograms + timer intervals for signal portion of every chopped recording
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
cutspec<-function(x) {
  a<-readWave(x)
  b<-fir(a, 
         from = 2500, 
         to = 10000, 
         bandpass = TRUE,
         output="Wave")
  c<-normalize(b, unit = c("24"))
  png(filename = paste(x, ".png", sep = ""),
      width = 634,
      height = 327)
  spectro(c, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 10),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7)
  par(new=T)
  try(timer(c, 
            dmin = 0.05,
            envt = "hil",
            msmooth=c(512, 90),
            threshold = 10))
  dev.off()
}
lapply(list.files(pattern = ".wav"), cutspec)
