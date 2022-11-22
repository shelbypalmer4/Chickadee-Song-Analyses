#### AOS poster ####
# extracting measurements from hybrid zone songs for multivariate analyses
# first thing to do: quickly visualize all spectrograms and subset to only those with satisfactory signal:noise ratio

# easy names for each working directory
Eliud<-"C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Eliud (Gm-GO_HZCH_SPPUA)/chopped"
Pre<-"C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Pre (Om-YR_HZCH_SPPUA)/chopped"
Remy<-"C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Remy (Rm-RR_HZCH_SPPUA)/chopped"
Nick<-"C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Nick (Ym-NG_HZCH_SPPUA)/chopped"
JayZ<-"C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Jay-Z (Nm-GR_HZCH_SPPUA)/chopped"
MrOrange<-"C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Mr Orange (Om-NO_HZCH_SPPUA)/chopped"


library(seewave)
library(tuneR)


setwd(Eliud)

# Jay's loop for naming image files
# for (i in 1:length(list.files())){
#   wavname <- list.files()[i]
#   dev.print(device = png,
#             filename = paste(wavname, ".png", sep = ""),
#             width = 1264,
#             height = 826)
# }


# I'll practice with just 1 song: read it in, make a spectrogram, bandpass filter to 2500-10000 Hz, and export the spectrogram to the working directory
a<-readWave("Poecile.sp_Gm.GO_Apr032022_SparrowfootPUA.HenryCO.MO_SMP_a_0.57.wav")
b<-bwfilter(a, 
            from=2500, 
            to=10000, 
            n = 8, 
            bandpass=TRUE, 
            output="Wave") 
c<-spectro(b, 
           wl = 512, 
           ovlp = 95, 
           collevels = seq(-42,0,6), 
           flim = c(0, 10),
           osc = F, 
           scale = F, 
           colgrid = "gray", 
           cexlab = 0.8, 
           cexaxis = 0.7) 
dev.print(device = png,   
          filename = "test1.png", 
          width = 1264,  
          height = 826)   


# this function reads in a WAV file in a working directory, BP filters in 2500-10000 Hz, normalizes amplitude, and makes a spectrogram with timer() intervals overlaid. lapply() applies it over the whole folder. Needs work.

allspec<-function(x) {
  a<-readWave(x)
  b<-fir(a, 
         from = 2500, 
         to = 10000, 
         bandpass = TRUE,
         output="Wave")
  normalize(b, 
            unit = c("24"))
  # rmnoise(b, 
  #         output = "Wave") # for JayZ
  # afilter(b, threshold = 5, output = "Wave")
  png(filename = paste("figures/", x, ".png", sep = ""))
  spectro(b, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 10),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7)
  par(new = T)
  timer(b, 
        dmin = 0.05,
        envt = "hil",
        msmooth=c(512, 95),
        threshold = 7)
  dev.off()
}
lapply(list.files(pattern = ".wav"), allspec)

# this function reads in a WAV file in a working directory, BP filters in 2500-10000 Hz, normalizes amplitude, cuts resulting WAV at beginning and end of signal given by timer(), and makes a spectrogram. lapply() applies it over the whole folder.
cutspec<-function(x) {
  a<-readWave(x)
  b<-fir(a, 
         from = 2500, 
         to = 10000, 
         bandpass = TRUE,
         output="Wave")
  c<-normalize(b, unit = c("24"))
  d<-timer(c, 
        dmin = 0.1,
        envt = "hil",
        msmooth=c(512, 90),
        threshold = 5) # add plot = FALSE when done checking cuts
  e<-cutw(c,
          from = d$s.start[1], 
          to = d$s.end[length(d$s.end)],
          output = "Wave")
  png(filename = paste("cut_spec/", x, ".png", sep = ""))
  spectro(e, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 10),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7) # Just to check cuts, can be removed
  dev.off()
}
lapply(list.files(pattern = ".wav"), cutspec)





#### SONG-LEVEL MEASUREMENTS ####
# re-set working directory for each individual


setwd(Remy)
RemyDF<-data.frame(list.files())
setwd(Pre)
PreDF<-data.frame(list.files())
setwd(JayZ)
JayZDF<-data.frame(list.files())
setwd(Nick)
NickDF<-data.frame(list.files())
setwd(MrOrange)
MrOrangeDF<-data.frame(list.files())

#### Eliud ####
setwd(Eliud)
EliudDF<-data.frame(list.files())

EliudDF$duration<-rep(NA, length(EliudDF[,1]))
EliudDF$max_frequency<-rep(NA, length(EliudDF[,1]))
EliudDF$min_frequency<-rep(NA, length(EliudDF[,1]))
EliudDF$median_frequency<-rep(NA, length(EliudDF[,1]))
EliudDF$frequency_BW<-rep(NA, length(EliudDF[,1]))
EliudDF$entropy<-rep(NA, length(EliudDF[,1]))
EliudDF$mean_frequency<-rep(NA, length(EliudDF[,1]))
EliudDF$note_number<-rep(NA, length(EliudDF[,1]))
EliudDF$signal_pause_ratio<-rep(NA, length(EliudDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  b<-fir(a,
         from = 2500,
         to = 10000,
         bandpass = TRUE,
         output="Wave")
  normalize(b,
            unit = c("24"))
  c<-timer(b,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  d<-cutw(b,
          from = c$s.start[1],
          to = c$s.end[length(c$s.end)],
          output = "Wave")
  e<-timer(d,
            dmin = 0.05,
            envt = "hil",
            msmooth=c(512, 95),
            threshold = 7,
            plot = FALSE)
  EliudDF$duration[i]<-duration(d)
  EliudDF$max_frequency[i]<-max(dfreq(d, plot = F)[,2])
  EliudDF$min_frequency[i]<-min(dfreq(d, plot = F)[,2])
  EliudDF$median_frequency[i]<-median(dfreq(d, plot = F)[,2])
  EliudDF$frequency_BW[i]<-(max(dfreq(d, plot = F)[,2])-min(dfreq(d, plot = F)[,2]))
  EliudDF$entropy[i]<-H(d)
  EliudDF$mean_frequency[i]<-mean(dfreq(d, plot=F)[,2])
  EliudDF$note_number[i]<-length(c$s)
  EliudDF$signal_pause_ratio[i]<-e$r
}
EliudDF$color_bands<-rep("Gm_GO", length(EliudDF[,1]))


#### Remy ####
setwd(Remy)
RemyDF<-data.frame(list.files())

RemyDF$duration<-rep(NA, length(RemyDF[,1]))
RemyDF$max_frequency<-rep(NA, length(RemyDF[,1]))
RemyDF$min_frequency<-rep(NA, length(RemyDF[,1]))
RemyDF$median_frequency<-rep(NA, length(RemyDF[,1]))
RemyDF$frequency_BW<-rep(NA, length(RemyDF[,1]))
RemyDF$entropy<-rep(NA, length(RemyDF[,1]))
RemyDF$mean_frequency<-rep(NA, length(RemyDF[,1]))
RemyDF$note_number<-rep(NA, length(RemyDF[,1]))
RemyDF$signal_pause_ratio<-rep(NA, length(RemyDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  b<-fir(a,
         from = 2500,
         to = 10000,
         bandpass = TRUE,
         output="Wave")
  normalize(b,
            unit = c("24"))
  c<-timer(b,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  d<-cutw(b,
          from = c$s.start[1],
          to = c$s.end[length(c$s.end)],
          output = "Wave")
  e<-timer(d,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  RemyDF$duration[i]<-duration(d)
  RemyDF$max_frequency[i]<-max(dfreq(d, plot = F)[,2])
  RemyDF$min_frequency[i]<-min(dfreq(d, plot = F)[,2])
  RemyDF$median_frequency[i]<-median(dfreq(d, plot = F)[,2])
  RemyDF$frequency_BW[i]<-(max(dfreq(d, plot = F)[,2])-min(dfreq(d, plot = F)[,2]))
  RemyDF$entropy[i]<-H(d)
  RemyDF$mean_frequency[i]<-mean(dfreq(d, plot=F)[,2])
  RemyDF$note_number[i]<-length(c$s)
  RemyDF$signal_pause_ratio[i]<-e$r
}
RemyDF$color_bands<-rep("Rm_RR", length(RemyDF[,1]))


#### Pre ####
setwd(Pre)
PreDF<-data.frame(list.files())

PreDF$duration<-rep(NA, length(PreDF[,1]))
PreDF$max_frequency<-rep(NA, length(PreDF[,1]))
PreDF$min_frequency<-rep(NA, length(PreDF[,1]))
PreDF$median_frequency<-rep(NA, length(PreDF[,1]))
PreDF$frequency_BW<-rep(NA, length(PreDF[,1]))
PreDF$entropy<-rep(NA, length(PreDF[,1]))
PreDF$mean_frequency<-rep(NA, length(PreDF[,1]))
PreDF$note_number<-rep(NA, length(PreDF[,1]))
PreDF$signal_pause_ratio<-rep(NA, length(PreDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  b<-fir(a,
         from = 2500,
         to = 10000,
         bandpass = TRUE,
         output="Wave")
  normalize(b,
            unit = c("24"))
  c<-timer(b,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  d<-cutw(b,
          from = c$s.start[1],
          to = c$s.end[length(c$s.end)],
          output = "Wave")
  e<-timer(d,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  PreDF$duration[i]<-duration(d)
  PreDF$max_frequency[i]<-max(dfreq(d, plot = F)[,2])
  PreDF$min_frequency[i]<-min(dfreq(d, plot = F)[,2])
  PreDF$median_frequency[i]<-median(dfreq(d, plot = F)[,2])
  PreDF$frequency_BW[i]<-(max(dfreq(d, plot = F)[,2])-min(dfreq(d, plot = F)[,2]))
  PreDF$entropy[i]<-H(d)
  PreDF$mean_frequency[i]<-mean(dfreq(d, plot=F)[,2])
  PreDF$note_number[i]<-length(c$s)
  PreDF$signal_pause_ratio[i]<-e$r
}
PreDF$color_bands<-rep("Om_YR", length(PreDF[,1]))


#### JayZ ####
setwd(JayZ)
JayZDF<-data.frame(list.files())

JayZDF$duration<-rep(NA, length(JayZDF[,1]))
JayZDF$max_frequency<-rep(NA, length(JayZDF[,1]))
JayZDF$min_frequency<-rep(NA, length(JayZDF[,1]))
JayZDF$median_frequency<-rep(NA, length(JayZDF[,1]))
JayZDF$frequency_BW<-rep(NA, length(JayZDF[,1]))
JayZDF$entropy<-rep(NA, length(JayZDF[,1]))
JayZDF$mean_frequency<-rep(NA, length(JayZDF[,1]))
JayZDF$note_number<-rep(NA, length(JayZDF[,1]))
JayZDF$signal_pause_ratio<-rep(NA, length(JayZDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  b<-fir(a,
         from = 2500,
         to = 10000,
         bandpass = TRUE,
         output="Wave")
  normalize(b,
            unit = c("24"))
  c<-timer(b,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  d<-cutw(b,
          from = c$s.start[1],
          to = c$s.end[length(c$s.end)],
          output = "Wave")
  e<-timer(d,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  JayZDF$duration[i]<-duration(d)
  JayZDF$max_frequency[i]<-max(dfreq(d, plot = F)[,2])
  JayZDF$min_frequency[i]<-min(dfreq(d, plot = F)[,2])
  JayZDF$median_frequency[i]<-median(dfreq(d, plot = F)[,2])
  JayZDF$frequency_BW[i]<-(max(dfreq(d, plot = F)[,2])-min(dfreq(d, plot = F)[,2]))
  JayZDF$entropy[i]<-H(d)
  JayZDF$mean_frequency[i]<-mean(dfreq(d, plot=F)[,2])
  JayZDF$note_number[i]<-length(c$s)
  JayZDF$signal_pause_ratio[i]<-e$r
}
JayZDF$color_bands<-rep("Nm_GR", length(JayZDF[,1]))


#### Nick ####
setwd(Nick)
NickDF<-data.frame(list.files())

NickDF$duration<-rep(NA, length(NickDF[,1]))
NickDF$max_frequency<-rep(NA, length(NickDF[,1]))
NickDF$min_frequency<-rep(NA, length(NickDF[,1]))
NickDF$median_frequency<-rep(NA, length(NickDF[,1]))
NickDF$frequency_BW<-rep(NA, length(NickDF[,1]))
NickDF$entropy<-rep(NA, length(NickDF[,1]))
NickDF$mean_frequency<-rep(NA, length(NickDF[,1]))
NickDF$note_number<-rep(NA, length(NickDF[,1]))
NickDF$signal_pause_ratio<-rep(NA, length(NickDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  b<-fir(a,
         from = 2500,
         to = 10000,
         bandpass = TRUE,
         output="Wave")
  normalize(b,
            unit = c("24"))
  c<-timer(b,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  d<-cutw(b,
          from = c$s.start[1],
          to = c$s.end[length(c$s.end)],
          output = "Wave")
  e<-timer(d,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  NickDF$duration[i]<-duration(d)
  NickDF$max_frequency[i]<-max(dfreq(d, plot = F)[,2])
  NickDF$min_frequency[i]<-min(dfreq(d, plot = F)[,2])
  NickDF$median_frequency[i]<-median(dfreq(d, plot = F)[,2])
  NickDF$frequency_BW[i]<-(max(dfreq(d, plot = F)[,2])-min(dfreq(d, plot = F)[,2]))
  NickDF$entropy[i]<-H(d)
  NickDF$mean_frequency[i]<-mean(dfreq(d, plot=F)[,2])
  NickDF$note_number[i]<-length(c$s)
  NickDF$signal_pause_ratio[i]<-e$r
}
NickDF$color_bands<-rep("Ym_NG", length(NickDF[,1]))


#### Mr. Orange ####
setwd(MrOrange)
MrOrangeDF<-data.frame(list.files())

MrOrangeDF$duration<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$max_frequency<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$min_frequency<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$median_frequency<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$frequency_BW<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$entropy<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$mean_frequency<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$note_number<-rep(NA, length(MrOrangeDF[,1]))
MrOrangeDF$signal_pause_ratio<-rep(NA, length(MrOrangeDF[,1]))

for (i in 1:length(list.files())) {
  a<-readWave(list.files()[i])
  b<-fir(a,
         from = 2500,
         to = 10000,
         bandpass = TRUE,
         output="Wave")
  normalize(b,
            unit = c("24"))
  c<-timer(b,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  d<-cutw(b,
          from = c$s.start[1],
          to = c$s.end[length(c$s.end)],
          output = "Wave")
  e<-timer(d,
           dmin = 0.05,
           envt = "hil",
           msmooth=c(512, 95),
           threshold = 7,
           plot = FALSE)
  MrOrangeDF$duration[i]<-duration(d)
  MrOrangeDF$max_frequency[i]<-max(dfreq(d, plot = F)[,2])
  MrOrangeDF$min_frequency[i]<-min(dfreq(d, plot = F)[,2])
  MrOrangeDF$median_frequency[i]<-median(dfreq(d, plot = F)[,2])
  MrOrangeDF$frequency_BW[i]<-(max(dfreq(d, plot = F)[,2])-min(dfreq(d, plot = F)[,2]))
  MrOrangeDF$entropy[i]<-H(d)
  MrOrangeDF$mean_frequency[i]<-mean(dfreq(d, plot=F)[,2])
  MrOrangeDF$note_number[i]<-length(c$s)
  MrOrangeDF$signal_pause_ratio[i]<-e$r
}
MrOrangeDF$color_bands<-rep("Om_NO", length(MrOrangeDF[,1]))

# one dataframe with all the birds + write to a csv
SparrowfootDF<-rbind(EliudDF, RemyDF, PreDF, NickDF, JayZDF, MrOrangeDF)
setwd("C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA")
write.csv(SparrowfootDF, 
            file="sparrowfoot_songleveldata_072122.csv")

########### PRINCIPAL COMPONENT ANALYSES ############

#### All individuals ####
# checking histograms of each measurement
hist(SparrowfootDF$duration)
which(SparrowfootDF$duration > 3)
SparrowfootDF<-SparrowfootDF[-c(503, 516, 668),]

hist(SparrowfootDF$max_frequency) #very bimodal

hist(SparrowfootDF$min_frequency)

hist(SparrowfootDF$frequency_BW) #also very bimodal

hist(SparrowfootDF$entropy)

hist(SparrowfootDF$note_number, 
     breaks=23, 
     xlim = range(1:22))

hist(SparrowfootDF$median_frequency)

hist(SparrowfootDF$signal_pause_ratio)
which(SparrowfootDF$signal_pause_ratio > 20)
SparrowfootDF<-SparrowfootDF[-c(398, 418),]

hist(SparrowfootDF$mean_frequency)


# running the PCA
pca1<-prcomp(SparrowfootDF[,2:10], center = T, scale. = T)
summary(pca1)

# getting eigenvalues
eig<-pca1$sdev^2

pca1$rotation

biplot(pca1) # SUPAH UGLYYYY

# making the plot
library(ggplot2)
library(AMR)
library(devtools)
library(ggbiplot)

ggbiplot(pca1, 
         groups = SparrowfootDF$color_bands,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

# plot with only 2 birds
library(colorspace)
alpha <- ifelse(exes$individuals==c("Om_NO", "Ym_NG"), 1, 0)
ggbiplot(pca1, 
         groups = SparrowfootDF$color_bands,
         alpha = alpha,
         varname.size = 3,
         varname.adjust = 1,
         obs.scale = 0.5)



#### PCA Eliud ####
# running the PCA
pcaEliud<-prcomp(EliudDF[,2:10], center = T, scale. = T)
summary(pcaEliud)

# getting eigenvalues
eigEliud<-pcaEliud$sdev^2
eigEliud

pcaEliud$rotation

# making the plot
ggbiplot(pcaEliud,
         obs.scale = 0.7,
         varname.size = 3,
         varname.adjust = 1)


#### PCA Remy ####
# running the PCA
pcaRemy<-prcomp(RemyDF[,2:10], center = T, scale. = T)
summary(pcaRemy)

# getting eigenvalues
eigRemy<-pcaRemy$sdev^2
eigRemy

pcaRemy$rotation

# making the plot
ggbiplot(pcaRemy,
         obs.scale = 0.7,
         varname.size = 3,
         varname.adjust = 1)


#### PCA Pre ####
# running the PCA
pcaPre<-prcomp(PreDF[,2:10], center = T, scale. = T)
summary(pcaPre)

# getting eigenvalues
eigPre<-pcaPre$sdev^2
eigPre

pcaPre$rotation

# making the plot
ggbiplot(pcaPre,
         obs.scale = 0.7,
         varname.size = 3,
         varname.adjust = 1)


#### PCA JayZ ####
# running the PCA
pcaJayZ<-prcomp(JayZDF[,2:10], center = T, scale. = T)
summary(pcaJayZ)

# getting eigenvalues
eigJayZ<-pcaJayZ$sdev^2
eigJayZ

pcaJayZ$rotation

# making the plot
ggbiplot(pcaJayZ,
         obs.scale = 0.7,
         varname.size = 3,
         varname.adjust = 1)


#### PCA Nick ####
# running the PCA
pcaNick<-prcomp(NickDF[,2:10], center = T, scale. = T)
summary(pcaNick)

# getting eigenvalues
eigNick<-pcaNick$sdev^2
eigNick

pcaNick$rotation

# making the plot
ggbiplot(pcaNick,
         obs.scale = 0.7,
         varname.size = 3,
         varname.adjust = 1)


#### PCA MrOrange ####
# running the PCA
pcaMrOrange<-prcomp(MrOrangeDF[,2:10], center = T, scale. = T)
summary(pcaMrOrange)

# getting eigenvalues
eigMrOrange<-pcaMrOrange$sdev^2
eigMrOrange

pcaMrOrange$rotation

# making the plot
ggbiplot(pcaMrOrange,
         obs.scale = 0.7,
         varname.size = 3,
         varname.adjust = 1)

#### Practicing note-level analyses on Mr. Orange ####



#### New Idea ####
# subset each bird's DF to only songs with 2 notes--make a new DF with all birds--run a PCA on that (i.e. Is there clustering by individual in BCCH-type songs, despite their stereotypy?)

# Two different ways to characterize BCCH song...
which(SparrowfootDF$note_number==2)
which(SparrowfootDF$max_frequency<4.5)

TwoNoteDF<-SparrowfootDF[which(SparrowfootDF$note_number==2),]
LowFreqDF<-SparrowfootDF[which(SparrowfootDF$max_frequency<4.5),]

# PCA with BCCH songs as characterized by max frequency < 4.5 kHz
pcaBCCH1<-prcomp(LowFreqDF[,2:10], center = T, scale. = T)
summary(pcaBCCH1)

# eigenvalues
eigLF<-pcaBCCH1$sdev^2
eigLF

# plotting it
library(ggplot2)
library(AMR)
library(devtools)
library(ggbiplot)

ggbiplot(pcaBCCH1, 
         groups = LowFreqDF$color_bands,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

# PCA with BCCH songs as characterized by note number = 2
pcaBCCH2<-prcomp(TwoNoteDF[,2:8,10], center = T, scale. = T)
summary(pcaBCCH2)

# eigenvalues
eigTN<-pcaBCCH2$sdev^2
eigTN

# plotting it
ggbiplot(pcaBCCH2, 
         groups = TwoNoteDF$color_bands,
         alpha = 0.5,
         varname.size = 3,
         varname.adjust = 1)

ggplot(SparrowfootDF, aes(x=note_number)) + 
  geom_histogram(binwidth=1)

par(mfrow=c(2,1))
plot(2, 13)
plot(3, 13)