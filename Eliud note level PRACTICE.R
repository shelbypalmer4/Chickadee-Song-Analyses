### Experimenting with note-level analyses on Eliud ###

setwd("C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/Eliud (Gm-GO_HZCH_SPPUA)/chopped")

library(seewave)
library(tuneR)
library(magrittr)

##### First we practice on one song. ####
# read in song file and filter
a<-readWave("Poecile.sp_Gm.GO_Apr032022_SparrowfootPUA.HenryCO.MO_SMP_a_3.39.wav")
a1<-fir(a,
         from=2500,
         to=10000,
         output="Wave")

# get temporal measurements
aNotes<-timer(a1,
              dmin=0.05, 
              envt="hil", 
              msmooth=c(512, 90), 
              threshold=7) 
aNotes

# checking timer() performance
spectro(a1,
        ovlp = 95,
        collevels = seq(-42,0,6),
        flim = c(0, 10),
        scale = F,
        colgrid = "gray",
        cexlab = 0.8,
        cexaxis = 0.7)
par(new=T)
timer(a1,
      dmin=0.05, 
      envt="hil", 
      msmooth=c(512, 90), 
      threshold=7)

# I want a single note to work with when needed
aFirstnote<-cutw(a1,
                 from=aNotes$s.start[1],
                 to=aNotes$s.end[1],
                 output="Wave",
                 plot=F)
aSecondnote<-cutw(a1,
                  from=aNotes$s.start[2],
                  to=aNotes$s.end[2],
                  output="Wave",
                  plot=F)
aThirdnote<-cutw(a1,
                 from=aNotes$s.start[3],
                 to=aNotes$s.end[3],
                 output="Wave",
                 plot=F)
aFourthnote<-cutw(a1,
                  from=aNotes$s.start[4],
                  to=aNotes$s.end[4],
                  output="Wave",
                  plot=F)

#### MEASUREMENT EXTRACTION FUNCTIONS ####

crit = -24 # critical amplitude value when we use dB='max0'

# Overall Frequency Measurements--using the mean frequency spectrum
MaxFreq<-function(x) {
  max(meanspec(x, 
               flim=c(0,10), 
               wl=512, 
               dB='max0',
               plot=F)[,1][meanspec(x, 
                                    flim=c(0,10), 
                                    wl = 512, 
                                    dB='max0',
                                    plot=F)[,2]>crit])
}

MinFreq<-function(x) {
  min(meanspec(x, 
               flim=c(0,10), 
               wl=512, 
               dB='max0',
               plot=F)[,1][meanspec(x, 
                                    flim=c(0,10), 
                                    wl = 512, 
                                    dB='max0',
                                    plot=F)[,2]>crit])
}

MeanFreq<-function(x) {
  mean(meanspec(x, 
               flim=c(0,10), 
               wl=512, 
               dB='max0',
               plot=F)[,1][meanspec(x, 
                                    flim=c(0,10), 
                                    wl = 512, 
                                    dB='max0',
                                    plot=F)[,2]>crit])
}

PeakFreq<-function(x) {
meanspec(x, 
         flim=c(0,10), 
         wl = 512, 
         dB='max0',
         plot=F)[,1][match(c(0),
                              meanspec(x, 
                                       flim=c(0,10), 
                                       wl = 512, 
                                       dB='max0',
                                       plot=F)[,2])]
}

# Dominant Frequency Measurements--using dfreq()
MaxDFreq<-function(x) {
  max(dfreq(x, plot = F)[,2])
}

MinDFreq<-function(x) {
  min(dfreq(d, plot = F)[,2])
}

MeanDFreq<-function(x) {
  mean(dfreq(d, plot = F)[,2])
}

MedianDFreq<-function(x) {
  median(dfreq(d, plot = F)[,2])
}

AbsDFreqMaxSlope<-function(x) {
  max(abs(diff(dfreq(x, 
                     ovlp=95, 
                     threshold=5, 
                     plot=F)[,2])))
}

#### Dataframe for just the first recording ####

firstDF<-data.frame(note_num=seq(1:length(aNotes$s.start)), 
                       file_name=rep("Poecile.sp_Gm.GO_Apr032022_SparrowfootPUA.HenryCO.MO_SMP_a_0.08.wav"))

for (i in 1:length(aNotes$s.start)) {
  firstDF$Duration[i]<-aNotes$s[i]
  firstDF$Max_Freq[i]<-MaxFreq(cutw(a1,
                                       from=aNotes$s.start[i],
                                       to=aNotes$s.end[i],
                                       output="Wave",
                                       plot=F))
  firstDF$Min_Freq[i]<-MinFreq(cutw(a1,
                                       from=aNotes$s.start[i],
                                       to=aNotes$s.end[i],
                                       output="Wave",
                                       plot=F))
  firstDF$Freq_Range[i]<-firstDF$Max_Freq[i]-firstDF$Min_Freq[i]
  firstDF$Mean_Freq[i]<-MeanFreq(cutw(a1,
                                         from=aNotes$s.start[i],
                                         to=aNotes$s.end[i],
                                         output="Wave",
                                         plot=F))
  firstDF$Peak_Freq[i]<-PeakFreq(cutw(a1,
                                         from=aNotes$s.start[i],
                                         to=aNotes$s.end[i],
                                         output="Wave",
                                         plot=F))
  firstDF$Max_Dom_Freq[i]<-MaxDFreq(cutw(a1,
                                    from=aNotes$s.start[i],
                                    to=aNotes$s.end[i],
                                    output="Wave",
                                    plot=F))
  firstDF$Min_Dom_Freq[i]<-MinDFreq(cutw(a1,
                                         from=aNotes$s.start[i],
                                         to=aNotes$s.end[i],
                                         output="Wave",
                                         plot=F))
  firstDF$Mean_Dom_Freq[i]<-MeanDFreq(cutw(a1,
                                         from=aNotes$s.start[i],
                                         to=aNotes$s.end[i],
                                         output="Wave",
                                         plot=F))
  firstDF$Median_Dom_Freq[i]<-MedianDFreq(cutw(a1,
                                         from=aNotes$s.start[i],
                                         to=aNotes$s.end[i],
                                         output="Wave",
                                         plot=F))
  firstDF$Dom_Freq_Range[i]<-firstDF$Max_Dom_Freq[i]-firstDF$Min_Dom_Freq[i]
  firstDF$Abs_DF_Max_Slope[i]<-AbsDFreqMaxSlope(cutw(a1,
                                                  from=aNotes$s.start[i],
                                                  to=aNotes$s.end[i],
                                                  output="Wave",
                                                  plot=F))
  firstDF$Entropy[i]<-H(cutw(a1,
                                from=aNotes$s.start[i],
                                to=aNotes$s.end[i],
                                output="Wave",
                                plot=F),
                           msmooth=c(512,90))
}

#### Eliud: All Measurements ####
# loop that binds every other set of measurements to firstDF. Maybe there is a way to write line 208 that allows this loop to do the whole working directory

for (i in 2:length(list.files())) {
  a<-readWave(list.files()[i])
  a1<-fir(a,
             from=2500,
             to=15000,
             output="Wave")
  aNotes<-timer(a1,
                  dmin=0.05, 
                  envt="hil", 
                  msmooth=c(512, 90), 
                  threshold=7,
                  plot=F)
  b<-data.frame(note_num=seq(1:length(aNotes$s.start)), 
                   file_name=rep(list.files()[i]))
  for (i in 1:length(aNotes$s.start)) {
    b$Duration[i]<-aNotes$s[i]
    b$Max_Freq[i]<-MaxFreq(cutw(a1,
                                from=aNotes$s.start[i],
                                to=aNotes$s.end[i],
                                output="Wave",
                                plot=F))
    b$Min_Freq[i]<-MinFreq(cutw(a1,
                                from=aNotes$s.start[i],
                                to=aNotes$s.end[i],
                                output="Wave",
                                plot=F))
    b$Freq_Range[i]<-b$Max_Freq[i]-b$Min_Freq[i]
    b$Mean_Freq[i]<-MeanFreq(cutw(a1,
                                  from=aNotes$s.start[i],
                                  to=aNotes$s.end[i],
                                  output="Wave",
                                  plot=F))
    b$Peak_Freq[i]<-PeakFreq(cutw(a1,
                                  from=aNotes$s.start[i],
                                  to=aNotes$s.end[i],
                                  output="Wave",
                                  plot=F))
    b$Max_Dom_Freq[i]<-MaxDFreq(cutw(a1,
                                     from=aNotes$s.start[i],
                                     to=aNotes$s.end[i],
                                     output="Wave",
                                     plot=F))
    b$Min_Dom_Freq[i]<-MinDFreq(cutw(a1,
                                     from=aNotes$s.start[i],
                                     to=aNotes$s.end[i],
                                     output="Wave",
                                     plot=F))
    b$Mean_Dom_Freq[i]<-MeanDFreq(cutw(a1,
                                       from=aNotes$s.start[i],
                                       to=aNotes$s.end[i],
                                       output="Wave",
                                       plot=F))
    b$Median_Dom_Freq[i]<-MedianDFreq(cutw(a1,
                                           from=aNotes$s.start[i],
                                           to=aNotes$s.end[i],
                                           output="Wave",
                                           plot=F))
    b$Dom_Freq_Range[i]<-b$Max_Dom_Freq[i]-b$Min_Dom_Freq[i]
    b$Abs_DF_Max_Slope[i]<-AbsDFreqMaxSlope(cutw(a1,
                                              from=aNotes$s.start[i],
                                              to=aNotes$s.end[i],
                                              output="Wave",
                                              plot=F))
    b$Entropy[i]<-H(cutw(a1,
                         from=aNotes$s.start[i],
                         to=aNotes$s.end[i],
                         output="Wave",
                         plot=F),
                    msmooth=c(512,90))
  }
  firstDF<-rbind(firstDF, b)
}

boxplot(firstDF$Abs_Max_PF_Slope~firstDF$note_num,
           vertical=T,
           pch=19,
           col="grey")

## DIDN'T WORK, DISREGARD
# NoteBox<-function(x) {
#   a<-readWave(x)
#   a1<-fir(a,
#           from=2500,
#           to=10000,
#           output="Wave")
#   aNotes<-timer(a1,
#                 dmin=0.05, 
#                 envt="hil", 
#                 msmooth=c(512, 90), 
#                 threshold=5,
#                 plot=F)
#   b<-data.frame(note_num=seq(1:length(aNotes$s.start)), 
#                 file_name=rep(x))
#   for (i in 1:length(aNotes$s.start)) {
#     b$Duration[i]<-aNotes$s[i]
#     b$Max_Freq[i]<-MaxFreq(cutw(a1,
#                                 from=aNotes$s.start[i],
#                                 to=aNotes$s.end[i],
#                                 output="Wave",
#                                 plot=F))
#     b$Min_Freq[i]<-MinFreq(cutw(a1,
#                                 from=aNotes$s.start[i],
#                                 to=aNotes$s.end[i],
#                                 output="Wave",
#                                 plot=F))
#     b$Freq_Range[i]<-b$Max_Freq[i]-b$Min_Freq[i]
#     b$Mean_Freq[i]<-MeanFreq(cutw(a1,
#                                   from=aNotes$s.start[i],
#                                   to=aNotes$s.end[i],
#                                   output="Wave",
#                                   plot=F))
#     b$Peak_Freq[i]<-PeakFreq(cutw(a1,
#                                   from=aNotes$s.start[i],
#                                   to=aNotes$s.end[i],
#                                   output="Wave",
#                                   plot=F))
#     b$Abs_Max_PF_Slope[i]<-AbsPFmaxslope(cutw(a1,
#                                               from=aNotes$s.start[i],
#                                               to=aNotes$s.end[i],
#                                               output="Wave",
#                                               plot=F))
#     b$Entropy[i]<-H(cutw(a1,
#                          from=aNotes$s.start[i],
#                          to=aNotes$s.end[i],
#                          output="Wave",
#                          plot=F),
#                     msmooth=c(512,90))
#     #firstDF<-rbind(firstDF, b)
#   }
#   print(b)
# }
# lapply(list.files()[2:length(list.files())], NoteBox) 




# looking at note-level amplitude profiles...
firstenv<-env(aFirstnote, msmooth=c(512,90), norm=T)
secondenv<-env(aSecondnote, msmooth=c(512,90), norm=T)
thirdenv<-env(aThirdnote, msmooth=c(512,90), norm=T)
fourthenv<-env(aFourthnote, msmooth=c(512,90), norm=T)

questionfour<-c(rep("other", 4), rep("0.01075", 4), rep("3.41e-6", 7), rep("0.172", 13), "0.162")
