#### Extraction of acoustic measurements from wav files of SPPUA HZCH songs ####

library(tuneR)
library(seewave)

setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses")
HZCH_QC <- read.csv("SPPUA_Hz_Quality-and-exceptions.csv")
setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses/AllSongsSPPUA")

# first things first: remove all the unusable recordings
garbage <- HZCH_QC$file_names[HZCH_QC$include=="N"]
recordings <- HZCH_QC$file_names[HZCH_QC$include=="Y"]
HZCH_QC_Y <- HZCH_QC[-c(which(HZCH_QC$include=="N")),]

# getting first file for practice and checks
test <- readWave(recordings[1])
timertest <- timer(test,
                   dmin=0.05, 
                   envt="hil", 
                   msmooth=c(512, 90), 
                   threshold=HZCH_QC$threshold[1])

# new function that returns the max dominant freq of each note in a file
# MaxDFreq2<-function(x,y) {
#   z <- c()
#   for (j in 1:length(y$s.start)) {
#     z[j] <- max(dfreq(cutw(x,
#                            from = y$s.start[j],
#                            to = y$s.end[j],
#                            output = "Wave",
#                            plot = F), 
#                       plot = F)[,2])
#   }
#   return(z)
# }

HZCH <- list(file_name = list.files()[1],
             Max_Freq <- MaxDFreq2(test,timertest))

# Okay, I think it works. Now to re-work the rest of the measurement extraction functions...

MaxDFreq2<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- max(dfreq(cutw(x,
                           from = y$s.start[j],
                           to = y$s.end[j],
                           output = "Wave",
                           plot = F), 
                      ovlp = 95,
                      wl = 1024,
                      plot = F)[,2])
  }
  return(z)
}

MinDFreq2<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- min(dfreq(cutw(x,
                           from = y$s.start[j],
                           to = y$s.end[j],
                           output = "Wave",
                           plot = F), 
                      ovlp = 95,
                      wl = 1024,
                      plot = F)[,2])
  }
  return(z)
}

MeanDFreq2<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- mean(dfreq(cutw(x,
                           from = y$s.start[j],
                           to = y$s.end[j],
                           output = "Wave",
                           plot = F),
                       ovlp = 95,
                       wl = 1024,
                      plot = F)[,2])
  }
  return(z)
}

MedianDFreq2<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- median(dfreq(cutw(x,
                           from = y$s.start[j],
                           to = y$s.end[j],
                           output = "Wave",
                           plot = F),
                         ovlp = 95,
                         wl = 1024,
                      plot = F)[,2])
  }
  return(z)
}

StdevDFreq2<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- sd(dfreq(cutw(x,
                          from = y$s.start[j],
                          to = y$s.end[j],
                          output = "Wave",
                          plot = F),
                     ovlp = 95,
                     wl = 1024,
                     plot = F)[,2])
  }
  return(z)
}

AbsDFreqMaxSlope2<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z[j] <- max(abs(diff(dfreq(cutw(x,
                                    from = y$s.start[j],
                                    to = y$s.end[j],
                                    output = "Wave",
                                    plot = F),
                               ovlp = 95,
                               wl = 1024,
                               threshold = 5,
                               plot = F)[,2])))
  }
  return(z)
}



# first recording only...
song <- readWave(recordings[1])
notes <- timer(song,
               dmin=0.05, 
               envt="hil", 
               msmooth=c(512, 90), 
               threshold=HZCH_QC_Y$threshold[1],
               plot = F)
HZCH <- data.frame(note_num=seq(1:length(notes$s.start)), 
                   file_name=rep(recordings[1]))
HZCH$max_freq <- MaxDFreq2(song,notes)
HZCH$min_freq <- MinDFreq2(song,notes)
HZCH$mean_freq <- MeanDFreq2(song,notes)
HZCH$median_freq <- MedianDFreq2(song,notes)
HZCH$sd_freq <- StdevDFreq2(song,notes)
HZCH$abs_max_slope <- AbsDFreqMaxSlope2(song,notes)
HZCH$duration <- notes$s

# this worked; now just need to put this in a loop that will go iteratively over each recording and bind together...
for (i in 2:length(recordings)) {
  song <- readWave(recordings[i])
  song <- fir(song,
              from = 2500,
              to = 10000,
              bandpass = T,
              output = "Wave")
  if(!is.na(HZCH_QC_Y$trim_s[i])){
    song <- cutw(song,
                 from=HZCH_QC_Y$trim_s[i],
                 to=HZCH_QC_Y$trim_f[i],
                 output="Wave")
  }
  notes <- timer(song,
                 dmin=0.05, 
                 envt="hil", 
                 msmooth=c(512, 90), 
                 threshold=HZCH_QC_Y$threshold[i],
                 plot = F)
  meas <- data.frame(note_num=seq(1:length(notes$s.start)), 
                     file_name=rep(recordings[i]))
  for (j in 1:length(notes$s.start)) {
    meas$max_freq[j] <- MaxDFreq2(song,notes)[j]
    meas$min_freq[j] <- MinDFreq2(song,notes)[j]
    meas$mean_freq[j] <- MeanDFreq2(song,notes)[j]
    meas$median_freq[j] <- MedianDFreq2(song,notes)[j]
    meas$sd_freq[j] <- StdevDFreq2(song,notes)[j]
    meas$abs_max_slope[j] <- AbsDFreqMaxSlope2(song,notes)[j]
    meas$duration[j] <- notes$s[j]
  }
  HZCH <- rbind(HZCH, meas)
}

# the loop now functions correctly. Don't run over the whole set of recordings until you have added all the measurements you think you want to take

## midpoint frequency function that I don't need because it's the same as median
# MidpointFreq2<-function(x,y) {
#   z <- c()
#   for (j in 1:length(y$s.start)) {
#     z[j] <- dfreq(cutw(x,
#                        from = y$s.start[j],
#                        to = y$s.end[j],
#                        output = "Wave",
#                        plot = F),
#                   ovlp=95,
#                   wl = 1024,
#                   plot = F)[ceiling(0.5*length(dfreq(cutw(x,
#                                                           from = y$s.start[j],
#                                                           to = y$s.end[j],
#                                                           output = "Wave",
#                                                           plot = F),
#                                                      ovlp=95,
#                                                      wl = 1024,
#                                                      plot = F)[,2])),2]
#   }
#   return(z)
# }

# messing with the minimum frequency function to exclude measurement of noise


