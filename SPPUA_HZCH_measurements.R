#### Extraction of acoustic measurements from wav files of SPPUA HZCH songs ####

library(tuneR)
library(seewave)

# setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses")
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
HZCH_QC <- read.csv("SPPUA_Hz_Quality-and-exceptions.csv")
# setwd("C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses/AllSongsSPPUA")
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")

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

View(HZCH)

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

setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
write.csv(HZCH, "HZCH_note-level-measurements_1.csv")

#### 8 May 2023 ####
# let's see what dfreq is doing with the recordings with minimum frequency measurements < 3
toolow <- unique(HZCH$file_name[which(HZCH$min_freq<3)])
toolowQC <- HZCH_QC_Y[which(HZCH_QC_Y$file_names %in% toolow),]
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
write.csv(toolowQC, "HZCH_QC_minfreq.csv")

setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
for (i in 1:length(toolow)) {
  a <- readWave(toolow[i])
  dfreq(a,
        wl = 1024,
        ovlp = 95)
  par(new = T)
  try(timer(a,
        dmin = 0.05, 
        envt = "hil", 
        msmooth = c(512, 90), 
        threshold = toolowQC$threshold[i],
        main = toolow[i]))
}

# messing with the min freq function

setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
test <- readWave(	
  "Poecile.sp_Ym.NG_Mar192022_SparrowfootPUA.HenryCO.MO_SMP_a_12.00.wav")
timertest <- timer(test,
                   dmin=0.05, 
                   envt="hil", 
                   msmooth=c(512, 90), 
                   threshold=12)

MinDFreq2(test,timertest)
# [1] 2.531250 3.515625 4.593750 3.140625

MinDFreq3<-function(x,y) {
  z <- c()
  for (j in 1:length(y$s.start)) {
    z1 <- dfreq(cutw(x,
                     from = y$s.start[j],
                     to = y$s.end[j],
                     output = "Wave",
                     plot = F), 
                 ovlp = 95,
                 wl = 1024,
                 plot = F)[,2]
    z[j] <- min(z1[which(z1>2.86)])
  }
  return(z)
}

MinDFreq3(test,timertest)
# [1] 4.687500 3.515625 4.593750 3.140625

# checking on durations
which(HZCH$duration>0.4)
toolong <- unique(HZCH$file_name[which(HZCH$duration>0.4)])
toolongQC <- HZCH_QC_Y[which(HZCH_QC_Y$file_names %in% toolong),]

setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
for (i in 1:length(toolong)) {
  a <- readWave(toolong[i])
  b <- fir(a, 
         from = 2500, 
         to = 10000, 
         bandpass = TRUE,
         output="Wave")
  c<-normalize(b, unit = c("24"))
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
            threshold = toolongQC$threshold[i],
            main = toolong[i]))
}

#### 9 May 2023 ####
# read in the new quality check spreadsheets and replace threshold values with those in the min frequency one
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
HZCH_QC <- read.csv("SPPUA_Hz_Quality-and-exceptions_2.csv")
toolowQC <- read.csv("HZCH_QC_minfreq.csv")
HZCH_QC$threshold <- replace(HZCH_QC$threshold, 
                             which(HZCH_QC$file_names %in% toolowQC$file_names), 
                             toolowQC$threshold)
HZCH_QC$include <- replace(HZCH_QC$include, 
                             which(HZCH_QC$file_names %in% toolowQC$file_names), 
                             toolowQC$include)
HZCH_QC_Y <- HZCH_QC[-c(which(HZCH_QC$include=="N")),]
write.csv(HZCH_QC, "SPPUA_Hz_Quality-and-exceptions_2.csv")

# looking at the slope values
hist(HZCH$abs_max_slope)
which(HZCH$abs_max_slope>3)
steep <- unique(HZCH$file_name[which(HZCH$abs_max_slope>3)])
steepQC <- HZCH_QC_Y[which(HZCH_QC_Y$file_names %in% steep),]

setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
for (i in 1:length(steep)) {
  a <- readWave(steep[i])
  b <- fir(a, 
           from = 2500, 
           to = 10000, 
           bandpass = TRUE,
           output="Wave")
  c<-normalize(b, unit = c("24"))
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
            threshold = steepQC$threshold[i],
            main = steep[i]))
}


# taking new measurements of the songs with suspicious measurements after changing parameters/removing bad ones
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
HZCH_QC <- read.csv("SPPUA_Hz_Quality-and-exceptions_2.csv")
HZCH_QC_Y <- HZCH_QC[-c(which(HZCH_QC$include=="N")),]
allweird <- sort(unique(c(toolow, toolong, steep)))
allweirdQC <- HZCH_QC[which(HZCH_QC$file_names %in% allweird),] # in alphabetical order
allweirdQC_Y <- allweirdQC[-c(which(allweirdQC$include=="N")),]
allweird <- allweird[which(allweird %in% allweirdQC_Y$file_names)]


# okay, the first recording just to establish the dataframe...
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
song <- readWave(allweird[1])
song <- fir(song,
            from = 2500,
            to = 10000,
            bandpass = T,
            output = "Wave")
notes <- timer(song,
               dmin=0.05, 
               envt="hil", 
               msmooth=c(512, 90), 
               threshold=allweirdQC_Y$threshold[1],
               plot = F)
weird <- data.frame(note_num=seq(1:length(notes$s.start)), 
                   file_name=rep(allweird[1]))
weird$max_freq <- MaxDFreq2(song,notes)
weird$min_freq <- MinDFreq3(song,notes)
weird$mean_freq <- MeanDFreq2(song,notes)
weird$median_freq <- MedianDFreq2(song,notes)
weird$sd_freq <- StdevDFreq2(song,notes)
weird$abs_max_slope <- AbsDFreqMaxSlope2(song,notes)
weird$duration <- notes$s

View(weird)

# and now all the weird ones...
for (i in 2:length(allweird)) {
  song <- readWave(allweird[i])
  song <- fir(song,
              from = 2500,
              to = 10000,
              bandpass = T,
              output = "Wave")
  if(!is.na(allweirdQC_Y$trim_s[i])){
    song <- cutw(song,
                 from=allweirdQC_Y$trim_s[i],
                 to=allweirdQC_Y$trim_f[i],
                 output="Wave")
  }
  notes <- timer(song,
                 dmin=0.05, 
                 envt="hil", 
                 msmooth=c(512, 90), 
                 threshold=allweirdQC_Y$threshold[i],
                 plot = F)
  meas <- data.frame(note_num=seq(1:length(notes$s.start)), 
                     file_name=rep(allweird[i]))
  for (j in 1:length(notes$s.start)) {
    meas$max_freq[j] <- MaxDFreq2(song,notes)[j]
    meas$min_freq[j] <- MinDFreq3(song,notes)[j]
    meas$mean_freq[j] <- MeanDFreq2(song,notes)[j]
    meas$median_freq[j] <- MedianDFreq2(song,notes)[j]
    meas$sd_freq[j] <- StdevDFreq2(song,notes)[j]
    meas$abs_max_slope[j] <- AbsDFreqMaxSlope2(song,notes)[j]
    meas$duration[j] <- notes$s[j]
  }
  weird <- rbind(weird, meas)
}

which(HZCH$file_name %in% weird$file_name)

# can't use the replace function because the dimensions didn't end up being the same due to different splitting of notes. I will have to remove all of the rows in HZCH with the file names present in weird and then rbind weird and HZCH. It's ok to do this because I have written a csv of HZCH with the first round of measurements.
# HZCH <- replace(HZCH[which(HZCH$file_name %in% weird$file_name),], 
#                 which(HZCH$file_name %in% weird$file_name), 
#                 weird)

HZCH <- HZCH[-which(HZCH$file_name %in% weird$file_name),]
HZCH <- rbind(HZCH, weird)
HZCH <- HZCH[-which(HZCH$file_name %in% allweirdQC$file_names[which(allweirdQC$include == "N")]),]


hist(HZCH$min_freq)
hist(HZCH$duration)
# check recordings with a minimum frequency above 7 kHz