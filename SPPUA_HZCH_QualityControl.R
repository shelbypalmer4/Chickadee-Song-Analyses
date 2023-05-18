#### Quality check prior to extracting measurements from wav files of HZCH song ####

# setwd("C:/Users/Shelby Palmer/Desktop/CHICKADEES/DATA/AllSongsSPPUA")
setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/AllSongsSPPUA")
library(tuneR)
library(seewave)

length(list.files())
# okay, I have 1347 chopped recordings. The first thing I need to do is quality control & exception handling, which will have to be done visually by looking at spectrograms.

AllSPPUA <- data.frame(file_names = list.files(),
                       include = rep(NA),
                       threshold = rep(NA))
write.csv(AllSPPUA, "C:/Users/Shelby Palmer/Desktop/The House Always Wins/Chickadee-Song-Analyses/SPPUA_Hz_Quality-and-exceptions.csv")

# setwd("/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses")
AllSPPUA <- read.csv("SPPUA_Hz_Quality-and-exceptions.csv")

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

# simply makes nice spectrograms for checks
specR<-function(x) {
  a<-readWave(x)
  b<-fir(a, 
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
          cexaxis = 0.7,
          main = x)
}


#### making a exemplary quality-checking figure for thesis ####
# hacking timer to remove overlapping labels
timerfig <- function (wave, f, channel = 1, threshold = 5, dmin = NULL, envt = "abs", 
                      power = 1, msmooth = NULL, ksmooth = NULL, ssmooth = NULL, 
                      asmooth = NULL, tlim = NULL, plot = TRUE, plotthreshold = TRUE, 
                      col = "blue", colval = "red", xlab = "", ylab = "",
                      ...) 
{
  input <- inputw(wave = wave, f = f, channel = channel)
  wave <- input$w
  f <- input$f
  rm(input)
  n <- length(wave)
  if (power == 0) 
    stop("'power' cannot equal to 0")
  if (!is.null(dmin)) {
    if (length(dmin) != 1) 
      stop("'dmin' should be a numeric vector of length 1")
    if (dmin <= 0) 
      stop("'dmin' cannot be negative or equal to 0")
    if (dmin >= n/f) 
      stop("'dmin' cannot equal or be higher than the wave duration")
  }
  if (!is.null(tlim)) {
    wave <- cutw(wave, f = f, from = tlim[1], to = tlim[2])
    n <- length(wave)
  }
  wave1 <- env(wave = wave, f = f, msmooth = msmooth, ksmooth = ksmooth, 
               ssmooth = ssmooth, asmooth = asmooth, envt = envt, norm = TRUE, 
               plot = FALSE)
  n1 <- length(wave1)
  f1 <- f * (n1/n)
  if (is.numeric(threshold)) {
    thres <- threshold/100
    thres.lab <- paste(as.character(threshold), "%")
  }
  else if (is.function(threshold)) {
    thres <- apply(wave1, MARGIN = 2, FUN = threshold)
    thres.lab <- paste(as.character(round(100 * thres, digits = 1)), 
                       "%")
  }
  if (power != 1) 
    wave1 <- wave1^power
  wave2 <- ifelse(wave1 <= thres, yes = 1, no = 2)
  n2 <- length(wave2)
  wave4 <- apply(as.matrix(1:(n2 - 1)), 1, function(x) wave2[x] + 
                   wave2[x + 1])
  n4 <- length(wave4)
  wave4[c(1, n4)] <- 3
  wave5 <- which(wave4 == 3)
  if (!is.null(dmin)) {
    event.dur <- diff(wave5)
    event.idx <- which(event.dur < dmin * f1)
    if (length(event.idx) != 0) {
      for (i in event.idx) {
        wave4[(wave5[i]):(wave5[i] + event.dur[i])] <- 2
      }
      wave4[which(abs(diff(wave4)) == 2)] <- 3
      wave4[c(1, n4)] <- 3
    }
    wave5 <- which(wave4 == 3)
    if (length(wave5) == 2) {
      stop("'dmin' was set to a too high value, there are no signal longer than 'dmin'")
    }
  }
  wave5[-1] <- wave5[-1] + 1
  f4 <- f * (n4/n)
  wave4 <- ts(wave4, start = 0, end = n4/f4, frequency = f4)
  positions <- time(wave4)[wave5]
  durations <- diff(positions)
  npos <- length(positions)
  if (npos <= 2) 
    stop("It seems that the sound is continuous, there are no signal/pause events.")
  first.non.transition <- which(wave4 != 3)[1]
  first.non.transition.even <- first.non.transition%%2 == 0
  if ((wave4[first.non.transition] == 2) == first.non.transition.even) {
    first <- "pause"
    pause <- durations[seq(1, npos - 1, by = 2)]
    signal <- durations[seq(2, npos - 1, by = 2)]
    start.signal <- positions[seq(2, npos - 1, by = 2)]
    end.signal <- positions[seq(3, npos, by = 2)]
  }
  else {
    first <- "signal"
    pause <- durations[seq(2, npos - 1, by = 2)]
    signal <- durations[seq(1, npos - 1, by = 2)]
    start.signal <- positions[seq(1, npos - 1, by = 2)]
    end.signal <- positions[seq(2, npos, by = 2)]
  }
  ratio <- sum(signal)/sum(pause)
  timer <- list(s = signal, p = pause, r = ratio, s.start = start.signal, 
                s.end = end.signal, first = first)
  if (plot) {
    plot(x = seq(0, n1/f1, length.out = n1), y = wave1, xlab = xlab, 
         ylab = ylab, yaxt = "n", ylim = c(0, 1 + 0.1), col = col, 
         type = "l", xaxs = "i", ...)
    if (plotthreshold) {
      abline(h = thres, col = col, lty = 2)
      mtext(thres.lab, side = 2, line = 0.5, at = thres, 
            las = 1, col = col, cex = 0.8)
    }
    outline <- wave4 - 3
    outline[outline < 0] <- 0
    outline[1] <- NA
    lines(x = seq(0, n1/f1, length.out = n1), y = c(outline), 
          col = colval)
    wave8 <- numeric(npos - 1)
    for (i in 2:npos) {
      wave8[i] <- ((wave5[i] - wave5[i - 1])/2) + wave5[i - 
                                                          1]
    }
    if ((wave4[first.non.transition] == 2) == first.non.transition.even) {
      wave8.1 <- wave8[seq(2, npos, by = 2)]/f1
      wave8.2 <- wave8[seq(3, npos, by = 2)]/f1
    }
    else {
      wave8.2 <- wave8[seq(2, npos, by = 2)]/f1
      wave8.1 <- wave8[seq(3, npos, by = 2)]/f1
    }
    ypl <- as.character(round(pause, 2))
    ysl <- as.character(round(signal, 2))
    text(x = wave8.1, y = 0.075, ypl, col = colval, cex = 0.8)
    text(x = wave8.2, y = 1.075, ysl, col = colval, cex = 0.8)
    invisible(timer)
  }
  else {
    return(timer)
  }
}


# make a timer spectro QC figure for thesis
cutspecfig<-function(x) {
  a<-readWave(x)
  b<-fir(a, 
         from = 2500, 
         to = 10000, 
         bandpass = TRUE,
         output="Wave")
  c<-normalize(b, unit = c("24"))
  png(filename = "/Users/shelbypalmer/Documents/GitHub/Chickadee-Song-Analyses/QualityCheckEx.png",
      width = 1200,
      height = 600,
      res = 200)
  spectro(c, 
          wl = 512, 
          ovlp = 95, 
          collevels = seq(-42,0,6),
          flim = c(0, 10),
          osc = F, 
          scale = F, 
          colgrid = "gray", 
          cexlab = 0.8,
          cexaxis = 0.7,
          axisX = F,
          palette = reverse.gray.colors.2)
  par(new=T)
  try(timerfig(c, 
            dmin = 0.05,
            envt = "hil",
            msmooth=c(512, 90),
            threshold = 10,
            main = x,
            cex.main = 0.75))
  title(ylab = "Amplitude", line = 2, col.lab = "blue")
  title(xlab = "Time (s)", line = 2, col.lab = "red")
  dev.off()
}

cutspecfig(list.files()[680])
# remember to move file out of folder with all recordings