# BIO8068 Assignment

# Part 1: Visualisation and analysis of wildlife acoustics 

# Setting up libraries
library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(stringr)
library(vegan) 
source("nes8010.R")
# warbleR makes it easy to bulk download from the xeno-cato database
library(warbleR)

# Common buzzard call vs tawny owl song

# Check how many recordings are available - limited to UK and 5-25 sec recordings ####

# Buzzard call
buzzard_call <- query_xc(qword = 'Buteo buteo cnt:"united kingdom" type:call len:5-25', download = FALSE)
# 50 recordings found

# Tawny owl
tawny_song <- query_xc(qword = 'Strix aluco cnt:"united kingdom" type:song len:5-25', download = FALSE)
# 32 recordings

# Common raven call
raven_call <- query_xc(qword = 'Corvus corax cnt:"united kingdom" type:call len:5-25', download = FALSE)
# 35 recordings

# Show the location of the buzzard
map_xc(buzzard_call, leaflet.map = TRUE)

# Show the location of the tawny owl
map_xc(tawny_song, leaflet.map = TRUE)

# Show the location of the raven
map_xc(raven_call, leaflet.map = TRUE)

# Create folders in the project to save the songs and calls to 
dir.create(file.path("buzzard_call"))
dir.create(file.path("tawny_song"))
dir.create(file.path("raven_call"))

# Download the .MP3 files into the relevant folders
query_xc(X = buzzard_call, path="buzzard_call")
query_xc(X = tawny_song, path="tawny_song")
query_xc(X = raven_call, path="raven_call")



# Rename files to make them uniform ####

# buzzard
old_files <- list.files("buzzard_call", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-call_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# tawny owl
old_files <- list.files("tawny_song", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-song_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# raven
old_files <- list.files("raven_call", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-call_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

# Copy all recordings to a new folder 
dir.create(file.path("bird_audio"))
file.copy(from=paste0("buzzard_call/",list.files("buzzard_call")),
          to="bird_audio")
file.copy(from=paste0("tawny_song/",list.files("tawny_song")),
          to="bird_audio")
file.copy(from=paste0("raven_call/",list.files("raven_call")),
          to="bird_audio")

# Convert MP3 to WAV
mp32wav(path="bird_audio", dest.path="bird_audio")
unwanted_mp3 <- dir(path="bird_audio", pattern="*.mp3")
# Remove unwanted MP3 
file.remove(paste0("bird_audio/", unwanted_mp3))



# Visualise and analyse the songs and calls ####

# Looking at a single bird - start with buzzard ####
buzzard_call_wav <- readWave("bird_audio/Buteobuteo-call_264996.wav")
buzzard_call_wav

# Create an oscillogram for the full audio clip
oscillo(buzzard_call_wav)

# Focus on a small area of the clip
oscillo(buzzard_call_wav, from = 3.59, to = 3.60)

# Create a spectrogram for the same clip
SpectrogramSingle(sound.file = "bird_audio/Buteobuteo-call_264996.wav",
                  Colors = "Colors")

# Create a spectrogram with ggplot
buzz_spec <- ggspectro(buzzard_call_wav, flim=c(0,7)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)
buzz_spec



# Now repeat for tawny owl ####
tawny_song_wav <- readWave("bird_audio/Strixaluco-song_473154.wav")
tawny_song_wav

# Create an oscillogram for the full audio clip
oscillo(tawny_song_wav)

# Focus on a small area of the clip
oscillo(tawny_song_wav, from = 1.49, to = 1.50)

# Create a spectrogram for the same clip
SpectrogramSingle(sound.file = "bird_audio/Strixaluco-song_473154.wav",
                  Colors = "Colors")

# Create a spectrogram with ggplot
tawny_spec <- ggspectro(tawny_song_wav, flim=c(0,6)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)
tawny_spec




# Now repeat for raven ####
raven_call_wav <- readWave("bird_audio/Corvuscorax-call_94437.wav")
raven_call_wav

# Create an oscillogram for the full audio clip
oscillo(raven_call_wav)

# Focus on a small area of the clip
oscillo(raven_call_wav, from = 3.49, to = 3.50)

# Create a spectrogram for the same clip
SpectrogramSingle(sound.file = "bird_audio/Corvuscorax-call_94437.wav",
                  Colors = "Colors")

# Create a spectrogram with ggplot
raven_spec <- ggspectro(raven_call_wav, flim=c(0,2)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)
raven_spec




# Feature extraction using MFCC ####

# Changing max frequency to 7000 - 2000 would be too low for these birds
bird_mfcc <- MFCCFunction(input.dir = "bird_audio",
                          max.freq=7000)
dim(bird_mfcc)

# PCA of the data ####

bird_pca <- ordi_pca(bird_mfcc[, -1], scale=TRUE)
summary(bird_pca)
# PC1 explains 20.29% of the variation, PC2 explains 10.54%
# Cumulative 1 and 2 is 30.83%

# Extract scores to display
bird_sco <- ordi_scores(bird_pca, display="sites")
bird_sco <- mutate(bird_sco, group_code = bird_mfcc$Class)

ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_point() 





