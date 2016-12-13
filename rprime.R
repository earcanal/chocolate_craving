library("rprime")
library("plyr")

setwd("/home/paul/src/R/phd/craving_chocolate")

# arg, have to understand ePrime data internals :(
process_eprime_file <- function(path) {
  lines  <- read_eprime(path)
  frames <- FrameList(lines)
  
  frame1 <- frames[[1]]
  
  # trials occur at level 3
  frames <- keep_levels(frames, 3)
  df     <- to_data_frame(frames)
  

  to_pick <- c("Eprime.Basename", "Running", "Item", "response", "Choice", "Cycle")
  if (frame1$Group == "Distraction" ) {
    # get Universes and elephants!
    to_pick <- c(to_pick,"MeditationCheck.RESP")
  }
  df            <- df[to_pick]
  df$subject    <- frame1$Subject
  df$researcher <- frame1$ResearcherID
  df$group      <- frame1$Group
  df$age        <- frame1$Age
  df$sex        <- frame1$Sex
  df$handedness <- frame1$Handedness
  
  # running_map <- c(TrialLists = "Trial", PracticeBlock = "Practice")
  # df$Running <- running_map[df$Running]
  
  # Renumber trials in the practice and experimental blocks separately.
  # Numerically code correct response.
  # df <- ddply(df, .(Running), mutate, 
  #                TrialNumber = seq(from = 1, to = length(Running)),
  #                CorrectResponse = ifelse(Correct == Response, 1, 0))
  df$Sample <- NULL
  
  # Save to CSV
  # csv <- paste0(file_path_sans_ext(sails_path), ".csv")
  # write.csv(df, csv, row.names = FALSE)
  df
}

# find . -name "*.txt" -exec iconv -f utf-16 -t utf-8 {} -o /utf8/{} \;
paths <- list.files("data", pattern = ".txt", full.names = TRUE)
paths
paths <- c("data/101-1.txt", "data/10-1.txt", "data/102-1.txt", "data/103-1.txt")
#paths
ensemble <- ldply(paths, process_eprime_file)
# overall <- ddply(ensemble, .(Eprime.Basename, Running), summarise, 
#                  Score = sum(CorrectResponse),
#                  PropCorrect = Score / length(CorrectResponse))
# overall
# 
# modules <- ddply(ensemble, .(Eprime.Basename, Running, Module), summarise, 
#                  Score = sum(CorrectResponse),
#                  PropCorrect = mean(CorrectResponse))
# modules