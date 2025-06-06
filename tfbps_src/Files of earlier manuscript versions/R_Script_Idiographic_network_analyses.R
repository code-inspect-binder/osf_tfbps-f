library(tidyverse)
library(car)
library(data.table)
library(psych)
library(plyr)
library(graphicalVAR)
library(qgraph)
library(ppcor)
library(animation)
library(ggplot2)
library(ggplotify)
library(grid)

# Import prepared datafile

setwd("//cnas.ru.nl/wrkgrp/FSW-BSI-DP-PhD_Daan_Hulsmans/Study 1 - Feasibility study/Data")

dataset_imp <- read_csv("df_networks_imputed.csv", col_names=T)
dataset_imp <- dataset_imp[,-1]

# creating a dataframe to visually inspect variances per item per participant
v <- data.frame(matrix(ncol = 9, nrow = length(unique(dataset_imp$Participant))))
colnames(v) <- c("Participant","Happy", "Worrying","Scared","Nervous","Act_later_regret","Act_without_thinking","Act_for_kicks","Restless")
v[,c("Participant", "Happy")] <- aggregate(Happy ~ Participant, dataset_imp, var)
v[,c("Participant", "Worrying")] <- aggregate(Worrying ~ Participant, dataset_imp, var)
v[,c("Participant", "Scared")] <- aggregate(Scared ~ Participant, dataset_imp, var)
v[,c("Participant", "Nervous")] <- aggregate(Nervous ~ Participant, dataset_imp, var)
v[,c("Participant", "Act_later_regret")] <- aggregate(Act_later_regret ~ Participant, dataset_imp, var)
v[,c("Participant", "Act_without_thinking")] <- aggregate(Act_without_thinking ~ Participant, dataset_imp, var)
v[,c("Participant", "Act_for_kicks")] <- aggregate(Act_for_kicks ~ Participant, dataset_imp, var)
v[,c("Participant", "Restless")] <- aggregate(Restless ~ Participant, dataset_imp, var)
v <- as.data.frame(v)
v

# Based on visual inspection of this datatframe, we remove four participants and two variables without variance for the rest of the 26 participants
dataset_imp <- subset(dataset_imp, dataset_imp$Participant != "5" & 
                        dataset_imp$Participant != "7" & 
                        dataset_imp$Participant != "34" & 
                        dataset_imp$Participant != "43")
dataset_imp <- dataset_imp[ , !(names(dataset_imp) %in% c("Scared", "Act_for_kicks"))]

## Descriptive statistics

describe(dataset_imp[,c("age", "tiq")])
count(dataset_imp$gender)$freq / 60
count(dataset_imp$P_Profile)$freq / 60

######################### Generate Autocorrelation functions per participant ############################

# create empry matrix
acfmatrix <- data.frame(matrix(ncol = 2, nrow = 60))
colnames(acfmatrix) <- c("count_sig_total", "sig_percentage")
rownames(acfmatrix) <- as.numeric(rownames(acfmatrix)) - 1
acfmatrix[is.na(acfmatrix)] <- 0

# fill empty matrix with the counts significant (p < .05) per lag (rows) and per individual (column)
for (j in unique(dataset_imp$Participant)){
  tempacf <- acf(dataset_imp[dataset_imp$Participant == j , -c(1,8:13)], lag.max = 60)
  acfmatrix[,paste0("participant", j)] <- 0
  for(i in as.numeric(rownames(acfmatrix))){
    tempmatrix <- tempacf$acf[i,,]
    tempvector <- tempmatrix[upper.tri(tempmatrix, diag = T)]
    acfmatrix[i,paste0("participant", j)] <- length(which(tempvector > 0.25 | tempvector < -0.25))
  }
}

# add summary statistics of counts and percentages across participants
acfmatrix[1,3:28] <- acfmatrix[1,3:28] - 6 # at lag-0 all autocorrelations with same variable are exactly 1. We therefore subtract 6 (for each of the variables) from the denuminator when calculating the percentage of significant associations relative all possible meaningful associations  
acfmatrix$count_sig_total <- rowSums(acfmatrix[3:28]) 
acfmatrix$sig_percentage <- acfmatrix[,"count_sig_total"] / (21*26) # denuminator is 21 (one diagonal of symmetrical matrix, 15, plus the diagnonal 6), for all lags of 1 and above across 26 participants
acfmatrix[1,2] <- acfmatrix[1,"count_sig_total"] / (15*26) # denuminator is 15 for lag-0 (one diagonal of symmetrical matrix, 15, without the diagnonal 6)
acfmatrix

# write in txt file so it can easily be imported in a Word table
setwd("//cnas.ru.nl/wrkgrp/FSW-BSI-DP-PhD_Daan_Hulsmans/Other/Netwerkmodellen")
write.table(acfmatrix, file = "ACF_matrix.txt")

## Networks for two example participants in manuscript

df.1 <- dataset_imp[dataset_imp$Participant == 2,-c(1,8:13)]
outcome.1 <- graphicalVAR(df.1, lags = 1)
qgraph(outcome.1$PCC, # PCC = partical contemporaneous correlations
       layout = "circle", 
       title = paste("Same-day symptom network of participant 1"), 
       edge.labels = T,
       edge.label.color = "black",
       edge.label.bg = F,
       edge.label.cex = 1.2,
       label.font = 2, 
       theme = "colorblind", 
       labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))


df.2 <- dataset_imp[dataset_imp$Participant == 4,-c(1,8:13)]
outcome.2 <- graphicalVAR(df.2, gamma = 0.5, lags = 1)
qgraph(outcome.2$PCC, 
       layout = "circle", 
       title = paste("Same-day symptom network of participant 2"), 
       edge.labels = T,
       edge.label.color = "black",
       edge.label.bg = F,
       edge.label.cex = 1.2,
       label.font = 2, 
       theme = "colorblind", 
       labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))


################### Create idiographic networks ######################################

## Generate dataframes for idiographic networks in subgroups

# create empty dataframes
m <- data.frame(matrix(ncol = 6, nrow = 6))
x <- c("Happy", "Worrying", "Nervous", "Act_later_regret", "Act_without_thinking", "Restless")
colnames(m) <- x
rownames(m) <- x
m[is.na(m)] <- 0
AS_df_pos <- m
IM_df_pos <- m
SS_df_pos <- m
NT_df_pos <- m
AS_df_neg <- m
IM_df_neg <- m
SS_df_neg <- m
NT_df_neg <- m
Female_df_pos <- m
Female_df_neg <- m
Male_df_pos <- m
Male_df_neg <- m
df_pos <- m
df_neg <- m

## Loop creating networks per participant plus data frame with counts for whole sample and per subgroup: gender and personality profile. Each idiographic network is saved as a .jpeg

for(i in unique(dataset_imp$Participant)){
  df.t <- dataset_imp[dataset_imp$Participant == i,-1]
  subgr_p <- as.character(df.t[1,"P_Profile"])
  subgr_g <- as.character(df.t[1,"gender"])
  df.t <- df.t[,-c(7:12)]
  outcome <- graphicalVAR(df.t, gamma = 0.5, lags = 1)
  jpeg(paste("Network ", i, ".jpeg", sep = ""), units = 'in', width = 15, height = 15, res = 600)
  qgraph(outcome$PCC, layout = "circle", title = paste("Same-day symptom network of participant ", i, sep = ""), label.font = 2, theme = "colorblind", labels = c("HAPPY","WORRY","NERV.","ACT_R","ACT_W","RESTL"))
  dev.off()
  temp.group.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
  df_pos <- temp.group.pos + df_pos
  temp.group.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
  df_neg <- temp.group.neg + df_neg  
  if(subgr_p == "AS"){
    temp.subgroup.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
    AS_df_pos <- temp.subgroup.pos + AS_df_pos
    temp.subgroup.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
    AS_df_neg <- temp.subgroup.neg + AS_df_neg
  }
  if(subgr_p == "NT"){
    temp.subgroup.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
    NT_df_pos <- temp.subgroup.pos + NT_df_pos
    temp.subgroup.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
    NT_df_neg <- temp.subgroup.neg + NT_df_neg
  }
  if(subgr_p == "SS"){
    temp.subgroup.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
    SS_df_pos <- temp.subgroup.pos + SS_df_pos
    temp.subgroup.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
    SS_df_neg <- temp.subgroup.neg + SS_df_neg
  }
  if(subgr_p == "IM"){
    temp.subgroup.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
    IM_df_pos <- temp.subgroup.pos + IM_df_pos
    temp.subgroup.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
    IM_df_neg <- temp.subgroup.neg + IM_df_neg
  }
  if(subgr_g == "2"){
    temp.subgroup.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
    Female_df_pos <- temp.subgroup.pos + Female_df_pos
    temp.subgroup.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
    Female_df_neg <- temp.subgroup.neg + Female_df_neg
  }
  if(subgr_g == "1"){
    temp.subgroup.pos <- as.data.frame(ifelse(outcome$PCC > 0, 1, 0))
    Male_df_pos <- temp.subgroup.pos + Male_df_pos
    temp.subgroup.neg <- as.data.frame(ifelse(outcome$PCC < 0, 1, 0))
    Male_df_neg <- temp.subgroup.neg + Male_df_neg
  }
}


######################## Create Group-level network visualizations ##############################

## Create color matrix for group-level networks

colmat <- data.frame(matrix(ncol = 6, nrow = 6))
x <- c("Happy", "Worrying", "Nervous", "Act_later_regret", "Act_without_thinking", "Restless")
colnames(colmat) <- x
rownames(colmat) <- x
colmat[is.na(colmat)] <- 0
colmat[upper.tri(colmat)] <- "red"
colmat[lower.tri(colmat, diag = T)] <- "blue"

## Visualize all group- and sub-grouplevel networks from counts matrices using the color matrix and save a .jpeg to directory

countsmatrix <- as.matrix(df_pos)
countsmatrix[upper.tri(countsmatrix)] <- df_neg[upper.tri(df_neg)]
jpeg("Summary_plot_labels.jpeg", units = 'in', width = 5, height = 5, res = 600)
summaryplot <- qgraph(countsmatrix, 
                      layout = "circle",
                      esize = 12,
                      arrows = F, 
                      edge.labels = T,
                      edge.label.color = "black",
                      edge.label.bg = F,
                      edge.label.position = 0.6,
                      edge.label.cex = 1.2,
                      curveAll = F, 
                      parallelEdge =T, 
                      edge.color = as.matrix(colmat),
                      title = "Summary Network total sample (n = 26)",
                      labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
summaryplot
dev.off()

AS_countsmatrix <- AS_df_pos
AS_countsmatrix[upper.tri(AS_countsmatrix)] <- AS_df_neg[upper.tri(AS_df_neg)]
jpeg("Network Profile AS.jpeg", units = 'in', width = 5, height = 5, res = 600)
AS_summaryplot <- qgraph(AS_countsmatrix, 
                         layout = "circle",
                         arrows = F, 
                         edge.labels = T,
                         edge.label.color = "black",
                         edge.label.bg = F,
                         edge.label.position = 0.6,
                         edge.label.cex = 1.2,
                         curveAll = F, 
                         parallelEdge =T, 
                         edge.color = as.matrix(colmat),
                         title = "Summary Network Anxiety Sensitivity (n = 8)",
                         labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
AS_summaryplot
dev.off()
SS_countsmatrix <- SS_df_pos
SS_countsmatrix[upper.tri(SS_countsmatrix)] <- SS_df_neg[upper.tri(SS_df_neg)]
jpeg("Network Profile SS.jpeg", units = 'in', width = 5, height = 5, res = 600)
SS_summaryplot <- qgraph(SS_countsmatrix, 
                         layout = "circle",
                         arrows = F, 
                         edge.labels = T,
                         edge.label.color = "black",
                         edge.label.bg = F,
                         edge.label.position = 0.6,
                         edge.label.cex = 1.2,
                         curveAll = F, 
                         parallelEdge =T, 
                         edge.color = as.matrix(colmat),
                         title = "Summary Network Sensation Seeking (n = 3)",
                         labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
SS_summaryplot
dev.off()
NT_countsmatrix <- NT_df_pos
NT_countsmatrix[upper.tri(NT_countsmatrix)] <- NT_df_neg[upper.tri(NT_df_neg)]
jpeg("Network Profile NT.jpeg", units = 'in', width = 5, height = 5, res = 600)
NT_summaryplot <- qgraph(NT_countsmatrix, 
                         layout = "circle",
                         arrows = F, 
                         edge.labels = T,
                         edge.label.color = "black",
                         edge.label.bg = F,
                         edge.label.position = 0.6,
                         edge.label.cex = 1.2,
                         curveAll = F, 
                         parallelEdge =T, 
                         edge.color = as.matrix(colmat),
                         title = "Summary Network Negative Thinking (n = 9)",
                         labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
NT_summaryplot
dev.off()
IM_countsmatrix <- IM_df_pos
IM_countsmatrix[upper.tri(IM_countsmatrix)] <- IM_df_neg[upper.tri(IM_df_neg)]
jpeg("Network Profile IM.jpeg", units = 'in', width = 5, height = 5, res = 600)
IM_summaryplot <- qgraph(IM_countsmatrix, 
                         layout = "circle",
                         arrows = F, 
                         edge.labels = T,
                         edge.label.color = "black",
                         edge.label.bg = F,
                         edge.label.position = 0.6,
                         edge.label.cex = 1.2,
                         curveAll = F, 
                         parallelEdge =T, 
                         edge.color = as.matrix(colmat),
                         title = "Summary Network Impulsivity (n = 6)",
                         labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
IM_summaryplot
dev.off()
F_countsmatrix <- Female_df_pos
F_countsmatrix[upper.tri(F_countsmatrix)] <- Female_df_neg[upper.tri(Female_df_neg)]
jpeg("Network Females.jpeg", units = 'in', width = 5, height = 5, res = 600)
F_summaryplot <- qgraph(F_countsmatrix, 
                        layout = "circle",
                        arrows = F, 
                        edge.labels = T,
                        edge.label.color = "black",
                        edge.label.bg = F,
                        edge.label.position = 0.6,
                        edge.label.cex = 1.2,
                        curveAll = F, 
                        parallelEdge =T, 
                        edge.color = as.matrix(colmat),
                        title = "Summary Network females (n = 15)",
                        labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
F_summaryplot
dev.off()
M_countsmatrix <- Male_df_pos
M_countsmatrix[upper.tri(M_countsmatrix)] <- Male_df_neg[upper.tri(Male_df_neg)]
jpeg("Network Males.jpeg", units = 'in', width = 5, height = 5, res = 600)
M_summaryplot <- qgraph(M_countsmatrix, 
                        layout = "circle",
                        arrows = F, 
                        edge.labels = T,
                        edge.label.color = "black",
                        edge.label.bg = F,
                        edge.label.position = 0.6,
                        edge.label.cex = 1.2,
                        curveAll = F, 
                        parallelEdge =T, 
                        edge.color = as.matrix(colmat),
                        title = "Summary Network males (n = 11)",
                        labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
plot(M_summaryplot)
dev.off()

################# Create Figure 5 ##########################

## examples of first two participants in dataset, pp2 and pp4
### pp2 (nr 1 in manuscript)

var_names <- c(
  `Happy` = "HAPPY",
  `Worrying` = "WORRY",
  `Nervous` = "NERVO",
  `Act_later_regret` = "ACT_R",
  `Act_without_thinking` = "ACT_W",
  `Restless` = "RESTL"
)

example1 <- dataset_imp[Participant == 2,c("Time","Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")]

example1_melt <- melt(example1[,c("Time","Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")],id.vars=1)
plot_ts1 <- ggplot(example1_melt, aes(Time, value, col=variable, group=1)) + 
  geom_line()+
  facet_grid(variable~., labeller = as_labeller(var_names)) + 
  scale_x_continuous(breaks=seq(0,60, 10)) +
  scale_color_manual(values = c(rainbow(6))) +
  theme_bw() + 
  ylab('Score') +
  xlab('Day number') +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        text = element_text(size = 22)) +
  theme(legend.position = "none")  
plot_ts1

jpeg("Examples_participant1_TS.jpeg", units = 'in', width = 8, height = 8, res = 200)
plot_ts1
dev.off()

matrix1_start <- pcor(example1[example1$Time %in% 1:30,c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
jpeg("Examples_participant1_start.jpeg", units = 'in', width = 8, height = 8, res = 200)
matrix1_start_plot <- qgraph(matrix1_start, 
                        layout = "circle",
                        arrows = F, 
                        label.cex = 1.8,
                        minimum = 0.2, 
                        theme = 'colorblind', 
                        border.color = rainbow(6),
                        curveAll = F, 
                        parallelEdge =T, 
                        title = "Day 1-30",
                        labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
dev.off()

matrix1_middle <- pcor(example1[example1$Time %in% 15:45,c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
jpeg("Examples_participant1_middle.jpeg", units = 'in', width = 8, height = 8, res = 200)
matrix1_middle_plot <- qgraph(matrix1_middle, 
                             layout = "circle",
                             arrows = F, 
                             minimum = 0.2, 
                             label.cex = 1.8, 
                             theme = 'colorblind',
                             curveAll = F, 
                             border.color = rainbow(6), 
                             parallelEdge =T, 
                             title = "Day 15-45",
                             labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
dev.off()

matrix1_end <- pcor(example1[example1$Time %in% 31:60,c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
jpeg("Examples_participant1_end.jpeg", units = 'in', width = 8, height = 8, res = 200)
matrix1_end_plot <- qgraph(matrix1_end, 
                             layout = "circle",
                             arrows = F, 
                             label.cex = 1.8, 
                             minimum = 0.2,  
                             border.color = rainbow(6),
                             theme = 'colorblind',
                             curveAll = F, 
                             parallelEdge =T, 
                             title = "Day 31-60",
                             labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
dev.off()

### pp4 (nr 2 in manuscript)
example2 <- dataset_imp[Participant == 4,c("Time","Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")]

example2_melt <- melt(example2[,c("Time","Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")],id.vars=1)
plot_ts2 <- ggplot(example2_melt, aes(Time, value, col=variable, group=1)) + 
  geom_line()+
  scale_x_continuous(breaks=seq(0,60, 10)) +
  scale_y_continuous(breaks=seq(0,4, 1)) +
  scale_color_manual(values = c(rainbow(6))) +
  facet_grid(variable~., labeller = as_labeller(var_names)) + 
  theme_bw() + 
  ylab('Score') +
  xlab('Day number') +
  theme(axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20),
        text = element_text(size = 22)) +
  theme(legend.position = "none")   

jpeg("Examples_participant2_TS.jpeg", units = 'in', width = 8, height = 8, res = 200)
plot_ts2
dev.off()

matrix2_start <- pcor(example2[example2$Time %in% 1:30,c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
jpeg("Examples_participant2_start.jpeg", units = 'in', width = 8, height = 8, res = 200)
matrix2_start_plot <- qgraph(matrix2_start, 
                             layout = "circle",
                             arrows = F,  
                             border.color = rainbow(6),
                             minimum = 0.2, 
                             label.cex = 1.8,
                             theme = 'colorblind',
                             curveAll = F, 
                             parallelEdge =T, 
                             title = "Day 1-30",
                             labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
dev.off()

matrix2_middle <- pcor(example2[example2$Time %in% 15:45,c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
jpeg("Examples_participant2_middle.jpeg", units = 'in', width = 8, height = 8, res = 200)
matrix2_middle_plot <- qgraph(matrix2_middle, 
                              layout = "circle",
                              arrows = F, 
                              border.color = rainbow(6),
                              minimum = 0.2, 
                              label.cex = 1.8,
                              theme = 'colorblind',
                              curveAll = F, 
                              parallelEdge =T, 
                              title = "Day 16-46",
                              labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
dev.off()

matrix2_end <- pcor(example2[example2$Time %in% 31:60,c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
jpeg("Examples_participant2_end.jpeg", units = 'in', width = 8, height = 8, res = 200)
matrix2_end_plot <- qgraph(matrix2_end, 
                           layout = "circle",
                           arrows = F, 
                           label.cex = 1.8,
                           border.color = rainbow(6),
                           minimum = 0.2, 
                           theme = 'colorblind',
                           curveAll = F, 
                           parallelEdge =T, 
                           title = "Day 31-60",
                           labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"))
dev.off()


################### Dynamic network videos ################################

#thanks to Sacha Epskamp for sharing code from Wichers et al. (2016), which was a nice starting point for the following script

Window <- 30

dataset_video <- dataset_imp[dataset_imp$Participant %in% c(2,4,9,18,22,24,25,26,28,29,30,31,35,42,44),] #Some participants were excluded for too much zero-variance within 30-day windows, which is why networks cannot be estimated

# Loop through all participants
for (pp_id in unique(dataset_video$Participant)){

  temp_data <- dataset_video[dataset_video$Participant == pp_id,-c(1,9:13)]
  
  # Create list with partial correlation matrices per window
  pcorlist <- list()
  for (d in seq_len(30))
  {
    pcorlist[[d]] <-  pcor(temp_data[temp_data$Time %in% d:(d+Window),c("Happy", "Worrying","Nervous","Act_later_regret","Act_without_thinking","Restless")])$estimate
  }
  
  pcorlist <- rapply(pcorlist,function(x) ifelse(x==0.00000000,0.00000001,x), how = "replace") # will be pruned 
  
  # Create list with network visualizations from partial correlation matrices per window
  GraphList <- qgraph.animate(pcorlist, graph = "pcor",  color = rainbow(6), vTrans = 150,  minimum = 0.2, layout="spring", labels = c("HAPPY","WORRY","NERVO","ACT_R","ACT_W","RESTL"),
                            theme = "colorblind", plotGraphs = F, smooth = TRUE, mar = c(5,5,5,5),edge.label = F,
                            title = paste("Dynamic network video of participant ", pp_id, sep = ""), title.cex = 3)

  ani.options(ffmpeg = "C:\\Users\\daanh\\OneDrive\\Documenten\\Pluryn\\ffmpeg-4.2.2-win64-static\\bin\\ffmpeg.exe")
  
  # Create video
  saveVideo({
    layout(matrix(c(
    0,2,2,2,2,1,1,1,1,1,1,
    0,3,3,3,3,1,1,1,1,1,1,
    0,4,4,4,4,1,1,1,1,1,1,
    0,5,5,5,5,1,1,1,1,1,1,
    0,6,6,6,6,1,1,1,1,1,1,
    0,7,7,7,7,1,1,1,1,1,1),6,11,byrow=TRUE))
  
    for (d in seq_along(GraphList))
    {
      if (d < length(GraphList))
      {
        smoothlist <- qgraph:::smoothAnimationList(GraphList[d:(d+1)], 5)
      } else smoothlist <- GraphList[d]
    
      for (p in seq_along(smoothlist))  {
          plot(smoothlist[[p]]) # This is the network, what follows are raw time-series plus window demarcations as vertical lines
          plot(temp_data$Time[temp_data$Time <= d+Window],  temp_data$Happy[temp_data$Time <= d+Window], type = 'b', bty = 'n', xlab = "", col = 'red', ylab = "Happy", xaxt = 'n', yaxt = 'n', xlim=c(1,nrow(temp_data)), ylim = range(0:4), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 2)
          axis(1, at=seq(1,60,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          axis(2, at=seq(0,4,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          abline(v = d)
          abline(v = d+Window)
          plot(temp_data$Time[temp_data$Time <= d+Window],  temp_data$Worrying[temp_data$Time <= d+Window], type = 'b', bty = 'n', xlab = "",col = 'yellow',  ylab = "Worry", xaxt = 'n', yaxt = 'n', xlim=c(1,nrow(temp_data)), ylim = range(0:4), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 2)
          axis(1, at=seq(1,60,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          axis(2, at=seq(0,4,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          abline(v = d)
          abline(v = d+Window)
          plot(temp_data$Time[temp_data$Time <= d+Window],  temp_data$Nervous[temp_data$Time <= d+Window], type = 'b', bty = 'n', xlab = "", col = 'green', ylab = "Nervous", xaxt = 'n', yaxt = 'n', xlim=c(1,nrow(temp_data)), ylim = range(0:4), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 2)
          axis(1, at=seq(1,60,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          axis(2, at=seq(0,4,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          abline(v = d)
          abline(v = d+Window)
          plot(temp_data$Time[temp_data$Time <= d+Window],  temp_data$Act_later_regret[temp_data$Time <= d+Window], type = 'b', bty = 'n', col = 'cyan', xlab = "", ylab = "Act_later_regret", xaxt = 'n', yaxt = 'n', xlim=c(1,nrow(temp_data)), ylim = range(0:4), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 2)
          axis(1, at=seq(1,60,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          axis(2, at=seq(0,4,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          abline(v = d)
          abline(v = d+Window)
          plot(temp_data$Time[temp_data$Time <= d+Window],  temp_data$Act_without_thinking[temp_data$Time <= d+Window], type = 'b', bty = 'n', col = 'blue', xlab = "", ylab = "Act_without_thinking", xaxt = 'n', yaxt = 'n', xlim=c(1,nrow(temp_data)), ylim = range(0:4), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 2)
          axis(1, at=seq(1,60,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          axis(2, at=seq(0,4,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          abline(v = d)
          abline(v = d+Window)
          plot(temp_data$Time[temp_data$Time <= d+Window],  temp_data$Restless[temp_data$Time <= d+Window], type = 'b', bty = 'n', xlab = "", col = 'magenta', ylab = "Restless", xaxt = 'n', yaxt = 'n', xlim=c(1,nrow(temp_data)), ylim = range(0:4), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 2)
          axis(1, at=seq(1,60,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          axis(2, at=seq(0,4,by=1), cex.axis = 2, cex.lab = 2, cex = 2, lwd = 1)
          abline(v = d)
          abline(v = d+Window)
      }
    } # lastly, save videos in folder as .avi
  }, ani.width = 1900, ani.height = 950, video.name = paste0("Dynamic_Networks_Video_Participant_",pp_id,".avi"), interval = 0.05, other.opts = "-vb 20M -vcodec mpeg4", clean = TRUE, outdir = "C:/Users/Daan/Videos/RVideos/")
}

