# this script plots head motion metrics across subjects for each of the parts of the scan (split into fifths)

library(vioplot)
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
library(gridExtra)

# define directories

data_dir <- c("S:/AG/AG-CSB_NeuroRad2/khalila/MULTIBAND/multiband_data_last/")
subjects <- list.files(path="S:/AG/AG-CSB_NeuroRad2/khalila/MULTIBAND/multiband_data_last/R_project/R_github", pattern="sub0+")

# loop through subjects, getting FD and DVARS values
fd_mean_1_all <- c()
fd_mean_2_all <- c()
fd_mean_3_all <- c()
fd_mean_4_all <- c()
fd_mean_5_all <- c()

fd_max_1_all <- c()
fd_max_2_all <- c()
fd_max_3_all <- c()
fd_max_4_all <- c()
fd_max_5_all <- c()

dvars_mean_1_all <- c()
dvars_mean_2_all <- c()
dvars_mean_3_all <- c()
dvars_mean_4_all <- c()
dvars_mean_5_all <- c()

dvars_max_1_all <- c()
dvars_max_2_all <- c()
dvars_max_3_all <- c()
dvars_max_4_all <- c()
dvars_max_5_all <- c()

fd_max_index_all <- c()
fd_max_value_all <- c()

dvars_max_index_all <- c()
dvars_max_value_all <- c()

for (s in subjects) {
  # load HM values
  fd <- read.csv(file = paste(data_dir, s, "/session_1/RS_E/fd.txt", sep=""))
  dvars <- read.csv(file = paste(data_dir, s, "/session_1/RS_E/dvars.txt", sep=""))/10
  
  # get mean HM in each part of the scan
  fd_mean_1 <- mean(fd$X0[0:164])
  fd_mean_1_all <- c(fd_mean_1_all, fd_mean_1)
  fd_mean_2 <- mean(fd$X0[165:329])
  fd_mean_2_all <- c(fd_mean_2_all, fd_mean_2)
  fd_mean_3 <- mean(fd$X0[330:494])
  fd_mean_3_all <- c(fd_mean_3_all, fd_mean_3)
  fd_mean_4 <- mean(fd$X0[495:659])
  fd_mean_4_all <- c(fd_mean_4_all, fd_mean_4)
  fd_mean_5 <- mean(fd$X0[660:824])
  fd_mean_5_all <- c(fd_mean_5_all, fd_mean_5)
  
  dvars_mean_1 <- mean(dvars$X0[0:164])
  dvars_mean_1_all <- c(dvars_mean_1_all, dvars_mean_1)
  dvars_mean_2 <- mean(dvars$X0[165:329])
  dvars_mean_2_all <- c(dvars_mean_2_all, dvars_mean_2)
  dvars_mean_3 <- mean(dvars$X0[330:494])
  dvars_mean_3_all <- c(dvars_mean_3_all, dvars_mean_3)
  dvars_mean_4 <- mean(dvars$X0[495:659])
  dvars_mean_4_all <- c(dvars_mean_4_all, dvars_mean_4)
  dvars_mean_5 <- mean(dvars$X0[660:824])
  dvars_mean_5_all <- c(dvars_mean_5_all, dvars_mean_5)
  
  # get number of HM spikes in each part of the scan
  fd_max_1 <- length(fd$X0[0:164][fd$X0[0:164]>0.3])
  fd_max_1_all <- c(fd_max_1_all, fd_max_1)
  fd_max_2 <- length(fd$X0[165:329][fd$X0[165:329]>0.3])
  fd_max_2_all <- c(fd_max_2_all, fd_max_2)
  fd_max_3 <- length(fd$X0[330:494][fd$X0[330:494]>0.3])
  fd_max_3_all <- c(fd_max_3_all, fd_max_3)
  fd_max_4 <- length(fd$X0[495:659][fd$X0[495:659]>0.3])
  fd_max_4_all <- c(fd_max_4_all, fd_max_4)
  fd_max_5 <- length(fd$X0[660:824][fd$X0[660:824]>0.3])
  fd_max_5_all <- c(fd_max_5_all, fd_max_5)
  
  dvars_max_1 <- length(dvars$X0[0:164][dvars$X0[0:164]>4])
  dvars_max_1_all <- c(dvars_max_1_all, dvars_max_1)
  dvars_max_2 <- length(dvars$X0[165:329][dvars$X0[165:329]>4])
  dvars_max_2_all <- c(dvars_max_2_all, dvars_max_2)
  dvars_max_3 <- length(dvars$X0[330:494][dvars$X0[330:494]>4])
  dvars_max_3_all <- c(dvars_max_3_all, dvars_max_3)
  dvars_max_4 <- length(dvars$X0[495:659][dvars$X0[495:659]>4])
  dvars_max_4_all <- c(dvars_max_4_all, dvars_max_4)
  dvars_max_5 <- length(dvars$X0[660:824][dvars$X0[660:824]>4])
  dvars_max_5_all <- c(dvars_max_5_all, dvars_max_5)
  
  # get value and index of maximum FD
  fd_max_value <- max(fd)
  fd_max_index <- which(fd==max(fd))
  
  fd_max_value_all <- c(fd_max_value_all, fd_max_value)
  fd_max_index_all <- c(fd_max_index_all, fd_max_index)
  
  dvars_max_value <- max(dvars)
  dvars_max_index <- which(dvars==max(dvars))
  
  dvars_max_value_all <- c(dvars_max_value_all, dvars_max_value)
  dvars_max_index_all <- c(dvars_max_index_all, dvars_max_index)
}
# create dataframes

fd_mean_all <- cbind(fd_mean_1_all, fd_mean_2_all, fd_mean_3_all, fd_mean_4_all, fd_mean_5_all)
colnames(fd_mean_all) <- c("A","B","C","D","E")
dvars_mean_all <- cbind(dvars_mean_1_all, dvars_mean_2_all, dvars_mean_3_all, dvars_mean_4_all, dvars_mean_5_all)
colnames(dvars_mean_all) <- c("A","B","C","D","E")

fd_max_all <- cbind(fd_max_1_all, fd_max_2_all, fd_max_3_all, fd_max_4_all, fd_max_5_all)
colnames(fd_max_all) <- c("A","B","C","D","E")
dvars_max_all <- cbind(dvars_max_1_all, dvars_max_2_all, dvars_max_3_all, dvars_max_4_all, dvars_max_5_all)
colnames(dvars_max_all) <- c("A","B","C","D","E")

# reshape data wide to long
fd_mean_all_melt <- melt(fd_mean_all)
colnames(fd_mean_all_melt) <- c("Subject","Segment","HM")

dvars_mean_all_melt <- melt(dvars_mean_all)
colnames(dvars_mean_all_melt) <- c("Subject","Segment","HM")

fd_max_all_melt <- melt(fd_max_all)
colnames(fd_max_all_melt) <- c("Subject","Segment","HM")


dvars_max_all_melt <- melt(dvars_max_all)
colnames(dvars_max_all_melt) <- c("Subject","Segment","HM")


# define a theme to make pretty plots
raincloud_theme = theme(
  text = element_text(size = 20),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text = element_text(size = 20),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


# calculate summary statistics
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

for (p in c("fd_mean_all_melt","fd_max_all_melt","dvars_mean_all_melt","dvars_max_all_melt")){

sumld<- ddply(get(p), ~Segment, summarise, mean = mean(HM), median = median(HM), lower = lb(HM), upper = ub(HM))
head(sumld)


# raincloud plots
g <- ggplot(data = get(p), aes(y = HM, x = Segment, fill = Segment)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = HM, color = Segment), position = position_jitter(width = .15), size = 1, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  #coord_flip() +
  theme_bw() +
  raincloud_theme +
  ylab(p) + 
  xlab("Scan length")
pdf(paste(p, ".pdf", sep=""), width = 9, height = 6)
plot(g)
dev.off()
}

# plot locations of maximum FD and DVARS
pdf("FD_max.pdf", width = 9, height = 6)
plot(fd_max_index_all*0.4, fd_max_value_all, log = "y", xlim = c(0,825*0.4), xlab = "Time (s)", ylab = "FD (mm)", cex.axis = 1.5, cex.lab=1.5, pch = 19, cex = 1.2)
dev.off()
pdf("DVARS_max.pdf", width = 9, height = 6)
plot(dvars_max_index_all*0.4, dvars_max_value_all, log = "y", xlim = c(0,825*0.4), xlab = "Time (s)", ylab = "DVARS (%*10)", cex.axis = 1.5, cex.lab=1.5, pch=19, cex = 1.2)
dev.off()