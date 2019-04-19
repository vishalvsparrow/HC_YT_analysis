library(stringr)
library(dplyr)
library(lubridate)
library(corrplot)
library(ggplot2)
rm(list = ls(all.names = TRUE))
setwd('C:\\Users\\Vishal Vatnani\\Documents\\HC_YouTube_Analysis')

dat = read.csv("youtube_inventory.csv", stringsAsFactors = FALSE)

# remove the blog row 
dat <- dat[-which(dat$Content.Platform == "Blog"),]

dat[grep("Hair install", dat$Category),]$Category <- "Tutorial, hair installation, Cylinder"

tmp_str <- dat[grep("scences", dat$Category),]$Category
# surgically correct the spelling for "scences"
dat[grep("scences", dat$Category),]$Category <- gsub("scences", "scenes", tmp_str, ignore.case = T)

# split into categories

#dat$level1 <- 
  
# split categories and likes/dislikes and categories

for (i in 1:length(dat$Category)) {

  dat$level1[i] <- tolower(trimws(strsplit(dat$Category[i], ",")[[1]][1]))
  dat$level2[i] <- tolower(trimws(strsplit(dat$Category[i], ",")[[1]][2]))
  dat$level1_level2[i] <-  paste(dat$level1[i], ":", dat$level2[i])
  
  if(is.na(dat$level2[i])) {
    dat$level2[i] <- dat$level1[i]
  }
  
  tmp_likes <- strsplit(dat$Likes..dislikes.[i], "/")[[1]][1]
  tmp_dislikes <- strsplit(dat$Likes..dislikes.[i], "/")[[1]][2]
  
  if(is.na(tmp_likes))
    dat$likes[i] <- 0
  else
    dat$likes[i] <- as.numeric(strsplit(dat$Likes..dislikes.[i], "/")[[1]][1])
  if(is.na(tmp_dislikes))
    dat$dislikes[i] <- 0
  else
  dat$dislikes[i] <- as.numeric(strsplit(dat$Likes..dislikes.[i], "/")[[1]][2])

}

# check all the level-1s and level-2s
unique(dat$level1)
unique(dat$level2)
unique(dat$level1_level2)
unique((dat$dislikes))

# as.numeric to likes and dislikes
dat$likes <- as.integer(dat$likes)
dat$dislikes <- as.integer(dat$dislikes)

# convert duration string to seconds
to_duration <- function(x) {
  
  dur <- (strsplit(x, ":")[[1]])
  mins <- as.integer(dur[1])
  secs <- as.integer(dur[2])
  
  total_secs <- (mins*60) + secs
  
  return(total_secs)
  
}

# convert impressions character to integer
dat$Impressions <- as.integer(dat$Impressions)

dat$total.seconds <- sapply(dat$Video.duration, to_duration)
dat$view.seconds <- sapply(dat$View.duration..Time.on.page., to_duration)

# factors to level1
dat$level1 <- as.factor(dat$level1)

# which video has 10 dislikes!?
dat[which(dat$dislikes > 5),]

# check correlation between level1 and various metrics

M <- cor((na.omit(dat[,c(5,18,19, 11, 20, 21)])))
corrplot(M)
M
# box plot!

boxplot(dat[,c(5,18,19, 11, 20, 21)])

# linear regression model!

model.lm <- lm(level1 ~ likes + Impressions, data = dat)
summary(model.lm)

# export the data to csv for pivot table
write.csv(dat, "yt_export.csv")

dat.pivot <- read.csv("pivot.csv", stringsAsFactors = FALSE)

row.names(dat.pivot) <- dat.pivot$level1

dat.pivot <- dat.pivot[-which(row.names(dat.pivot) =="Grand Total"),]

# convert % to numeric
dat.pivot$Impression.CTR <- as.numeric(sub("%", "", dat.pivot$Impression.CTR))/100
dat.pivot$X.engagement <- as.numeric(sub("%", "", dat.pivot$X.engagement))/100

# correlation 
M <- cor(scale(dat.pivot[,-1]))
corrplot(M)

# clustering
d <- dist(scale(dat.pivot[,-c(1, 2, 7, 3, 4, 6, 5, 8)]), method = "euclidean")
fit <- hclust(d, method = "ward.D2")
plot(fit)

# ANOVA
dat.anova <- dat
dat.anova$level1 <- factor(dat$level1)

ggplot(dat.anova, aes(x = level1, y = view.seconds)) + geom_boxplot(fill = "grey80", color  = "blue")

# remove categeories with only one level
dat.anova <- dat.anova[-which(dat.anova$level1 %in% names(which(table(dat.anova$level1) < 5))), ]

# run ANOVA
lm(likes ~ level1, data = dat.anova[which(dat.anova$level1 %in% c("education", "tutorial")),])

