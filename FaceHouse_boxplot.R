# Script for making boxplots of reaction time data from one subject
# Emma March 2012

# Load library
library("ggplot2")

# Load dataframe
sub27 <- read.csv("~/Dropbox/Berkeley/Attention_Training_Project/Pilot02/LetterFaceTask/Analysis/sub027/Sub027_FaceHouse.csv")
View(sub27)

bp <- ggplot(subset(sub27, Acc==1), aes(x=Cond, y=RT) )
bp + geom_boxplot(outlier.colour="red", outlier.size=3) + 
  geom_jitter(aes(colour=Load)) +
#   geom_text() +
  facet_grid(.~Day) +
  opts(title="Reaction Time for Sub 27")