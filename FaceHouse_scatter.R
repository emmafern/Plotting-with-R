# Script for making scatterplots of Pilot02 data
# Emma March 2012

# Load library
library("ggplot2")

# Load dataframe
FaceHouse <- read.csv("~/Dropbox/Berkeley/Attention_Training_Project/Pilot02/LetterFaceTask/FaceHouse_Summary_Group.csv")
View(FaceHouse)

# Morph - House RT (Low Load)
# get r-squared values
sp1_r2_1 = function(FaceHouse){
  m1 = lm(Morph.HouseLoRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
  eq1 <-substitute(~~italic(r)^2~"="~r2,
                  list(r2 = format(summary(m1)$r.squared, digits=3)))
  as.character(as.expression(eq1));
}
sp1_r2_2 = function(FaceHouse){
  m2 = lm(Morph.HouseLoRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
  eq2 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m2)$r.squared, digits=3)))
  as.character(as.expression(eq2));
}

# scatter plot with different colors and regression lines for day
sp1 <- ggplot(FaceHouse, aes(x=TraitAnx,y=Morph.HouseLoRT, color=factor(Day)))
sp1 + geom_point(shape=19, size=4) + 
  geom_smooth(method=lm, se=F, fullrange=T) + 
  geom_text(aes(x=25, y=.03, label = sp1_r2_1(FaceHouse)), 
            parse=TRUE) +
  geom_text(aes(x=25, y=-.035, label = sp1_r2_2(FaceHouse)), 
            parse=TRUE) +
  opts(title="Trait Anxiety and Morph-House RT under Low Load")

# this does a similar thing as above
# qplot(TraitAnx, Morph.HouseLoRT, data=FaceHouse, color=factor(Day)) +
#   geom_smooth(method=lm, se=FALSE, fullrange=T) + geom_point(size=4)
# # facet the plot based on day
# qplot(TraitAnx, Morph.HouseLoRT, data=FaceHouse, facets = .~ Day) + 
#   geom_smooth(method=lm, se=FALSE, fullrange=T) + geom_point(size=4)

# Morph - House RT (High Load)
# get r-squared value
sp2_r2_1 = function(FaceHouse){
  m3 = lm(Morph.HouseHiRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
  eq3 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m3)$r.squared, digits=3)))
  as.character(as.expression(eq3));
}
sp2_r2_2 = function(FaceHouse){
  m4 = lm(Morph.HouseHiRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
  eq4 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m4)$r.squared, digits=3)))
  as.character(as.expression(eq4));
}

# scatter plot with different colors and regression lines for day
sp2 <- ggplot(FaceHouse, aes(x=TraitAnx,y=Morph.HouseHiRT, color=factor(Day)))
sp2 + geom_point(shape=19, size=4) + 
  geom_smooth(method=lm, se=FALSE, fullrange=T) +
  geom_text(aes(x=25, y=-.1, label = sp2_r2_1(FaceHouse)), 
            parse=TRUE) +
  geom_text(aes(x=25, y=.035, label = sp2_r2_2(FaceHouse)), 
            parse=TRUE) +
  opts(title="Trait Anxiety and Morph-House RT under High Load")

# Morph-House RT Day 1 vs. Day 2
sp5_r2_2 = function(FaceHouse){
  m13 = lm(subset(FaceHouse$Morph.HouseLoRT,FaceHouse$Day==1) ~ subset(FaceHouse$Morph.HouseLoRT,FaceHouse$Day==2));
  eq13 <-substitute(~~italic(r)^2~"="~r2,
                    list(r2 = format(summary(m13)$r.squared, digits=3)))
  as.character(as.expression(eq13));
}

sp5 <- ggplot(FaceHouse, 
              aes(x=subset(FaceHouse$Morph.HouseLoRT,FaceHouse$Day==1), 
                  y=subset(FaceHouse$Morph.HouseLoRT,FaceHouse$Day==2)))
sp5 + geom_point(size=4) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  geom_text(aes(x=-0.03, y=0, label = sp5_r2_2(FaceHouse)), 
            parse=TRUE) +
              opts(title="Morph-House RT under Low Load, Day 1 vs. Day 2")

# # Morph RT (High Load)
# # get r-squared values
# sp3_r2_1 = function(FaceHouse){
#   m5 = lm(MorphHiRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
#   eq5 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m5)$r.squared, digits=3)))
#   as.character(as.expression(eq5));
# }
# sp3_r2_2 = function(FaceHouse){
#   m6 = lm(MorphHiRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
#   eq6 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m6)$r.squared, digits=3)))
#   as.character(as.expression(eq6));
# }
# 
# # scatter plot with different colors and regression lines for day
# sp3 <- ggplot(FaceHouse, aes(x=TraitAnx, y=MorphHiRT, color=factor(Day)))
# sp3 + geom_point(shape=19, size=4) +
#   geom_smooth(method=lm, se=F, fullrange=T) +
#   geom_text(aes(x=25, y=.8, label = sp3_r2_1(FaceHouse)), 
#             parse=TRUE) +
#   geom_text(aes(x=25, y=.6, label = sp3_r2_2(FaceHouse)), 
#             parse=TRUE) +
#   opts(title="Trait Anxiety and Morph RT under High Load")

# # Morph RT (Low Load)
# # get r-squared values
# sp6_r2_1 = function(FaceHouse){
#   m9 = lm(MorphLoRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
#   eq9 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m9)$r.squared, digits=3)))
#   as.character(as.expression(eq9));
# }
# sp6_r2_2 = function(FaceHouse){
#   m10 = lm(MorphLoRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
#   eq10 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m10)$r.squared, digits=3)))
#   as.character(as.expression(eq10));
# }
# 
# # scatter plot with different colors and regression lines for day
# sp6 <- ggplot(FaceHouse, aes(x=TraitAnx, y=MorphLoRT, color=factor(Day)))
# sp6 + geom_point(shape=19, size=4) +
#   geom_smooth(method=lm, se=F, fullrange=T) +
#   geom_text(aes(x=25, y=.65, label = sp6_r2_1(FaceHouse)), 
#             parse=TRUE) +
#   geom_text(aes(x=25, y=.5, label = sp6_r2_2(FaceHouse)), 
#             parse=TRUE) +
#   opts(title="Trait Anxiety and Morph RT under Low Load")

# # House RT (High Load)
# # get r-squared values
# sp4_r2_1 = function(FaceHouse){
#   m7 = lm(HouseHiRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
#   eq7 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m7)$r.squared, digits=3)))
#   as.character(as.expression(eq7));
# }
# sp4_r2_2 = function(FaceHouse){
#   m8 = lm(HouseHiRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
#   eq8 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m8)$r.squared, digits=3)))
#   as.character(as.expression(eq8));
# }
# 
# # scatter plot with different colors and regression lines for day
# sp4 <- ggplot(FaceHouse, aes(x=TraitAnx, y=HouseHiRT, color=factor(Day)))
# sp4 + geom_point(shape=19, size=4) +
#   geom_smooth(method=lm, se=F, fullrange=T) +
#   geom_text(aes(x=25, y=.9, label = sp4_r2_1(FaceHouse)), 
#             parse=TRUE) +
#   geom_text(aes(x=25, y=.6, label = sp4_r2_2(FaceHouse)), 
#             parse=TRUE) +
#   opts(title="Trait Anxiety and House RT under High Load")

# # House RT (Low Load)
# # get r-squared values
# sp7_r2_1 = function(FaceHouse){
#   m11 = lm(HouseLoRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
#   eq11 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2= format(summary(m11)$r.squared,digits=3)))
#   as.character(as.expression(eq11));
# }
# sp7_r2_2 = function(FaceHouse){
#   m12 = lm(HouseLoRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
#   eq12 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2= format(summary(m12)$r.squared,digits=3)))
#   as.character(as.expression(eq12));
# }
# 
# # scatter plot with different colors and regression lines for day
# sp7 <- ggplot(FaceHouse, aes(x=TraitAnx, y=HouseLoRT, color=factor(Day)))
# sp7 + geom_point(shape=19, size=4) +
#   geom_smooth(method=lm, se=F, fullrange=T) +
#   geom_text(aes(x=25, y=.65, label = sp7_r2_1(FaceHouse)), 
#             parse=TRUE) +
#   geom_text(aes(x=25, y=.5, label = sp7_r2_2(FaceHouse)), 
#             parse=TRUE) +
#   opts(title="Trait Anxiety and House RT under Low Load")

# Morph - Fear RT (Low Load)
# get r-squared values
sp1_r2_1 = function(FaceHouse){
  m1 = lm(Morph.FearLoRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
  eq1 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m1)$r.squared, digits=3)))
  as.character(as.expression(eq1));
}
sp1_r2_2 = function(FaceHouse){
  m2 = lm(Morph.FearLoRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
  eq2 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m2)$r.squared, digits=3)))
  as.character(as.expression(eq2));
}

# scatter plot with different colors and regression lines for day
sp1 <- ggplot(FaceHouse, aes(x=TraitAnx,y=Morph.FearLoRT, color=factor(Day)))
sp1 + geom_point(shape=19, size=4) + 
  geom_smooth(method=lm, se=F, fullrange=T) + 
  geom_text(aes(x=25, y=.03, label = sp1_r2_1(FaceHouse)), 
            parse=TRUE) +
  geom_text(aes(x=25, y=-.035, label = sp1_r2_2(FaceHouse)), 
            parse=TRUE) +
  opts(title="Trait Anxiety and Morph-Fear RT under Low Load")

# Morph - Fear RT (High Load)
# get r-squared values
sp2_r2_1 = function(FaceHouse){
  m3 = lm(Morph.FearHiRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
  eq3 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m3)$r.squared, digits=3)))
  as.character(as.expression(eq3));
}
sp2_r2_2 = function(FaceHouse){
  m4 = lm(Morph.FearHiRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
  eq4 <-substitute(~~italic(r)^2~"="~r2,
                   list(r2 = format(summary(m4)$r.squared, digits=3)))
  as.character(as.expression(eq4));
}

# scatter plot with different colors and regression lines for day
sp1 <- ggplot(FaceHouse, aes(x=TraitAnx,y=Morph.FearHiRT, color=factor(Day)))
sp1 + geom_point(shape=19, size=4) + 
  geom_smooth(method=lm, se=F, fullrange=T) + 
  geom_text(aes(x=25, y=.03, label = sp2_r2_1(FaceHouse)), 
            parse=TRUE) +
  geom_text(aes(x=25, y=-.035, label = sp2_r2_2(FaceHouse)), 
            parse=TRUE) +
  opts(title="Trait Anxiety and Morph-Fear RT under High Load")

# Morph-Fear RT Day 1 vs. Day 2
sp5_r2_2 = function(FaceHouse){
  m13 = lm(subset(FaceHouse$Morph.FearLoRT,FaceHouse$Day==1) ~ subset(FaceHouse$Morph.FearLoRT,FaceHouse$Day==2));
  eq13 <-substitute(~~italic(r)^2~"="~r2,
                    list(r2 = format(summary(m13)$r.squared, digits=3)))
  as.character(as.expression(eq13));
}

sp5 <- ggplot(FaceHouse, 
              aes(x=subset(FaceHouse$Morph.FearLoRT,FaceHouse$Day==1), 
                  y=subset(FaceHouse$Morph.FearLoRT,FaceHouse$Day==2)))
sp5 + geom_point(size=4) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  geom_text(aes(x=-0.025, y=-0.01, label = sp5_r2_2(FaceHouse)), 
            parse=TRUE) +
  opts(title="Morph-Fear RT under Low Load, Day 1 vs. Day 2")

# # Morph - Neut RT (Low Load)
# # get r-squared values
# sp1_r2_1 = function(FaceHouse){
#   m1 = lm(Morph.NeutLoRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
#   eq1 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m1)$r.squared, digits=3)))
#   as.character(as.expression(eq1));
# }
# sp1_r2_2 = function(FaceHouse){
#   m2 = lm(Morph.NeutLoRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
#   eq2 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m2)$r.squared, digits=3)))
#   as.character(as.expression(eq2));
# }
# 
# # scatter plot with different colors and regression lines for day
# sp1 <- ggplot(FaceHouse, aes(x=TraitAnx,y=Morph.NeutLoRT, color=factor(Day)))
# sp1 + geom_point(shape=19, size=4) + 
#   geom_smooth(method=lm, se=F, fullrange=T) + 
#   geom_text(aes(x=25, y=.03, label = sp1_r2_1(FaceHouse)), 
#             parse=TRUE) +
#   geom_text(aes(x=25, y=-.035, label = sp1_r2_2(FaceHouse)), 
#             parse=TRUE) +
#   opts(title="Trait Anxiety and Morph-Neut RT under Low Load")
# 
# # Morph - Neut RT (High Load)
# # get r-squared values
# sp1_r2_1 = function(FaceHouse){
#   m1 = lm(Morph.NeutHiRT ~ TraitAnx, data=subset(FaceHouse, Day==1));
#   eq1 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m1)$r.squared, digits=3)))
#   as.character(as.expression(eq1));
# }
# sp1_r2_2 = function(FaceHouse){
#   m2 = lm(Morph.NeutHiRT ~ TraitAnx, data=subset(FaceHouse, Day==2));
#   eq2 <-substitute(~~italic(r)^2~"="~r2,
#                    list(r2 = format(summary(m2)$r.squared, digits=3)))
#   as.character(as.expression(eq2));
# }
# 
# # scatter plot with different colors and regression lines for day
# sp1 <- ggplot(FaceHouse, aes(x=TraitAnx,y=Morph.NeutHiRT, color=factor(Day)))
# sp1 + geom_point(shape=19, size=4) + 
#   geom_smooth(method=lm, se=F, fullrange=T) + 
#   geom_text(aes(x=25, y=-.035, label = sp1_r2_1(FaceHouse)), 
#             parse=TRUE) +
#   geom_text(aes(x=25, y=.03, label = sp1_r2_2(FaceHouse)), 
#             parse=TRUE) +
#   opts(title="Trait Anxiety and Morph-Neut RT under High Load")

# # make bar charts for group mean RT and accuracy
# meanRT <- tapply(FaceHouse$MorphLoRT, FaceHouse$Day, mean)
# Day <- factor(levels(factor(FaceHouse$Day)), levels = levels(factor(FaceHouse$Day)))
# qplot(Day, meanRT, geom="bar", width=.5, stat="identity")

# how to create new "column" of data based on two existing ones
# a difference of two conditions?
# morphhouseLoRT <- FaceHouse$MorphLoRT - FaceHouse$HouseLoRT