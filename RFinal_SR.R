# 21:219:220 Fundamentals of Data Visualization, Spring 2021
# Final Assignment
# Sasha Rider, 198008430

#loading in libraries
library(GISTools)
library(Cairo)
library(RColorBrewer)
library(corrplot)

#choosing color brewer colours & adding transparency 
colours <- brewer.pal(12, "Paired")
coloursT <- add.alpha(colours, .5)

#loading in dataset 
musicData <- read.csv("/Users/Sasha/Desktop/top10s.csv")

#will return TRUE/FALSE based on whether there are any NA's
is.na(musicData)
#data is clean, no NA values

newData <- musicData[-1]
#removing first column that is called X, it is not useful

summary(newData)
#seeing what is in the dataframe
str(newData)
#15 variables, title, artist, top.genre are all character, rest are integer

unique(newData$artist)
#184 different artists
unique(newData$top.genre)
#50 genres of music
unique(newData$pop)
range(newData$pop)
#0 to 99 range in popularity
sort(unique(newData$artist))
#184 artists 

#this table will show how many songs each artist has 
artists <- newData[("artist")]
artists <- sort(table(artists), decreasing = TRUE)[1:5]
#shows top 5 artists 

#Creating most popular artists barplot 
cairo_pdf("ArtistGenrebySong.pdf", width = 8, height = 6)
par(mfrow = c(2,1), cex = 0.8, mai = c(0.5,1,0.5,0.5))
barplot(artists, 
        xlab = "Frequency",
        col = colours[1],
        beside = TRUE,
        space = c(.3),
        las = 1,
        horiz = TRUE,
        axes = FALSE
        )
axis(1, at=seq(0,17,1))
mtext("Artist by number of popular songs", side = 3, line = 1.2, cex = 1.2)

genres <- newData[("top.genre")]
genres <- sort(table(genres), decreasing = TRUE)[1:5]
#shows top 5 genres

#Creating most popular genres barplot 
par(mai = c(1, 1.05, 0.5, 0.5), cex = 0.8)
barplot(genres, 
        col = coloursT[10],
        beside = TRUE,
        space = c(.3),
        las = 1,
        horiz = TRUE,
        axes = FALSE
)
axis(1, at=seq(0,330,10))
mtext("Genre by number of popular songs", side = 3, line = 1.2, cex = 1.2)
mtext("Frequency", side = 1, cex = 1, line = 3)
dev.off()

#removing all non-numeric columns to plot correlation matrix
matrixData <- musicData[5:15]
str(matrixData)

### Correlation Matrix ###
cairo_pdf("MusicCorrMatrix.pdf", width = 8, height = 10)
M <- cor(matrixData, use = "everything")
corrplot(M, method = "ellipse", col = hcl.colors(180, "PiYG"),
         type = "upper", tl.cex = 0.8, tl.col = "azure4")
#corr matrix tells us that energy & db are positively correlated
#val and nrgy +, dance and valence +
#energy and acousticeness negatively correlated
dev.off()

#shows median value for each column 
medianData <- apply(newData[,5:dim(newData)[2]],2, median)

#checking which songs have popularity = 0
check <- newData[newData$pop == 0, ]
#Adele song "Million Years ago" has bpm of 0, which is missing data
cleandata <- newData[newData$bpm != 0,]
cleandata$bpm == 0
#all False values are returned, meaning there is now no missing data for bpm
check2 <- cleandata[cleandata$pop == 0, ]
#now million years ago does not appear

#Finding songs with 99 popularity which is highest value in this dataset
cleandata[cleandata$pop == 99,]
#Memories by Maroon 5 is the most popular song 

#plotting bpm predictor 
xlimit <- range(cleandata$bpm)
ylimit <- range(cleandata$pop)
cairo_pdf("MusicPredictors.pdf", width = 8, height = 12)
par(mfrow = c(3,3))
plot(x = NULL, y = NULL, xlim = xlimit, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$bpm, cleandata$pop, pch = 21, col = colours[1], cex = 1.2)
axis(1, at = seq(40,240, 40), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(40,240, 40))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("BPM (Beats Per Minute)", side = 1, cex = 1, line = 2.5)
mtext("BPM on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ bpm, data = cleandata)
abline(67.89362, -.01063, col = "black")
cor(cleandata$bpm, cleandata$pop)
#r = -0.018
mtext("r = -0.018", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#fairly evenly distributed, most popular songs lie between 80-120 range

#plotting energy predictor 
xlimit2 <- range(cleandata$nrgy)
plot(x = NULL, y = NULL, xlim = xlimit2, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$nrgy, cleandata$pop, pch = 21, col = colours[2], cex = 1.2)
axis(1, at = seq(0,100,10), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(0,100,10))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Energy", side = 1, cex = 1, line = 2.5)
mtext("Energy on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ nrgy, data = cleandata)
abline(72.50895, -.08323, col = "black")
cor(cleandata$nrgy, cleandata$pop)
#r = -0.0936
mtext("r = -0.0936", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#Pretty even, except popularity slightly goes down as energy goes up. Most popular songs
#lie between 30-40 range. If there is too much energy, popularity goes down as it can 
#be too much. 

#plotting danceability predictor
xlimit3 <- range(cleandata$dnce)
plot(x = NULL, y = NULL, xlim = xlimit3, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$dnce, cleandata$pop, pch = 21, col = colours[3], cex = 1.2)
axis(1, at = seq(20,100, 10), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(20,100, 10))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Danceability", side = 1, cex = 1, line = 2.5)
mtext("Danceability on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ dnce, data = cleandata)
abline(60.8548, .08958, col = "black")
cor(cleandata$dnce, cleandata$pop)
#r = .0823
mtext("r = 0.0823", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#As danceability goes up, so does popularity. Most popular songs are
#between 50-80 range. This makes sense as dance pop is the #1 genre. People are also
#more likely to enjoy a song if they feel they can move to it. 

#plotting decibel predictor 
xlimit4 <- range(cleandata$dB)
plot(x = NULL, y = NULL, xlim = xlimit4, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$dB, cleandata$pop, pch = 21, col = colours[4], cex = 1.2)
axis(1, at = seq(0, -16, -2), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(0, -16, -2))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("dB (decibels)", side = 1, cex = 1, line = 2.5)
mtext("Decibels on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ dB, data = cleandata)
abline(67.2930, 0.1206 ,col = "black")
cor(cleandata$dB, cleandata$pop)
mtext("r = 0.0144", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#decibels range as certain songs need different volumes. Most songs lie within the
#-2 to -6 dB range. 

#plotting liveliness predictor 
xlimit5 <- range(cleandata$live)
plot(x = NULL, y = NULL, xlim = xlimit5, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$live, cleandata$pop, pch = 21, col = colours[5], cex = 1.2)
axis(1, at = seq(0, 75, 15), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(0, 75, 15))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Liveliness", side = 1, cex = 1, line = 2.5)
mtext("Liveliness on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ live, data = cleandata)
abline(68.33477, -0.09568 ,col = "black")
cor(cleandata$live, cleandata$pop)
#r = -0.0877
mtext("r = -0.0877", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#as liveliness goes up, popularity goes down. This is because people want to hear a song
#live when they are at concerts but not when they are using Spotify. Most people want 
#a song with higher production value. 
cleandata[cleandata$live == 74,]

#Plotting valence predictor 
xlimit6 <- range(cleandata$val)
plot(x = NULL, y = NULL, xlim = xlimit6, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$val, cleandata$pop, pch = 21, col = colours[6], cex = 1.2)
axis(1, at = seq(0, 100, 10), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(0, 100, 10))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Valence", side = 1, cex = 1, line = 2.5)
mtext("Valence on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ val, data = cleandata)
abline(65.90734, 0.01384 ,col = "black")
cor(cleandata$val, cleandata$pop)
#r = 0.0217
mtext("r = 0.0217", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#Very evenly distributed, as valence is how positive the mood is for the song
#music is very expressive of emotions and thus has popular sad songs as well
#as happy songs. As the song becomes more positive, popularity slightly goes up.
#However, a song with only 10 valence comes in second place to a song that has
#between 50-60 valence with top popularity level.

#Plotting duration predictor 
xlimit7 <- range(cleandata$dur)
plot(x = NULL, y = NULL, xlim = xlimit7, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$dur, cleandata$pop, pch = 21, col = colours[8], cex = 1.2)
axis(1, at = seq(100, 450, 50), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(100, 450, 50))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Duration", side = 1, cex = 1, line = 2.5)
mtext("Duration on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ dur, data = cleandata)
abline(76.55534, -0.04417,col = "black")
cor(cleandata$dur, cleandata$pop)
#r = -0.105
mtext("r = -0.105", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#As duration goes up, popularity of song goes down. Most popular songs lie in the
#150-200 range which is 2.5-3 minutes. This is typical time for a song. Songs that
#are too short (less than 150) and songs that exceed 300 also seem less popular.
#Probably due to the fact that if a song is too short then it does not satisfy
#what we are used to in terms of length of a song. And on the contrary, if a song
#is too long, it may get dragged out and become tedious to listen to.

#Plotting acousticeness predictor 
xlimit8 <- range(cleandata$acous)
plot(x = NULL, y = NULL, xlim = xlimit8, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$acous, cleandata$pop, pch = 21, col = colours[9], cex = 1.2)
axis(1, at = seq(0, 100, 10), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(0, 100, 10))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Acousticeness", side = 1, cex = 1, line = 2.5)
mtext("Acousticeness on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ acous, data = cleandata)
abline(66.416, 0.015 ,col = "black")
cor(cleandata$acous, cleandata$pop)
#r = 0.0218
mtext("r = 0.0218", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#This is evenly distributed, however most songs cluster around the 0-20 range.
#Most songs will be in this range as there is singing involved, if a song is in the higher
#range of acousticeness, then there is more instruments and less singing. Surprisingly,
#the most popular song has a high level of acousticeness at 85

#Plotting speechiness predictor 
xlimit9 <- range(cleandata$spch)
plot(x = NULL, y = NULL, xlim = xlimit9, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$spch, cleandata$pop, pch = 21, col = colours[10], cex = 1.2)
axis(1, at = seq(0, 50, 5), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(0, 50, 5))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Speechiness", side = 1, cex = 1, line = 2.5)
mtext("Speechiness on Popularity", side = 3, line = 2, cex = 1)
lm(pop ~ spch, data = cleandata)
abline(67.44513, -0.09722 ,col = "black")
cor(cleandata$spch, cleandata$pop)
#r = -0.0509
mtext("r = -0.0509", side = 1, line = -1, cex = 0.8, adj = 0.1, padj = -0.4)
#Most songs are clustered around the 0-15 mark. Speechiness measures the amount of
#spoken words in a song. As most songs have singing and not speaking, there will not
#be many spoken words. In songs like "Sexy and I know it" they are talking not
#singing, hence the high speechiness score. The songs with highest popularity do
#not have a lot of talking, as singing is preferred. As speechiness goes up, popularity
#goes down.
dev.off()

#Creating popularity by year chart to demonstrate how 2019 has highest popularity 
cairo_pdf("PopularitybyYear.pdf", width = 8, height = 7)
par(mfrow = c(1,1), mai = c(1.7,1,0.5,0.5))
xlim <- range(cleandata$year)
plot(x = NULL, y = NULL, xlim = xlim, ylim = ylimit, ann = FALSE, axes = FALSE)
points(cleandata$year, cleandata$pop, pch = 21, col = colours[10], cex = 1.2)
axis(1, at = seq(2010, 2019, 1), tck = -.02)
axis(2, at = seq(0,100,20), tck = -.02, las = 1)
axis(3, tck = -.01, labels = FALSE, at = seq(2010, 2019, 1))
axis(4, tck = -.01, labels = FALSE)
mtext("Popularity", side = 2, las = 0, cex = 1, line = 2.5)
mtext("Year", side = 1, cex = 1, line = 2.5)
mtext("Year on Popularity", side = 3, line = 1, cex = 1)
dev.off()
#More recent = more popular, popularity is calculated by how many plays 
# a track has had and how recent those plays are (Spotify website explained) 

#Setting sizes of train, valid, and test sets 
IDX <-sample(1:dim(cleandata)[1])
trainSize <- round(.5 * dim(cleandata)[1])
validSize <- round(.25 * dim(cleandata)[1])
testSize <- round(.25 * dim(cleandata)[1])

#Splitting up train, valid, and test datasets 
trainData <- cleandata[IDX[1:trainSize],]
#train will go from 1 to the length of trainSize which is 50% so that is 
#301 out of 602
validData <- cleandata[IDX[(trainSize+1):(trainSize+validSize)],]
#validation set will start from the end of training set and go to the 
#length of the full validation size so 25% which is 150 out of 602
testData <- cleandata[IDX[(validSize+1):(validSize+testSize)],]
#test set will start from where validation set ends and go to the end
#it will also contain 150 columns 

#Creating model 1 using training dataset
model1 <- glm(pop ~ bpm + spch + acous + dnce + val + nrgy + year,
              data = trainData)
#Predicting model 1 on validation dataset
Y1 <- predict(model1, validData)

#Creating model 2 using training dataset
model2 <- glm(pop ~ bpm + dnce + nrgy + val + year,
              data = trainData)
#Predicting model 2 on validation dataset
Y2 <- predict(model2, validData)

#Creating model 3 using training dataset
model3 <- glm(pop ~ spch + acous + val,
              data = trainData)
#Predicting model 3 on validation dataset
Y3 <- predict(model3, validData)

#Creating model 4 using training dataset 
model4 <- glm(pop ~ dnce + val + year,
              data = trainData)
#Predicting model 4 on validation dataset
Y4 <- predict(model4, validData)

#AIC function 
calculateAIC <- function(nParams, nObservs, Ypred, Y){
  ssE <- sum((Ypred - Y)**2)
  sigma = ssE / nObservs
  AIC = nObservs * log(sigma) + 2*nParams
  return(AIC)
}

#Calculating AIC scores of all 4 models to see which is the best fit 
AIC <- NULL
AIC[1] <- calculateAIC(length(model1$coefficients),
                       dim(validData)[1], Y1, validData$pop)
#Model 1 AIC 746.29
AIC[2] <- calculateAIC(length(model2$coefficients),
                       dim(validData)[1], Y2, validData$pop)
#Model 2 AIC 740.84
AIC[3] <- calculateAIC(length(model3$coefficients),
                       dim(validData)[1], Y3, validData$pop)
#Model 3 AIC 758.58
AIC[4] <- calculateAIC(length(model4$coefficients),
                       dim(validData)[1], Y4, validData$pop)
#Model 4 AIC 737.72
#Will show AIC scores 1 through 4 side by side 
AIC[1:4]

#Plotting model 1
cairo_pdf("MusicModels.pdf", width = 8.5, height = 5)
par(mfcol = c(1,4))
xlimit10 <- range(pretty(Y1))
plot(x = NULL, y = NULL, xlim = xlimit10, ylim = ylimit,
     axes = FALSE, ann = FALSE)
axis(1, tck = 0.02, lwd = 2, at = seq(50,100,5))
axis(2, tck = 0.02, lwd = 2, at = seq(0,100,10), las = 1)
axis(3, tck = 0.02, lwd = 2, labels = FALSE)
axis(4, tck = 0.02, lwd = 2, labels = FALSE)
points(Y1, validData$pop, pch = 21, col = colours[1], bg = colours[1])
mtext("Model 1", 3, cex = 2, line = 1)
mtext("Popularity \n Predicted", 1, cex = .8, line = 3.5)
mtext("Popularity \n Real", 2, cex = .8, line = 1.8, las = 0)
mtext("AIC = 746.29", 1, line = -2, cex = 0.7, padj = -1)
abline(lm(ylimit ~ xlimit10, data = trainData), col = colours[1])

#Plotting model 2
xlimit11 <- range(pretty(Y2))
plot(x = NULL, y = NULL, xlim = xlimit11, ylim = ylimit,
     axes = FALSE, ann = FALSE)
axis(1, tck = 0.02, lwd = 2, at = seq(50,100,5))
axis(2, tck = 0.02, lwd = 2, at = seq(0,100,10), las = 1)
axis(3, tck = 0.02, lwd = 2, labels = FALSE)
axis(4, tck = 0.02, lwd = 2, labels = FALSE)
points(Y2, validData$pop, pch = 21, col = colours[3], bg = colours[3])
mtext("Model 2", 3, cex = 2, line = 1)
mtext("Popularity \n Predicted", 1, cex = .8, line = 3.5)
#mtext("Popularity \n Real", 2, cex = .8, line = 1.8, las = 0)
mtext("AIC = 740.84", 1, line = -2, cex = 0.7, padj = -1)
abline(lm(ylimit ~ xlimit11, data = trainData), col = colours[3])

#Plotting model 3
xlimit12 <- range(pretty(Y3))
plot(x = NULL, y = NULL, xlim = xlimit12, ylim = ylimit,
     axes = FALSE, ann = FALSE)
axis(1, tck = 0.02, lwd = 2, at = seq(60,70,1))
axis(2, tck = 0.02, lwd = 2, at = seq(0,100,10), las = 1)
axis(3, tck = 0.02, lwd = 2, labels = FALSE, at = seq(60,70,1))
axis(4, tck = 0.02, lwd = 2, labels = FALSE)
points(Y3, validData$pop, pch = 21, col = colours[5], bg = colours[5])
mtext("Model 3", 3, cex = 2, line = 1)
mtext("Popularity \n Predicted", 1, cex = .8, line = 3.5)
#mtext("Popularity \n Real", 2, cex = .8, line = 1.8, las = 0)
mtext("AIC = 758.58", 1, line = -2, cex = 0.7, padj = -1)
abline(lm(ylimit ~ xlimit12, data = trainData), col = colours[5])

#Plotting model 4 
xlimit13 <- range(pretty(Y4))
plot(x = NULL, y = NULL, xlim = xlimit13, ylim = ylimit,
     axes = FALSE, ann = FALSE)
axis(1, tck = 0.02, lwd = 2, at = seq(50,100,5))
axis(2, tck = 0.02, lwd = 2, at = seq(0,100,10), las = 1)
axis(3, tck = 0.02, lwd = 2, labels = FALSE)
axis(4, tck = 0.02, lwd = 2, labels = FALSE)
points(Y4, validData$pop, pch = 21, col = colours[9], bg = colours[9])
mtext("Model 4", 3, cex = 2, line = 1)
mtext("Popularity \n Predicted", 1, cex = .8, line = 3.5)
#mtext("Popularity \n Real", 2, cex = .8, line = 1.8, las = 0)
mtext("AIC = 737.72", 1, line = -2, cex = 0.7, padj = -1)
abline(lm(ylimit ~ xlimit13, data = trainData), col = colours[9])

dev.off()

#Applying best model to test dataset 
P4 <- predict(model4, testData)
AIC[5] <- calculateAIC(length(model4$coefficients),
                       dim(testData)[1], P4, testData$pop)
#AIC with testdata is 791.66
AIC[5]-AIC[4]
#AIC in test data increased by 53.94

#Will plot best fitting model : model 4 
cairo_pdf("Bestmodel.pdf", width = 8, height = 8.5)
par(mfrow = c(1,1), mai = c(1.8,1,0.7,0.5))
xlimit13 <- range(pretty(Y4))
plot(x = NULL, y = NULL, xlim = xlimit13, ylim = ylimit,
     axes = FALSE, ann = FALSE)
axis(1, tck = 0.02, lwd = 2, at = seq(50,100,5))
axis(2, tck = 0.02, lwd = 2, at = seq(0,100,10), las = 1)
axis(3, tck = 0.02, lwd = 2, labels = FALSE)
axis(4, tck = 0.02, lwd = 2, labels = FALSE)
points(Y4, validData$pop, pch = 21, col = colours[9], bg = colours[9])
mtext("Best Fitting Model: Model 4", 3, cex = 2, line = 1)
mtext("Popularity \n Predicted", 1, cex = 1, line = 3.5)
mtext("Popularity \n Real", 2, cex = 1, line = 2, las = 0)
mtext("AIC = 737.72", 1, line = -2, cex = 1, padj = -1)
abline(lm(ylimit ~ xlimit13, data = trainData), col = colours[9])
dev.off()
#Model 4 contains danceability + valence + year 

