# Principal Component Analysis
load("~/training.Rda")
load("~/testing.Rda")
labels <- training[,1]
data <- training[,-1]

# Compute / load PC's
pc <- princomp(data)
save(pc, file = "~/Desktop/Winter_2014/STA_135/Project/PCA/pca.rda")
load("~/Desktop/Winter_2014/STA_135/Project/PCA/pca.rda")

# Create scree plot
head(cumprop, 120)
cumprop <- cumsum(pc$sd^2)/sum(pc$sd^2)
png("scree.png")
num <- c(40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260, 280, 300, 320, 340, 360, 380)
vars <- 100*round(cumprop[num], 4) # *100 to be as percentage
plot(cumprop, main = 'Scree Plot', xlab = '# Of Principal Components', ylab = '% Variability Explained')
abline(v = num)
text(num[1]-9, .35, paste(num[1], " components accounting for ", vars[1], "% of the variability explained", sep = ""), srt = 90, cex = .60)
text(num[-1]-9, .35, paste(num[-1], "             \"             ", vars[-1], "%              \"              ", sep = ""), srt = 90, cex = .60)
dev.off()

# Visual display of first 25 principal components
png("visual_eigenvectors.png")
par(mfrow = c(5,5), mar = c(.1,.1,.1,.1))
for (i in 1:25) {
	data_matrix <- matrix(pc$loadings[,i], 28, 28)
	image(data_matrix, col = gray(255:0 / 255), ylim = NULL, xlab = '', ylab = '', main = '')
}
dev.off()