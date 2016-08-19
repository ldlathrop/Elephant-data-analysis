library(ggfortify)
library(timeSeries)
library(broom)
library(ggvis)
library(ggmap)
library(choroplethr)



letterFreqs = c(8.167, 1.492, 2.782, 4.253, 12.702, 2.228,
                2.015, 6.094, 6.966, 0.153, 0.772, 4.025, 2.406, 6.749, 7.507, 1.929,
                0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074)
letterFreqs = letterFreqs/100
letterFrame = data.frame(letter = letters, freq=letterFreqs)

# now let's generate letters according to their letter frequencies
N = 1000
randomDraws = data.frame(draw=1:N, letter=sample(letterFrame$letter, size=N, replace=TRUE, prob=letterFrame$freq))

WVPlots::ClevelandDotPlot(randomDraws, "letter", title = "Example Cleveland-style dot plot")
WVPlots::ClevelandDotPlot(randomDraws, "letter", limit_n = 10,  title = "Top 10 most frequent letters")
WVPlots::ClevelandDotPlot(randomDraws, "letter", sort=0, title="Example Cleveland-style dot plot, unsorted")
WVPlots::ClevelandDotPlot(randomDraws, "letter", sort=1, stem=TRUE, title="Example with increasing sort order + coord_flip, no stem") + ggplot2::coord_flip()

### Lattice Cleveland Dot Plot
png("actProjected.png", height=800, width = 494)
n <- dotplot(Definite ~ Year | RegionID, 
             data = elephantDataSubset,
        layout = c(1, 5), aspect = 0.7, 
        origin = 0, type = c("p", "h"),
        main = "Actual elephant counts vs. Projected", 
        xlab = "Year")
n
dev.off()

# Testing a theory
x <- seq(0,20,0.001)
plot(x,tanh(x),pch=".", col="red", ylab="y")
points(x,(1 / (1 + exp(-x)))*2-1, pch=".",col="blue")
