fb2016 <- read.xlsx("fbPosts2016.xlsx", sheetName = "Sheet1")
names(fb2016)
cols <- magenta2green(11)
fb2016$Category <- as.factor(fb2016$Category)

quartz()
png("fb2016.png", width = 480, height = 480)
fb <- ggplot(fb2016, aes(x=Category, y=Posts, group=Category, fill=as.factor(Category))) +
  geom_bar(position="dodge", stat="identity")
fb <- fb + scale_fill_manual(values = cols)
fb <- fb + theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1, size = 6),
        axis.title.x = element_text(""), 
        axis.title.y=element_text(""),
        title = element_text(""),
        legend.title= element_text(""))
print(fb)
dev.off()

histogram(fb2016$Posts, breaks=35)