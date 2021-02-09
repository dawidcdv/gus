require(ggplot2)
require(reshape2)

chleb <-
  read.csv(file = "", sep = ";", header =
             TRUE)

title <- ""
ylabel <- ""

d1 <- as.Date(paste0("200601", "01"), "%Y%m%d")
d2 <- as.Date(paste0("201912", "01"), "%Y%m%d")
dat <- format(seq(d1, d2, by = "month"), "%Y-%m-01")

skipRow <- 1
skipCol <- 2
rangeOfYears <- 14
range <- rangeOfYears * 12 + skipCol

states <- # name => index row
  c(
    Dolnoslaskie = 1,
    KujawskoPomorskie = 2,
    Lubelskie = 3,
    Lubuskie = 4,
    Lodzkie = 5,
    Malopolskie = 6,
    Mazowieckie = 7,
    Opolskie = 8,
    Podkarpackie = 9,
    Podlaskie = 10,
    Pomorskie = 11,
    Slaskie = 12,
    Swietokrzyskie = 13,
    Warminskomazurskie = 14,
    Wielkopolskie = 15,
    Zachodnopomorskie = 16
  )


totalData <- data.frame(time = as.Date(dat))
for (state in states) { 
  resultState <- list()
  index <- 1
  for (year in 1:rangeOfYears) {
    for (monthIndex in seq(skipCol + year, range, rangeOfYears)) {
      resultState[index] <- chleb[state + skipRow, monthIndex]
      index <- index + 1
    }
  }
  col <- data.frame(col = as.numeric(resultState))
  colnames(col) <- c(names(states)[state])
  totalData <- cbind(totalData, col)
}

mdf <- reshape2::melt(totalData, id.var = "time")
mdf <- na.omit(mdf)

ggplot() +  geom_line(data = mdf, aes(x = time, y = value, colour =  variable)) +
  ggtitle(title) +
  xlab("Czas") + ylab(ylabel)  + theme(
    plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
    axis.title.y = element_text(color = "#993333", size = 14, face = "bold")
  )
print("Range")
print(range(mdf$value))

print("Deviation")
print(sd(mdf$value))

print("Variance")
print(var(mdf$value))

print("Mediana")
print(median(mdf$value))

print("Mean")
print(mean(mdf$value))

print(aggregate(mdf$value~variable,mdf,mean))
