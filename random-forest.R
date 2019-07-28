library(randomForest)

require(caTools)

data <- read.csv(
  # 'processed.switzerland.data',
  "processed.cleveland.data",
  header=FALSE
)

names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thai", "num")

dim(data)

head(data)

sapply(data, class)

data$num[data$num > 1] <- 1

summary(data)

# data$num



# data <- subset(data, select = -c("fbs", "ca"))


data

data[ data == "?"] <- NA

head(data)

head(is.na(data))

colSums(is.na(data))

data$fbs <- NULL
data$ca <- NULL
data$thai <- NULL

data <- data[!(data$thalach %in% c(NA)),]

colSums(is.na(data))

data

data <- transform(
  data,
  trestbps=as.integer(trestbps),
  oldpeak=as.numeric(oldpeak),
  num=as.factor(num),
  thalach=as.integer(thalach),
  sex=as.factor(sex),
  cp=as.factor(cp)
)

data$restecg <- factor(data$restecg)
data$exang <- factor(data$exang)
data$slope <- factor(data$slope)



summary(data)

data$num <- as.factor(data$num)

data$slope[which(is.na(data$slope))] <- 2

data

sapply(data, class)

colSums(is.na(data))

data$trestbps[which(is.na(data$trestbps))] <- mean(data$trestbps, na.rm = TRUE)
data$oldpeak[which(is.na(data$oldpeak))] <- mean(data$oldpeak, na.rm = TRUE)

colSums(is.na(data))

sample = sample.split(data$num, SplitRatio = .75)

train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)

dim(train)
dim(test)

sapply(data, class)

data

summary(data)

summary(train)

rf <- randomForest(
  num ~ .,
  data=train
  
  # x=train[-11],
  # y=train$num
)

pred = predict(rf, newdata=test[-11])

cm = table(test[,11], pred)

cm
# test[which(is.na(test))]


