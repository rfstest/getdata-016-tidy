# directory with the data
DATA.DIR <- 'UCI HAR Dataset'

read.data <- function (set) {
    x.file <- file.path(DATA.DIR, set, paste('X_', set, '.txt', sep=''))
    y.file <- file.path(DATA.DIR, set, paste('y_', set, '.txt', sep=''))
    sub.file <- file.path(DATA.DIR, set, paste('subject_', set, '.txt', sep=''))

    # read data from files
    x.data <- read.table(x.file, col.names=features)
    y.data <- read.table(y.file)
    sub.data <- read.table(sub.file)

    # filter only mean|std features
    x.data  <-  x.data[, mean.std.features.filter]

    # get activity names
    y.data[,2] <- activities[y.data[,1]]
    names(y.data) <- c('ActivityID', 'Activity')
    names(sub.data) <- c('Subject')

    cbind(sub.data, y.data, x.data)
}

# read the features and activity labels
features <- read.table(file.path(DATA.DIR, 'features.txt'))[,2]
activities <- read.table(file.path(DATA.DIR, 'activity_labels.txt'))[,2]

# create a filter for finding only mean and std features
mean.std.features.filter = grepl('mean|std', features)

# read the TRAIN set
train.data <- read.data('train')
test.data <- read.data('test')

# merge the TRAIN and TEST sets
data = rbind(train.data, test.data)

# use reshape2 for melting
library(reshape2)

id.labels = c("Subject", "ActivityID", "Activity")
measure.labels = setdiff(names(data), id.labels)
melted.data = melt(data, id=id.labels, measure.vars=measure.labels)

# find the average for the melted data
tidy.data = dcast(melted.data, Subject + Activity ~ variable, mean)

# write a file with tidy data
write.table(tidy.data, file="tidy_data.txt")

