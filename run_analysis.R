t <- read.table(file="features.txt")
fnames <- as.vector(t[2]$V2)
mn <- grep("mean()", fnames, ignore.case = FALSE, perl = FALSE, value = FALSE,
     fixed = TRUE, useBytes = FALSE, invert = FALSE)
sd <- grep("std()", fnames, ignore.case = FALSE, perl = FALSE, value = FALSE,
     fixed = TRUE, useBytes = FALSE, invert = FALSE)
mnsd <- c(mn,sd)
mnsd <- sort(mnsd)
relfnames <- fnames[mnsd]

xtest <- read.table("test/X_test.txt")
relxtest <- xtest[,mnsd]
names(relxtest) <- relfnames

xtrain <- read.table("train/X_train.txt")
relxtrain <- xtrain[,mnsd]
names(relxtrain) <- relfnames

merged_data <- rbind(relxtest, relxtrain)

ytest <- read.table("test/y_test.txt")
ytrain <- read.table("train/y_train.txt")

y3 <- rbind(ytest, ytrain)
names(y3) <- "Activity_Class"

merged_data <- cbind(merged_data, y3)

subtest <- read.table("test/subject_test.txt")
subtrain <- read.table("train/subject_train.txt")

subs <- rbind(subtest, subtrain)
names(subs) <- "Subject"

merged_data <- cbind(merged_data, subs)

anames <- read.table("activity_labels.txt")
anames2 <- as.vector(anames$V2)

merged_data$Activity_Class <- sapply(merged_data$Activity_Class, function(x){anames2[x]})

subs<-1:30
mydf = data.frame()
cnt = 1
subvec = vector()
actvec = vector()
acts <- c("LAYING", "SITTING", "STANDING", "WALKING", "WALKING_DOWNSTAIRS", "WALKING_UPSTAIRS")
for( j in 1:length(acts)){
	for( i in 1:length(subs)){
		res <- colMeans(merged_data[merged_data$Activity_Class == acts[j] & merged_data$Subject == subs[i],][,1:66])
		mydf <- rbind(mydf, res)
		subvec[cnt] <- subs[i]
		actvec[cnt] <- acts[j]
		cnt <- cnt+1
	}
}
mydf <- cbind(mydf, actvec)
mydf <- cbind(mydf, subvec)
names(mydf) <- names(merged_data)
mydf
