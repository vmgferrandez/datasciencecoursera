library (dplyr)

read_obs <- function(ob_type="train") {
    # 
    # read  data, the features as names and the activity of the data set and build a dataframe
    #
    dir_name <- "./UCI HAR Dataset/"
    fname <- paste (dir_name,ob_type,"/X_",ob_type, ".txt", sep = "" )
    d_obs_raw <- read.table (fname, header = FALSE)
    d_obs_features <- read.table ("UCI HAR Dataset/features.txt", header = FALSE, stringsAsFactors = FALSE)
    names(d_obs_raw) <- d_obs_features$V2
    # eliminate columns with duplicated names (there are no mean or std there anyway)
    d_obs_ndup <- d_obs_raw[!duplicated(names(d_obs_raw))]
    # select column names for mean(), not the ones starting with angle that contains Mean, so
    # do not ignore case
    sel_col_mean <- d_obs_features[grep("mean", ignore.case = FALSE, d_obs_features$V2),"V2"]
    # and for std()
    sel_col_std <- d_obs_features[grep("std", ignore.case = FALSE, d_obs_features$V2),"V2"]
    # and keep just the columns dealing with mean or std
    d_obs_mean_std<- select(d_obs_ndup, one_of(c(sel_col_mean, sel_col_std)))
    # add data about activity labels
    act_labels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, stringsAsFactors = FALSE)
    fname <- paste (dir_name,ob_type,"/y_",ob_type, ".txt", sep = "" )
    activity <- read.table (fname, header = FALSE)
    names(activity) <-"activity"    
    activity <- apply (activity,2,function(x) act_labels[x, "V2"])

    d_obs_mean_std <- cbind(d_obs_mean_std, activity)
    d_obs_mean_std
    
}

run_analysis <- function () {
    train_raw <- read_obs("train")
    test_raw <- read_obs("test")
    train_test_data <- rbind(train_raw, test_raw)
    train_test_data
    
}