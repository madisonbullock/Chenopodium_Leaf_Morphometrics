### 1. Reformatting Landmark Spreadsheet ###
## This step takes the landmark coordinates output from ImageJ and converts them into a format that is necessary for future steps. ##

data <- read.table('landmark_coords.txt', header=TRUE)

len <- length(data$x)

# This example uses 4 landmarks. Adapt the code as needed to the number of landmarks chosen. #

overall.table <- matrix(nrow=len/4, ncol=(4*2)+1)

overall.length <- len/4

for(j in c(1:overall.length)) {
  
  print(j)
  
  sub.data <- as.matrix(data[ (1+4*(j-1)):((1+4*(j-1))+3), 2:4])
  
  overall.table[j,1] <- sub.data[1, 1]
  overall.table[j,2:3] <- sub.data[1, 2:3]
  overall.table[j,4:5] <- sub.data[2, 2:3]
  overall.table[j,6:7] <- sub.data[3, 2:3]
  overall.table[j,8:9] <- sub.data[4, 2:3]
  overall.table[j,8:9] <- sub.data[4, 2:3]
  overall.table[j,8:9] <- sub.data[4, 2:3]

}

# Confirm the above loop worked. #

head(overall.table)
tail(overall.table)

# Assign the new table column names and write out the reformatted table into the working directory. #

colnames(overall.table) <- c("label", "x1", "y1", "x2", "y2", "x3", "y3", "x4", "y4")

write.table(overall.table, "reformatted_landmark_coords.txt")