### 2. Landmark Analysis and Visualization ###
## Transforms the data points so that the shapes of leaves based on chosen landmarks can be compared across the data set. ##

# Install and load packages. #
install.packages("shapes")
install.packages("ggplot2") 
library(shapes)
library(spdep)
library(ade4)
library(ggplot2)

# Specify number of total landmarks (k), landmark dimensions (m; in this case, this means that there is both an x and y coordinate for each landmark), and number of samples in the data set (n). #
k <- 6
m <- 2
n <- 100

# Read in a spreadsheet that is comprised of only the x,y coordinates of the landmarks in the order of the data set. This should not include and headers or sample labels. #
data <- read.in("only_coords.txt",k,m)

# Run a GPA on data set. #
GPA <- procGPA(data, reflect=TRUE)

# Run a PCA on Procrustes data and eigenleaves. #
shapepca(GPA)

# Specify desired PCs for visualization. In this case, the code will display PCs 1-4. #
shapepca(GPA, pcno=c(1,2,3,4), joinline=c(1,3,2,4,1), mag=0.5, color=2)

# Output data to a .txt file in the working directory. #
# PC scores per sample and PC. #
t <- as.matrix(GPA$stdscores)
write.table(t, file="PC_scores.txt")

# Percent variance explained by each PC. #
p <- as.matrix(GPA$percent)
write.table(p, file="PC_percents.txt")

# Procrustes-adjusted coordinates for each sample. #
c <- as.matrix(GPA$rotated)
write.table(c, file="Procrustes_coord.txt")

## Produces .png files with plots containing all landmarks connected to create the shape of the leaf. Saves them in the working directory. ##

# Read in reformatted landmark coordinate data set table that was created in step 1. #
data <- read.table("reformatted_landmark_coords.txt", header=TRUE)

# Find the length of the data set, which is also the total number of leaves being analyzed. #
len <- length(data[,1])

# Run a loop to plot the coordinates and connect the correct landmarks to one another. Change the geom_segment arguments as needed so that the correct landmarks are connected to one another by lines on the plot. #
# Midpoints can also be added to the plot as well. #
# Change the ggsave argument in order to automatically save the plot as a file type and "label" of your choice into the working directory. #
for(i in 1:len) { 
  
  print(i)
  
  graph <- data[i,c(2,3:14)]
  label <- as.character(graph[1,1])
  
  midpoint_colour <- "mediumpurple3"
  margin_colour <- "forestgreen"
  
  p <- ggplot(graph, aes(x=x1, y=y1, xend=x4, yend=y4))
  
  plot <- p + geom_segment(colour=margin_colour) + geom_segment(aes(x=x1, y=y1, xend=x4, yend=y4), colour=margin_colour) + geom_segment(aes(x=x4, y=y4, xend=x2, yend=y2), colour=margin_colour) + geom_segment(aes(x=x2, y=y2, xend=x3, yend=y3), colour=margin_colour) + geom_segment(aes(x=x3, y=y3, xend=x1, yend=y1), colour=margin_colour) + geom_point(aes(x=x5, y=y5), colour=midpoint_colour) + geom_point(aes(x=x6, y=y6), colour=midpoint_colour) + geom_text(aes(x=(min(x1,x2,x3,x4,x5,x6)+max(x1,x2,x3,x4,x5,x6))/2, y=min(x1,x2,x3,x4,x5,x6)+10,label=label), size=5, colour="black") + theme_bw() + coord_fixed(ratio=1)
  
  # After the loop above creates a plot of each leaf in the data file, each leaf will be printed as a separate file in the current working directory using the command below.
  ggsave(plot,filename=paste(label,".png"))
  
}

## Visualize the PCs in a scatterplot. ##
# Read in data. It is easiest to create a master spreadsheet that includes demographic information as well as landmark coordinates and PC scores for each sample. #
data <- read.table("master_data.txt", header=TRUE)

# For this study, PC plots were color-coded based on three different identifiers: subspecies, population, and island. Change the "colour" sections accordingly.
# Subspecies #
p <- ggplot(data, aes(LMK_PC1, LMK_PC2, colour=subspecies))
plota <- p + geom_point(size=2.5, alpha=0.75) +
  # The scales can be chosen manually as below, or automatically designate if the two lines below are removed. #
  scale_x_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_y_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_colour_manual(values=c("purple4","green4","blue")) + 
  theme_bw() + 
  labs(y ="LMK_PC2", x = "LMK_PC1", colour = "subspecies")

p <- ggplot(data, aes(LMK_PC3, LMK_PC4, colour=subspecies))
plotb <- p + geom_point(size=2.5, alpha=0.75) + 
  scale_x_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_y_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_colour_manual(values=c("purple4","green4","blue")) + 
  theme_bw() + 
  labs(y ="LMK_PC4", x = "LMK_PC3", colour = "subpecies")

# Arrange the two plots created above into a single figure. #
grid.arrange(plota, plotb)


# Population #
p <- ggplot(data, aes(LMK_PC1, LMK_PC2, colour=population))
plota <- p + geom_point(size=2.5, alpha=0.75) + 
  scale_x_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_y_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  theme_bw() + 
  labs(y ="LMK_PC2", x = "LMK_PC1", colour = "population")

p <- ggplot(data, aes(LMK_PC3, LMK_PC4, colour=population))
plotb <- p + geom_point(size=2.5, alpha=0.75) + 
  scale_x_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_y_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  theme_bw() + 
  labs(y ="LMK_PC4", x = "LMK_PC3", colour = "population")

grid.arrange(plota, plotb)


# Island #
p <- ggplot(data, aes(LMK_PC1, LMK_PC2, colour=island))
plota <- p + geom_point(size=2.5, alpha=0.75) + 
  scale_x_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_y_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_colour_manual(values=c("purple4","green4","blue","red","magenta")) + 
  theme_bw() + 
  labs(y ="LMK_PC2", x = "LMK_PC1", colour = "island")

p <- ggplot(data, aes(LMK_PC3, LMK_PC4, colour=island))
plotb <- p + geom_point(size=2.5, alpha=0.75) + 
  scale_x_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_y_continuous(breaks=seq(-4, 4, by = 2), limits = c(-4,4)) + 
  scale_colour_manual(values=c("purple4","green4","blue","red","magenta")) + 
  theme_bw() +
  labs(y ="LMK_PC4", x = "LMK_PC3", colour = "island")

grid.arrange(plota, plotb)