### Step 3. Mean Leaf Shapes of Landmark Data ###
## Calculates and visualizes the mean Procrustes-adjusted leaf shape for a demographic category superimposed over all the Procrustes adjusted leaf shapes for all sample in said demographic category. ##

# Install and load packages. #
install.packages("ggplot2")
library(ggplot2)

# Read in data. #
data <- read.table("master_data.txt", header=TRUE)

# Create the plots of all leaves superimposed with the mean leaf per category. #

# Subspecies. #

# Specify the subspecies you wish to visualize. #
spe <- "Chenopodium_oahuense_oahuense"

# Subset the data to create a data set for only that subspecies. #
sub <- subset(data, subspecies==spe)

# Take the mean of the subset data. #
mean <- colMeans(sub[11:22])

 #Transpose the mean values and create a data frame. #
mean <- as.data.frame(t(mean))

# Specify the size, alpha, and color of the lines for all the leaves. #
size=1
alpha=0.2
colour="green"
colour2="purple"

# Specify the size, alpha, and color of the lines for the mean leaf. #
m_size=2
m_alpha=1
m_colour="black"

# Graph out the mean leaf shape and all the leaves. #
# For all leaves, make sure the appropriate landmarks are connected to one another via the geom_segment arguments. #
# The top part is for all leaves in the subset. #
p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
# The bottom part is for the mean leaf. #
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  
  # The Procrustes coordinates create an inverted leaf, so uninvert the leaf. #
  scale_y_reverse() + 
  
  # Fix the axes scales so the shape is not distorted. #
  coord_fixed() +
  
  # Get rid of all the other graph stuff if wanted. #
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

# Save the plot with your preferred file type label. #
ggsave(plot,filename=paste(spe,".png"))

# Repeat with all other types in category. #
spe <- "Chenopodium_oahuense_ilioense"
sub <- subset(data, subspecies==spe)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(spe,".png"))

spe <- "ILS_Hybrid"
sub <- subset(data, subspecies==spe)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(spe,".png"))

# Population. #
pop <- "Big_Island"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png")).#


pop <- "Ilio_Point"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png"))


pop <- "Maui"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png"))


pop <- "Mokio_Point"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png"))


pop <- "Maui"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png"))


pop <- "Oahu"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png"))


pop <- "Puu_Ka_Pele"
sub <- subset(data, population==pop)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(pop,".png"))


# Island. #
isl <- "Big_Island"
sub <- subset(data, island==isl)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(isl,".png"))


isl <- "Molokai"
sub <- subset(data, island==isl)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +

  scale_y_reverse() + 

  coord_fixed() +

  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(isl,".png"))


isl <- "Maui"
sub <- subset(data, island==isl)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(isl,".png"))


isl <- "Oahu"
sub <- subset(data, island==isl)
mean <- colMeans(sub[11:22])
mean <- as.data.frame(t(mean))

size=1
alpha=0.2
colour="green"
colour2="purple"

m_size=2
m_alpha=1
m_colour="black"

p <- ggplot(data=sub, aes(x=x1, y=y1, xend=x4, yend=y4))
plot <- p + geom_segment(size=size, alpha=alpha, colour=colour) + 
  geom_segment(data=sub, aes(x=x4, y=y4, xend=x2, yend=y2), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x2, y=y2, xend=x3, yend=y3), size=size, alpha=alpha, colour=colour) +
  geom_segment(data=sub, aes(x=x3, y=y3, xend=x1, yend=y1), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x5, y=y5), size=size, alpha=alpha, colour=colour) +
  geom_point(data=sub, aes(x=x6, y=y6), size=size, alpha=alpha, colour=colour) +
  
  geom_segment(data=mean, aes(x=x4, y=y4, xend=x2, yend=y2), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x2, y=y2, xend=x3, yend=y3), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x3, y=y3, xend=x1, yend=y1), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_segment(data=mean, aes(x=x1, y=y1, xend=x4, yend=y4), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x5, y=y5), size=m_size, alpha=m_alpha, colour=m_colour) +
  geom_point(data=mean, aes(x=x6, y=y6), size=m_size, alpha=m_alpha, colour=m_colour) +
  
  scale_y_reverse() + 
  
  coord_fixed() +
  
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none")

ggsave(plot,filename=paste(isl,".png"))