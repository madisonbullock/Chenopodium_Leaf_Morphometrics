### Step 5. Shape Descriptor Comparison and Visualization ###
## Pairs two measured shape descriptors against one another for all samples in the data set. Visualizes them on a scatterplot. Outputs scatterplot as a file into the working directory. #

# Install and load packages. #
install.packages("ggplot2")
install.packages("shapes")
install.packages("spdep")
install.packages("ade4")

library(ggplot2)
library(shapes)
library(spdep)
library(ade4)

# Read in data. #
data <- read.table("master_data.txt", header=TRUE)

# Subspecies. #
# Plots Area against Thickness with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(Area_mm2, Thickness, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +
  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("AvT_subs.png")

# Plots Thicness against Petiole Length with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(Thickness, Petiole_Length_mm, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("TvPL_subs.png")

# Plots Area against Petiole Length with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(Area_mm2, Petiole_Length_mm, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("AvPL_subs.png")

# Plots Area against Aspect Ratio with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(Area_mm2, AR, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5))
ggsave("AvAR_subs.png")

# Plots Aspect Ratio against Petiole Length with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(AR, Petiole_Length_mm, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("ARvPL_subs.png")

# Plots Aspect Ratio against Thickness with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(AR, Thickness, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("ARvT_subs.png")

# Plots Circularity against Thickness with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(Circ, Thickness, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("CvT_subs.png")

# Plots Circularity against Petiole Length with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(Circ, Petiole_Length_mm, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("CvPL_subs.png")

# Plots Aspect Ratio against Circularity with data separated by subspecies class and outputs as file to working directory. #
p <- ggplot(data, aes(AR, Circ, colour=subspecies))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue")) + 
  labs(colour = "subspecies") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1))
ggsave("ARvC_subs.png")

# Repeat with all other demographic categories. #
# Population. #
p <- ggplot(data, aes(Area_mm2, Thickness, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("AvT_pop.png")

p <- ggplot(data, aes(Thickness, Petiole_Length_mm, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("TvPL_pop.png")

p <- ggplot(data, aes(Area_mm2, Petiole_Length_mm, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("AvPL_pop.png")

p <- ggplot(data, aes(Area_mm2, AR, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5))
ggsave("AvAR_pop.png")

p <- ggplot(data, aes(AR, Petiole_Length_mm, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("ARvPL_pop.png")

p <- ggplot(data, aes(AR, Thickness, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("ARvT_pop.png")

p <- ggplot(data, aes(Circ, Thickness, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("CvT_pop.png")

p <- ggplot(data, aes(Circ, Petiole_Length_mm, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("CvPL_pop.png")

p <- ggplot(data, aes(AR, Circ, colour=population))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","cyan4","red","blue","chocolate3","green4","magenta")) + 
  labs(colour = "population") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1))
ggsave("ARvC_loc.png")

# Island. #
p <- ggplot(data, aes(Area_mm2, Thickness, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("AvT_isl.png")

p <- ggplot(data, aes(Thickness, Petiole_Length_mm, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") +  scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("TvPL_isl.png")

p <- ggplot(data, aes(Area_mm2, Petiole_Length_mm, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("AvPL_isl.png")

p <- ggplot(data, aes(Area_mm2, AR, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 3000, by = 500), limits = c(0, 3000)) + 
  scale_y_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5))
ggsave("AvAR_isl.png")

p <- ggplot(data, aes(AR, Petiole_Length_mm, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("ARvPL_isl.png")

p <- ggplot(data, aes(AR, Thickness, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("ARvT_isl.png")

p <- ggplot(data, aes(Circ, Thickness, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0, 2, by = 0.5), limits = c(0, 2))
ggsave("CvT_isl.png")

p <- ggplot(data, aes(Circ, Petiole_Length_mm, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0, 75, by = 10), limits = c(0, 75))
ggsave("CvPL_isl.png")

p <- ggplot(data, aes(AR, Circ, colour=island))
p + geom_point(size=3, alpha=0.6) + 
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none") + scale_colour_manual(values=c("purple4","green4","blue","red")) + 
  labs(colour = "island") +
  scale_x_continuous(breaks=seq(0, 4.5, by = 0.5), limits = c(0, 4.5)) + 
  scale_y_continuous(breaks=seq(0, 1, by = 0.1), limits = c(0, 1))
ggsave("ARvC_isl.png")
