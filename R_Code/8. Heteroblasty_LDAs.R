### Step 8. Linear Discriminant Analyses of Landmarks, EFDs, and Shape Descriptors by Heteroblasty ###
## Creates confusion matrices for actual vs predicted week collected using landmark coordinate, EFD (harmonic), and shape descriptor data. ##

# Install and load packages. #
install.packages("MASS")
install.packages("reshape2")
install.packages("ggplot2")

library(MASS)
library(reshape2)
library(ggplot2)

# Read in data and subset by subspecies identity. #
data <- read.table("master_data.txt", header=TRUE)
sub_data <- data[ which(data$subspecies=='Chenopodium_oahuense_ilioense'),]
sub_data2 <- data[ which(data$subspecies=='Chenopodium_oahuense_oahuense'),]
sub_data3 <- data[ which(data$subspecies=='ILS_Hybrid'),]

# Remove NAs (Be careful, substituting the data frame with itself!). #
sub_data <- subset(sub_data, Week != "NA")
sub_data2 <- subset(sub_data2, Week != "NA")
sub_data3 <- subset(sub_data3, Week != "NA")

## Landmark LDAs separated by Subspecies Identity and performed by Week Collected. ##
## Chenopodium oahuense ssp. ilioense ##

lda_week <- lda (Week~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6, data=sub_data, CV=TRUE)

# Create a data frame with actual and predicted week. #
actual_week <- as.data.frame(sub_data$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

# Create a table with actual vs. predicted week. #
t_week_predict <- table(week_predict)

# Transform the table into proportions and use the melt function to reformat into a plottable format. #
p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

# Plot out a confusion matrix. #
p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

# Write confusion matrix tables with values. #
write.table(m_week_predict, file = "ilioense_LMK_predicted_vs_actual_week")


## Repeat for other subspecies identities. #
## Chenopodium oahuense ssp. oahuense ##

lda_week <- lda (Week~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6, data=sub_data2, CV=TRUE)

actual_week <- as.data.frame(sub_data2$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "oahuense_LMK_predicted_vs_actual_week")

## ILS/Hybrid ##

lda_week <- lda (Week~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6, data=sub_data3, CV=TRUE)

actual_week <- as.data.frame(sub_data3$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "ILS_Hybrid_LMK_predicted_vs_actual_week")

## EFDs LDAs separated by Subspecies Identity and performed by Week Collected. ##
## Chenopodium oahuense ssp. ilioense ##

lda_week <- lda (Week~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=sub_data, CV=TRUE)

# Create a data frame with actual and predicted week. #
actual_week <- as.data.frame(sub_data$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

# Create a table with actual vs. predicted week. #
t_week_predict <- table(week_predict)

# Transform the table into proportions and use the melt function to reformat into a plottable format. #
p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

# Plot out a confusion matrix. #
p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

# Write confusion matrix tables with values. #
write.table(m_week_predict, file = "ilioense_EFD_predicted_vs_actual_week")


## Repeat for other subspecies identities. #
## Chenopodium oahuense ssp. oahuense ##

lda_week <- lda (Week~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=sub_data2, CV=TRUE)

actual_week <- as.data.frame(sub_data2$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "oahuense_EFD_predicted_vs_actual_week")

## ILS/Hybrid ##

lda_week <- lda (Week~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=sub_data3, CV=TRUE)

actual_week <- as.data.frame(sub_data3$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "ILS_Hybrid_EFD_predicted_vs_actual_week")

## Shape Descriptor LDAs separated by Subspecies Identity and performed by Week Collected. ##
## Chenopodium oahuense ssp. ilioense ##

lda_week <- lda (Week~AR+Circ+Area_mm2+Thickness+Petiole_Length_mm, data=sub_data, CV=TRUE)

# Create a data frame with actual and predicted week. #
actual_week <- as.data.frame(sub_data$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

# Create a table with actual vs. predicted week. #
t_week_predict <- table(week_predict)

# Transform the table into proportions and use the melt function to reformat into a plottable format. #
p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

# Plot out a confusion matrix. #
p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

# Write confusion matrix tables with values. #
write.table(m_week_predict, file = "ilioense_SD_predicted_vs_actual_week")


## Repeat for other subspecies identities. #
## Chenopodium oahuense ssp. oahuense ##

lda_week <- lda (Week~AR+Circ+Area_mm2+Thickness+Petiole_Length_mm, data=sub_data2, CV=TRUE)

actual_week <- as.data.frame(sub_data2$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "oahuense_SD_predicted_vs_actual_week")

## ILS/Hybrid ##

lda_week <- lda (Week~AR+Circ+Area_mm2+Thickness+Petiole_Length_mm, data=sub_data3, CV=TRUE)

actual_week <- as.data.frame(sub_data3$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "ILS_Hybrid_SD_predicted_vs_actual_week")

## LDAs using all analyses methods separated by Subspecies Identity and performed by Week Collected. ##
## Chenopodium oahuense ssp. ilioense ##

lda_week <- lda (Week~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+Area_mm2+Thickness+Petiole_Length_mm+AR+Circ, data=sub_data, CV=TRUE)

# Create a data frame with actual and predicted week. #
actual_week <- as.data.frame(sub_data$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

# Create a table with actual vs. predicted week. #
t_week_predict <- table(week_predict)

# Transform the table into proportions and use the melt function to reformat into a plottable format. #
p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

# Plot out a confusion matrix. #
p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

# Write confusion matrix tables with values. #
write.table(m_week_predict, file = "ilioense_All_predicted_vs_actual_week")


## Repeat for other subspecies identities. #
## Chenopodium oahuense ssp. oahuense ##

lda_week <- lda (Week~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+Area_mm2+Thickness+Petiole_Length_mm+AR+Circ, data=sub_data2, CV=TRUE)

actual_week <- as.data.frame(sub_data2$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "oahuense_All_predicted_vs_actual_week")

## ILS/Hybrid ##

lda_week <- lda (Week~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+Area_mm2+Thickness+Petiole_Length_mm+AR+Circ, data=sub_data3, CV=TRUE)

actual_week <- as.data.frame(sub_data3$Week)
predicted_week <- as.data.frame(lda_week$class)
week_predict <- cbind(actual_week, predicted_week)
colnames(week_predict) <- c("actual_week", "predicted_week")

t_week_predict <- table(week_predict)

p_week_predict <- prop.table(t_week_predict, 1)
m_week_predict <- melt(p_week_predict, id="actual_week")

m_week_predict$actual_week <- factor(m_week_predict$actual_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

m_week_predict$predicted_week <- factor(m_week_predict$predicted_week, levels=c("10","12","14","16","18","20","22","24","26","28"))

p <- ggplot(m_week_predict, aes(predicted_week, actual_week, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_week_predict, file = "ILS_Hybrid_All_predicted_vs_actual_week")