### Step 6. Linear Discriminant Analyses of Landmarks, EFDs, and Shape Descriptors ###
## Creates confusion matrices for actual vs predicted class using landmark coordinate, EFD (harmonic), and shape descriptor data. ##

# Install and load packages. #
install.packages("MASS")
install.packages("reshape2")
install.packages("ggplot2")

library(MASS)
library(reshape2)
library(ggplot2)

#Read in data.#
data <- read.table("master_data.txt", header=TRUE)

## Landmark LDA performed by Subspecies. ##

# Remove NAs (Be careful, substituting the data frame with itself!). #
data <- subset(data, subspecies != "NA")

lda_subsp <- lda(subspecies~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6, data=data, CV=TRUE)

# Create a data frame with actual and predicted subspecies classes. #
actual_subsp <- as.data.frame(data$subspecies)
predicted_subsp <- as.data.frame(lda_subsp$class)
subsp_predict <- cbind(actual_subsp, predicted_subsp)
colnames(subsp_predict) <- c("actual_subsp", "predicted_subsp")

# Create a table with actual vs. predicted subspecies classes. #
t_subsp_predict <- table(subsp_predict)

# Transform the table into proportions and use the melt function to reformat into a plottable format. #
p_subsp_predict <- prop.table(t_subsp_predict, 1)
m_subsp_predict <- melt(p_subsp_predict, id="actual_subsp")

m_subsp_predict$actual_subsp <- factor(m_subsp_predict$actual_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

m_subsp_predict$predicted_subsp <- factor(m_subsp_predict$predicted_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

# Plot out a confusion matrix. #
p <- ggplot(m_subsp_predict, aes(predicted_subsp, actual_subsp, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

# Write confusion matrix tables with values. #
write.table(m_subsp_predict, file = "LMK_predicted_vs_actual_subspecies")

## Repeat for other classes. ##
## Population. ##

data <- subset(data, population != "NA")

lda_pop <- lda (population~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6, data=data, CV=TRUE)

actual_pop <- as.data.frame(data$population)
predicted_pop <- as.data.frame(lda_pop$class)
pop_predict <- cbind(actual_pop, predicted_pop)
colnames(pop_predict) <- c("actual_pop", "predicted_pop")

t_pop_predict <- table(pop_predict)

p_pop_predict <- prop.table(t_pop_predict, 1)
m_pop_predict <- melt(p_pop_predict, id="actual_pop")

m_pop_predict$actual_pop <- factor(m_pop_predict$actual_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

m_pop_predict$predicted_pop <- factor(m_pop_predict$predicted_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

p <- ggplot(m_pop_predict, aes(predicted_pop, actual_pop, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_pop_predict, file = "LMK_predicted_vs_actual_population")

## Island. ##
data <- subset(data, island != "NA")

lda_isl <- lda (island~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6, data=data, CV=TRUE)

actual_isl <- as.data.frame(data$island)
predicted_isl <- as.data.frame(lda_isl$class)
isl_predict <- cbind(actual_isl, predicted_isl)
colnames(isl_predict) <- c("actual_isl", "predicted_isl")

t_isl_predict <- table(isl_predict)

p_isl_predict <- prop.table(t_isl_predict, 1)
m_isl_predict <- melt(p_isl_predict, id="actual_isl")

m_isl_predict$actual_isl <- factor(m_isl_predict$actual_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

m_isl_predict$predicted_isl <- factor(m_isl_predict$predicted_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

p <- ggplot(m_isl_predict, aes(predicted_isl, actual_isl, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_isl_predict, file = "LMK_predicted_vs_actual_island")

## EFD LDA performed by Subspecies. Same steps as above, but the coordinate values are changed to harmonic values. ##
data <- subset(data, subspecies != "NA")

lda_subsp <- lda(subspecies~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=data, CV=TRUE)

actual_subsp <- as.data.frame(data$subspecies)
predicted_subsp <- as.data.frame(lda_subsp$class)
subsp_predict <- cbind(actual_subsp, predicted_subsp)
colnames(subsp_predict) <- c("actual_subsp", "predicted_subsp")

t_subsp_predict <- table(subsp_predict)

p_subsp_predict <- prop.table(t_subsp_predict, 1)
m_subsp_predict <- melt(p_subsp_predict, id="actual_subsp")

m_subsp_predict$actual_subsp <- factor(m_subsp_predict$actual_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

m_subsp_predict$predicted_subsp <- factor(m_subsp_predict$predicted_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

p <- ggplot(m_subsp_predict, aes(predicted_subsp, actual_subsp, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_subsp_predict, file = "EFD_predicted_vs_actual_subspecies")

## Repeat for other classes. ##
## Population. ##

data <- subset(data, population != "NA")

lda_pop <- lda (population~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=data, CV=TRUE)

actual_pop <- as.data.frame(data$population)
predicted_pop <- as.data.frame(lda_pop$class)
pop_predict <- cbind(actual_pop, predicted_pop)
colnames(pop_predict) <- c("actual_pop", "predicted_pop")

t_pop_predict <- table(pop_predict)

p_pop_predict <- prop.table(t_pop_predict, 1)
m_pop_predict <- melt(p_pop_predict, id="actual_pop")

m_pop_predict$actual_pop <- factor(m_pop_predict$actual_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

m_pop_predict$predicted_pop <- factor(m_pop_predict$predicted_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

p <- ggplot(m_pop_predict, aes(predicted_pop, actual_pop, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_pop_predict, file = "EFD_predicted_vs_actual_population")

## Island. ##
data <- subset(data, island != "NA")

lda_isl <- lda (island~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20, data=data, CV=TRUE)

actual_isl <- as.data.frame(data$island)
predicted_isl <- as.data.frame(lda_isl$class)
isl_predict <- cbind(actual_isl, predicted_isl)
colnames(isl_predict) <- c("actual_isl", "predicted_isl")

t_isl_predict <- table(isl_predict)

p_isl_predict <- prop.table(t_isl_predict, 1)
m_isl_predict <- melt(p_isl_predict, id="actual_isl")

m_isl_predict$actual_isl <- factor(m_isl_predict$actual_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

m_isl_predict$predicted_isl <- factor(m_isl_predict$predicted_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

p <- ggplot(m_isl_predict, aes(predicted_isl, actual_isl, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_isl_predict, file = "EFD_predicted_vs_actual_island")

## Shape Descriptor LDA performed by Subspecies. Same steps as above, but the coordinate values are changed to shape descriptor measurements. ##
data <- subset(data, subspecies != "NA")

lda_subsp <- lda(subspecies~AR+Circ+Area_mm2+Thickness+Petiole_Length_mm, data=data, CV=TRUE)

actual_subsp <- as.data.frame(data$subspecies)
predicted_subsp <- as.data.frame(lda_subsp$class)
subsp_predict <- cbind(actual_subsp, predicted_subsp)
colnames(subsp_predict) <- c("actual_subsp", "predicted_subsp")

t_subsp_predict <- table(subsp_predict)

p_subsp_predict <- prop.table(t_subsp_predict, 1)
m_subsp_predict <- melt(p_subsp_predict, id="actual_subsp")

m_subsp_predict$actual_subsp <- factor(m_subsp_predict$actual_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

m_subsp_predict$predicted_subsp <- factor(m_subsp_predict$predicted_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

p <- ggplot(m_subsp_predict, aes(predicted_subsp, actual_subsp, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_subsp_predict, file = "SD_predicted_vs_actual_subspecies")

## Repeat for other classes. ##
## Population. ##

data <- subset(data, population != "NA")

lda_pop <- lda (population~AR+Circ+Area_mm2+Thickness+Petiole_Length_mm, data=data, CV=TRUE)

actual_pop <- as.data.frame(data$population)
predicted_pop <- as.data.frame(lda_pop$class)
pop_predict <- cbind(actual_pop, predicted_pop)
colnames(pop_predict) <- c("actual_pop", "predicted_pop")

t_pop_predict <- table(pop_predict)

p_pop_predict <- prop.table(t_pop_predict, 1)
m_pop_predict <- melt(p_pop_predict, id="actual_pop")

m_pop_predict$actual_pop <- factor(m_pop_predict$actual_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

m_pop_predict$predicted_pop <- factor(m_pop_predict$predicted_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

p <- ggplot(m_pop_predict, aes(predicted_pop, actual_pop, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_pop_predict, file = "SD_predicted_vs_actual_population")

## Island. ##
data <- subset(data, island != "NA")

lda_isl <- lda (island~AR+Circ+Area_mm2+Thickness+Petiole_Length_mm, data=data, CV=TRUE)

actual_isl <- as.data.frame(data$island)
predicted_isl <- as.data.frame(lda_isl$class)
isl_predict <- cbind(actual_isl, predicted_isl)
colnames(isl_predict) <- c("actual_isl", "predicted_isl")

t_isl_predict <- table(isl_predict)

p_isl_predict <- prop.table(t_isl_predict, 1)
m_isl_predict <- melt(p_isl_predict, id="actual_isl")

m_isl_predict$actual_isl <- factor(m_isl_predict$actual_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

m_isl_predict$predicted_isl <- factor(m_isl_predict$predicted_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

p <- ggplot(m_isl_predict, aes(predicted_isl, actual_isl, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_isl_predict, file = "SD_predicted_vs_actual_island")

## LDA using Landmarks, EFDs, and Shape Descriptors performed by Subspecies. Same steps as above, but the coordinate values are changed to include all measurements. ##
data <- subset(data, subspecies != "NA")

lda_subsp <- lda(subspecies~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+Area_mm2+Thickness+Petiole_Length_mm+AR+Circ, data=data, CV=TRUE)

actual_subsp <- as.data.frame(data$subspecies)
predicted_subsp <- as.data.frame(lda_subsp$class)
subsp_predict <- cbind(actual_subsp, predicted_subsp)
colnames(subsp_predict) <- c("actual_subsp", "predicted_subsp")

t_subsp_predict <- table(subsp_predict)

p_subsp_predict <- prop.table(t_subsp_predict, 1)
m_subsp_predict <- melt(p_subsp_predict, id="actual_subsp")

m_subsp_predict$actual_subsp <- factor(m_subsp_predict$actual_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

m_subsp_predict$predicted_subsp <- factor(m_subsp_predict$predicted_subsp, levels=c("Chenopodium_oahuense_ilioense","Chenopodium_oahuense_oahuense","ILS_Hybrid"))

p <- ggplot(m_subsp_predict, aes(predicted_subsp, actual_subsp, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_subsp_predict, file = "All_predicted_vs_actual_subspecies")

## Repeat for other classes. ##
## Population. ##

data <- subset(data, population != "NA")

lda_pop <- lda (population~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+Area_mm2+Thickness+Petiole_Length_mm+AR+Circ, data=data, CV=TRUE)

actual_pop <- as.data.frame(data$population)
predicted_pop <- as.data.frame(lda_pop$class)
pop_predict <- cbind(actual_pop, predicted_pop)
colnames(pop_predict) <- c("actual_pop", "predicted_pop")

t_pop_predict <- table(pop_predict)

p_pop_predict <- prop.table(t_pop_predict, 1)
m_pop_predict <- melt(p_pop_predict, id="actual_pop")

m_pop_predict$actual_pop <- factor(m_pop_predict$actual_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

m_pop_predict$predicted_pop <- factor(m_pop_predict$predicted_pop, levels=c("Big_Island","Maui","Ilio_Point","Mokio_Point","Oahu","Puu_Ka_Pele"))

p <- ggplot(m_pop_predict, aes(predicted_pop, actual_pop, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_pop_predict, file = "All_predicted_vs_actual_population")

## Island. ##
data <- subset(data, island != "NA")

lda_isl <- lda (island~x1+y1+x2+y2+x3+y3+x4+y4+x5+y5+x6+y6+A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14+A15+A16+A17+A18+A19+A20+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+B13+B14+B15+B16+B17+B18+B19+B20+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+C13+C14+C15+C16+C17+C18+C19+C20+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12+D13+D14+D15+D16+D17+D18+D19+D20+Area_mm2+Thickness+Petiole_Length_mm+AR+Circ, data=data, CV=TRUE)

actual_isl <- as.data.frame(data$island)
predicted_isl <- as.data.frame(lda_isl$class)
isl_predict <- cbind(actual_isl, predicted_isl)
colnames(isl_predict) <- c("actual_isl", "predicted_isl")

t_isl_predict <- table(isl_predict)

p_isl_predict <- prop.table(t_isl_predict, 1)
m_isl_predict <- melt(p_isl_predict, id="actual_isl")

m_isl_predict$actual_isl <- factor(m_isl_predict$actual_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

m_isl_predict$predicted_isl <- factor(m_isl_predict$predicted_isl, levels=c("Big_Island","Maui","Molokai","Oahu"))

p <- ggplot(m_isl_predict, aes(predicted_isl, actual_isl, fill=value))
p + geom_tile() + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + scale_fill_gradient(low="white", high="black", limits=c(0,1)) + geom_vline(xintercept=c(2.5,9.5,19.5,23.5,27.5,33.5)) + geom_hline(yintercept=c(2.5,9.5,19.5,23.5,27.5,33.5))+ coord_fixed()

write.table(m_isl_predict, file = "All_predicted_vs_actual_island")