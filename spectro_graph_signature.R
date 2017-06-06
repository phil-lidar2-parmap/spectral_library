#' Compute the spectral signatures and output a graph for quality assessment
#' 
#' @description 
#' This code is to compute for the spectral signatures of PARMap-prescribed
#' acquisitions. Meaning there should be a white reference dataset along with
#' three replicate acquisitions.
#' 
#' @return 
#' Graph of the spectral signature.
#' Optional:
#' 1. Textfile of Replicate 1
#' 2. Textfile of Replicate 2
#' 3. Textfile of Replicate 3
#' 4. Graph of Replicate 1
#' 5. Graph of Replicate 2
#' 6. Graph of Replicate 3
#' 7. Textfile of Signature
#' 8. Graph of Signature
#' 
#' @author 
#' Daniel Marc G. dela Torre
#' Phil-LiDAR 2 PARMap
#' 
#' @name spectro_graph_spectro

library(ggplot2)

###############################################################################
###############################################################################
# Input Filenames Here
in.directory <- "E:/DANIEL/Spectrometry/SUC_Submissions/MIT/MAY_SPECTRAL/CASSAVA/CASSAVA2/"  # Input directory
outputFilename <- "E:/test.tif"  # Output file for graph of spectral signature
###############################################################################

# Assign variables to respective folders
list.directories <- list.dirs(in.directory, recursive = FALSE)
file1 <- list.directories[which(basename(list.directories) == "B_reading_1")]
file2 <- list.directories[which(basename(list.directories) == "B_reading_2")]
file3 <- list.directories[which(basename(list.directories) == "B_reading_3")]
filewr <- list.directories[which(basename(list.directories) == "B_white_ref")]

SpectroMean <- function(x, verbose = TRUE) {
  # Computes the average of the spectral profile.
  #
  # Args:
  #  x: Path of the folder
  #
  # Returns:
  #  A vector containing the spectral signature.
  list <- list.files(path = x, full.names = TRUE)
  a <- read.table(file = list[1], sep = '', skip = 15)
  colnames(a) <- c('Wavelengths', 'Intensity')
  rownum <- nrow(a)
  mat <- matrix(data = 1L, nrow = nrow(a), ncol = length(list) + 1)
  mat[, 1] <- a$Wavelengths
  for (i in 1:length(list)) {
    b <- read.table(file = list[i], sep = '', skip = 15)
    colnames(b) <- c('Wavelengths', 'Intensity')
    mat[, i + 1] <- b$Intensity
  }
  mat <- data.frame(mat)
  means <- rowMeans(mat[, 2:length(list)+1])
  if (verbose)
    cat("File printed: ", x)
  return(means)
}

# Compute spectral means
spectroA <- SpectroMean(file1) / SpectroMean(filewr)
spectroB <- SpectroMean(file2) / SpectroMean(filewr)
spectroC <- SpectroMean(file3) / SpectroMean(filewr)

# Plot (optional)
plot <- 1 < 0
if (plot) {
  plot(spectroA, ylim = c(0,1))
  plot(spectroB, ylim = c(0,1))
  plot(spectroC, ylim = c(0,1))
}

# Compute the average spectral signature
meanSpectroTable <- data.frame(cbind(spectroA, spectroB, spectroC))
meanSpectro <- apply(meanSpectroTable, 1, mean)
sdSpectro <- apply(meanSpectroTable, 1, sd)

# Extract values of the wavelengths
wvnm <- list.files(path = filewr, full.names = TRUE)
wvnm <- read.table(file = wvnm[1], sep = '', skip = 15)
colnames(wvnm) <- c('Wavelengths', 'Intensity')

# Plot spectral signature
allPlots <- data.frame(cbind(wvnm$Wavelengths, spectroA * 100, spectroB * 100, spectroC * 100, meanSpectro * 100))
colnames(allPlots) <- c("Wavelength", "Replicate 1", "Replicate 2", "Replicate 3", "Average")
ggplot(allPlots, aes(x = allPlots$Wavelength)) +
  geom_line(aes(y = allPlots$`Replicate 1`, colour = "Replicate 1")) +
  geom_line(aes(y = allPlots$`Replicate 2`, colour = "Replicate 2")) +
  geom_line(aes(y = allPlots$`Replicate 3`, colour = "Replicate 3")) +
  geom_line(aes(y = allPlots$`Average`, colour = "Average")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = paste("Spectral Signatures of ", basename(in.directory)), x = "Wavelength\n(nm)", y = "Reflectance\n(%)") +
  scale_colour_discrete(name = "Legend")

# Save file as tiff
if (outputFilename == TRUE) {
  ggsave(outputFilename, plot = last_plot(), device = "tiff", width = 10, height = 8, units = "in", dpi = 300)
} 
  
## END
