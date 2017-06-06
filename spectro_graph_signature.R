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
#' @name spectro_graph_signature

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

# Filenames
file1 <- "~/Spectrometry/SUC_Submissions/MIT/MAY_SPECTRAL/CASSAVA/CASSAVA1/B_reading_1"
file2 <- "~/Spectrometry/SUC_Submissions/MIT/MAY_SPECTRAL/CASSAVA/CASSAVA1/B_reading_2"
file3 <- "~/Spectrometry/SUC_Submissions/MIT/MAY_SPECTRAL/CASSAVA/CASSAVA1/B_reading_3"
filewr <- "~/Spectrometry/SUC_Submissions/MIT/MAY_SPECTRAL/CASSAVA/CASSAVA1/B_white_ref"

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

wvnm <- list.files(path = filewr, full.names = TRUE)
wvnm <- read.table(file = wvnm[1], sep = '', skip = 15)
colnames(wvnm) <- c('Wavelengths', 'Intensity')

par(mfrow = c(2, 2))
plot(wvnm$Wavelengths, spectroA, ylim = c(0, 1), main = "Replicate 1", type = "l", col = "red", xlab = "Wavelength\n(nm)", ylab = "Reflectance Ratio")
plot(wvnm$Wavelengths, spectroB, ylim = c(0, 1), main = "Replicate 2", type = "l", col = "red", xlab = "Wavelength\n(nm)", ylab = "Reflectance Ratio")
plot(wvnm$Wavelengths, spectroC, ylim = c(0, 1), main = "Replicate 3", type = "l", col = "red", xlab = "Wavelength\n(nm)", ylab = "Reflectance Ratio")
plot(wvnm$Wavelengths, meanSpectro, ylim = c(0, 1), main = "Average Spectral Signature", type = "l", col = "red", xlab = "Wavelength\n(nm)", ylab = "Reflectance Ratio")
#abline(h = 0, lty = 2)
#abline(h = 1, lty = 2)
#abline(v = min(wvnm$Wavelengths), lty = 2, col = "blue")
#abline(v = max(wvnm$Wavelengths), lty = 2, col = "blue")

allPlots = data.frame(cbind(spectroA, spectroB, spectroC, meanSpectro))
colnames(allPlots) <- c("Replicate 1", "Replicate 2", "Replicate 3", "Average")
matplot(wvnm$Wavelengths, allPlots * 100, col = 1:4, ylim = c(0, 100), main = "All Spectra",
        xlab = "Wavelength\n(nm)", ylab = "Reflectance (%)", type = "l", lwd = 1)
legend("topleft", legend = colnames(allPlots), col = 1:4, lwd = 1)
