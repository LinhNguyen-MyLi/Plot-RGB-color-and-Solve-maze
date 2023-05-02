# Linh Nguyen - 124112
# First function to transform dataframe
readInData <- function(path) {
  # Read in the file
  lines <- readLines(path)
  
  # Extract the data from each line
  x_vals <- gsub("\\[x = (.*?), y = (.*?)\\].*", "\\1", lines)
  y_vals <- gsub("\\[x = (.*?), y = (.*?)\\].*", "\\2", lines)
  red_vals <- gsub(".*\\[red = (.*?),.*", "\\1", lines)
  green_vals <- gsub(".*green = (.*?),.*", "\\1", lines)
  blue_vals <- gsub(".*blue = (.*?)]", "\\1", lines)
  
  # Convert the extracted data to a data frame
 d0 <- data.frame(x = as.numeric(x_vals), y = as.numeric(y_vals),
                      red = as.numeric(red_vals), green = as.numeric(green_vals), 
                      blue = as.numeric(blue_vals))
  
  return(d0)
}

d0 <- readInData(path = "D:/STUDY/STUDY/R/Color.csv")
head(d0)

# Second function to plot the graph
makePlot <- function(d0, pch = 1, col = rgb(d0$red, d0$green, d0$blue),
                     xlab = "", ylab = "", main = "", cex = 1) {
  plot(d0$x, d0$y, pch = pch, col = col, xlab = xlab, ylab = ylab, main = main, cex = cex)
}

makePlot(d0, pch = 20, xlab = "x", ylab = "y", cex = 0.8)


