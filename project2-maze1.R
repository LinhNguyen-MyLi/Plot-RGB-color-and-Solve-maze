# Linh Nguyen - 124112
# The code is the same for both maze_0 and maze_1. You just need to change nrow value at line 15 from 4 to 18 to solve maze_1

readMap <- function(path) {
  # Read the file
  lines <- readLines(path)
  
  # Remove any leading or trailing white spaces
  lines <- trimws(lines)
  
  # Split the lines into characters
  chars <- strsplit(lines, "")
  
  # Create a 4-row matrix to hold the maze
  maze <- matrix("", nrow = 18, ncol = length(chars[[1]]))
  
  # Copy the characters into the matrix
  for (i in 1:18) {
    maze[i, ] <- chars[[i]]
  }
  
  # Return the maze matrix
  return(maze)
}
maze <- readMap("D:/STUDY/STUDY/R/maze_1.MAP")


findWay <- function(maze) {
  # Find the starting and ending points
  start <- which(maze == "s", arr.ind = TRUE)
  end <- which(maze == "e", arr.ind = TRUE)
  
  # Define the recursive DFS function
  dfs <- function(pos, path) {
    # Base case: found the exit point
    if (all(pos == end)) {
      return(path)
    }
    
    # Recursive case: explore neighboring positions
    neighbors <- list(
      c(pos[1] - 1, pos[2]), # up
      c(pos[1] + 1, pos[2]), # down
      c(pos[1], pos[2] - 1), # left
      c(pos[1], pos[2] + 1)  # right
    )
    
    for (neighbor in neighbors) {
      row <- neighbor[1]
      col <- neighbor[2]
      if (row >= 1 && row <= nrow(maze) && col >= 1 && col <= ncol(maze) && maze[row, col] != "x" && !any(apply(path, 1, function(p) all(p == neighbor)))) {
        # The neighbor is within the bounds of the map, is not a wall, and has not already been visited
        path <- rbind(path, neighbor)
        result <- dfs(neighbor, path)
        if (!is.null(result)) {
          # Found a path to the exit point
          return(result)
        }
        # Remove the neighbor from the path if we didn't find a path to the exit point through it
        path <- path[-nrow(path),]
      }
    }
    
    # No path found through any of the neighbors
    return(NULL)
  }
  
  # Start the DFS search from the starting point
  dfs(start, start)
}

path <- findWay(maze)



# Function to plot the maze
plotMap <- function(m, ...) {
  n <- nrow(m)
  p <- ncol(m)
  plot(0, 0, xlim=c(0, p), ylim=c(0, n), type="n", axes=FALSE, xlab="", ylab="", ...)
  for (i in 1:n) {
    for (j in 1:p) {
      if (m[i,j] == "x") {
        rect(j-1, n-i, j, n-i+1, col="orange", border=NA)
      } else if (m[i,j] == "s") {
        rect(j-1, n-i, j, n-i+1, col="green", border=NA)
      } else if (m[i,j] == "e") {
        rect(j-1, n-i, j, n-i+1, col="red", border=NA)
      }
    }
  }
}

# Function to plot the path on the maze
plotPath <- function(m, p, ...) {
  plotMap(m, ...)
  lines(p[,2]- 0.5, nrow(m)-p[,1]+ 0.5, col="blue", lwd=2)
}


# Call plotMap() with customized graphical arguments
plotMap(maze, col = "black", border = "gray", lwd = 2)

# Call plotPath() with customized graphical arguments
plotPath(maze, path, col = "blue", lwd = 3)
