if (!require("remotes", character.only = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("jeffreyevans/spatialEco")

# when the installation fails
if (!require("spatialEco", character.only = TRUE)) {
  if(.Platform$OS.type == "unix") {
    dotR <- file.path(Sys.getenv("HOME"), ".R")
    if (!file.exists(dotR))
      dir.create(dotR)
    M <- file.path(dotR, "Makevars")
    if (!file.exists(M))
      file.create(M) 
    cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
        "CXX14=g++ -std=c++11",
        "CXX14STD='-std=c++14'",
        file = M, sep = "\n", append = TRUE)
    
    remotes::install_github("jeffreyevans/spatialEco")
    
    if (!require("spatialEco", character.only = TRUE)) {
      print("Error in installation on Unix systems. Please report an issue to the authors.")
    }
  } else {
    print("Error in installation on non-Unix systems. Please report an issue to the authors.")
  }
}
