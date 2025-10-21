################################################################################

# Title: directory creation and package loading.

# Script Author: Rory Macklin (rmacklin@birdscanada.org)

# Date: Oct 8 2025.

################################################################################

# Create necessary directories.

necessary_dirs <- c("./Data/Raw", "./Data/Cleaned", "./Scripts/RData", "./Outputs")  

for(i in necessary_dirs) {
  
  if(!dir.exists(i)) {
    
    dir.create(i, recursive = T)
    
  }
  
}

# Load necessary packages

if(system.file(package = "librarian") == "") {
  
  install.packages("librarian")
  
}

if(system.file(package = "naturecounts") == "") {
  
  install.packages("naturecounts", 
                   repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
                             CRAN = 'https://cloud.r-project.org'))
  
}

librarian::shelf(naturecounts, tidyverse, sf, "USEPA/elevatr", terra, exactextractr,
                 "rspatial/geodata", biooracler, "rspatial/luna", landscapemetrics)

# Delete unnecessary objects

rm(i)
rm(necessary_dirs)