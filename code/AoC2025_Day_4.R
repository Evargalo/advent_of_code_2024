# Aoc 2024 Day 4

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D4.txt",header = FALSE)
m <- df_from_vector_of_strings(dat$V1) %>% as.matrix

# A
res <- 0
for(j in 1:(ncol(m)-3)){
  for(i in 1:nrow(m)){
    if(m[i,j]=="X" & m[i,j+1]=="M" & m[i,j+2]=="A" & m[i,j+3]=="S") res <- res+1
  }
}
for(j in 1:ncol(m)){
  for(i in 1:(nrow(m)-3)){
    if(m[i,j]=="X" & m[i+1,j]=="M" & m[i+2,j]=="A" & m[i+3,j]=="S") res <- res+1
  }
}
for(j in 4:ncol(m)){
  for(i in 1:nrow(m)){
    if(m[i,j]=="X" & m[i,j-1]=="M" & m[i,j-2]=="A" & m[i,j-3]=="S") res <- res+1
  }
}
for(j in 1:ncol(m)){
  for(i in 4:nrow(m)){
    if(m[i,j]=="X" & m[i-1,j]=="M" & m[i-2,j]=="A" & m[i-3,j]=="S") res <- res+1
  }
}
for(j in 1:(ncol(m)-3)){
  for(i in 1:(nrow(m)-3)){
    if(m[i,j]=="X" & m[i+1,j+1]=="M" & m[i+2,j+2]=="A" & m[i+3,j+3]=="S") res <- res+1
  }
}
for(j in 1:(ncol(m)-3)){
  for(i in 4:nrow(m)){
    if(m[i,j]=="X" & m[i-1,j+1]=="M" & m[i-2,j+2]=="A" & m[i-3,j+3]=="S") res <- res+1
  }
}
for(j in 4:ncol(m)){
  for(i in 4:nrow(m)){
    if(m[i,j]=="X" & m[i-1,j-1]=="M" & m[i-2,j-2]=="A" & m[i-3,j-3]=="S") res <- res+1
  }
}
for(j in 4:ncol(m)){
  for(i in 1:(nrow(m)-3)){
    if(m[i,j]=="X" & m[i+1,j-1]=="M" & m[i+2,j-2]=="A" & m[i+3,j-3]=="S") res <- res+1
  }
}
res

# B
res <- 0
for(j in 2:(ncol(m)-1)){
  for(i in 2:(nrow(m)-1)){
    if(m[i,j]=="A"){
      if((m[i-1,j-1]=="M" & m[i+1,j+1]=="S") || (m[i-1,j-1]=="S" & m[i+1,j+1]=="M")){
        if((m[i-1,j+1]=="M" & m[i+1,j-1]=="S") || (m[i-1,j+1]=="S" & m[i+1,j-1]=="M")){
          res <- res+1
        }
      } 
    }
  }
}
res
