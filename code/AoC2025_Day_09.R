# Aoc 2024 Day 9

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- readLines("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D9.txt")
d <- str_split(dat[[1]],"") %>% unlist %>% as.integer

d_test <- str_split("2333133121414131402","") %>% unlist %>% as.integer
# d <- d_test

l<-length(d)

len_blocs <- d[1+2*(0:(l%/%2))] 
len_blocs %>% tail
val_blocs <- 0:(length(len_blocs)-1)
len_spaces <- c(0,d[2*(1:(l%/%2))])

# A

range_blocs <- function(len_blocs,val_blocs,len_spaces,v=c(),ite=0){
#  print(len_blocs);print(val_blocs);print(len_spaces);print(v);print("###")
  if(ite==1000) return(list(len_blocs,val_blocs,len_spaces,v))
   print(length(val_blocs))
  
  if(length(len_blocs)==0) return(v)
  if(length(len_spaces)==0) return(c(v,rep(val_blocs[1],len_blocs[1])))
  
  gap <- len_spaces[1]
  if(gap==0){
    v <- c(v,rep(val_blocs[1],len_blocs[1]))
    val_blocs <- val_blocs[-1]
    len_blocs <- len_blocs[-1]
    len_spaces <- len_spaces[-1]
  } else{
    l <- length(val_blocs)
    fil <- len_blocs[l]
    if(gap>fil){
      v <- c(v,rep(val_blocs[l],fil))
      len_spaces[1] <- gap-fil
      val_blocs <- val_blocs[-l]
      len_blocs <- len_blocs[-l]
      len_spaces <- len_spaces[-l]
    } else{
      v <- c(v,rep(val_blocs[l],gap))
      len_blocs[l] <- fil-gap
      len_spaces[1] <- 0
    }
  }
  range_blocs(len_blocs,val_blocs,len_spaces,v,ite+1)
}
# test
range_blocs(c(1,3,5),0:2,c(2,4))

v <- range_blocs(len_blocs,val_blocs,len_spaces)
while(length(v)==4){
  len_blocs <- v[[1]]
  val_blocs <- v[[2]]
  len_spaces <- v[[3]]
  v <- range_blocs(len_blocs,val_blocs,len_spaces,v[[4]])
}

sum(v*(0:(length(v)-1)))
# 6359213660505

# B
length(val_blocs)
length(len_blocs)
length(len_spaces)
l <- length(val_blocs)

pos_blocs=cumsum(len_blocs+len_spaces)-len_blocs

inserer_bloc <- function(i){
  if(all(len_spaces<len_blocs[i])) return(FALSE)
  j <- first(which(len_spaces>=len_blocs[i]))
  if(j>i) return(FALSE)
  pos_blocs[i] <<- pos_blocs[j] - len_spaces[j]
  if(j<i){
    len_spaces[j] <<- len_spaces[j] - len_blocs[i]
  }
  return(TRUE)
}

for(i in l:1) inserer_bloc(i)

sum(val_blocs*len_blocs*(2*pos_blocs+len_blocs-1)/2)

# 6381624803796