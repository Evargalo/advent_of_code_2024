# Aoc 2024 Day 11

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

initState <- c(0,7,6618216,26481,885,42,202642,8791)

# A

evol_stone <- function(i){
  if(i==0) return(i+1)
  i_c <- as.character(i)
  l <- nchar(i_c)
  if(l%%2==0){
    a <- substr(i,1,l%/%2)
    b <- substr(i,l%/%2+1,l)
    return(c(a,b) %>% as.integer)
  }
  return(2024*i)
}

state <- initState
for(k in 1:25){
  state <- mapply(evol_stone, state) %>% unlist
}

length(state)
# 213625

# B

# Bad idea, of course :
# for(k in 26:75){
#  state <- mapply(evol_stone, state) %>% unlist
# }
# length(state)

res <- state %>% table
names(res)
# situation in a tibble of frequencies - more compact than an extensive vector
build_situation <- function(state){
  res <- state %>% table
  tibble(val=as.numeric(names(res)),n=res)
}
situ_0 <- build_situation(initState)

# what happens from stone i after 5 blinks - store the result for later usage
evol_5 <- function(i){
  if(paste0("table_",i) %in% ls()) {
    res <- get(paste0("table_",i)) 
  } else {
    state <- i
    for(k in 1:5){
      state <- mapply(evol_stone, state) %>% unlist
    }
    res <- build_situation(state)
    assign(x = paste0("table_",i) , res, envir = .GlobalEnv)
  }
  res
}
evol_5(0) # ok
# advance 5 blinks and store the new situation
change_situation <- function(situation){
  pmap_dfr(situation,function(val,n){
    nn<-n
    evol_5(val) %>% mutate(n=nn*n)
  }) %>% 
    group_by(val) %>% summarise(n=sum(n)) %>% ungroup
}
# it works :
situ_5 <- change_situation(situ_0)
# let's go :
for(k in 5*(2:15)){
  print(k)
  prev <- get(paste0("situ_",k-5))
  assign(x = paste0("situ_",k),value = change_situation(prev))
}
v <- situ_75 %>% summarise(sum(n))
# For visibility, so that the console don't show 2.52e14 anymore
v + 0.1
# 252442982856820