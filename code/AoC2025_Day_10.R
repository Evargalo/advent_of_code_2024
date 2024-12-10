# Aoc 2024 Day 10

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D10.txt",sep = " ",header = FALSE,colClasses = 'character')

maze <- buildMaze(dat$V1)

m <- maze$Maze
drawMaze(m)

# A

get_t <- function(xx,yy){
  m %>% filter(x==xx,y==yy) %>% pull(t)
}
get_t(2,5)

end_points <- tibble(x0=integer(),y0=integer(),x9=integer(),y9=integer())

add_step <- function(val,xx,yy,x0,y0){
 # print(val)
  if(val==9) {
    end_points <<- end_points %>% add_row(x0=x0,y0=y0,x9=xx,y9=yy)
    return(TRUE)
  }
  z <- m %>% filter(abs(xx-x)+abs(yy-y)==1 & t==val+1)
  if(nrow(z)==0) return(FALSE)
  pmap(.l = z %>% select(xx=x,yy=y), .f=function(xx,yy){add_step(val=val+1,xx=xx,yy=yy,x0=x0,y0=y0)})
  return(TRUE)
}
pmap(maze$Char0Spots, .f=function(x,y){add_step(val=0,xx=x,yy=y,x0=x,y0=y)})

end_points %>% unique %>% nrow
# 822

# B

end_points %>% nrow
# 1801
