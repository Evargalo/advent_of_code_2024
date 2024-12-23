# Aoc 2024 Day 23

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")

##########################################################
# Data

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D23.txt",sep = "-",header = FALSE)

comps <- c(dat$V1,dat$V2) %>% unique
tcomps <- comps[substr(comps,1,1)=="t"]

##########################################################
# A

g <- graph_from_data_frame(dat, directed = FALSE)
plot(g)
map(cliques(g,3,3),function(x)any(names(x) %in% tcomps)) %>% reduce(sum)
# 1077

##########################################################
# B

names(largest_cliques(g)[[1]]) %>% sort %>% paste(collapse = ",")
# bc,bf,do,dw,dx,ll,ol,qd,sc,ua,xc,yu,zt


##########################################################
# A without package igraph

# 3 links chains
connections <- dat %>% rename(a=V1,b=V2) %>% 
  bind_rows(dat %>% rename(b=V1,a=V2)) %>% unique
bi_connections <- connections %>% rename(c=b,b=a) %>% 
  left_join(connections, relationship = "many-to-many") %>% unique
tri_connections <- bi_connections %>% rename(d=c,c=b,b=a) %>% 
  left_join(connections, relationship = "many-to-many") %>% unique
# 3-cliques
tri_connections %<>% filter(a==d)
tri_t <- tri_connections %>% filter(a %in% tcomps | b %in% tcomps | c %in% tcomps )
(tri_t %>% nrow)/6
# 1077