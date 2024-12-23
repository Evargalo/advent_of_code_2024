# Aoc 2024 Day 23

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D23.txt",sep = "-",header = FALSE)

##########################################################
# A

comps <- c(dat$V1,dat$V2) %>% unique
tcomps <- comps[substr(comps,1,1)=="t"]

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

##########################################################
# B

g <- graph_from_data_frame(connections, directed = FALSE)
plot(g)
res <- largest_cliques(g)
names(res[[1]]) %>% sort %>% paste(collapse = ",")
# bc,bf,do,dw,dx,ll,ol,qd,sc,ua,xc,yu,zt
