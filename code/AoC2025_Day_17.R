# Aoc 2024 Day 17

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data


# test
# Register A: 729
# Register B: 0
# Register C: 0
# Program: 0,1,5,4,3,0

# Register A: 66171486
# Register B: 0
# Register C: 0
# Program: 2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0

a <- 66171486; b<-0 ; c<-0 ;p <- "2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0"
init <- function(aa=a,bb=b,cc=c,pp=p){
  A <<- aa
  B <<- bb
  C <<- cc
  prog <<- str_split_1(pp,",") %>% as.integer()
  pointer <<- 0
  output <<- c()
}
init(66171486,0,0,"2,4,1,6,7,5,4,6,1,4,5,5,0,3,3,0")
init_test <- function(){
  init(729,0,0,"0,1,5,4,3,0")
}
init()


##########################################################
# A

print_situ <- function(){
  print(prog);print(A);print(B);print(C)
  print(pointer)
  print(output)
}
print_situ()

combo <- function(i){
  if(i==4) return(A)
  if(i==5) return(B)
  if(i==6) return(C)
  return(i)
}

t <- numToBits(24)
packBits(t,type = "double")

bxl <- function(a,b){
 bin_a <- intToBits(a)
 bin_b <- intToBits(b)
 xor(bin_a,bin_b) %>% packBits(type = "integer")
}
bxl(2,5)
bxl(1,3)
op <- function(instr,ope){
  jump <- FALSE
  if(instr==0) A <<- A %/% (2^combo(ope))
  if(instr==1) B <<- bxl(B,ope)
  if(instr==2) B <<- combo(ope) %% 8
  if(instr==3 & A!=0) {
    pointer <<- ope - 2
  }
  if(instr==4) B <<- bxl(B,C)
  if(instr==5) output <<- c(output,combo(ope) %% 8)
  if(instr==6) B <<- A %/% (2^combo(ope))
  if(instr==7) C <<- A %/% (2^combo(ope))
    pointer <<- pointer + 2
    # print("###")
    # print(instr)
    # print(ope)
    # print_situ()
}

exec_prog <- function(aa=a,bb=b,cc=c,pp=p){
  init(aa,bb,cc,pp)
 # i<-0
  while(pointer<length(prog)-1){
  #  if(prog[pointer+1]==3) {print(i);print(A)}
    op(prog[pointer+1],prog[pointer+2])
#    if(i %% 500 ==0) print(i)
#    i <- i+1
  }
  return(output)
}

init()
A <- 10
exec_prog(A,B,C,"5,0,5,1,5,4")

init()
A <- 2024
exec_prog(A,B,C,"0,1,5,4,3,0")
A

init()
A <- 729
print_situ()
exec_prog(A,B,C,"0,1,5,4,3,0")
# div A per 2, print A mod 8, repeat

init_test()
print_situ()

init()
B <- 29
prog <- str_split_1("1,7",",") %>% as.integer()
exec_prog(A,B,C,"1,7")

init()
B <- 2024
C <- 43690
prog <- str_split_1("4,0",",") %>% as.integer()
exec_prog()
B

exec_prog()
paste0(output,collapse=",")
# 2,3,6,2,1,6,1,2,1

######################################################
# B
print_situ()

res <- tibble(x=integer(),rep=character())

for(x in 1:1000) {
  init(aa = x,pp = p)
  i<-0
  while(pointer<length(prog)-1){
    op(prog[pointer+1],prog[pointer+2])
    if(i %% 500 ==0) print(i)
    i <- i+1
  }
  rep  <- output %>% paste0(collapse=",")
  res %<>% add_row(x,rep)
}

res

log(6,base = 8)
log(66171486,base = 8)
#
exec_prog()

##########################################################
# B

p
nchar(p)
# 16 entiers -> input doit être entre 8^15 et 8^16
exec_prog(8^15) %>% length
exec_prog(8^16-1) %>% length

# adapt to big integers
bxl <- function(a,b){
  seuil <- 2^15
  if(a > seuil | b > seuil) return(paste0(bxl(a %/% seuil, b %/% seuil),bxl(a %% seuil, b %% seuil)) %>% as.numeric)
  bin_a <- intToBits(a)
  bin_b <- intToBits(b)
  xor(bin_a,bin_b) %>% packBits(type = "integer")
}

res <- tibble(x=integer(),rep=character(),size=integer())

for(x in 8^15 + 1 + (1:7*8^3)*8^12) {
  init(aa = x,pp = p)
  i<-0
  while(pointer<length(prog)-1){
    op(prog[pointer+1],prog[pointer+2])
    if(i %% 500 ==0) print(i)
    i <- i+1
  }
  rep  <- output %>% paste0(collapse=",")
  res %<>% add_row(x,rep,size=nchar(rep))
}
res %<>% filter(size==31)
res %<>% mutate(rn=row_number()) 
res %>% mutate(rn=row_number()) %>% filter(substr(rep,31,31)=="0")
deb <- res %>% filter(rn==518) %>% pull(x) %>% unlist
fin <- res %>% filter(rn==520) %>% pull(x) %>% unlist
res %>% nrow

for(x in deb + 1024 + 513 + (0:512)*8^10) {
  init(aa = x,pp = p)
  i<-0
  while(pointer<length(prog)-1){
    op(prog[pointer+1],prog[pointer+2])
    if(i %% 500 ==0) print(i)
    i <- i+1
  }
  rep  <- output %>% paste0(collapse=",")
  res %<>% add_row(x,rep,size=nchar(rep))
}

res %>% mutate(rn=row_number()) %>% filter(substr(rep,29,31)=="3,0")
res %>% group_by(substr(rep,29,31)) %>% count
deb <- res %>% tail(1) %>% pull(x) %>% unlist

for(x in deb + 1 + (0:64)*8^12) {
  init(aa = x,pp = p)
  i<-0
  while(pointer<length(prog)-1){
    op(prog[pointer+1],prog[pointer+2])
    if(i %% 500 ==0) print(i)
    i <- i+1
  }
  rep  <- output %>% paste0(collapse=",")
  res %<>% add_row(x,rep,size=nchar(rep))
}

res %<>% arrange(x) %>% mutate(rn=row_number())
res %<>% mutate(fin=substr(rep,31,31)) 
res %>% group_by(fin) %>% count

ok <- 1
target <- "3,0"
old_target <- "0"
res %<>% mutate(old_fin=substr(rep,31,31))
best_sol <- 8^16
best_sol <- 90938893795565

while(ok<31){
  res %<>% unique
  res %<>% mutate(old_fin=substr(rep,32-ok,31))
  res %<>% mutate(fin=substr(rep,30-ok,31))
  old_target <- substr(p,32-ok,31)
  target <- substr(p,30-ok,31)
  
  if(any(res$fin == target)){
    old_target <- target
    res %<>% mutate(old_fin=fin)
    ok <- ok+2
    target <- substr(p,32-ok,31)
    res %<>% mutate(fin=substr(rep,30-ok,31))
   print("ok=") ; print(ok)
   tries <- 1
  }
  res %<>% arrange(x) %>% mutate(rn=row_number())
  rn_deb <- res %>% filter(old_fin==old_target) %>% head(1) %>% pull(rn) %>% unlist
  rn_end <- res %>% filter(old_fin==old_target) %>% tail(1) %>% pull(rn) %>% unlist
  
  if(rn_deb==1) {
    deb <- 8^15
  } else {
  deb <- res %>% filter(rn==rn_deb-1) %>% pull(x) %>% unlist
  }
  if(rn_end==max(res$rn)) {
    end <- best_sol
  } else {
    end <- res %>% filter(rn==rn_end+1) %>% pull(x) %>% unlist
  } 
  if(end-deb > 8*tries) deb <- deb - 8*tries
  vals <- deb + (0:(8*tries))*((end-deb) %/% (8*tries)
                                )
vals <- vals[vals >= 8^15 & vals < best_sol] %>% unique
  for(x in vals ) {
    init(aa = x,pp = p)
    while(pointer<length(prog)-1){
      op(prog[pointer+1],prog[pointer+2])
    }
    rep <- output %>% paste0(collapse=",")
    res %<>% add_row(x,rep,size=nchar(rep),fin=substr(rep,32-ok,31))
  }
  tries <- tries +1
  if(tries %% 20 == 0) print(tries)
  if(tries>60) {
    print("jump"); print(ok)
    new_x <- res$x - 2^(32-ok)
    for(x in new_x ) {
      init(aa = x,pp = p)
      while(pointer<length(prog)-1){
        op(prog[pointer+1],prog[pointer+2])
      }
      rep <- output %>% paste0(collapse=",")
      res %<>% add_row(x,rep,size=nchar(rep),fin=substr(rep,32-ok,31))
    }
    ok <- 1
  }
}

sol <- res %>% filter(rep==p) %>% pull(x)
sol[1]+0.1

exec_prog(90938893795561)
p
# 90938893795566 too high
# 90938893795565 too high
# 90938893795561 correct