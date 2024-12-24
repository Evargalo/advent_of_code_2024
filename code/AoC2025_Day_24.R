# Aoc 2024 Day 24

source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/packages.R")
source("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/code/usefulFunctions.R")


##########################################################
# Data

dat_init <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D24_init.txt",sep = " ",header = FALSE)

dat <- read.csv2("C:/Users/MMAJR1/Documents/Perso/R for fun/AdventOfCode/AoC2024/advent_of_code_2024/data/D24.txt",sep = " ",header = FALSE)

dat_init %>% head
dat_init %<>% mutate(V1=cutAfter(V1,":"))

targets <- dat %>% filter(substr(V5,1,1)=="z") %>% pull(V5)
##########################################################
# A

dat %<>% mutate(W1=logical(222),W3=logical(222),W5=logical(222),
                W1=NA,W3=NA,W5=NA)
op <- function(ope,a,b){
  if(ope=="AND") return(a&b)
  if(ope=="OR") return(a|b)
  if(ope=="XOR") return(xor(a,b))
}
update_dat <- function(V,W){
  dat <<- dat %>% rowwise() %>% 
    mutate(
      W1=if_else(V1 == V,W,W1),
      W3=if_else(V3 == V,W,W3),
      W5=op(V2,W1,W3) 
    ) %>% 
    ungroup
}
vals <- dat_init %>% select(V=V1,W=V2)

while(!all(targets %in% vals$V)){
  pmap(vals,update_dat)
  vals <- dat %>% filter(!is.na(W5)) %>% select(V=V5,W=W5)
}
sum(is.na(dat$W5))

bits <- vals %>% filter(V %in% targets) %>% arrange(V)
(bits %>% pull(W) * 2^(0:45)) %>% sum
# 53755311654662 

##########################################################
# B

# visualisation doesn't help, it's too messy :
d <- bind_rows(
  dat %>% select(orig=V1,dest=V5),
  dat %>% select(orig=V3,dest=V5)
)
g <- graph_from_data_frame(d, directed = TRUE)
plot(g)

# algorithm for bit-wise sum :
# Init rank 0
# for any rank, 5 steps :
# bit_0 : xor (x,y)
# retenue_0 : AND(x,y)
# retenue_1 : AND(bit_0, retenue_2 from lower rank)
# bit_1 : xor(bit_0, retenue_2 from lower rank)
# retenue_2 : OR(retenue_0, retenue_1)

# We dig in the algorithm to attribute to each variable its role (five possibilities) in its rank(from 0 to 44) ; then we look for irregularities anywhere but at the borders.

f_step <- dat %>% filter(V1 %in% dat_init$V1 | V2 %in% dat_init$V1) %>% arrange(V2,V1)
f_step %<>% mutate(
  nb_1=substr(V1,1,1),rg_1=substr(V1,2,3) %>% as.integer,
  nb_2=substr(V3,1,1),rg_2=substr(V3,2,3) %>% as.integer
) %>% 
  mutate(
    res=case_when(
      rg_1 != rg_2 ~ "error",
      V2=="AND" ~ "retenue",
      V2=="XOR" ~ "bit",
      V2=="OR"~ "or"
    ),
    rg=case_when(
      rg_1 != rg_2 ~ 1000,
      TRUE ~ rg_1
    ),
    prev_rg=case_when(
      rg_1 != rg_2 ~ 1000,
      TRUE ~ rg_2
    )
  )
role <- f_step %>% select(node=V5,role=res,rg=rg,prev_rg=prev_rg)
while(all(role$rg != 1000) & nrow(role) < 222){
  new_step <- dat %>% filter(V1 %in% role$node & V3 %in% role$node & V5 %notin% role$node) %>% 
    arrange(V2,V1) %>% 
    left_join(role %>% rename(V1=node,role_1=role,rg_1=rg),by="V1") %>% 
    left_join(role %>% rename(V3=node,role_3=role,rg_3=rg),by="V3") %>% 
    mutate(res=case_when(
      abs(rg_1 - rg_3) > 1 ~ "error",
      V2=="AND" ~ "retenue",
      V2=="XOR" ~ "bit",
      V2=="OR"~ "or"
    ),
    rg=case_when(
      abs(rg_1 - rg_3) > 1 ~ 1000,
      TRUE ~ max(rg_1,rg_3)
    ),
    prev_rg=case_when(
      abs(rg_1 - rg_3) > 1 ~ 1000,
      TRUE ~ min(rg_1,rg_3)
    )
    )
  print(nrow(role))
  role %<>% bind_rows(new_step %>% select(node=V5,role=res,rg=rg,prev_rg=prev_rg))
}
role_z <- role %>% filter(substr(node,1,1)=="z")
# 3 of them fishy
role %>% filter(role=="bit",rg %in% c(5,15,20))
role %>% filter(role=="retenue",rg == 44)

dat %>% filter(V1=="ntr" | V3=="ntr" | V5 =="ntr")
dat %>% filter(substr(V1,1,1)=="z" | substr(V3,1,1)=="z")
dat %>% filter(V5=="z20")

role %<>% mutate(role_det=paste0(role,"_",rg-prev_rg))
role %>% group_by(role_det) %>% count

dd <- dat %>% 
  left_join(role %>% mutate(role_v1=role_det) %>% select(V1=node,role_v1,rg_v1=rg)) %>% 
  left_join(role %>% mutate(role_v3=role_det) %>% select(V3=node,role_v3,rg_v3=rg)) %>% 
  arrange(V2,rg_v1,rg_v3) 
dd %>% 
  group_by(V2,role_v1,role_v3) %>% 
  count
# let's compare z20 with a non-suspicious one, e.g. z08:
dd %>% filter(rg_v1 %in% 19:20 | rg_v3 %in% 19:20)
dd %>% filter(rg_v1 %in% 7:8 | rg_v3 %in% 7:8)
# z20 calculated in a funny way

# swap Z20 <-> hhh

dat2 <- dat %>% mutate(V5=case_when(
  V5=="hhh" ~ "z20",
  V5=="z20" ~ "hhh",
  TRUE ~ V5
))

f_step <- dat2 %>% filter(V1 %in% dat_init$V1 | V2 %in% dat_init$V1) %>% arrange(V2,V1)
f_step %<>% mutate(
  nb_1=substr(V1,1,1),rg_1=substr(V1,2,3) %>% as.integer,
  nb_2=substr(V3,1,1),rg_2=substr(V3,2,3) %>% as.integer
) %>% 
  mutate(
    res=case_when(
      rg_1 != rg_2 ~ "error",
      V2=="AND" ~ "retenue",
      V2=="XOR" ~ "bit",
      V2=="OR"~ "or"
    ),
    rg=case_when(
      rg_1 != rg_2 ~ 1000,
      TRUE ~ rg_1
    ),
    prev_rg=case_when(
      rg_1 != rg_2 ~ 1000,
      TRUE ~ rg_2
    )
  )
role <- f_step %>% select(node=V5,role=res,rg=rg,prev_rg=prev_rg)

while(all(role$rg != 1000) & nrow(role) < 222){
  new_step <- dat2 %>% filter(V1 %in% role$node & V3 %in% role$node & V5 %notin% role$node) %>% 
    arrange(V2,V1) %>% 
    left_join(role %>% rename(V1=node,role_1=role,rg_1=rg),by="V1") %>% 
    left_join(role %>% rename(V3=node,role_3=role,rg_3=rg),by="V3") %>% 
    mutate(res=case_when(
      abs(rg_1 - rg_3) > 1 ~ "error",
      V2=="AND" ~ "retenue",
      V2=="XOR" ~ "bit",
      V2=="OR"~ "or"
    ),
    rg=case_when(
      abs(rg_1 - rg_3) > 1 ~ 1000,
      TRUE ~ max(rg_1,rg_3)
    ),
    prev_rg=case_when(
      abs(rg_1 - rg_3) > 1 ~ 1000,
      TRUE ~ min(rg_1,rg_3)
    )
    )
  print(nrow(role))
  role %<>% bind_rows(new_step %>% select(node=V5,role=res,rg=rg,prev_rg=prev_rg))
}

role %<>% mutate(role_det=paste0(role,"_",rg-prev_rg))
role %>% group_by(role_det) %>% count

dd <- dat %>% 
  left_join(role %>% mutate(role_v1=role_det) %>% select(V1=node,role_v1,rg_v1=rg)) %>% 
  left_join(role %>% mutate(role_v3=role_det) %>% select(V3=node,role_v3,rg_v3=rg)) %>% 
  arrange(V2,rg_v1,rg_v3) 

role_z <- role %>% filter(substr(node,1,1)=="z")
# z05 and z15 are still fishy

role %>% filter(role=="bit",rg %in% c(5,15))

dd %>% filter(rg_v1 %in% 14:15 | rg_v3 %in% 14:15)
dd %>% filter(rg_v1 %in% 7:8 | rg_v3 %in% 7:8)
dd %>% filter(rg_v1 %in% 4:5 | rg_v3 %in% 4:5)
dd %>% filter(V5=="z05")
dd %>% filter(V5=="fdd")

# switch htp and z15
# switch dkr and z05

dat3 <- dat2 %>% mutate(V5=case_when(
  V5=="htp" ~ "z15",
  V5=="z15" ~ "htp",
  V5=="dkr" ~ "z05",
  V5=="z05" ~ "dkr",
  TRUE ~ V5
))

f_step <- dat3 %>% filter(V1 %in% dat_init$V1 | V2 %in% dat_init$V1) %>% arrange(V2,V1)
f_step %<>% mutate(
  nb_1=substr(V1,1,1),rg_1=substr(V1,2,3) %>% as.integer,
  nb_2=substr(V3,1,1),rg_2=substr(V3,2,3) %>% as.integer
) %>% 
  mutate(
    res=case_when(
      rg_1 != rg_2 ~ "error",
      V2=="AND" ~ "retenue",
      V2=="XOR" ~ "bit",
      V2=="OR"~ "or"
    ),
    rg=case_when(
      rg_1 != rg_2 ~ 1000,
      TRUE ~ rg_1
    ),
    prev_rg=case_when(
      rg_1 != rg_2 ~ 1000,
      TRUE ~ rg_2
    )
  )
role <- f_step %>% select(node=V5,role=res,rg=rg,prev_rg=prev_rg)

while(all(role$rg != 1000) & nrow(role) < 222){
  new_step <- dat3 %>% filter(V1 %in% role$node & V3 %in% role$node & V5 %notin% role$node) %>% 
    arrange(V2,V1) %>% 
    left_join(role %>% rename(V1=node,role_1=role,rg_1=rg),by="V1") %>% 
    left_join(role %>% rename(V3=node,role_3=role,rg_3=rg),by="V3") %>% 
    mutate(res=case_when(
      abs(rg_1 - rg_3) > 1 ~ "error",
      V2=="AND" ~ "retenue",
      V2=="XOR" ~ "bit",
      V2=="OR"~ "or"
    ),
    rg=case_when(
      abs(rg_1 - rg_3) > 1 ~ 1000,
      TRUE ~ max(rg_1,rg_3)
    ),
    prev_rg=case_when(
      abs(rg_1 - rg_3) > 1 ~ 1000,
      TRUE ~ min(rg_1,rg_3)
    )
    )
  print(nrow(role))
  role %<>% bind_rows(new_step %>% select(node=V5,role=res,rg=rg,prev_rg=prev_rg))
}

role %<>% mutate(role_det=paste0(role,"_",rg-prev_rg))
role %>% group_by(role_det) %>% count

dd <- dat3 %>% 
  left_join(role %>% mutate(role_v1=role_det) %>% select(V1=node,role_v1,rg_v1=rg)) %>% 
  left_join(role %>% mutate(role_v3=role_det) %>% select(V3=node,role_v3,rg_v3=rg)) %>% 
  arrange(V2,rg_v1,rg_v3) 

dd %<>% 
  left_join(role %>% mutate(role_v5=role_det) %>% select(V5=node,role_v5,rg_v5=rg)) %>% 
  arrange(V2,rg_v5,rg_v1,rg_v3) 
dd %>% 
  group_by(V2,role_v5,role_v1,role_v3) %>% 
  count
# 2 lines are weird :
dd %>% filter(role_v5=="or_0" & role_v3=="bit_0")
dd %>% filter(role_v5=="retenue_1" & role_v1=="retenue_0")
# let's compare :
dd %>% filter(rg_v1 %in% 34:36 | rg_v3 %in% 34:36) %>% arrange(rg_v5)
dd %>% filter(rg_v1 %in% 6:8 | rg_v3 %in% 6:8) %>% arrange(rg_v5)
# switch ggk <-> rhv

# dkr,ggk,hhh,htp,rhv,z05,z15,z20
