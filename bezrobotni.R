library(bdl)
library(dplyr)

### Bezrobotni zarejestrowani wg stażu pracy ####

#P2514
v <- get_variables("P2514") %>% arrange(n3)

k <- v %>% filter(n2=="kobiety")
m <- v %>% filter(n2=="mężczyźni")
o <- v %>% filter(n2=="ogółem")

kobiety <- NULL
for (i in 1:nrow(k)){
dane <- get_data_by_variable(as.character(k$id[i]), year = 2005:2020, unitLevel = 5) %>% select(name, year, val)
dane$n1 <- k$n1[i]
dane$n2 <- k$n2[i]
dane$n3 <- k$n3[i]
kobiety <- rbind(kobiety, dane)
Sys.sleep(2)
}

faceci <- NULL
for (i in 1:nrow(k)){
  dane <- get_data_by_variable(as.character(m$id[i]), year = 2005:2020, unitLevel = 5) %>% select(name, year, val)
  dane$n1 <- m$n1[i]
  dane$n2 <- m$n2[i]
  dane$n3 <- m$n3[i]
  faceci <- rbind(faceci, dane)
  Sys.sleep(2)
}

ogolem <- NULL
for (i in 1:nrow(k)){
  dane <- get_data_by_variable(as.character(o$id[i]), year = 2005:2020, unitLevel = 5) %>% select(name, year, val)
  dane$n1 <- o$n1[i]
  dane$n2 <- o$n2[i]
  dane$n3 <- o$n3[i]
  ogolem <- rbind(ogolem, dane)
  Sys.sleep(2)
}

write.csv2(kobiety, "kobiety.csv")
write.csv2(faceci, "faceci.csv")
write.csv2(ogolem, "ogolem.csv")

# ------
dir.create("pliki")
typ <- get_data_by_variable(as.character(v$id[1]), year = 2005:2020, unitLevel = 5) %>% select(name, year, val)
typ$n1 <- v$n1[1]
typ$n2 <- v$n2[1]
typ$n3 <- v$n3[1]
for (i in 2:nrow(v)){
  dane <- get_data_by_variable(as.character(v$id[i]), year = 2005:2020, unitLevel = 5) %>% select(name, year, val)
  dane$n1 <- v$n1[i]
  dane$n2 <- v$n2[i]
  dane$n3 <- v$n3[i]
  if (dane$n3[i]!=typ$n3[i] | i == nrow(v)){
    print(paste("zapis:", typ$n3[i]))
    write.csv2(typ, paste0("pliki\\", "typ-", typ$n3[i], ".csv"))
    typ<-dane
  } else {
    typ <- rbind(typ, dane)
    }
  Sys.sleep(2)
}
