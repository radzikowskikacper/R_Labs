#MED 2017Z Lab7 grupowanie
#Kacper Radzikowski

#Zadanie Grupowanie zbioru pokemonów. Znalezienie najlepszego grupowania.
#Grupwanie należy ocenić wg następujących kryteriów:
#1. Łączej wewnątrzgrupowej sumy kwadratów odległości punktów od środka grupy (im mniejsza tym lepsza)
#2. Czystości grup - średnia liczba typów pokemonów w grupie (typ jest reprezentowany w grupie jeśli ma przynajmniej trzech przedstawicieli) im mniejsza tym lepsza
#W obu przypadkach im mniej grup typ lepiej

#opis: https://www.kaggle.com/abcsds/pokemon
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/Pokemon.csv','Pokemon.csv')
pokemon <- read.csv("Pokemon.csv")
summary(pokemon)

################# BADANIE ALGORYTMU KMEANS #################
#1. Łączna wewnątrzgrupowa suma kwadratów odległości od środka
#1.a) Wszystkie kolumny oprócz typów, nazwy, 'legendarności', oraz numeru porządkowego
wss = wss <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  a <- kmeans(subset(pokemon, select = -c(X., Type.1, Type.2, Name, Legendary)), centers = i, nstart=20, iter.max = 100)
  wss[i] <- a$tot.withinss
  print(table(pokemon$Type.1, a$cluster))
  print(wss[i])
}

plot(1:length(unique(pokemon$Type.1)), wss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Całkowita suma odległości wewnątrzgrupowych")

#Najlepszy wynik 1709049 dla liczby grup = 18

#1.b) Kolumny 7-10, (Attack, Defense, Special Attack, Special Defense)
wss = wss <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  a <- kmeans(pokemon[, c(7:10)], centers = i, nstart=20, iter.max = 100)
  wss[i] <- a$tot.withinss
  print(table(pokemon$Type.1, a$cluster))
  print(wss[i])
}

plot(1:length(unique(pokemon$Type.1)), wss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Całkowita suma odległości wewnątrzgrupowych")

#Najlepszy wynik 611128 dla 18 grup

#1.c) Kolumny 5:6, (Total, HP)
wss = wss <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  a <- kmeans(pokemon[, c(5:6)], centers = i, nstart=20, iter.max = 100)
  wss[i] <- a$tot.withinss
  print(table(pokemon$Type.1, a$cluster))
  print(wss[i])
}

plot(1:length(unique(pokemon$Type.1)), wss, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Całkowita suma odległości wewnątrzgrupowych")

#Najlepszy wynik 252232 dla 18 grup
#Jest to zarazem najlepszy wynik uzyskany względem tej miary.

#2. Czystość grup
#2.a) Wszystkie kolumny oprócz typów, nazwy, 'legendarności', oraz numeru porządkowego
purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  a <- kmeans(subset(pokemon, select = -c(Type.1, Type.2, Name, Legendary)), centers = i, nstart=20)
  temp = vector(mode = 'integer', length = i)
  for (j in 1:i){
    temp[j] = sum(table(pokemon[a$cluster == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 7.16 dla 18 grup jest najlepszy

#2.b) Kolumny 7-10, (Attack, Defense, Special Attack, Special Defense)
purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  a <- kmeans(pokemon[, c(7:10)], centers = i, nstart=20)
  temp = vector(mode = 'integer', length = i)
  for (j in 1:i){
    temp[j] = sum(table(pokemon[a$cluster == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 6.11 dla 18 grup jest najlepszy

#2.c) Kolumny 5-6, (Total, HP)
purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  a <- kmeans(pokemon[, c(5:6)], centers = i, nstart=20)
  temp = vector(mode = 'integer', length = i)
  for (j in 1:i){
    temp[j] = sum(table(pokemon[a$cluster == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 6.5 dla 18 grup jest najlepszy

################# BADANIE ALGORYTMU HIERARCHICZNEGO #################
#2. Czystość grup
#2.a) Wszystkie kolumny oprócz typów, nazwy, 'legendarności', oraz numeru porządkowego
d<-dist(pokemon[,-c(1, 2,3,4,13)],method="euclidean")
fit<-hclust(d, method = 'average')
plot(fit,main="Average Linkage",col.main="green",xlab="average linkage",ylab="height")

purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  temp = vector(mode = 'integer', length = i)
  pok<-cutree(fit,k=i)
  
  for (j in 1:i){
    temp[j] = sum(table(pokemon[pok == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 3.39 dla 18 grup jest najlepszy

#2.b) Kolumny 7-10, (Attack, Defense, Special Attack, Special Defense)
d<-dist(pokemon[,c(7:10)],method="euclidean")
fit<-hclust(d, method = 'average')
plot(fit,main="Average Linkage",col.main="green",xlab="average linkage",ylab="height")

purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  temp = vector(mode = 'integer', length = i)
  pok<-cutree(fit,k=i)
  
  for (j in 1:i){
    temp[j] = sum(table(pokemon[pok == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 2.75 dla 14 grup jest najlepszy

#2.c) Kolumny 5-6, (Total, HP)
d<-dist(pokemon[,c(5,6)],method="euclidean")
fit<-hclust(d, method = 'average')
plot(fit,main="Average Linkage",col.main="green",xlab="average linkage",ylab="height")

purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  temp = vector(mode = 'integer', length = i)
  pok<-cutree(fit,k=i)
  
  for (j in 1:i){
    temp[j] = sum(table(pokemon[pok == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 4.16 dla 16 grup jest najlepszy

#2.d) Kolumny 7-11
d<-dist(pokemon[,c(7:11)],method="euclidean")
fit<-hclust(d, method = 'average')
plot(fit,main="Average Linkage",col.main="green",xlab="average linkage",ylab="height")

purity <- vector(mode = "integer" ,length = length(unique(pokemon$Type.1)))

for (i in 1:length(unique(pokemon$Type.1))){
  temp = vector(mode = 'integer', length = i)
  pok<-cutree(fit,k=i)
  
  for (j in 1:i){
    temp[j] = sum(table(pokemon[pok == j, c('Type.1')]) > 2)
  }
  purity[i] <- mean(temp)
  print(purity[i])
}

plot(1:length(unique(pokemon$Type.1)), purity, type = "b", 
     xlab = "Liczba grup", 
     ylab = "Czystość")

#Wynik 2.5 dla 13 grup jest najlepszy
#Wnioski
#Dla algorytmu kmeans, im większa zadana ilość grup, tym lepsze grupowanie uzyskujemy, zarówno w sensie miary łącznej wewnątrzgrupowej sumy kwadratów odległości, jak i w sensie miary czystości grup.
#To zachowanie ma miejsce w każdym z 3 przykładowych zestawów cech.

#Jeżeli chodzi o algorytm DBSCAN, zależnie od wyboru jednego z 3 zestawów cech, udawało mi się uzyskać lepszy wynik dla 14 grup niż dla 18, lub 2.5 dla 13 grup w sensie czystości grup.