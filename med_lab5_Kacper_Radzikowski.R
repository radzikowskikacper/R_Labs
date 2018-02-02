#MED Lab 5
#Kacper Radzikowski

#1. Cel eksperymentów
#1.1. W zbiorze cars atrybutem klasy jest atrybut category.
#     Posiada on 4 klasy: unacc, acc, good, vgood
#1.3. Potencjalnym praktycznym zastosowaniem klasyfikatora zbudowanego z użyciem tego zbioru,
#     może być np. oszacowanie jak jak dobry jest podany samochód, np. podczas kupna auta.
#1.2. Z uwagi na to, że chcemy uzyskać klasyfikator, który ma służyć do oceny, czy dane auto jest dobre,
#     nie chcemy aby klasyfikator klasyfikował złe auta, jako 'bardzo dobre'.
#     Miarą oceny jakości klasyfikatora jaką zastosujemy w tym przypadku, będzie jego precyzja dla klasy 'vgood'.
#     Dokładność klasyfikatora w tym przypadku ma drugorzędne znaczenie, ponieważ interesuje nas tylko klasa 'vgood'.
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')
cars = read.csv("car.data", header = FALSE, col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category") )
summary(cars)

idTrainData <- unlist(createDataPartition(cars$category,p=0.7))
traincars <- cars[idTrainData,]
testcars <- cars[-idTrainData,]
#Sprawdzimy czy oba zbiory mają reprezentatywną przykładów ilość każdej z klas.
table(traincars$category)
table(testcars$category)

print_measures <- function(trainset, testset, trainsetPred, testsetPred){
  #macierz pomyłek na zbiorze trenującym
  print(table(trainset$category,trainsetPred))
  #precyzja na zbiorze trenującym, dla klasy vgood
  print(sum(trainsetPred == trainset$category & trainsetPred == 'vgood') / sum(trainsetPred == 'vgood'))
  #dokładność na zbiorze trenującym
  print(mean(trainsetPred == trainset$category))
  #macierz pomyłek dla zbioru testowego
  print(table(testset$category,testsetPred))
  #precyzja na zbiorze testowym dla klasy vgood
  print(sum(testsetPred == testset$category & testsetPred == 'vgood') / sum(testsetPred == 'vgood'))
  #dokładność na zbiorze testowym
  print(mean(testsetPred == testset$category))
}

#######################################
myFormula <- category ~ buying + maint + doors + persons + lug_boot + safety

#Próba znalezienia najlepiej klasyfikującego drzewa
rpControl = rpart.control(minbucket = 25, cp = 0.01, 
                          maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                          surrogatestyle = 0, maxdepth = 30);
rpTree <- rpart(myFormula,  method="class", data=traincars,
                control =rpControl,
                parms = list(split = "information" ))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   237    0    26     6
#good   28    0     0    21
#unacc  65    0   778     4
#vgood  11    0     0    35
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.530303
#[1] 0.867052
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   101    0    10     4
#good   11    0     0     9
#unacc  25    0   338     0
#vgood   2    0     0    17
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 0.5666667
#[1] 0.8820116

rpControl = rpart.control(minbucket = 10, maxDepth = 5);
rpTree <- rpart(myFormula,  method="class", data=traincars,
                control =rpControl,
                parms = list(split = "information" ))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   255    8     5     1
#good    8   36     0     5
#unacc  34    4   809     0
#vgood  11    0     0    35
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.8536585
#[1] 0.9372419
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   106    5     2     2
#good    5   11     0     4
#unacc  11    0   352     0
#vgood   2    0     0    17
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 0.7391304
#[1] 0.9400387

rpTree <- rpart(myFormula,  method="class", data=traincars)
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   254    9     5     1
#good    0   44     0     5
#unacc  34    4   809     0
#vgood  11    0     0    35
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.8536585
#[1] 0.9430223
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   104    7     2     2
#good    0   16     0     4
#unacc  11    0   352     0
#vgood   2    0     0    17
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 0.7391304
#[1] 0.9458414

#Próby użycia macierzy kosztów pomyłek
lossM=matrix(c(0,0,0,1, 0,0,0,1, 0,0,0,1, 1,1,1,0), byrow=TRUE, nrow=4)
rpTree <- rpart(myFormula,  method="class", data=traincars,
                parms = list(loss = lossM))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   265    0     0     4
#good   44    0     0     5
#unacc 847    0     0     0
#vgood   0    0     0    46
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.8363636
#[1] 0.2568126
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   113    0     0     2
#good   16    0     0     4
#unacc 363    0     0     0
#vgood   0    0     0    19
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 0.76
#[1] 0.2553191

lossM=matrix(c(0,0,0,10, 0,0,0,10, 0,0,0,10, 10,10,1,0), byrow=TRUE, nrow=4)
rpTree <- rpart(myFormula,  method="class", data=traincars,
                parms = list(loss = lossM))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   217    0    52     0
#good   44    0     5     0
#unacc 844    0     3     0
#vgood   0    0    24    22
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 1
#[1] 0.1998348
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc    85    0    30     0
#good   16    0     4     0
#unacc 362    0     1     0
#vgood   0    0     9    10
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 1
#[1] 0.1856867

lossM=matrix(c(0,0,10,10, 0,0,0,10, 0,0,0,10, 10,10,1,0), byrow=TRUE, nrow=4)
rpTree <- rpart(myFormula,  method="class", data=traincars,
                parms = list(loss = lossM))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   265    0     1     3
#good   44    0     5     0
#unacc 847    0     0     0
#vgood   0    0    13    33
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.9166667
#[1] 0.2460776
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   113    0     2     0
#good   16    0     4     0
#unacc 363    0     0     0
#vgood   0    0     7    12
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 1
#[1] 0.2417795

lossM=matrix(c(0,10,10,10, 10,0,0,10, 0,0,0,10, 10,10,1,0), byrow=TRUE, nrow=4)
rpTree <- rpart(myFormula,  method="class", data=traincars,
                parms = list(loss = lossM))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   261    4     1     3
#good    4   40     5     0
#unacc 845    2     0     0
#vgood   0    0    13    33
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.9166667
#[1] 0.2758051
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   108    5     2     0
#good    3   13     4     0
#unacc 363    0     0     0
#vgood   0    0     7    12
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 1
#[1] 0.2572534

lossM=matrix(c(0,10,10,10, 10,0,0,10, 1,1,0,10, 10,10,1,0), byrow=TRUE, nrow=4)
rpTree <- rpart(myFormula,  method="class", data=traincars,
                parms = list(loss = lossM))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   256    4     6     3
#good    0   28    21     0
#unacc 110    0   737     0
#vgood   0    0    35    11
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.7857143
#[1] 0.8521883
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   106    5     4     0
#good    0   11     9     0
#unacc  40    0   323     0
#vgood   0    0    17     2
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 1
#[1] 0.8549323

lossM=matrix(c(0,10,10,10, 10,0,0,10, 1,1,0,10, 10,10,2,0), byrow=TRUE, nrow=4)
rpTree <- rpart(myFormula,  method="class", data=traincars,
                parms = list(loss = lossM))
trainPred = predict(rpTree,traincars,type = "class")
testPred = predict(rpTree,testcars,type = "class")
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   256    4     6     3
#good    0   28    21     0
#unacc 110    0   737     0
#vgood   0    0    13    33
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.9166667
#[1] 0.8703551
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc   106    5     4     0
#good    0   11     9     0
#unacc  40    0   323     0
#vgood   0    0     7    12
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 1
#[1] 0.8742747

plot(pRpTree, uniform=TRUE,     main="Classification for cars")
text(pRpTree, use.n=TRUE, all=TRUE, cex=.6)

######################################
#Próby z Naiwnym klasyfikatorem Bayesa
?naiveBayes
nbClasif <- naiveBayes(myFormula, data=traincars, laplace = 0)
trainPred = predict(nbClasif,traincars)
testPred = predict(nbClasif,testcars)
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   204    5    60     0
#good   29   19     0     1
#unacc  37    2   808     0
#vgood  17    1     0    28
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.9655172
#[1] 0.8744839
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc    86    8    21     0
#good   13    5     1     1
#unacc  10    0   353     0
#vgood   7    2     0    10
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 0.9090909
#[1] 0.8781431

nbClasif <- naiveBayes(myFormula, data=traincars, laplace = 2)
trainPred = predict(nbClasif,traincars)
testPred = predict(nbClasif,testcars)
print_measures(traincars, testcars, trainPred, testPred)

#Macierz pomyłek dla zbioru trenującego
#trainPred
#      acc good unacc vgood
#acc   198    2    69     0
#good   33   14     1     1
#unacc  34    0   813     0
#vgood  26    1     0    19
#Uzyskana precyzja i dokładność na zbiorze trenującym wynoszą
#[1] 0.95
#[1] 0.8620974
#Macierz pomyłek dla zbioru testowego
#testPred
#      acc good unacc vgood
#acc    85    7    23     0
#good   15    4     1     0
#unacc   9    0   354     0
#vgood  11    2     0     6
#Uzyskana precyzja i dokładność na zbiorze testowym wynoszą
#[1] 1
#[1] 0.868472

#3. Wnioski
#Zarówno dla klasyfikatorów opartych o drzewa decyzyjne, jak i dla klasyfikatora Bayesowskiego, 
#udało mi się osiągnąć wysokie wartości precyzji dla klasy 'vgood'.
#Oba klasyfikatory mają też dość wysoką dokładność.