#MED Lab 2
#Kacper Radzikowski

#Cel eksperymentów
#
#2. Najlepsza reguła powinna cechować się odpowiednio wysokim wsparciem i zaufaniem
#Samo wysokie wsparcie lub samo wysokie zaufanie nie wystarczy, gdyż możliwe są sytuacje:
# występowanie 1000 transakcji spełniających spełniają regułę A => B, oraz wystepowanie 1000 transakcji mających tylko i wyłącznie poprzednik 
#  (wsparcie może być wysokie, lecz od razu widać że reguła A => B zachodzi tylko w 50% przypadków poprzednika)
# występowanie 1 transakcji A => B daje wysokie zaufanie takiej reguły, pomimo tego, że wsparcie jest bardzo małe
#3. Odkryte reguły mogą być potencjalnie do podwyższenia przychodu w sklepie.
# Przykładowo, jeżeli jakaś reguła mówi, że klienci bardzo często kupują produkt C, jeżeli kupili również produkt B i A, to być może zmiana usytuowania produktów w sklepie tak, 
# aby odległość między produktami A, B a C była większa, dzięki czemu klienci pokonując większą drogę podczas zakupów mogą potencjalnie kupic jeszcze inne rzeczy.
# Po drugie można też rozważyć lekkie podniesienie ceny produktu C

#Eksperymenty
?Groceries
data(Groceries)
summary(Groceries)

#################################################
#1. Znalezienie reguły o największym współczynniku podniesienia, dla zaufania 0.8
aRules <-apriori(Groceries,parameter = list(support=0.001, confidence = 0.8, minlen =2, target = "rules", arem="diff", aval=TRUE))
rulesLift1.2 <- subset(aRules, subset =  lift > 1.2)
inspect(head(sort(rulesLift1.2, by="support"), 10))
#Po wykonaniu powyższych komend, pokazuje się lista pierwszych 10 wykrytych reguł o współczynniku podniesienia > 1.2, posortowanych po współczynniku podniesienia.
#Regułą rzucającą się w oczy, to reguła #8, gdyż przy wysokim współczynnku podniesienia, jej pewność wynosi 0.9 co mówi, że jest duże prawdopodobieństwo kupienia piwa butelkowanego, jeżeli kupiło się likier i czerwone wino
#8   {liquor,red/blush wine}                                    => {bottled beer}    0.001931876 0.9047619  0.8242332 11.235269
#Wsparcie tej reguły to 0.0019, czyli ok 20 transakcji.
#Dla porównania maksymalne wsparcie reguły znalezionej przez powyższy kod to 0.0032
#393 {citrus fruit,tropical fruit,root vegetables,whole milk}        => {other vegetables} 0.003152008 0.8857143  0.6922217 4.577509
inspect(head(sort(rulesLift1.2, by="support"), 1))

#grafika prezentująca wybrane regułu
plot(rulesLift1.2, shading="order", control=list(main = "Two-key plot" ))

#2. Znalezienie reguły o największym wsparciu, dla zaufania 0.8
inspect(head(sort(rulesLift1.2, by="support"), 1))

#Znaleziona reguła to 
#393 {citrus fruit,tropical fruit,root vegetables,whole milk}        => {other vegetables} 0.003152008 0.8857143  0.6922217 4.577509
#o wsparciu 0.0031 i zaufaniu 0.89

#3. Wybór reguł na podstawie wybranego wskaźka jak bardzo dana reguła jest interesująca
?interestMeasure
aRules <-apriori(Groceries,parameter = list(support=0.001, confidence = 0.8, minlen =2, target = "rules", arem="diff", aval=TRUE))
#reguły dla których współczynnik poprawy jest większy od 0,01
resTbl <- interestMeasure(aRules,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)
intersRule <- aRules[intres] 
inspect(intersRule)

#4. Wybór reguł ze współczynnikiem podniesienia większym niż 5
resTbl <- interestMeasure(aRules,"lift", asets)
intres <- which(sapply(resTbl, function(x) {x > 5})==TRUE)
intersRule <- aRules[intres] 
inspect(intersRule)

#5. Wybór reguł ze wsparciem większym od 0.0025
resTbl <- interestMeasure(aRules,"support", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.0025})==TRUE)
intersRule <- aRules[intres] 
inspect(intersRule)

#6. Reguły ze współczynnikiem podobieństwa Lermana większym niż 0.3
aRules <-apriori(Groceries,parameter = list(support=0.001, confidence = 0.5, minlen =2, target = "rules", arem="diff", aval=TRUE))
resTbl <- interestMeasure(aRules,"lerman", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.3})==TRUE)
intersRule <- aRules[intres] 
inspect(intersRule)
# Ten eksperyment zdaje się dawać najlepsze wyniki.
# Przykładowo, oprócz wykrytej wcześniej:
# 18   {liquor,red/blush wine}                                   => {bottled beer}     0.001931876 0.9047619  0.8242332 11.235269
# można zauważyć też inne ciekawe reguły, jak np.
# 37   {soda,popcorn}                                            => {salty snack}      0.001220132 0.6315789  0.5937548 16.697793
# 444  {flour,baking powder}                                     => {sugar}            0.001016777 0.5555556  0.5216969 16.408075

#7. Generowanie reguł na podstawie wcześniej utworzonego zbioru zbiorów częstych
ecParam  = new("ECparameter", "confidence" = 0.5, "support" =0.002) 
fsets <- eclat(Groceries,ecParam)
iERules = ruleInduction(fsets, Groceries, confidence = 0.8,control=list(method ="ptree"))
summary(iERules)
length(iERules)
inspect(head(iERules))

