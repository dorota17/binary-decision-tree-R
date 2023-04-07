#Biblioteki do wczytania pliku xslx oraz do rysowania drzewa.
library("readxl")
library("data.tree")

#Wczytanie tabeli oraz zadeklarowanie liczby możliwych konkluzji.

tablica <- read_excel("zbior_uczacy.xlsx", col_names = TRUE)
liczba_konkluzji <- 5

#Funkcja licząca entropię całego zbioru danych, który przekazywany jest jako argument.
I <- function(tab)
{
  suma<-0
  for(i in 1:liczba_konkluzji){
    if(sum(tab[,ncol(tab)-i+1])!=0){
      suma <- suma - sum(tab[,ncol(tab)-i+1])/nrow(tab) * log2(sum(tab[,ncol(tab)-i+1])/nrow(tab))
    }
  }
  return(suma) # entropia konkluzji
}

#Funkcja licząca przyrost informacji, czyli różnicę między entropią przed podziałem a ważoną sumą entropii po podziale (I-E).
I_E <- function(a, b, ulamek1, ulamek2, tabl)
{
  war_potwierdzenie <- c()
  for(i in 1:liczba_konkluzji){
    war_potwierdzenie[i] <- nrow(a[a[,ncol(a)-i+1] == "1",])/nrow(a)
  }
  e1 = 0
  for(i in 1:length(war_potwierdzenie)){
    if (war_potwierdzenie[i] != "0") {
      e1 = e1 - (war_potwierdzenie[i] * log(war_potwierdzenie[i],2))
    }
  }
  war_zaprzeczenie <- c()
  for(i in 1:liczba_konkluzji){
    war_zaprzeczenie[i] <- nrow(b[b[,ncol(b)-i+1] == "1",])/nrow(b)
  }
  e2 = 0
  for(i in 1:length(war_zaprzeczenie)){
    if (war_zaprzeczenie[i] != "0") {
      e2 = e2 - (war_zaprzeczenie[i] * log(war_zaprzeczenie[i],2))
    }
  }
  E = e1 * ulamek1 + e2 * ulamek2
  return (przyrost <- (I(tabl) - E))
}

#Funkcja licząca przyrost informacji dla każdej przesłanki.
wektor_I_E<- function(tab){
  przyrosty<-c()
  for (i in 1:(ncol(tab)-liczba_konkluzji)){
    if(sum(tab[,i])==nrow(tab) || sum(tab[,i]) == 0){
      przyrosty <- append(przyrosty, 0)
    }
    else {
      ulamek1 <- nrow(tab[tab[,i] == "1",])/nrow(tab)
      a <- subset(tab, tab[,i]=="1")
      ulamek2 <- nrow(tab[tab[,i] != "1",])/nrow(tab)
      b <- subset(tab, tab[,i]!="1")
      przyrosty <- append(przyrosty, I_E(a, b, ulamek1, ulamek2, tab))
    }
  }
  return(przyrosty)
}

#Funkcja sprawdzająca dla jakiej przesłanki przyrost informacji jest największy i dzieląca na tej podstawie tabelę oraz tworząca drzewo decyzyjne. Działa rekurencyjnie. Najpierw sprawdza czy tabela nie jest jednorodna - czy w ogóle trzeba ją jeszcze dzielić. Jeżeli nie to rysowany jest liść drzewa.

podzial <- function(drzewo, tab){
  
  czy_nie_dzielic <- FALSE
  for(i in 1:liczba_konkluzji){
    if(nrow(tab[tab[ncol(tab)-i+1] == "1",])/(nrow(tab)) == 1){
      child <- drzewo$AddChild(colnames(tab[ncol(tab)-i+1]))
      drzewo$przeslanka <- "wynik"
      czy_nie_dzielic <- TRUE
      break
    }
  }
  
  if(czy_nie_dzielic == FALSE){
    entropie_w_kolumnach <- wektor_I_E(tab)
    max <- which.max(entropie_w_kolumnach)
    tabele <- list()
    
    drzewo$przeslanka <- colnames(tab[,max])
    
    tabele[[1]] <- subset(tab, tab[,max]=="1")
    tabele[[2]] <- subset(tab, tab[,max]!="1")
    
    for(i in 1:length(tabele)){
      if(i==1){
        child <- drzewo$AddChild("1")
      } else{
        child <- drzewo$AddChild("0")
      }
      podzial(child, tabele[[i]])
    }
  }
}

#Tworzenie drzewa, na podstawie zbioru uczącego.

drzewo <- Node$new("wybór zwierzaka")
podzial(drzewo, tablica)
print(drzewo, 'przeslanka')

#Funkcja do tworzenia predykcji, na podstawie podanych wartości przesłanek.

Predykcja <- function(drzewo, przeslanki) {
  if (drzewo$children[[1]]$isLeaf) return (drzewo$children[[1]]$name)
  child <- drzewo$children[[przeslanki[[drzewo$przeslanka]]]]
  return (Predykcja(child, przeslanki))
}

Predykcja(drzewo, c(alergia_na_siersc='1', brak_alergii_na_siersc = '0', 
                    osoba_aktywna = '0', osoba_nieaktywna = '0', osoba_srednio_aktywna = '1', 
                    fundusze_ponizej_100 = '1', fundusze_100_200 = "0", fundusze_201_300 = '0', fundusze_powyzej_300 = '0', 
                    czas_ponizej_1h_dziennie = '1', czas_1_2h_dziennie = '0', czas_powyzej_2h_dziennie = '0', 
                    oczek_dl_zycia_ponizej_5_l = '0', oczek_dl_zycia_5_15_l= '0', oczek_dl_zycia_powyzej_15_l = '1'))

Predykcja(drzewo, c(alergia_na_siersc='1', brak_alergii_na_siersc = '0', 
                    osoba_aktywna = '0', osoba_nieaktywna = '0', osoba_srednio_aktywna = '1', 
                    fundusze_ponizej_100 = '0', fundusze_100_200 = "1", fundusze_201_300 = '0', fundusze_powyzej_300 = '0', 
                    czas_ponizej_1h_dziennie = '0', czas_1_2h_dziennie = '1', czas_powyzej_2h_dziennie = '0', 
                    oczek_dl_zycia_ponizej_5_l = '0', oczek_dl_zycia_5_15_l = '1', oczek_dl_zycia_powyzej_15_l = '0'))

Predykcja(drzewo, c(alergia_na_siersc='0', brak_alergii_na_siersc = '1', 
                    osoba_aktywna = '1', osoba_nieaktywna = '0', osoba_srednio_aktywna = '0', 
                    fundusze_ponizej_100 = '0', fundusze_100_200 = "1", fundusze_201_300 = '0', fundusze_powyzej_300 = '0', 
                    czas_ponizej_1h_dziennie = '0', czas_1_2h_dziennie = '1', czas_powyzej_2h_dziennie = '0', 
                    oczek_dl_zycia_ponizej_5_l = '1', oczek_dl_zycia_5_15_l = '0', oczek_dl_zycia_powyzej_15_l = '0'))

Predykcja(drzewo, c(alergia_na_siersc='0', brak_alergii_na_siersc = '1', 
                    osoba_aktywna = '0', osoba_nieaktywna = '0', osoba_srednio_aktywna = '1', 
                    fundusze_ponizej_100 = '0', fundusze_100_200 = "0", fundusze_201_300 = '0', fundusze_powyzej_300 = '1', 
                    czas_ponizej_1h_dziennie = '0', czas_1_2h_dziennie = '0', czas_powyzej_2h_dziennie = '1', 
                    oczek_dl_zycia_ponizej_5_l = '0', oczek_dl_zycia_5_15_l = '0', oczek_dl_zycia_powyzej_15_l = '1'))

