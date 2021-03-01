
<!-- README.md is generated from README.Rmd. Please edit that file -->

## TreeGrOSSinR

Das Paket enthält Funktionen des Bestandeswachstumsimulators
[TreeGrOSS](https://www.nw-fva.de/index.php?id=477) (Tree Growth Open
Source Software). Dieser versammelt eine große Anzahl von einfachen
ertragskundlichen Funktionen in einer XML-Datei.

Für die Verwendung in R wurde die Datei `ForestSimulatorNWGermany*.xml`
geparst (ausgelesen und nach R übersetzt). Alle TreeGrOSS-Funktionen,
die ohne zusätzlichen Informationen, welche nur innerhalb des
Bestandeswachstumsimulators verfügbar sind (z.B. C66 oder geerntete
Bäume) werden hier angeboten. Insgesamt werden Funktionen für 112
Baumarten bereitgestellt, wobei die meisten Arten nicht explizit
parametrisiert wurden, sondern durch Ersetzungsregeln mit geläufigen
Baumarten abgedeckt werden.

## Installation

Das Paket kann mit dem Paket `remotes` von Github installiert werden

``` r
remotes::install_github("rnuske/TreeGrOSSinR")
```

## Nutzung

### Verfügbare TreeGrOSS-Funktionen

-   `tg_baumarten()` eine Liste aller verfügbaren Baumarten in der
    TreeGrOSS-Kollektion
-   `tg_fun_info()` zeigt die verwendete Formel, Quellenangaben und ggf.
    Ersetzungen
-   `tg_hoehe()` Baumhöhe (m) mittels Einheitshöhenkurve
-   `tg_volumen()` Derbholzvolumen (m³)
-   `tg_kronenbreite()` Kronenbreite (m) unter Annahme kreisförmiger
    Kronen
-   `tg_kronenansatz()` Höhe des Kronenansatzes (m)
-   `tg_bonitaet100()` Oberhöhenbonität (m) mit Bezugsalter 100
-   `tg_hoehenzuwachs()` Höhenzuwachs (m) für 5 Jahresperioden

### Beispiel

Zunächst das Paket laden und ein Blick in die Liste der verfügbaren
Baumarten werfen

``` r
library(TreeGrOSSinR)


# Anfang der Liste der verfügbaren Baumarten
head(tg_baumarten())
#>   code kurz      name                latein
#> 1  110   Ei     Eiche               Quercus
#> 2  211   Bu     Buche       Fagus sylvatica
#> 3  511   Fi    Fichte           Picea abies
#> 4  611  Dgl Douglasie Pseudotsuga menziesii
#> 5  711   Ki    Kiefer      Pinus sylvestris
#> 6  100  LbH  Laubholz          Angiospermae
```

Nun mal schauen, wie die Formel konkret lautet und woher die
Parameterisierung stammt (Quellenangabe). Hier werden auch Ersetzungen
mit anderen Baumarten angezeigt.

``` r
# Welche Parametrisierung wird verwendet?
tg_fun_info('Stieleiche', 'tg_hoehe')
#> $formel
#> [1] "1.3+(hg-1.3)*exp(0.14657227*(1.0-(dg/bhd)))*exp(3.78686023*((1.0/dg)-(1.0/bhd)))"
#> 
#> $quelle
#> [1] "Eiche (NAGEL 1999)"
#> 
#> $info
#> [1] "Keine Formel für 'Quercus robur (111)'. Verwende stattdessen 'Quercus (110)'."
```

Ein paar `tg_*` Funktionen ausprobieren

``` r
# Baumart mittels lateinischem Namen auswählen (Verkürzung mögl)
tg_volumen(ba='Fagus syl', bhd=52.3, h=37.8)
#> [1] 4.140625

# oder über den gewöhnlichen Namen
tg_kronenbreite(ba='Buche', bhd=20)
#> [1] 5.058341

# oder den nds. Baumartencode (NW-FVA Version)
tg_bonitaet100(ba=211, alter=150, h100=40)
#> [1] 33.67246
```

## Zitat

Die Funktionen können zitiert werden als

> Nagel, J.; Duda, H.; Hansen, J. (2006): Forest Simulator BWINPro7.
> Forst und Holz 61(10): 427-429.
>
> Nagel, J. (2021): TreeGrOSS. <https://www.nw-fva.de/index.php?id=477>

und für die Bereitstellung der Funktion in R

> Nuske, R. (2021). TreeGrOSSinR: Wachstumsfunktionen aus TreeGrOSS. R
> package version 0.1.0. <https://github.com/rnuske/TreeGrOSSinR>
