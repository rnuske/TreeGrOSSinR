#==============================================================================
#
#  TreeGrOSS Collection
#
#  Functions from ForestSimulatorNWGermany6.xml
#  Parsed and preprocessed to a data.frame with data-raw/prepare_treegross_XML.R
#
#  Started by Robert Nuske 2016-03-31
#  ins Deutsche übertragen 2020-05-15 (RSN)
#==============================================================================

# The spf data.frame is available as internal dataset R/sysdata.rda
# It's the parsed version of ForestSimulatorNWGermany*.xml
# preprocessing done with data-raw/prepare_treegross_XML.R


#------------------------------------------------------------------------------
# TreeGrOSS Parameterbeschreibung
#------------------------------------------------------------------------------

#' Alle Parameter der TreeGrOSS-Kollektion
#'
#' Diese Hilfeseite existiert hauptsächlich damit alle `tg_`-Funktionen
#' einheitliche Parameterbeschreibungen erhalten. Technisch erfolgt dies in
#' roxygen durch Vererbung (`"@inheritParams"`).
#'
#' @md
#' @name tg_parameter
#' @rdname tg_parameter
#' @keywords internal
#'
#' @param ba Name der Baumart (Kürzel, deutscher Name, lateinischer Name) oder
#'   Baumartencode (nach niedersächsischer Verschlüsselung). Nur ein Wert.
#'   Für vollständige Liste siehe [tg_baumarten()].
#' @param bhd numerischer Vektor der Brusthöhendurchmesser in Zentimeter.
#'   Einer oder mehrere.
#' @param h numerischer Vektor der Baumhöhen in Meter (vgl. [tg_hoehe()]).
#'   Eine oder mehrere.
#' @param alter numerischer Vektor der Baumalter in Jahren. Eines oder mehrere.
#' @param bon100 numerischer Vektor mit Oberhöhenbonitäten in Meter. Oberhöhe
#'   zum Bezugsalter 100 (vgl. [tg_bonitaet100()]). Eine oder mehrere.
#'
#' @param dg Durchmesser des Grundflächenmittelstammes der betrachteten Kohorte,
#'   z.B. Baumart oder Stichprobenpunkt, in Zentimeter. Nur ein Wert.
#' @param hg Höhe des Grundflächenmittelstammes der betrachteten Kohorte,
#'   z.B. Baumart oder Stichprobenpunkt, in Meter. Nur ein Wert.
#' @param h100 Oberhöhe, Höhe des Grundflächenmittelstammes der
#'   100 stärksten Bäume je Hektar der betrachteten Kohorte, z.B. Baumart oder
#'   Stichprobenpunkt, in Meter. Nur ein Wert.
#'
#' @param fun Name einer TreeGrOSS Funktion. Eine von `"tg_hoehe"`,
#'   `"tg_volumen"`, `"tg_kronenbreite"`, `"tg_kronenansatz"`,
#'   `"tg_bonitaet100"`, `"tg_hoehenzuwachs"`
#'
#' @param info boolescher Wert. Sollen begleitende Informationen ausgegeben
#'   werden? Für weitere Details siehe [tg_fun_info()].
#'
#' @param TestParameter Sollte auf keiner der tg-Hilfeseiten sichtbar sein.
#'
#'
#' @references
#' Nagel, J.; Duda, H.; Hansen, J. (2006): Forest Simulator BWINPro7.
#'  Forst und Holz 61(10): 427-429.
#'
#' Nagel, J. (2021): TreeGrOSS. \url{https://www.nw-fva.de/index.php?id=477}.
NULL


#------------------------------------------------------------------------------
# TreeGrOSS Funktionen
#------------------------------------------------------------------------------

#' Baumarten der TreeGrOSS-Kollektion
#'
#' Listet alle in der TreeGrOSS-Kollektion verfügbaren Baumarten. Angegeben
#' sind Baumartencode, Kürzel, deutscher und lateinischer Name.
#' Jede dieser Angaben kann in allen `tg_`-Funktionen verwendet werden, um eine
#' Baumart auszuwählen. Die Suche nach der passenden Baumart erfolgt in
#' der oben genannten Reihenfolge.
#'
#' @return Ein Dataframe mit allen in der TreeGrOSS-Kollektion verfügbaren
#'   Baumarten mit den Spalten `code`, `kurz`, `name`, `latein`.
#'
#' @inherit tg_parameter references
#'
#' @seealso [tg_hoehe()], [tg_volumen()], [tg_kronenbreite()],
#'   [tg_kronenansatz()], [tg_bonitaet100()], [tg_hoehenzuwachs()],
#'   [tg_fun_info()]
#'
#' @examples
#' tg_baumarten()
#'
#' @md
#' @export

tg_baumarten <- function(){
  xx <- spf[, 1:4]
  colnames(xx) <- c("code", "kurz", "name", "latein")
  return(xx)
}


#' Infos über Funktionen der TreeGrOSS-Kollektion
#'
#' Zeigt die Formel, einen Hinweis zur Quelle und Informationen über
#' baumartenweise Ersatz der Formel, denn nicht alle Baumarten haben eigene
#' Formeln. Manchmal wird die Formel einer anderen Baumart von TreeGrOSS als
#' Ersatz für die angefragte Baumart verwendet. Dies wird in der Regel im
#' `info`-Abschnittvermerkt.
#'
#' In der TreeGrOSS XML-Datei werden Ersatzformeln auf zwei Arten definiert.
#' Erstens durch Angabe einer Referenz auf eine andere Art (z.B.
#' `HandledLikeCode 211`). Alle `tg_`-Funktionen werten diese Referenzen aus
#' und teilen die Ersetzung im `info`-Abschnitt mit. Zweitens durch Kopieren und
#' Einfügen der Formel von einer Baumart zu einer anderen Baumart. In diesem
#' Fall kann die Ersetzung im `quelle`-Abschnitt sichtbar sein (z.B.
#' `Eiche (NAGEL 1999)`) oder auch nicht.
#'
#' @inheritParams tg_parameter
#' @inherit tg_parameter references
#'
#' @return Ein Listen-Objekt mit den Elementen `formel`, `quelle` und `info`,
#'   welche die verwendete Formel, die Quellenangabe (Zitat) und ggf.
#'   Informationen über einen Ersatz enthält.
#'
#' @seealso [tg_hoehe()], [tg_volumen()], [tg_kronenbreite()],
#'  [tg_kronenansatz()], [tg_bonitaet100()], [tg_hoehenzuwachs()],
#'  [tg_baumarten()]
#'
#' @examples
#' tg_fun_info(ba=211, fun="tg_volumen")
#' tg_fun_info(ba="Traubeneiche", fun="tg_hoehenzuwachs")
#'
#' @md
#' @export

tg_fun_info <- function(ba, fun){
  if(missing(ba) | missing(fun))
    stop("Baumart (ba) und Funktionsname (fun) m\u00fcssen angegeben werden.")

  spec.code <- get.spec.code(ba)

  fun <- switch(fun,
                tg_hoehe          = "UniformHeightCurveXML",
                tg_volumen        = "VolumeFunctionXML",
                tg_kronenbreite   = "Crownwidth",
                tg_kronenansatz   = "Crownbase",
                tg_bonitaet100    = "SiteIndex",
                tg_hoehenzuwachs  = "HeightIncrement",
                stop(paste("Funktion", sQuote(fun), "existiert nicht."))
  )

  return(get.formula(spec.code, fun))
}


#' Baumhöhen aus Einheitshöhenkurve (TreeGrOSS)
#'
#' Baumhöhen aus baumartenspezifischen Einheitshöhenkurven mit Parametern aus
#' TreeGrOSS.
#'
#' Die Funktion nutzt das Element `"UniformHeightCurveXML"` aus der
#' TreeGrOSS-XML-Datei `ForestSimulatorNWGermany*.xml` (Nagel, 2006, 2016).
#' Sie wurde für Nordwestdeutschland parametrisiert.
#'
#' @inheritParams tg_parameter
#' @inherit tg_parameter references
#'
#' @return Ein numerischer Vektor der Baumhöhen in Meter, wenn `info=FALSE`.
#'   Ansonsten ein Listen-Objekt mit den Elementen `werte`, `formel`,
#'   `quelle` und `info`, welche ein vektor der Baumhöhen, die verwendete
#'   Formel, die Quellenangabe (Zitat) und ggf. Informationen über einen
#'   Ersatz enthält.
#'
#' @seealso [tg_volumen()], [tg_kronenbreite()], [tg_kronenansatz()],
#'   [tg_bonitaet100()], [tg_hoehenzuwachs()], [tg_fun_info()], [tg_baumarten()]
#'
#' @examples
#' tg_hoehe(ba=211, bhd=32, hg=29.3, dg=29.4)
#' tg_hoehe(511, bhd=12:16, hg=19, dg=17.5)
#' tg_hoehe(ba="Stieleiche", bhd=60, hg=33.4, dg=63.2, info=TRUE)
#'
#' @md
#' @export

tg_hoehe <- function(ba, bhd, dg, hg, info=FALSE){

  if(missing(ba) | missing(bhd) | missing(hg) | missing(dg))
    stop('ba, bhd, hg und dg m\u00fcssen angegeben werden.')
  if(length(ba) > 1){
    ba <- ba[1]
    warning('Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  }
  if(length(dg)+length(hg) > 2){
    dg <- dg[1]
    hg <- hg[1]
    warning('Die betrachtete Kohorte kann nur ein dg und ein hg haben. ',
            'Verwende jeweils das erste Element.')
  }

  spec.code <- get.spec.code(ba)
  res <- get.formula(spec.code, 'UniformHeightCurveXML')
  number <- eval(parse(text=res$formel))

  if(info){
    res <- append(res, list(werte=number), 0)
  } else {
    res <- number
  }
  return(res)
}

#' Derbholzvolumen (TreeGrOSS)
#'
#' Volumen der oberirdischen Holzmasse ab 7cm Durchmesser in Kubikmeter.
#'
#' Die Funktion nutzt das Element `"VolumeFunctionXML"` aus der
#' TreeGrOSS-XML-Datei `ForestSimulatorNWGermany*.xml` (Nagel, 2006, 2016).
#' Sie wurde für Nordwestdeutschland parametrisiert.
#'
#' @inheritParams tg_parameter
#' @inherit tg_parameter references
#'
#' @return Ein numerischer Vektor der Derbholzvolumen in Kubikmeter, wenn
#'   `info=FALSE`. Ansonsten ein Listen-Objekt mit den Elementen `werte`,
#'   `formel`, `quelle` und `info`, welche ein Vektor der Derbholzvolumen, die
#'   verwendete Formel, die Quellenangabe (Zitat) und ggf. Informationen über
#'   einen Ersatz enthält.
#'
#' @seealso [tg_hoehe()], [tg_kronenbreite()], [tg_kronenansatz()],
#'   [tg_bonitaet100()], [tg_hoehenzuwachs()], [tg_fun_info()], [tg_baumarten()]
#'
#' @examples
#' tg_volumen(ba='Fagus syl', bhd=52.3, h=37.8)
#' tg_volumen(511, c(15.5, 23.0, 30.7, 37.6), c(16.6, 24.7, 29.7, 33.3))
#' tg_volumen('Alnus incana', bhd=29.0, h=24.7, info=TRUE)
#'
#' @md
#' @export

tg_volumen <- function(ba, bhd, h, info=FALSE){
  if(missing(ba) | missing(bhd) | missing(h))
    stop('ba, bhd, h m\u00fcssen angegeben werden.')
  if(length(bhd) != length(h))
    stop('bhd und h m\u00fcssen gleich lang sein.')
  if(length(ba) > 1){
    ba <- ba[1]
    warning('Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  }

  spec.code <- get.spec.code(ba)
  res <- get.formula(spec.code, "VolumeFunctionXML")
  number <- eval(parse(text=res$formel))

  if(info){
    res <- append(res, list(werte=number), 0)
  } else {
    res <- number
  }
  return(res)
}


#' Kronenbreite (TreeGrOSS)
#'
#' Durchmesser der Baumkrone in Abhängigkeit vom Brusthöhendurchmesser.
#' Die Erlenarten (Code 420-423) benötigen die zusätzlich Parameter Baumhöhe `h`
#' und Kronenansatz `ka` (vgl. [tg_kronenansatz()]). Es wird angenommen,
#' dass die  Kronen kreisförmig sind.
#'
#' Die Funktion nutzt das Element `"Crownwidth"` aus der TreeGrOSS-XML-Datei
#' `ForestSimulatorNWGermany*.xml` (Nagel, 2006, 2016). Sie wurde für
#' Nordwestdeutschland parametrisiert.
#'
#' @inheritParams tg_parameter
#' @param ... um für die Erlenarten (420-423) die zusätzlichen Parameter
#'   `h` und `ka`zu übergeben.
#'
#' @inherit tg_parameter references
#'
#' @return Ein numerischer Vektor der Kronenbreiten in Meter, wenn
#'   `info=FALSE`. Ansonsten ein Listen-Objekt mit den Elementen `werte`,
#'   `formel`, `quelle` und `info`, welche ein Vektor der Kronenbreiten, die
#'   verwendete Formel, die Quellenangabe (Zitat) und ggf. Informationen über
#'   einen Ersatz enthält.
#'
#' @seealso [tg_hoehe()], [tg_volumen()], [tg_kronenansatz()],
#'   [tg_bonitaet100()], [tg_hoehenzuwachs()], [tg_fun_info()], [tg_baumarten()]
#'
#' @examples
#' tg_kronenbreite(211, 20)
#' tg_kronenbreite('Buche', 20, info=TRUE)
#' tg_kronenbreite('Alnus incana', bhd=40, h=29, ka=10.2)
#'
#' @md
#' @export

tg_kronenbreite <- function(ba, bhd, ..., info=FALSE){

  # bring the parameter passed via ... to the environment of the function
  list2env(list(...), envir=environment())

  if(missing(ba) | missing(bhd))
    stop('ba und bhd m\u00fcssen angegeben werden.')
  if(length(ba) > 1){
    ba <- ba[1]
    warning('Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  }

  spec.code <- get.spec.code(ba)

  if((spec.code >= 420 & spec.code <= 423) & (!exists('h') | !exists('ka')))
    stop("F\u00fcr die Erlen Arten m\u00fcssen zus\u00e4tzlich h und ka ",
         "angegeben werden.")

  res <- get.formula(spec.code, "Crownwidth")
  number <- eval(parse(text=res$formel))

  if(info){
    res <- append(res, list(werte=number), 0)
  } else {
    res <- number
  }
  return(res)
}


#' Kronenansatzhöhe (TreeGrOSS)
#'
#' Höhe des untersten Teils der lebenden Krone (z.B. unterster Grünast oder
#' unterster Quirl mit drei grünen Ästen).
#'
#' Die Funktion nutzt das Element `"Crownbase"` aus der TreeGrOSS-XML-Datei
#' `ForestSimulatorNWGermany*.xml` (Nagel, 2006, 2016). Sie wurde für
#' Nordwestdeutschland parametrisiert.
#'
#' @inheritParams tg_parameter
#' @inherit tg_parameter references
#'
#' @return Ein numerischer Vektor der Kronenansatzhöhen in Meter, wenn
#'   `info=FALSE`. Ansonsten ein Listen-Objekt mit den Elementen `werte`,
#'   `formel`, `quelle` und `info`, welche ein Vektor der Kronenansatzhöhen,
#'   die verwendete Formel, die Quellenangabe (Zitat) und ggf. Informationen
#'   über einen Ersatz enthält.
#'
#' @seealso [tg_hoehe()], [tg_volumen()], [tg_kronenbreite()],
#'  [tg_bonitaet100()], [tg_hoehenzuwachs()], [tg_fun_info()], [tg_baumarten()]
#'
#' @examples
#' tg_kronenansatz(211, bhd=44, h=36, h100=38)
#' tg_kronenansatz(111, bhd=34, h=26, h100=28, info=TRUE)
#' tg_kronenansatz('Lärche', bhd=c(31,33,34), h=c(28,29,30), h100=32)
#'
#' @md
#' @export

tg_kronenansatz <- function(ba, bhd, h, h100, info=FALSE){
  if(missing(ba) | missing(bhd) | missing(h) | missing(h100))
    stop('ba, bhd, th, and h100 m\u00fcssen angegeben werden.')
  if(length(bhd) != length(h))
    stop('bhd und h m\u00fcssen gleich lang sein.')
  if(length(ba) > 1){
    ba <- ba[1]
    warning('Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  }
  if(length(h100) > 1){
    h100 <- h100[1]
    warning('Die betrachtete Kohorte kann nur ein h100 haben. ',
            'Verwende das erste Element.')
  }

  spec.code <- get.spec.code(ba)
  res <- get.formula(spec.code, "Crownbase")
  number <- eval(parse(text=res$formel))

  if(info){
    res <- append(res, list(werte=number), 0)
  } else {
    res <- number
  }
  return(res)
}


#' Oberhöhenbonität (TreeGrOSS)
#'
#' Baumweise Bonität als Oberhöhebonität mit Bezugsalter 100 in Meter.
#'
#' Die Funktion nutzt das Element `"SiteIndex"` aus der TreeGrOSS-XML-Datei
#' `ForestSimulatorNWGermany*.xml` (Nagel, 2006, 2016). Sie wurde für
#' Nordwestdeutschland parametrisiert.
#'
#' @inheritParams tg_parameter
#' @inherit tg_parameter references
#'
#' @return Ein numerischer Vektor der Oberhöhenbonitäten in Meter, wenn
#'   `info=FALSE`. Ansonsten ein Listen-Objekt mit den Elementen `werte`,
#'   `formel`, `quelle` und `info`, welche Oberhöhenbonitäten, die verwendete
#'   Formel, die Quellenangabe (Zitat) und ggf. Informationen über einen
#'   Ersatz enthält.
#'
#' @seealso [tg_hoehe()], [tg_volumen()], [tg_kronenbreite()],
#'  [tg_kronenansatz()], [tg_hoehenzuwachs()], [tg_fun_info()], [tg_baumarten()]
#'
#' @examples
#' tg_bonitaet100(211, alter=150, h100=40)
#' tg_bonitaet100(111, alter=100, h100=27.6, info=TRUE)
#' tg_bonitaet100('Esche', alter=50:55, h100=26)
#'
#' @md
#' @export

tg_bonitaet100 <- function(ba, alter, h100, info=FALSE){
  if(missing(ba) | missing(alter) | missing(h100))
    stop('ba, alter und h100 m\u00fcssen angegeben werden.')
  if(length(ba) > 1){
    ba <- ba[1]
    warning('Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  }
  if(length(h100) > 1){
    h100 <- h100[1]
    warning('Die betrachtete Kohorte kann nur ein h100 haben. ',
            'Verwende das erste Element.')
  }

  spec.code <- get.spec.code(ba)
  res <- get.formula(spec.code, "SiteIndex")
  number <- eval(parse(text=res$formel))

  if(info){
    res <- append(res, list(werte=number), 0)
  } else {
    res <- number
  }
  return(res)
}



#' Höhenzuwachs (TreeGrOSS)
#'
#' Höhenzuwachs für 5-Jahresperioden in Meter.
#'
#' Die Funktion nutzt das Element `"PotentialHeightIncrement"` aus der
#' TreeGrOSS-XML-Datei `ForestSimulatorNWGermany*.xml` (Nagel, 2006, 2016).
#' Sie wurde für Nordwestdeutschland parametrisiert.
#'
#' @inheritParams tg_parameter
#' @inherit tg_parameter references
#'
#' @return Ein numerischer Vektor der Höhenzuwächse für 5-Jahres Perioden in
#'   Meter, wenn `info=FALSE`. Ansonsten ein Listen-Objekt mit den Elementen
#'   `werte`, `formel`, `quelle` und `info`, welche ein Vektor der
#'   Höhenzuwächse, die verwendete Formel, die Quellenangabe (Zitat) und ggf.
#'   Informationen über einen Ersatz enthält.
#'
#' @seealso [tg_hoehe()], [tg_volumen()], [tg_kronenbreite()],
#'  [tg_kronenansatz()], [tg_bonitaet100()],
#'  [tg_fun_info()], [tg_baumarten()]
#'
#' @examples
#' tg_hoehenzuwachs(511, alter=50, h=23, bon100=35.5, h100=23)
#' tg_hoehenzuwachs('Douglas', alter=30, h=20, bon100=46, h100=20.6)
#' tg_hoehenzuwachs(522, alter=120, h=37, bon100=35.4, h100=37.3, info=TRUE)
#' tg_hoehenzuwachs(211, alter=50:54, h=16:20, bon100=rep(33.5,5), h100=19.5)
#'
#' @md
#' @export

tg_hoehenzuwachs <- function(ba, alter, h, bon100, h100, info=FALSE){
  if(missing(ba) | missing(alter) | missing(h) | missing(bon100) |
     missing(h100))
    stop('ba, alter, h, bon100 und h100 m\u00fcssen angegeben werden.')
  if(!(length(alter) == length(h) & length(h) == length(bon100)))
    stop('alter, h und bon100 m\u00fcssen gleich lang sein.')
  if(length(ba) > 1){
    ba <- ba[1]
    warning('Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  }
  if(length(h100) > 1){
    h100 <- h100[1]
    warning('Die betrachtete Kohorte kann nur ein h100 haben. ',
            'Verwende das erste Element.')
  }

  # potential height increment
  spec.code <- get.spec.code(ba)
  formel <- get.formula(spec.code, "PotentialHeightIncrement")$formel
  hinc <- eval(parse(text=formel))

  # height increment
  res <- get.formula(spec.code, "HeightIncrement")
  number <- eval(parse(text=res$formel))

  if(info){
    res <- append(res, list(werte=number), 0)
  } else {
    res <- number
  }
  return(res)
}


#------------------------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------------------------

get.spec.code <- function(species){
  # assuming species is numeric or character
  if(is.numeric(species)){
    if(!(species %in% spf$Code))
      stop(paste('Der Baumartencode', sQuote(species), 'ist nicht vorhanden.'))
  } else {
    pos <- pmatch(tolower(species),
                  tolower(c(spf$ShortName, spf$LongName, spf$LatinName)))
    if(is.na(pos)){
      stop(paste0('Die Baumart ', sQuote(species), ' ist nicht vorhanden ',
                  'oder der Name ist mehrdeutig (z.B. ', sQuote('Berg'), ').'))
    } else {
      species <- spf[(pos %% nrow(spf)), 'Code']
    }
  }
  return(species)
}


get.formula <- function(spec.code, func.name){
  # assuming valid species code & valid function name
  res <- list(formel="", quelle="", info="kein Ersatz vorgenommen")

  res$formel <- spf[spf$Code == spec.code, func.name]

  old.code <- spec.code
  while(res$formel == ""){
    standin <- spf[spf$Code == old.code, "HandledLikeCode"]
    if (old.code == standin){
      # This should never happen!
      stop('Keine Formel und kein Ersatz.')  # nocov
    } else {
      res$formel <- spf[spf$Code == standin, func.name]

      res$info <- paste0('Keine Formel f\u00fcr ',
                         sQuote(paste0(spf[spf$Code == spec.code, 'LatinName'],
                                       ' (', spec.code, ')')),
                         '. Verwende stattdessen ',
                         sQuote(paste0(spf[spf$Code == standin, 'LatinName'],
                                       ' (', standin, ')')), '.')
      old.code <- standin
    }
  }

  # replace everything before /*, /* itself, and reluctantly whitespace after /*
  #  OR replace reluctantly whitespace before */ and */ itself with nothing
  res$quelle <- gsub('^.*/\\*\\s*|\\s*\\*/', '', res$formel)
  # replace whitespace reluctantly and evrything between /* */ with nothing
  res$formel <- gsub('(\\s*)/\\*.*\\*/', '', res$formel)
  return(res)
}
