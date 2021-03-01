library(TreeGrOSSinR)
context('TreeGrOSS')


#- tg_baumarten -------------------------------------------------------------------
test_that("tg_baumarten returns a data.frame", {
  tspec <- tg_baumarten()
  expect_is(tspec, 'data.frame')
  expect_length(tspec, 4)
  expect_named(tspec, c("code", "kurz", "name", "latein"))
})


#- tg_fun_info ----------------------------------------------------------------
test_that("tg_fun_info", {
  tinf <- tg_fun_info(ba=211, fun='tg_volumen')
  expect_type(tinf, 'list')
  expect_length(tinf, 3)
  expect_match(tinf$formel, paste0("3.141592*h*(bhd/200)^2*",
                                    "(0.4039+0.0017335*h+1.1267/h-118.188/",
                                    "(bhd*bhd*bhd)+0.0000042*bhd*bhd)"),
               fixed=TRUE)
  expect_match(tinf$info, 'kein Ersatz vorgenommen')

  tinf2 <- tg_fun_info(ba='Traubeneiche', fun='tg_hoehenzuwachs')
  expect_type(tinf2, 'list')
  expect_length(tinf2, 3)
  expect_match(tinf2$formel, paste0("h*((((1.2164*bon100*(1.0-exp(-0.0194*",
                                     "(alter+5.0)))^1.1344)-(1.2164*bon100*(1.0-",
                                     "exp(-0.0194*alter))^1.1344))/h100)+",
                                     "(0.01676*(hinc^1.3349)))"), fixed=TRUE)
  expect_match(tinf2$info,
               paste0("Keine Formel für ", sQuote("Quercus petraea (112)"), ". ",
                      "Verwende stattdessen ", sQuote("Quercus (110)"), "."),
               fixed=TRUE)
})


#- tg_hoehe ------------------------------------------------------------------
test_that("tg_hoehe", {
  expect_equal(tg_hoehe(ba=211, bhd=32, hg=29.3, dg=29.4), 30.2108, tol=1e-5)
  expect_equal(tg_hoehe(511, bhd=12:16, hg=19, dg=17.5),
               c(15.32389, 16.14617, 16.88925, 17.56327, 18.1769), tol=1e-5)

  tgh <- tg_hoehe(ba="Stieleiche", bhd=60, hg=33.4, dg=63.2, info=TRUE)
  expect_equal(tgh$werte, 33.04843, tolerance=1e-5)
  expect_length(tgh, 4)
  expect_match(tgh$info,
               paste0("Keine Formel für ", sQuote("Quercus robur (111)"), ". ",
                      "Verwende stattdessen ", sQuote("Quercus (110)"), "."),
               fixed=TRUE)

  expect_warning(
    tgh <- tg_hoehe(ba=c(110, 111), bhd=50, hg=30, dg=55),
    'Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  expect_equal(tgh, 29.38834, tolerance=0.00001)

  expect_warning(
    tgh <- tg_hoehe(ba=110, bhd=c(50, 51), hg=c(30, 31), dg=c(55, 56)),
    paste0('Die betrachtete Kohorte kann nur ein dg und ein hg haben. ',
           'Verwende jeweils das erste Element.'))
  expect_equal(tgh, c(29.38834, 29.51916), tolerance=0.00001)
})


#- tg_volumen -----------------------------------------------------------------
test_that("tg_volumen", {
  expect_equal(tg_volumen(ba="Fagus syl", bhd=52.3, h=37.8), 4.14063, tol=1e-5)
  expect_equal(tg_volumen(511, bhd=c(15.5, 23, 30.7, 37.6),
                          h=c(16.6, 24.7, 29.7, 33.3)),
               c(0.15163, 0.49704, 1.02104, 1.65363), tolerance=1e-5)

  tgv <- tg_volumen('Alnus incana', bhd=29.0, h=24.7, info=TRUE)
  expect_equal(tgv$werte, 0.78016, tolerance=1e-5)
  expect_length(tgv, 4)
  expect_match(tgv$info,
               paste0("Keine Formel für ", sQuote("Alnus incana (422)"), ". ",
                      "Verwende stattdessen ", sQuote("Alnus glutinosa (421)"),
                      "."), fixed=TRUE)

  expect_warning(
    tgv <- tg_volumen(ba=c(211, 212), bhd=50, h=40),
    'Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  expect_equal(tgv, 4.01309, tolerance=0.00001)
})


#- tg_kronenbreite --------------------------------------------------------------
test_that("tg_kronenbreite", {
  expect_equal(tg_kronenbreite(211, 20), 5.05834, tolerance=1e-5)
  expect_equal(tg_kronenbreite("Alnus incana", bhd=40, h=29, ka=10.2),
               8.18451, tolerance=1e-5)

  tgc <- tg_kronenbreite('Buche', 20, info=TRUE)
  expect_equal(tgc$werte, 5.05834, tolerance=1e-5)
  expect_length(tgc, 4)
  expect_match(tgc$info, "kein Ersatz vorgenommen", fixed=TRUE)

  expect_warning(
    tgc <- tg_kronenbreite(ba=c(211, 212), bhd=10),
    'Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  expect_equal(tgc, 3.14593, tolerance=0.00001)
})


#- tg_kronenansatz ---------------------------------------------------------------
test_that("tg_kronenansatz", {
  expect_equal(tg_kronenansatz(211, bhd=44, h=36, h100=38), 19.26465, tol=1e-5)
  expect_equal(tg_kronenansatz('Lärche', bhd=c(31,33,34), h=c(28,29,30), h100=32),
               c(19.42394, 20.07209, 20.80884), tolerance=1e-5)

  tgc <- tg_kronenansatz(111, bhd=34, h=26, h100=28, info=TRUE)
  expect_equal(tgc$werte, 17.02993, tolerance=1e-5)
  expect_length(tgc, 4)
  expect_match(tgc$info,
               paste0("Keine Formel für ", sQuote("Quercus robur (111)"), ". ",
                      "Verwende stattdessen ", sQuote("Quercus (110)"), "."),
               fixed=TRUE)

  expect_warning(tgc <- tg_kronenansatz(ba=c(211, 212), bhd=44, h=36,
                                     h100=38),
                 'Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  expect_equal(tgc, 19.26465, tolerance=0.00001)

  expect_warning(
    tgc <- tg_kronenansatz(ba=211, bhd=c(44, 45), h=c(36, 37), h100=c(38, 39)),
    'Die betrachtete Kohorte kann nur ein h100 haben. ',
    'Verwende das erste Element.')
  expect_equal(tgc, c(19.26465, 19.77245), tolerance=0.00001)
})


#- tg_bonitaet100 --------------------------------------------------------------
test_that("tg_bonitaet100", {
  expect_equal(tg_bonitaet100(211, alter=150, h100=40), 33.67246, tolerance=1e-5)
  expect_equal(tg_bonitaet100('Esche', alter=50:55, h100=26),
               c(34.30755, 34.01688, 33.73582, 33.46386, 33.20055, 32.94545),
               tolerance=1e-5)

  tgsi <- tg_bonitaet100(111, alter=100, h100=27.6, info=TRUE)
  expect_equal(tgsi$werte, 27.05603, tolerance=1e-5)
  expect_length(tgsi, 4)
  expect_match(tgsi$info,
               paste0("Keine Formel für ", sQuote("Quercus robur (111)"), ". ",
                      "Verwende stattdessen ", sQuote("Quercus (110)"), "."),
               fixed=TRUE)

  expect_warning(tgsi <- tg_bonitaet100(c(211, 212), alter=150, h100=40),
                 'Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  expect_equal(tgsi, 33.67246, tolerance=0.00001)

  expect_warning(
    tgsi <- tg_bonitaet100(211, alter=150, h100=c(40, 50)),
    'Die betrachtete Kohorte kann nur ein h100 haben. ',
    'Verwende das erste Element.')
  expect_equal(tgsi, 33.67246, tolerance=0.00001)
})


#- tg_hoehenzuwachs --------------------------------------------------------------
test_that("tg_hoehenzuwachs behaves", {
  expect_equal(tg_hoehenzuwachs(511, alter=50, h=23, bon100=35.5, h100=23),
               1.816, tolerance=1e-5)
  expect_equal(tg_hoehenzuwachs('Douglas', alter=30, h=20, bon100=46,
                             h100=20.6),
               3.68511, tolerance=1e-5)
  expect_equal(tg_hoehenzuwachs(211, alter=50:54, h=16:20, bon100=rep(33.5,5),
                             h100=19.5),
               c(1.54556, 1.60507, 1.66183, 1.716, 1.76773), tolerance=1e-5)

  tghi <- tg_hoehenzuwachs(522, alter=120, h=37, bon100=35.4, h100=37.3,
                        info=TRUE)
  expect_equal(tghi$werte, 0.7655, tolerance=1e-5)
  expect_length(tghi, 4)
  expect_match(tghi$info,
               paste0("Keine Formel für ", sQuote("Abies nordmanniana (522)"),
                      ". Verwende stattdessen ", sQuote("Picea abies (511)"),
                      "."), fixed=TRUE)

  expect_warning(tghi <- tg_hoehenzuwachs(ba=c(511,512), alter=50, h=23,
                                       bon100=35.5, h100=23),
                 'Kann nur eine Baumart pro Aufruf behandeln. Verwende die Erste.')
  expect_equal(tghi, 1.816, tolerance=0.00001)

  expect_warning(
    tghi <- tg_hoehenzuwachs(ba=511, alter=c(50, 51), h=c(23, 24),
                             bon100=c(35.5, 36), h100=c(23, 24)),
    'Die betrachtete Kohorte kann nur ein h100 haben. ',
    'Verwende das erste Element.')
  expect_equal(tghi, c(1.816, 1.86936), tolerance=0.00001)

})


#- get.spec.code --------------------------------------------------------------
test_that("get.spec.code", {
  expect_identical(TreeGrOSSinR:::get.spec.code(211), 211)
  expect_identical(TreeGrOSSinR:::get.spec.code('Bu'), as.integer(211))
  expect_identical(TreeGrOSSinR:::get.spec.code('bu'), as.integer(211))
  expect_identical(TreeGrOSSinR:::get.spec.code('Buche'), as.integer(211))
  expect_identical(TreeGrOSSinR:::get.spec.code('buch'), as.integer(211))
  expect_identical(TreeGrOSSinR:::get.spec.code('Fagus syl'), as.integer(211))

  expect_error(TreeGrOSSinR:::get.spec.code(2111),
               paste0("Der Baumartencode ", sQuote("2111"),
                      " ist nicht vorhanden."), fixed=TRUE)
  expect_error(TreeGrOSSinR:::get.spec.code('Vu'),
               paste0("Die Baumart ", sQuote("Vu"),
                      " ist nicht vorhanden oder der Name ist mehrdeutig (z.B. ",
                      sQuote("Berg"), ")."), fixed=TRUE)
})


#- get.formula ----------------------------------------------------------------
test_that("get.formula", {
  # assuming valid species code & valid function name

  expect_identical(TreeGrOSSinR:::get.formula(211, 'UniformHeightCurveXML'),
                   structure(
                     list(
                       formel=paste0("1.3+(hg-1.3)*exp(0.20213328*",
                                     "(1.0-(dg/bhd)))*exp(5.64023296*",
                                     "((1.0/dg)-(1.0/bhd)))"),
                       quelle="Buche (NAGEL 1999)",
                       info="kein Ersatz vorgenommen"),
                     .Names=c("formel", "quelle", "info"))
                   )

  expect_identical(TreeGrOSSinR:::get.formula(442, 'UniformHeightCurveXML'),
                   list(formel=paste0("1.3+(hg-1.3)*exp(0.14657227*",
                                      "(1.0-(dg/bhd)))*exp(3.78686023*",
                                      "((1.0/dg)-(1.0/bhd)))"),
                        quelle="Eiche (NAGEL 1999",
                        info=paste0("Keine Formel für ",
                                    sQuote("Aesculus hippocastanum (442)"),
                                    ". Verwende stattdessen ",
                                    sQuote("Acer pseudoplatanus (321)"),
                                    "."))
  )
})


#- bug reports ----------------------------------------------------------------
test_that("cristoph's pappel volumne bug is fixed", {

  # convert xml to R: log -> log10 & ln -> log
  expect_equal(tg_volumen(430, 30, 24), 0.71317, tolerance=1e-5)
  expect_equal(tg_volumen(430, 50, 36), 3.10623, tolerance=1e-5)

  # 431, 432, 433, 434 have identical volume functions -> same fix
  expect_equal(tg_volumen(431, 50, 36), 3.10623, tolerance=1e-5)
  expect_equal(tg_volumen(432, 50, 36), 3.10623, tolerance=1e-5)
  expect_equal(tg_volumen(433, 50, 36), 3.10623, tolerance=1e-5)
  expect_equal(tg_volumen(434, 50, 36), 3.10623, tolerance=1e-5)

  # the problem exists also with StemVolumeFunction of 511, 611,
  # but this function is not exported at the moment
})

