\name{list.of.lm}
\alias{list.of.lm}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(list.of.lm)}
\format{
  The format is:
List of 1340
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.01e+05 5.86e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:563] 33594 31373 31214 30897 30738 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:563] -16993662 2051958 27951 27640 27484 ...
  .. ..- attr(*, "names")= chr [1:563] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:563] 567678 570917 571148 571611 571842 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:563, 1:2] -23.7276 0.0421 0.0421 0.0421 0.0421 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:563] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 561
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	563 obs. of  2 variables:
  .. ..$ y: num [1:563] 601272 602289 602362 602507 602580 ...
  .. ..$ x: num [1:563] 624986 630510 630904 631693 632088 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 4.20e+05 3.36e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:563] 20166 20128 20125 20120 20117 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:563] -16993662 2027426 18159 18155 18153 ...
  .. ..- attr(*, "names")= chr [1:563] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:563] 581106 582161 582237 582388 582463 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:563, 1:2] -23.7276 0.0421 0.0421 0.0421 0.0421 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:563] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 561
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	563 obs. of  2 variables:
  .. ..$ y: num [1:563] 601272 602289 602362 602507 602580 ...
  .. ..$ x: num [1:563] 479562 482707 482932 483381 483605 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 3.36e+05 1.69e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -48733 -48543 -48164 -47974 -45507 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2744311 -44112 -43923 -41464 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 526696 526738 526821 526862 527402 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1127492 1127738 1128230 1128475 1131672 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.71e+05 2.32e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -53094 -52915 -52558 -52379 -50055 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2770248 -48235 -48057 -45745 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 531057 531110 531215 531267 531951 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1123954 1124180 1124634 1124861 1127809 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.35e+05 3.41e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -39872 -39718 -39410 -39256 -37254 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2816875 -36038 -35885 -33896 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 517835 517912 518067 518144 519149 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1123954 1124180 1124634 1124861 1127809 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -6.58e+04 5.04e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -22165 -22042 -21797 -21675 -20083 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2797488 -19802 -19680 -18099 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 500128 500237 500454 500563 501979 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1121932 1122148 1122579 1122795 1125601 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.04e+06 1.36
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 1067 1075 1091 1099 1201 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2055728 965 973 1077 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 476896 477120 477566 477790 480694 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1112328 1112492 1112820 1112984 1115115 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 3.17e+05 3.03e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -60074 -59883 -59499 -59308 -56817 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2775185 -54739 -54549 -52068 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 538037 538077 538157 538196 538713 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.57e+05 3.74e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -52426 -52244 -51879 -51697 -49327 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2739158 -47575 -47393 -45034 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 530390 530439 530537 530586 531222 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.70e+05 4.74e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -38784 -38615 -38277 -38107 -35908 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2770802 -34954 -34786 -32597 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 516747 516810 516934 516996 517804 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.39e+04 6.53e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -23521 -23376 -23084 -22939 -21045 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2734237 -20949 -20804 -18919 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 501484 501570 501741 501827 502940 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.06e+05 1.21
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -235.9 -163.2 -17.9 54.7 999.3 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -1.85e+07 2.28e+06 2.36 7.50e+01 1.02e+03 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 478199 478358 478675 478834 480896 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 3.69e+05 3.72e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -38633 -38445 -38067 -37879 -35427 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2672252 -34689 -34501 -32056 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 516596 516639 516725 516767 517323 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 396230 396344 396574 396689 398180 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.97e+05 6.04e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -55695 -55521 -55174 -55001 -52744 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2807551 -50716 -50543 -48300 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 533658 533716 533831 533889 534639 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 392691 392787 392978 393074 394317 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 79539.22 1.15
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -51611 -51490 -51246 -51124 -49542 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2847364 -47083 -46964 -45405 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 529574 529684 529903 530013 531437 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 392691 392787 392978 393074 394317 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -2.73e+05 2.01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -34743 -34687 -34576 -34520 -33796 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2857406 -31608 -31554 -30855 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 512706 512881 513233 513408 515691 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 391175 391262 391437 391525 392661 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.88e+06 -6.07
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -92373 -91943 -91082 -90652 -85057 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2676320 -84552 -84115 -78438 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 570336 570137 569739 569540 566953 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.04
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 381066 381098 381164 381197 381623 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 3.18e+05 3.57e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -60520 -60330 -59952 -59762 -57300 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2780388 -55169 -54981 -52529 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 538483 538525 538609 538651 539195 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.56e+05 4.44e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -52408 -52229 -51871 -51692 -49363 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2739565 -47568 -47389 -45073 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 530371 530423 530528 530580 531259 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.68e+05 5.65e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -38537 -38372 -38042 -37877 -35732 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2786976 -34749 -34585 -32451 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 516500 516567 516699 516766 517628 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.83e+04 7.81e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -22495 -22355 -22076 -21936 -20121 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2751642 -20035 -19896 -18089 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 500458 500550 500733 500825 502017 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.14e+05 1.44
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 1277 1338 1462 1524 2328 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2272289 1317 1379 2184 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 476687 476856 477195 477364 479567 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 3.39e+05 1.84e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -47935 -47747 -47371 -47183 -44740 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2744737 -43374 -43187 -40753 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 525898 525941 526028 526072 526635 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1014470 1014705 1015175 1015410 1018464 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.71e+05 2.57e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -53226 -53050 -52697 -52520 -50225 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2773347 -48368 -48193 -45910 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 531190 531244 531354 531409 532121 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1010426 1010639 1011066 1011279 1014049 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.30e+05 3.84e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -40485 -40335 -40036 -39887 -37944 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2828438 -36627 -36479 -34550 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 518448 518530 518693 518775 519840 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1010426 1010639 1011066 1011279 1014049 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -8.07e+04 5.76e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -21976 -21863 -21636 -21523 -20049 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 2813576 -19663 -19550 -18087 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 499939 500057 500293 500411 501944 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 1008910 1009115 1009525 1009730 1012393 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.17e+06 1.65
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 1483 1467 1433 1417 1201 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -18459435 1986255 1257 1240 1026 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 476480 476728 477224 477472 480695 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.1
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 477963 478194 478657 478888 481896 ...
  .. ..$ x: num [1:746] 998801 998951 999251 999402 1001355 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -6.40e+06 3.76
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -5343 -5977 -6295 -6612 -7352 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 125970 -4670 -5044 -5917 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 867603 870943 872613 874284 878181 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.25
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1931855 1932744 1933188 1933632 1934669 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -5.06e+06 3.08
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 919 820 771 722 607 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 183675 548 504 400 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 861341 864145 865548 866950 870221 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1920828 1921737 1922192 1922647 1923708 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.34e+06 1.10
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 3855 3343 3088 2832 2235 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 183142 2147 1912 1365 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 858405 861622 863231 864840 868593 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.21
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1994621 1997537 1998995 2000454 2003856 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.47e+06 2.78
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 35.2 31.8 30.1 28.4 24.5 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6.84e+06 1.84e+05 2.16e+01 2.01e+01 1.65e+01 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 862225 864934 866289 867643 870804 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1915730 1916703 1917190 1917677 1918812 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.66e+07 -8.08
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -72422 -58467 -51490 -44513 -28233 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 -131351 -40253 -35215 -23460 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 934682 923433 917809 912185 899061 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.1
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1937425 1938817 1939513 1940208 1941832 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -2.15e+08 1.11e+02
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -41652 -38946 -37593 -36240 -33083 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 73144 -31694 -30341 -27184 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 903911 903911 903911 903911 903911 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.03
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1944615 1944615 1944615 1944615 1944615 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -3.91e+07 2.07e+01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 14263 -2377 -10697 -19017 -38430 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 81369 -11560 -20224 -40440 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 847997 867343 877016 886688 909258 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.4
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1932425 1933360 1933827 1934294 1935385 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -7.55e+06 4.37
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 11359 9426 8459 7492 5237 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 173102 5562 4682 2628 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 850901 855540 857860 860179 865592 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.26
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1921061 1922122 1922653 1923183 1924421 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.03e+07 -4.80
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -27665 -17834 -12918 -8003 3467 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 179916 -7850 -2677 9395 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 889925 882800 879237 875674 867361 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.12
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1949148 1950632 1951374 1952115 1953846 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.11e+07 -5.26
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -1.29e-08 8.86e-10 1.12e-09 1.34e-09 4.46e-10 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6.84e+06 1.84e+05 2.25e-09 2.48e-09 1.62e-09 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1952303 1951788 1951531 1951274 1950674 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.28e+05 2.79e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:657] 33485 33540 33785 33948 34057 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:657] -17870066 2315538 30345 30521 30638 ...
  .. ..- attr(*, "names")= chr [1:657] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:657] 520811 521130 522561 523516 524152 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:657, 1:2] -25.632 0.039 0.039 0.039 0.039 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:657] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 655
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	657 obs. of  2 variables:
  .. ..$ y: num [1:657] 554297 554669 556346 557464 558209 ...
  .. ..$ x: num [1:657] 1049326 1050468 1055605 1059031 1061314 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.82e+05 5.76e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:657] 24330 24579 25057 25376 25589 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:657] -17870066 2161216 22517 22844 23062 ...
  .. ..- attr(*, "names")= chr [1:657] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:657] 529967 530090 531289 532087 532620 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:657, 1:2] -25.632 0.039 0.039 0.039 0.039 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:657] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 655
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	657 obs. of  2 variables:
  .. ..$ y: num [1:657] 554297 554669 556346 557464 558209 ...
  .. ..$ x: num [1:657] 430189 430403 432482 433868 434792 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.50e+05 1.22
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 67316 68162 68501 68755 69432 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5929546 62781 63036 63716 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 200724 201106 201259 201374 201680 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 287571 287884 288010 288104 288355 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 4729837.8 -13.6
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] -623707 -614924 -611411 -608777 -601751 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 -4542868 -555597 -553241 -546958 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 891746 884193 881171 878905 872863 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 281287 281841 282062 282229 282671 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -367046 2
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 67959 67464 67266 67117 66721 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5998571 61620 61477 61095 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 200080 201805 202494 203011 204391 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 282952 283813 284157 284415 285103 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.78e+04 7.01e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 38921 39286 39432 39541 39833 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5906558 36286 36397 36693 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 229118 229983 230328 230588 231279 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 352272 353505 353998 354368 355355 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.02e+05 1.57
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 118500 118366 118313 118127 117632 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5114120 106427 106253 105789 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 149540 150902 151447 152001 153480 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 351313 352180 352527 352880 353821 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.03e+05 8.56e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 66982 67420 67595 67726 68076 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5777976 61843 61977 62334 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 201058 201849 202166 202403 203036 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 355019 355944 356314 356592 357332 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -2.55e+05 1.71
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:843] -4864 -6083 -9754 -13425 -15260 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:843] -18692418 3875534 -9173 -12850 -14689 ...
  .. ..- attr(*, "names")= chr [1:843] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:843] 331055 333422 337858 342294 344512 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:843, 1:2] -29.0345 0.0344 0.0344 0.0344 0.0344 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:843] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 841
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	843 obs. of  2 variables:
  .. ..$ y: num [1:843] 326191 327339 328104 328869 329252 ...
  .. ..$ x: num [1:843] 343846 345234 347835 350436 351737 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 26204.3 0.99
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:843] -40322 -40548 -42357 -44166 -45071 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:843] -18692418 3805987 -38472 -40306 -41223 ...
  .. ..- attr(*, "names")= chr [1:843] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:843] 366513 367887 370461 373035 374323 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:843, 1:2] -29.0345 0.0344 0.0344 0.0344 0.0344 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:843] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 841
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	843 obs. of  2 variables:
  .. ..$ y: num [1:843] 326191 327339 328104 328869 329252 ...
  .. ..$ x: num [1:843] 343846 345234 347835 350436 351737 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -3.16e+04 8.01e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:843] -45463 -46615 -47383 -48150 -48534 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:843] -18692418 3777729 -42974 -43759 -44151 ...
  .. ..- attr(*, "names")= chr [1:843] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:843] 371655 373954 375487 377020 377786 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:843, 1:2] -29.0345 0.0344 0.0344 0.0344 0.0344 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:843] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 841
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	843 obs. of  2 variables:
  .. ..$ y: num [1:843] 326191 327339 328104 328869 329252 ...
  .. ..$ x: num [1:843] 503682 506554 508469 510383 511341 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.85e+05 1.54
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:843] 36751 33483 31305 29126 28037 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:843] -18692418 3744562 27433 25277 24200 ...
  .. ..- attr(*, "names")= chr [1:843] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:843] 289440 293856 296799 299743 301215 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:843, 1:2] -29.0345 0.0344 0.0344 0.0344 0.0344 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:843] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 841
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	843 obs. of  2 variables:
  .. ..$ y: num [1:843] 326191 327339 328104 328869 329252 ...
  .. ..$ x: num [1:843] 503682 506554 508469 510383 511341 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.75e+05 3.61e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:464] 47633 47154 46196 45078 44120 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:464] -15885649 1578600 40325 39254 38336 ...
  .. ..- attr(*, "names")= chr [1:464] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:464] 584571 585339 586877 588670 590207 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:464, 1:2] -21.5407 0.0464 0.0464 0.0464 0.0464 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:464] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.05 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 462
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	464 obs. of  2 variables:
  .. ..$ y: num [1:464] 632204 632493 633072 633748 634327 ...
  .. ..$ x: num [1:464] 855975 858102 862357 867321 871576 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.32e+05 3.29e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:464] 40831 40607 40158 39635 39186 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:464] -15885649 1613063 35289 34792 34367 ...
  .. ..- attr(*, "names")= chr [1:464] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:464] 591373 591886 592914 594113 595141 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:464, 1:2] -21.5407 0.0464 0.0464 0.0464 0.0464 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:464] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.05 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 462
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	464 obs. of  2 variables:
  .. ..$ y: num [1:464] 632204 632493 633072 633748 634327 ...
  .. ..$ x: num [1:464] 1396032 1397595 1400722 1404369 1407495 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -3.93e+05 1.34
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:276] 5960 5740 5447 5300 5153 ...
  .. ..- attr(*, "names")= chr [1:276] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:276] -13057652 1199075 4741 4595 4450 ...
  .. ..- attr(*, "names")= chr [1:276] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:276] 691928 692396 693022 693334 693647 ...
  .. ..- attr(*, "names")= chr [1:276] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:276, 1:2] -16.6132 0.0602 0.0602 0.0602 0.0602 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:276] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.06 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 274
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	276 obs. of  2 variables:
  .. ..$ y: num [1:276] 697887 698136 698468 698634 698800 ...
  .. ..$ x: num [1:276] 808106 808455 808921 809154 809387 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -425119.9 0.8
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:276] 9897 9847 9782 9749 9716 ...
  .. ..- attr(*, "names")= chr [1:276] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:276] -13057652 1209264 8565 8534 8503 ...
  .. ..- attr(*, "names")= chr [1:276] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:276] 687991 688289 688687 688885 689084 ...
  .. ..- attr(*, "names")= chr [1:276] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:276, 1:2] -16.6132 0.0602 0.0602 0.0602 0.0602 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:276] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.06 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 274
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	276 obs. of  2 variables:
  .. ..$ y: num [1:276] 697887 698136 698468 698634 698800 ...
  .. ..$ x: num [1:276] 1391167 1391540 1392037 1392285 1392534 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.50e+06 2.66
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -1.51e-09 1.39e-10 1.93e-10 2.64e-10 1.22e-11 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6.84e+06 1.84e+05 3.20e-10 3.93e-10 1.46e-10 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 887904 888923 889432 889941 891129 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.19e+06 2.60
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 2.45e-10 5.83e-11 -2.99e-12 -6.87e-11 7.28e-11 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6.84e+06 1.84e+05 -3.64e-11 -1.02e-10 4.00e-11 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 787335 788374 788894 789414 790626 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.05e+05 3.78e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:657] 16717 16824 17305 17626 17840 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:657] -17870066 2289279 15669 15995 16212 ...
  .. ..- attr(*, "names")= chr [1:657] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:657] 537580 537846 539041 539838 540369 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:657, 1:2] -25.632 0.039 0.039 0.039 0.039 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:657] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 655
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	657 obs. of  2 variables:
  .. ..$ y: num [1:657] 554297 554669 556346 557464 558209 ...
  .. ..$ x: num [1:657] 1143961 1144663 1147823 1149930 1151335 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.41e+05 2.47e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:936] -101686 -101761 -101779 -101965 -102002 ...
  .. ..- attr(*, "names")= chr [1:936] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:936] -18681097 4677954 -94584 -94770 -94808 ...
  .. ..- attr(*, "names")= chr [1:936] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:936] 407486 407486 407486 407486 407486 ...
  .. ..- attr(*, "names")= chr [1:936] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:936, 1:2] -30.5941 0.0327 0.0327 0.0327 0.0327 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:936] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.04
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 934
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	936 obs. of  2 variables:
  .. ..$ y: num [1:936] 305800 305725 305707 305521 305483 ...
  .. ..$ x: num [1:936] 674000 674000 674000 674000 674000 674000 674000 674000 674000 674000 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 3.40e+05 2.14e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:563] 25232 24397 24337 24218 24158 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:563] -16993662 2037686 21890 21773 21715 ...
  .. ..- attr(*, "names")= chr [1:563] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:563] 576040 577893 578025 578290 578422 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:563, 1:2] -23.7276 0.0421 0.0421 0.0421 0.0421 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:563] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 561
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	563 obs. of  2 variables:
  .. ..$ y: num [1:563] 601272 602289 602362 602507 602580 ...
  .. ..$ x: num [1:563] 1104548 1113216 1113836 1115074 1115693 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.14e+05 5.02e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:657] 29332 29239 29379 29473 29535 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:657] -17870066 2365368 26457 26562 26632 ...
  .. ..- attr(*, "names")= chr [1:657] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:657] 524964 525430 526967 527991 528674 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:657, 1:2] -25.632 0.039 0.039 0.039 0.039 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:657] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 655
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	657 obs. of  2 variables:
  .. ..$ y: num [1:657] 554297 554669 556346 557464 558209 ...
  .. ..$ x: num [1:657] 619137 620065 623123 625163 626522 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.28e+05 2.79e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:657] 33485 33540 33785 33948 34057 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:657] -17870066 2315538 30345 30521 30638 ...
  .. ..- attr(*, "names")= chr [1:657] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:657] 520811 521130 522561 523516 524152 ...
  .. ..- attr(*, "names")= chr [1:657] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:657, 1:2] -25.632 0.039 0.039 0.039 0.039 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:657] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 655
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	657 obs. of  2 variables:
  .. ..$ y: num [1:657] 554297 554669 556346 557464 558209 ...
  .. ..$ x: num [1:657] 1049326 1050468 1055605 1059031 1061314 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.13e+05 5.52e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:843] -28677 -29882 -31610 -33338 -34202 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:843] -18692418 3834653 -28724 -30470 -31342 ...
  .. ..- attr(*, "names")= chr [1:843] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:843] 354869 357220 359714 362207 363454 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:843, 1:2] -29.0345 0.0344 0.0344 0.0344 0.0344 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:843] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 841
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	843 obs. of  2 variables:
  .. ..$ y: num [1:843] 326191 327339 328104 328869 329252 ...
  .. ..$ x: num [1:843] 847528 851788 856304 860820 863077 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -2.01e+05 6.22e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:843] 159 -1344 -3389 -5434 -6457 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:843] -18692418 3844852 -3296 -5342 -6365 ...
  .. ..- attr(*, "names")= chr [1:843] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:843] 326033 328683 331493 334303 335708 ...
  .. ..- attr(*, "names")= chr [1:843] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:843, 1:2] -29.0345 0.0344 0.0344 0.0344 0.0344 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:843] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 841
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	843 obs. of  2 variables:
  .. ..$ y: num [1:843] 326191 327339 328104 328869 329252 ...
  .. ..$ x: num [1:843] 847528 851788 856304 860820 863077 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -3.59e+05 1.95
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:464] -63859 -62466 -59681 -56432 -53647 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:464] -15885649 1409900 -55085 -51730 -48854 ...
  .. ..- attr(*, "names")= chr [1:464] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:464] 696062 694959 692753 690179 687973 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:464, 1:2] -21.5407 0.0464 0.0464 0.0464 0.0464 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:464] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.05 1.03
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 462
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	464 obs. of  2 variables:
  .. ..$ y: num [1:464] 632204 632493 633072 633748 634327 ...
  .. ..$ x: num [1:464] 540057 539493 538364 537047 535919 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.75e+05 3.61e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:464] 47633 47154 46196 45078 44120 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:464] -15885649 1578600 40325 39254 38336 ...
  .. ..- attr(*, "names")= chr [1:464] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:464] 584571 585339 586877 588670 590207 ...
  .. ..- attr(*, "names")= chr [1:464] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:464, 1:2] -21.5407 0.0464 0.0464 0.0464 0.0464 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:464] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.05 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 462
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	464 obs. of  2 variables:
  .. ..$ y: num [1:464] 632204 632493 633072 633748 634327 ...
  .. ..$ x: num [1:464] 855975 858102 862357 867321 871576 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.41e+05 1.93
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:276] 13030 13234 13506 13642 13778 ...
  .. ..- attr(*, "names")= chr [1:276] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:276] -13057652 1208251 11853 11989 12125 ...
  .. ..- attr(*, "names")= chr [1:276] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:276] 684858 684903 684963 684993 685023 ...
  .. ..- attr(*, "names")= chr [1:276] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:276, 1:2] -16.6132 0.0602 0.0602 0.0602 0.0602 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:276] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.06 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 274
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	276 obs. of  2 variables:
  .. ..$ y: num [1:276] 697887 698136 698468 698634 698800 ...
  .. ..$ x: num [1:276] 583061 583085 583116 583131 583147 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -8.56e+05 1.54
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 147987 147023 146637 146205 145052 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 4833971 130609 130198 129103 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 120053 122246 123123 123924 126060 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 632600 634021 634589 635109 636493 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.87e+05 6.04e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 69930 70080 70140 70186 70306 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5865986 64184 64233 64363 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 198110 199188 199620 199943 200806 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 637971 639757 640471 641006 642434 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -6.75e+04 4.46e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:1087] 50154 50693 50908 51070 51501 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:1087] -18696795 5922285 46775 46939 47374 ...
  .. ..- attr(*, "names")= chr [1:1087] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:1087] 217886 218576 218852 219059 219611 ...
  .. ..- attr(*, "names")= chr [1:1087] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:1087, 1:2] -32.9697 0.0303 0.0303 0.0303 0.0303 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:1087] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.03 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 1085
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	1087 obs. of  2 variables:
  .. ..$ y: num [1:1087] 268040 269269 269760 270129 271112 ...
  .. ..$ x: num [1:1087] 639842 641389 642008 642472 643710 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 876721.64 -7.99
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:56] -71930 -58101 -51187 -44272 -28139 ...
  .. ..- attr(*, "names")= chr [1:56] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:56] -6787565 -128566 -40154 -35191 -23612 ...
  .. ..- attr(*, "names")= chr [1:56] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:56] 934190 923067 917505 911944 898967 ...
  .. ..- attr(*, "names")= chr [1:56] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:56, 1:2] -7.483 0.134 0.134 0.134 0.134 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:56] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.1
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 54
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	56 obs. of  2 variables:
  .. ..$ y: num [1:56] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:56] -7190 -5798 -5102 -4407 -2783 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1070611.5 -17.5
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] 4780 -8858 -15676 -22495 -38406 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 68920 -14392 -22014 -39798 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 857480 873823 881995 890167 909234 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.39
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 12190 11255 10788 10321 9230 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 913649.64 -4.67
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:56] 1719 3834 4892 5950 8418 ...
  .. ..- attr(*, "names")= chr [1:56] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:56] -6784552 174892 4018 5081 7561 ...
  .. ..- attr(*, "names")= chr [1:56] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:56] 860541 861131 861426 861721 862410 ...
  .. ..- attr(*, "names")= chr [1:56] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:56, 1:2] -7.483 0.134 0.134 0.134 0.134 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:56] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.23
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 54
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	56 obs. of  2 variables:
  .. ..$ y: num [1:56] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:56] 11364 11237 11174 11111 10964 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 9.36e+05 2.43
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -4904 -1170 697 2564 6920 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 181860 1370 3238 7598 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 867164 866136 865622 865108 863908 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] -28087 -28509 -28721 -28932 -29425 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 915012.4 -4.72
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -67650 -55509 -49438 -43368 -29203 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6843822 -37849 -35195 -33697 -30204 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 929910 920475 915757 911039 900031 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.3
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] -3155 -1157 -158 841 3172 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.34e+06 1.32
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:57] -3.80e-09 2.62e-10 1.99e-10 3.40e-10 2.75e-10 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:57] -6.84e+06 1.84e+05 5.31e-10 6.77e-10 6.22e-10 ...
  .. ..- attr(*, "names")= chr [1:57] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..- attr(*, "names")= chr [1:57] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:57, 1:2] -7.55 0.132 0.132 0.132 0.132 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:57] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.13 1.2
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 55
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	57 obs. of  2 variables:
  .. ..$ y: num [1:57] 862260 864966 866319 867672 870828 ...
  .. ..$ x: num [1:57] 1675239 1677297 1678326 1679355 1681755 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 420974 1
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:563] -289 -154 -144 -125 -115 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:563] -3.08e+07 3.50e+06 -1.24e+02 -1.04e+02 -9.48e+01 ...
  .. ..- attr(*, "names")= chr [1:563] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:563] 1045925 1051448 1051842 1052631 1053026 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:563, 1:2] -23.7276 0.0421 0.0421 0.0421 0.0421 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:563] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.07
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 561
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	563 obs. of  2 variables:
  .. ..$ y: num [1:563] 1045636 1051294 1051698 1052506 1052910 ...
  .. ..$ x: num [1:563] 624986 630510 630904 631693 632088 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 7.90e+05 5.78e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:563] -21006 -17165 -16891 -16342 -16068 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:563] -30827247 3490389 -15069 -14521 -14248 ...
  .. ..- attr(*, "names")= chr [1:563] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:563] 1066642 1068459 1068589 1068848 1068978 ...
  .. ..- attr(*, "names")= chr [1:563] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:563, 1:2] -23.7276 0.0421 0.0421 0.0421 0.0421 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:563] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 561
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	563 obs. of  2 variables:
  .. ..$ y: num [1:563] 1045636 1051294 1051698 1052506 1052910 ...
  .. ..$ x: num [1:563] 479562 482707 482932 483381 483605 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 5.94e+05 3.12e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -21447 -21196 -20695 -20444 -17184 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5062942 -18920 -18669 -15413 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 945575 945652 945805 945881 946878 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1127492 1127738 1128230 1128475 1131672 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 4.77e+05 4.25e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -30840 -30609 -30148 -29917 -26915 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5084998 -27643 -27413 -24418 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 954968 955065 955258 955354 956609 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1123954 1124180 1124634 1124861 1127809 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.38e+05 6.19e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -9700 -9513 -9139 -8952 -6522 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5114790 -8327 -8140 -5712 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 933829 933969 934250 934390 936215 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1123954 1124180 1124634 1124861 1127809 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.29e+05 9.18e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 23183 23313 23571 23700 25380 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5091229 21469 21599 23290 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 900945 901143 901539 901738 904314 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1121932 1122148 1122579 1122795 1125601 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -1.98e+06 2.54
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 74480 74391 74212 74123 72962 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 3834453 65475 65393 64324 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 849648 850065 850898 851315 856732 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.09
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1112328 1112492 1112820 1112984 1115115 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 5.63e+05 5.55e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -44139 -43884 -43375 -43120 -39811 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5084283 -39883 -39629 -36327 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 968267 968340 968485 968558 969504 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 4.47e+05 6.91e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -27722 -27485 -27012 -26775 -23697 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5063577 -24742 -24506 -21433 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 951850 951941 952122 952213 953390 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.96e+05 8.67e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -5416 -5202 -4775 -4561 -1783 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5071354 -4321 -4107 -1331 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 929544 929658 929885 929999 931477 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 23849.8 1.2
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 23517 23687 24027 24198 26409 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5020068 21874 22045 24265 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 900611 900768 901083 901240 903284 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -8.00e+05 2.26
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 72489 72521 72583 72614 73018 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 4249987 64627 64663 65125 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 851639 851935 852528 852824 856676 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 731262 731393 731656 731787 733492 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 6.49e+05 6.93e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -55.5 192.3 687.9 935.7 4157.2 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 4976289 679 927 4149 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 924184 924263 924422 924502 925536 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 396230 396344 396574 396689 398180 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 532247 1.1
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -38487 -38264 -37819 -37596 -34703 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5096763 -34743 -34521 -31637 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 962615 962720 962929 963034 964397 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 392691 392787 392978 393074 394317 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 1.58e+05 2.04
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -35883 -35751 -35487 -35355 -33639 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5075415 -32595 -32464 -30764 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 960011 960207 960597 960793 963332 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 392691 392787 392978 393074 394317 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -4.64e+05 3.57
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -6891 -6876 -6845 -6829 -6628 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5074461 -6256 -6241 -6045 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 931020 931331 931955 932267 936321 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 391175 391262 391437 391525 392661 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 5356244.6 -11.4
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -98958 -98258 -96858 -96158 -87055 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5013596 -89871 -89164 -79973 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 1023087 1022714 1021968 1021595 1016749 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.04
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 381066 381098 381164 381197 381623 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 5.66e+05 6.52e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -45280 -45029 -44528 -44277 -41018 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5087245 -40955 -40705 -37453 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 969408 969485 969638 969715 970711 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 4.44e+05 8.21e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -27688 -27458 -26996 -26765 -23763 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5064330 -24729 -24499 -21503 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 951817 951913 952106 952203 953457 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.94e+05 1.03
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -5810 -5604 -5191 -4985 -2303 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5086156 -4704 -4498 -1817 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 929938 930059 930301 930423 931996 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 16722.59 1.43
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 24544 24703 25022 25181 27254 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5038564 22777 22937 25019 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 899585 899753 900088 900256 902439 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.06
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] -8.15e+05 2.69
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] 75224 75235 75257 75268 75412 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 4242421 66957 66973 67181 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 848904 849220 849853 850169 854282 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.08
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 617735 617852 618087 618205 619732 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 5.99e+05 3.40e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -19970 -19722 -19227 -18980 -15764 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5063820 -17571 -17324 -14111 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 944098 944178 944338 944418 945457 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1014470 1014705 1015175 1015410 1018464 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 4.79e+05 4.72e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -31278 -31051 -30597 -30370 -27421 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5086970 -28059 -27833 -24891 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 955406 955506 955707 955808 957114 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1010426 1010639 1011066 1011279 1014049 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
 $ :List of 12
  ..$ coefficients : Named num [1:2] 2.33e+05 6.96e-01
  .. ..- attr(*, "names")= chr [1:2] "(Intercept)" "x"
  ..$ residuals    : Named num [1:746] -11647 -11468 -11110 -10931 -8602 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ effects      : Named num [1:746] -33342159 5120793 -10136 -9957 -7633 ...
  .. ..- attr(*, "names")= chr [1:746] "(Intercept)" "x" "" "" ...
  ..$ rank         : int 2
  ..$ fitted.values: Named num [1:746] 935775 935923 936220 936368 938296 ...
  .. ..- attr(*, "names")= chr [1:746] "1" "2" "3" "4" ...
  ..$ assign       : int [1:2] 0 1
  ..$ qr           :List of 5
  .. ..$ qr   : num [1:746, 1:2] -27.313 0.0366 0.0366 0.0366 0.0366 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:746] "1" "2" "3" "4" ...
  .. .. .. ..$ : chr [1:2] "(Intercept)" "x"
  .. .. ..- attr(*, "assign")= int [1:2] 0 1
  .. ..$ qraux: num [1:2] 1.04 1.05
  .. ..$ pivot: int [1:2] 1 2
  .. ..$ tol  : num 1e-07
  .. ..$ rank : int 2
  .. ..- attr(*, "class")= chr "qr"
  ..$ df.residual  : int 744
  ..$ xlevels      : Named list()
  ..$ call         : language lm(formula = y ~ x)
  ..$ terms        :Classes 'terms', 'formula' length 3 y ~ x
  .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. ..$ : chr "x"
  .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..$ model        :'data.frame':	746 obs. of  2 variables:
  .. ..$ y: num [1:746] 924128 924456 925110 925438 929694 ...
  .. ..$ x: num [1:746] 1010426 1010639 1011066 1011279 1014049 ...
  .. ..- attr(*, "terms")=Classes 'terms', 'formula' length 3 y ~ x
  .. .. .. ..- attr(*, "variables")= language list(y, x)
  .. .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. .. ..$ : chr [1:2] "y" "x"
  .. .. .. .. .. ..$ : chr "x"
  .. .. .. ..- attr(*, "term.labels")= chr "x"
  .. .. .. ..- attr(*, "order")= int 1
  .. .. .. ..- attr(*, "intercept")= int 1
  .. .. .. ..- attr(*, "response")= int 1
  .. .. .. ..- attr(*, ".Environment")=<environment: 0x0000000016637668> 
  .. .. .. ..- attr(*, "predvars")= language list(y, x)
  .. .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "numeric"
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "y" "x"
  ..- attr(*, "class")= chr "lm"
  [list output truncated]
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(list.of.lm)
## maybe str(list.of.lm) ; plot(list.of.lm) ...
}
\keyword{datasets}
