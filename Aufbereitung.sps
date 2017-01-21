* Encoding: UTF-8.

*** Zu Beginn manuelle Entfernung der '0' als fehlender Wert bei den vier Kontaktvariablen (v79-v82) sowie bei v189, der Religionszugehörigkeit.***
***Hierzu ist keine Möglichkeit bekannt dies über die Syntax zu erledigen ***

*************************************************
*************************************************
**********Aufbereitung der Variablen **********
*************************************************
*************************************************

*** Umbennen Variablen (ohne Variablen zur Rekonstruktion der Bildungsjahre) ***

RENAME VARIABLES (v83 v84 v32 v33 v34 v35 v507 v119 v101 v8 v748 v220 v79 v80 v81 v82 v230 v75 v76 v77 v78 v204 v205 v206 v207 v208
=AU1 AU2 an1 an2 an3 an4 Lebenszufriedenheit BefWirtschftlLage LinksRechts Ost Land Alter K2 K1 K4 K3 Abschluss R1 R2 R3 R4 AR1 AR2 AR3 AR4 AR5).
EXECUTE. 
 
***********************
AUTORITARISMUS
***********************
*** Berechnunung u. Bennenung der Skala für Autoritarismus ***

COMPUTE Autori_Skala= MEAN.1(Au1,Au2).
EXECUTE.

VARIABLE LABELS Autori_Skala 'Autoritarismus'.
EXECUTE. 


**********
ANOMIE
**********
*** Dummysierung der Variablen der späteren Skala für Anomie *** 

RECODE an1 an2 an3 an4 (2=0) (1=1).
EXECUTE.

*** Anpassung der Labels an die neue Codierung der Anomievariablen *** 

VAL LAB an1 an2 an3 an4
0 'BIN ANDERER MEINUNG'
1 'BIN DERS. MEINUNG'
8 'WEISS NICHT'
9 'KEINE ANGABE'.
EXECUTE. 

*** Berechnung u. Bennenung Skala für Anomie ***

COMPUTE Anomie_Skala= MEAN.2 (an1,an2,an3,an4).
EXECUTE. 

VARIABLE LABELS Anomie_Skala 'Anomie'.


************************************************
ALLGEMEINE LEBENSZUFRIEDENHEIT
************************************************
*** Umcodierung Variable Allgemeine Lebenszufriedenheit ***

RECODE Lebenszufriedenheit (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3) (8=2) (9=1) (10=0).
EXECUTE. 

*** Anpassung der Labels an die neue Codierung Variable Allgemeine Lebenszufriedenheit ***

VAL LAB Lebenszufriedenheit
0 'GANZ ZUFRIEDEN'
10 'GANZ UNZURIEDEN'
99 'KEINE ANGABE'.
EXECUTE. 


**************
 OST/WEST
**************
*** Dummysierung Ost/Westvariable *** 

RECODE Ost (2=1) (1=0).
EXECUTE. 

*** Anpassung der Labels an die neue Codierung der Ost/Westvariable ***

VAL LAB Ost 
0 'ALTE BUNDESLAENDER'
1 'NEUE BUNDESLAENDER'.
EXECUTE.


****************
STADT/LAND
****************
*** Umcodierung, Dummysierung und Bennung der BIK-Regionen ***

RECODE Land  (1 thru 2=1) (ELSE=0) INTO LAND_klein.
VARIABLE LABELS  Land_klein 'Land_klein'.
EXECUTE.

RECODE Land (1 thru 4=1) (ELSE=0).
EXECUTE.

VAL LAB Land 
0 '>50.001'
1 '<50.000'.
EXECUTE.


**********************
BILDUNGSJAHRE
**********************
*** Codierung Schuljahre anhand der Bildungsabschlüsse sowie Bennenung der neuen Variable ***

RECODE Abschluss (1=7) (2=9) (3=10) (4=12) (5=13) (6=10) (SYSMIS=SYSMIS) (7=SYSMIS) INTO Schuljahre.
VARIABLE LABELS  Schuljahre 'Schuljahre'.

*** Codierung der Ausbildungsdauer (Ausbildungsjahre) anhand der Arten des Abschlusses sowie Bennenung der neuen Variable***

IF (v231 = 1 | v232 = 1 | v235 = 1) Ausbildungsjahre= 1.
IF (v233 = 1 | v234 = 1 | v236 = 1 | v241 = 1) Ausbildungsjahre= 1.5.
IF (v237 = 1 | v238 = 1) Ausbildungsjahre= 2.
IF (v239 = 1 ) Ausbildungsjahre= 3.
IF (v240 = 1 ) Ausbildungsjahre= 5.
EXECUTE.

RECODE Ausbildungsjahre(SYSMIS=0).
VARIABLE LABELS Ausbildungsjahre 'Ausbildungsjahre'.
EXECUTE. 

*** Zusammenfassung der Schul- & Ausbildungsjahre um die Bildungsjahre zu erhalten sowie Bennenung der neuen Variable***

COMPUTE Bildungsjahre = Schuljahre + Ausbildungsjahre.
EXECUTE.

VARIABLE LABELS Bildungsjahre 'Bildungsjahre'.
EXECUTE. 


************
KONTAKT
************
*** Dummysierung Kontaktvariablen *** 

RECODE KFAM KARBEI KNACHBA KFREUN (0=99) (1=0) (2=1) (9=9) (9=SYSMIS) (99=SYSMIS).
EXECUTE.
***0 als fehlenden Wert bei Kontakt entfernen

*** Anpassung der Labels an die neue Codierung der Kontaktvariablen ***

VAL LAB K2 K1 K4 K3
0 'JA'
1 'NEIN'
9 'KEINE ANGABE' 
99 'TRIFFT NICHT ZU'.
EXECUTE.

*** Berechnung und Bennenung der Kontaktskala***

COMPUTE Kontakt_Index= MEAN.1(K1,K2,K3,K4).
EXECUTE.

VARIABLE LABELS Kontakt_Index 'Kontakt_Index '.


***************************
Rassismus Mittelwertindex
***************************

COMPUTE Rassismus_Index= MEAN.2(R1,R2,R3,R4).
EXECUTE.

VARIABLE LABELS Rassismus_Index 'Rassismus_Index'.


*********************************************
Anitmuslimischer Rassismus Mittelwertindex
*********************************************
***Umcodieren 'Islam passt in deutsche Gesellschaft' u. 'Islamischer Bürgermeister in Ordnung'***

RECODE AR2 AR5 (0=0) (99=99) (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1).
EXECUTE.

***Anpassung der Labels 'Islam passt in deutsche Gesellschaft' u. 'Islamischer Bürgermeister in Ordnung'***

VAL LAB AR2 AR5
0 'TRIFFT NICHT ZU'
1 'STIMME VOELLIG ZU'
2 '..'
3 '..'
4 '..'
5 '..'
6 '..'
7 'STIMME GAR NICHT ZU' 
99 'TRIFFT NICHT ZU'.
EXECUTE.

***Berechnung und Bennenung der Skala zum antimuslimischen Rassismus***

COMPUTE AntiMus_Index= MEAN.3(AR1,AR2,AR3,AR4,AR5).
EXECUTE.

VARIABLE LABELS AntiMus_Index 'AntiMus_Index'.

*** anschließend den Datensatz aufsteigend sortieren um das "ziehen" eines Subsets in R zu erleichtern***






