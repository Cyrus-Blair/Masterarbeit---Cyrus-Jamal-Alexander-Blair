# Masterarbeit---Cyrus-Jamal-Alexander-Blair
### Der Einfluss des Digital Markets Act auf Self-Preferencing bei Amazon -
### Eine erneute Untersuchung unter Verwendung der Sichtbarkeitsmetrik von Sistrix


eingereicht beiProf. Dr. Skiera,

Professur für Betriebswirtschaftslehre, insbesondere Marketing

Fachbereich Wirtschaftswissenschaften, Johann Wolfgang Goethe-Universität Frankfurt am Main


von cand. rer. pol. Cyrus Jamal Alexander Blair
 
Esztergomstraße 53

63477 Maintal

0173 9071632

Cyrus.j.a.blair@gmail.com


Fachsemester 4

Matrikelnummer: 7225657



## Projektübersicht

Dieses GitHub-Repository enthält alle Skripte und zugehörigen Datendateien, die im Rahmen meiner Masterarbeit zur Untersuchung potenzielles Self-Preferencing von Amazon nach Inkrafttreten des Digital Markets Act (DMA) verwendet wurden. Die Arbeit analysiert, wie sich die Sichtbarkeit von Produkte die von Amazon verkauft werden, sowie Amazon Eigenmarkenprodukte auf dem Amazon Marketplace nach der Designation Amazons als Gatekeeper gemäß des DMA verändert hat.

Die hier bereitgestellten Skripte sind in zwei Hauptabschnitte unterteilt: Datenerhebung und Datenanalyse. Die Datenerhebung wurde hauptsächlich mit Python und der Sistrix-API durchgeführt, während die Datenanalyse in R erfolgte. Zusätzlich zu den Skripten sind auch die erzeugten CSV-Dateien im Repository verfügbar. 




## Abschnitt 1: Datenerhebung

Dieser Abschnitt beschreibt die Skripte zur Sammlung, Aufbereitung und Zusammenführung der Daten.



1. __Pythonscript: *Getting Top Products per Key Word*__
   
    Dieses Skript ruft die meistverkauften Produkte für die Liste der 100 Suchbegriffevon Waldfogel (2024) von der Sistrix-API ab. Die Ergebnisse, einschließlich Keyword, Datum, Position und ASIN (Amazon Standard Identification Number), werden in einer CSV-Datei gespeichert.
   
3. __Pythonscript: *Getting Details for Top Products per Key Word*__

    Aufbauend auf dem ersten Skript, liest dieses Skript die ASINs aus der zuvor erstellten CSV-Datei und ruft über die Sistrix-API zusätzliche Produktdetails wie Preis, Verkäufername und Kundenbewertungen ab. Diese Informationen werden in einer neuen CSV-Datei gespeichert.

4. __R Script: *MA - Transforming Top Products per Keyword with details*__

    Dieses R-Skript verarbeitet die Produktdetails-Datei. Es bereinigt die Daten und identifiziert Amazon-Eigenmarken. Anschließend wird ein stratifiziertes Stichprobenverfahren angewendet, um einen ausgewogenen Datensatz von 400 Produkten für die Hauptanalyse zu erstellen. Dieser Datensatz umfasst jeweils 200 Produkte, die von Amazon verkauft werden, und 200 Produkte von Drittanbietern, inklusive aller 94 identifizierten Eigenmarken.

5. __Pythonscript: *Getting Visibility Data via Sistrix API*__

    Mit diesem Skript werden die wöchentlichen Sichtbarkeitsdaten für die ausgewählten 400 Produkte von der Sistrix-API abgerufen. Die Daten umfassen den Zeitraum vom 1. Juli 2023 bis zum 24. Februar 2024 und wurden für fünf Länder (Deutschland, Frankreich, Italien, Spanien, Großbritannien) gesammelt. Aufgrund der hohen Anzahl von API-Anfragen, die zu Timeout Fehlern und fehlenden Credits führen können, ist dieses Skript darauf ausgelegt, in mehreren Schritten mit manuell angepassten Funktionsaufrufen ausgeführt zu werden.

6. __R Script 2: *MA - Join data into final dataset*__

    Dieses Skript führt die Produktdetails und die Sichtbarkeitsdaten zusammen, um einen einzigen vollständigen Datensatz zu erstellen. Das Ergebnis wird in einer neuen CSV-Datei gespeichert.



## Abschnitt 2: Datenanalyse

Dieser Abschnitt umfasst die Analyse des finalen Datensatzes.



__R Script: *MA - Analyse der Sistrix Daten*__

Dieses zentrale Skript bereitet den finalen Datensatz für die Analyse vor. Es führt deskriptive Statistiken, Mittelwertsvergleiche und die primären Analysen der Masterarbeit durch. Dazu gehören:

- Deskriptive Beschreibungen des Datensatz
- Histogramme für die abhängige Variable
- Voruntersuchungen: Einfache OLS-Regressionen, um erste Zusammenhänge zu erkennen.
- Zentrale Analyse: Difference-in-Differences-Analysen (DiD), um den kausalen Effekt der DMA-Gatekeeper-Designation auf die Produktsichtbarkeit zu schätzen.
- Robustheitsanalysen: Überprüfung der Ergebnisse durch Variation des Stichtags, um mögliche Anpassungs- und Antizipationseffekte zu berücksichtigen.
- Event Studies: Weitere Analysen zur Untersuchung der zeitlichen Dynamik der Effekte.
  
Das Skript erstellt außerdem alle Tabellen und Abbildungen, die in der Masterarbeit verwendet wurden.








