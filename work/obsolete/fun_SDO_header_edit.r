#library laden

library(stringr)

#Dateinamen einlesen und unter t abspeichern

t <- "20140101_080207_2048_1700.jpg"

#Namen ändern

stringi <- str_replace(t, '(^.{4})(.{2})(.{2})_(.{2})(.{2})(.{2}).*','\\1-\\2-\\3T\\4:\\5:\\6')
 
stringi

#Daten in Header eintragen

header <- addKwv('DATE-OBS', stringi, header=header)
header

closeHdr(header)
