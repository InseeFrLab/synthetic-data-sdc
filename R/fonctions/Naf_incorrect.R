naf_incorrect <- function(data) {
  # NAFG004UN et NAFG010UN
  incorrect1 <- dim(data)[1] -
    table(data$NAFG004UN, data$NAFG010UN)["00", "00"] -
    table(data$NAFG004UN, data$NAFG010UN)["999", "999"] -
    table(data$NAFG004UN, data$NAFG010UN)["ES", "AZ"] -
    table(data$NAFG004UN, data$NAFG010UN)["ET", "BE"] -
    table(data$NAFG004UN, data$NAFG010UN)["EU", "FZ"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "GI"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "JZ"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "KZ"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "LZ"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "MN"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "OQ"] -
    table(data$NAFG004UN, data$NAFG010UN)["EV", "RU"]
  
  # NAFG004UN et NAFG017UN
  incorrect2 <- dim(data)[1] -
    table(data$NAFG004UN, data$NAFG017UN)["00", "00"] -
    table(data$NAFG004UN, data$NAFG017UN)["999", "999"] -
    table(data$NAFG004UN, data$NAFG017UN)["ES", "AZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["ET", "C1"] -
    table(data$NAFG004UN, data$NAFG017UN)["ET", "C2"] -
    table(data$NAFG004UN, data$NAFG017UN)["ET", "C3"] -
    table(data$NAFG004UN, data$NAFG017UN)["ET", "C4"] -
    table(data$NAFG004UN, data$NAFG017UN)["ET", "C5"] -
    table(data$NAFG004UN, data$NAFG017UN)["ET", "DE"] -
    table(data$NAFG004UN, data$NAFG017UN)["EU", "FZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "GZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "HZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "IZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "JZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "KZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "LZ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "MN"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "OQ"] -
    table(data$NAFG004UN, data$NAFG017UN)["EV", "RU"]
  
  # NAFG004UN et NAFG021UN  
  incorrect3 <- dim(data)[1] -
    table(data$NAFG004UN, data$NAFG021UN)["00", "0"] -
    table(data$NAFG004UN, data$NAFG021UN)["999", "999"] -
    table(data$NAFG004UN, data$NAFG021UN)["ES", "A"] -
    table(data$NAFG004UN, data$NAFG021UN)["ET", "B"] -
    table(data$NAFG004UN, data$NAFG021UN)["ET", "C"] -
    table(data$NAFG004UN, data$NAFG021UN)["ET", "D"] -
    table(data$NAFG004UN, data$NAFG021UN)["ET", "E"] -
    table(data$NAFG004UN, data$NAFG021UN)["EU", "F"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "G"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "H"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "I"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "J"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "K"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "L"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "M"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "N"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "O"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "P"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "Q"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "R"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "S"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "T"] -
    table(data$NAFG004UN, data$NAFG021UN)["EV", "U"]
  
  #NAFG010UN et NAFG017UN
  incorrect4 <- dim(data)[1] -
    table(data$NAFG010UN, data$NAFG017UN)["00", "00"] -
    table(data$NAFG010UN, data$NAFG017UN)["999", "999"] -
    table(data$NAFG010UN, data$NAFG017UN)["AZ", "AZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["BE", "C1"] -
    table(data$NAFG010UN, data$NAFG017UN)["BE", "C2"] -
    table(data$NAFG010UN, data$NAFG017UN)["BE", "C3"] -
    table(data$NAFG010UN, data$NAFG017UN)["BE", "C4"] -
    table(data$NAFG010UN, data$NAFG017UN)["BE", "C5"] -
    table(data$NAFG010UN, data$NAFG017UN)["BE", "DE"] -
    table(data$NAFG010UN, data$NAFG017UN)["FZ", "FZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["GI", "GZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["GI", "HZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["GI", "IZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["JZ", "JZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["KZ", "KZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["LZ", "LZ"] -
    table(data$NAFG010UN, data$NAFG017UN)["MN", "MN"] -
    table(data$NAFG010UN, data$NAFG017UN)["OQ", "OQ"] -
    table(data$NAFG010UN, data$NAFG017UN)["RU", "RU"]
  
  # NAFG010UN et NAFG021UN
  incorrect5 <- dim(data)[1] -
    table(data$NAFG010UN, data$NAFG021UN)["00", "0"] -
    table(data$NAFG010UN, data$NAFG021UN)["999", "999"] -
    table(data$NAFG010UN, data$NAFG021UN)["AZ", "A"] -
    table(data$NAFG010UN, data$NAFG021UN)["BE", "B"] -
    table(data$NAFG010UN, data$NAFG021UN)["BE", "C"] -
    table(data$NAFG010UN, data$NAFG021UN)["BE", "D"] -
    table(data$NAFG010UN, data$NAFG021UN)["BE", "E"] -
    table(data$NAFG010UN, data$NAFG021UN)["FZ", "F"] -
    table(data$NAFG010UN, data$NAFG021UN)["GI", "G"] -
    table(data$NAFG010UN, data$NAFG021UN)["GI", "H"] -
    table(data$NAFG010UN, data$NAFG021UN)["GI", "I"] -
    table(data$NAFG010UN, data$NAFG021UN)["JZ", "J"] -
    table(data$NAFG010UN, data$NAFG021UN)["KZ", "K"] -
    table(data$NAFG010UN, data$NAFG021UN)["LZ", "L"] -
    table(data$NAFG010UN, data$NAFG021UN)["MN", "M"] -
    table(data$NAFG010UN, data$NAFG021UN)["MN", "N"] -
    table(data$NAFG010UN, data$NAFG021UN)["OQ", "O"] -
    table(data$NAFG010UN, data$NAFG021UN)["OQ", "P"] -
    table(data$NAFG010UN, data$NAFG021UN)["OQ", "Q"] -
    table(data$NAFG010UN, data$NAFG021UN)["RU", "R"] -
    table(data$NAFG010UN, data$NAFG021UN)["RU", "S"] -
    table(data$NAFG010UN, data$NAFG021UN)["RU", "T"] -
    table(data$NAFG010UN, data$NAFG021UN)["RU", "U"]
  
  #NAFG017UN et NAFG021UN  
  incorrect6 <- dim(data)[1] -
    table(data$NAFG017UN, data$NAFG021UN)["00", "0"] -
    table(data$NAFG017UN, data$NAFG021UN)["999", "999"] -
    table(data$NAFG017UN, data$NAFG021UN)["AZ", "A"] -
    table(data$NAFG017UN, data$NAFG021UN)["C1", "C"] -
    table(data$NAFG017UN, data$NAFG021UN)["C2", "C"] -
    table(data$NAFG017UN, data$NAFG021UN)["C3", "C"] -
    table(data$NAFG017UN, data$NAFG021UN)["C4", "C"] -
    table(data$NAFG017UN, data$NAFG021UN)["C5", "C"] -
    table(data$NAFG017UN, data$NAFG021UN)["DE", "B"] -
    table(data$NAFG017UN, data$NAFG021UN)["DE", "D"] -
    table(data$NAFG017UN, data$NAFG021UN)["DE", "E"] -
    table(data$NAFG017UN, data$NAFG021UN)["FZ", "F"] -
    table(data$NAFG017UN, data$NAFG021UN)["GZ", "G"] -
    table(data$NAFG017UN, data$NAFG021UN)["HZ", "H"] -
    table(data$NAFG017UN, data$NAFG021UN)["IZ", "I"] -
    table(data$NAFG017UN, data$NAFG021UN)["JZ", "J"] -
    table(data$NAFG017UN, data$NAFG021UN)["KZ", "K"] -
    table(data$NAFG017UN, data$NAFG021UN)["LZ", "L"] -
    table(data$NAFG017UN, data$NAFG021UN)["MN", "M"] -
    table(data$NAFG017UN, data$NAFG021UN)["MN", "N"] -
    table(data$NAFG017UN, data$NAFG021UN)["OQ", "O"] -
    table(data$NAFG017UN, data$NAFG021UN)["OQ", "P"] -
    table(data$NAFG017UN, data$NAFG021UN)["OQ", "Q"] -
    table(data$NAFG017UN, data$NAFG021UN)["RU", "R"] -
    table(data$NAFG017UN, data$NAFG021UN)["RU", "S"] -
    table(data$NAFG017UN, data$NAFG021UN)["RU", "T"] -
    table(data$NAFG017UN, data$NAFG021UN)["RU", "U"]
  
  cat("NAFG004UN - NAFG010UN : ", incorrect1,"\n",
      "NAFG004UN - NAFG017UN : ", incorrect2,"\n",
      "NAFG004UN - NAFG021UN : ", incorrect3,"\n",
      "NAFG010UN - NAFG017UN : ", incorrect4,"\n",
      "NAFG010UN - NAFG021UN : ", incorrect5,"\n",
      "NAFG017UN - NAFG021UN : ", incorrect6,"\n")
}
