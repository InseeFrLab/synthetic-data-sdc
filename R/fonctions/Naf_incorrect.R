naf_incorrect <- function(data_syn) {
  # NAFG004UN et NAFG010UN
  incorrect1 <- dim(data_syn)[1] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["00", "00"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["999", "999"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["ES", "AZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["ET", "BE"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EU", "FZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "GI"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "JZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "KZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "LZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "MN"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "OQ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG010UN)["EV", "RU"]
  
  # NAFG004UN et NAFG017UN
  incorrect2 <- dim(data_syn)[1] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["00", "00"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["999", "999"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ES", "AZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ET", "C1"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ET", "C2"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ET", "C3"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ET", "C4"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ET", "C5"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["ET", "DE"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EU", "FZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "GZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "HZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "IZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "JZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "KZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "LZ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "MN"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "OQ"] -
    table(data_syn$NAFG004UN, data_syn$NAFG017UN)["EV", "RU"]
  
  # NAFG004UN et NAFG021UN  
  incorrect3 <- dim(data_syn)[1] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["00", "0"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["999", "999"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["ES", "A"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["ET", "B"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["ET", "C"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["ET", "D"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["ET", "E"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EU", "F"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "G"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "H"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "I"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "J"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "K"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "L"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "M"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "N"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "O"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "P"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "Q"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "R"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "S"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "T"] -
    table(data_syn$NAFG004UN, data_syn$NAFG021UN)["EV", "U"]
  
  #NAFG010UN et NAFG017UN
  incorrect4 <- dim(data_syn)[1] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["00", "00"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["999", "999"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["AZ", "AZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["BE", "C1"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["BE", "C2"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["BE", "C3"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["BE", "C4"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["BE", "C5"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["BE", "DE"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["FZ", "FZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["GI", "GZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["GI", "HZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["GI", "IZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["JZ", "JZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["KZ", "KZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["LZ", "LZ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["MN", "MN"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["OQ", "OQ"] -
    table(data_syn$NAFG010UN, data_syn$NAFG017UN)["RU", "RU"]
  
  # NAFG010UN et NAFG021UN
  incorrect5 <- dim(data_syn)[1] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["00", "0"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["999", "999"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["AZ", "A"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["BE", "B"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["BE", "C"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["BE", "D"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["BE", "E"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["FZ", "F"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["GI", "G"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["GI", "H"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["GI", "I"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["JZ", "J"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["KZ", "K"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["LZ", "L"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["MN", "M"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["MN", "N"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["OQ", "O"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["OQ", "P"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["OQ", "Q"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["RU", "R"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["RU", "S"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["RU", "T"] -
    table(data_syn$NAFG010UN, data_syn$NAFG021UN)["RU", "U"]
  
  #NAFG017UN et NAFG021UN  
  incorrect6 <- dim(data_syn)[1] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["00", "0"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["999", "999"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["AZ", "A"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["C1", "C"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["C2", "C"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["C3", "C"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["C4", "C"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["C5", "C"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["DE", "B"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["DE", "D"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["DE", "E"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["FZ", "F"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["GZ", "G"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["HZ", "H"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["IZ", "I"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["JZ", "J"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["KZ", "K"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["LZ", "L"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["MN", "M"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["MN", "N"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["OQ", "O"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["OQ", "P"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["OQ", "Q"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["RU", "R"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["RU", "S"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["RU", "T"] -
    table(data_syn$NAFG017UN, data_syn$NAFG021UN)["RU", "U"]
  
  cat("NAFG004UN - NAFG010UN : ", incorrect1,"\n",
      "NAFG004UN - NAFG017UN : ", incorrect2,"\n",
      "NAFG004UN - NAFG021UN : ", incorrect3,"\n",
      "NAFG010UN - NAFG017UN : ", incorrect4,"\n",
      "NAFG010UN - NAFG021UN : ", incorrect5,"\n",
      "NAFG017UN - NAFG021UN : ", incorrect6,"\n")
}
