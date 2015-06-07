asciiToSixBit <- function(asciiIn) {
  # A function to convert a single ASCII character to 6-bit binary.  Necessary
  # because NMEA uses 6-bit packing.
  
  # Make sure that the input is a single ASCII character.
  asciiIn <- as.character(asciiIn)
  if(nchar(asciiIn) > 1) stop("Input must be a single ASCII character.")
  
  # Covert the ASCII to an integer.  To prepare the value for 6-bit binary,
  # subtract 48.  If the resulting value is greater than 40, subtract 8.
  intValue <- as.integer(charToRaw(asciiIn)) - 48L
  if(intValue > 40L) intValue <- intValue - 8L
  
  # Convert to 6-bit binary using a method proposed here:
  # http://stackoverflow.com/questions/12088080/how-to-convert-number-into-binary-vector
  reverseBin <- rev(as.numeric(intToBits(intValue)))
  lrB <- length(reverseBin)
  rev(reverseBin[lrB:(lrB-5)])
}

nmeaToBin <- function(nmeaIn) {
  # Converts the ASCII portion of a NMEA message to 6-bit binned binary.
  
  nmeaIn <- as.character(nmeaIn)
  splitNMEA <- unlist(strsplit(nmeaIn, split = ""))
  as.numeric(sapply(splitNMEA, FUN = asciiToSixBit))
}

binToDec <- function(binVec, startInd, endInd) {
  strtoi(paste0(binVec[startInd:endInd], collapse = ""), base = 2)
}

posRepParse <- function(posRep) {
  # Takes as input a binary AIS message such as that returned by nmeaToBin.
  
  MessageID <- binToDec(posRep, 1, 6)
  RepInd <- binToDec(posRep, 7, 8)
  MMSI <- binToDec(posRep, 9, 38)
  NavStatus <- binToDec(posRep, 39, 42)
  ROT <- binToDec(posRep, 43, 50) # Correct this - this is signed binary
  SOG <- binToDec(posRep, 51, 60)
  PosAcc <- binToDec(posRep, 61, 61)
  
  
  list(MessageID, RepInd, MMSI, NavStatus, ROT, SOG, PosAcc)
}