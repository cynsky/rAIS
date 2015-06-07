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
  nmeaIn <- as.character(nmeaIn)
  splitNMEA <- unlist(strsplit(nmeaIn, split = ""))
  as.numeric(sapply(splitNMEA, FUN = asciiToSixBit))
}