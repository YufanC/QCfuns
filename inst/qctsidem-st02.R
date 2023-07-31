# Create sample data
qc <- qc_rtf2df("tsidem-st02", system.file(package = "QCfuns"))
rtf <- qc_rtf2df("tsidem-st02", system.file(package = "QCfuns"))

# Store outputs in temporary diretory
dir_temp <- tempdir()
qc_comparedf(qc, rtf, path = dir_temp, filename = "tsidem-st02")
