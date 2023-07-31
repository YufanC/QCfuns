# Create sample data
qc <- qc_rtf2df("tsflab-st01", system.file(package = "QCfuns"))
rtf <- qc_rtf2df("tsflab-st01", system.file(package = "QCfuns"))
warning("test warning")

# Store outputs in temporary diretory
dir_temp <- tempdir()
qc_comparedf(qc, rtf, path = dir_temp, filename = "tsflab-st01")
