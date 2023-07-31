# Create sample data
qc <- qc_rtf2df("tsfae-st03", system.file(package = "QCfuns")) %>% 
  mutate(Combined = ifelse(X == "Avg exposure (days)", '97.2', Combined))
rtf <- qc_rtf2df("tsfae-st03", system.file(package = "QCfuns"))

# Store outputs in temporary diretory
dir_temp <- tempdir()
qc_comparedf(qc, rtf, path = dir_temp, filename = "tsfae-st03")
