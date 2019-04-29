
# Code for updating sysdata.rda in directory "R"
# get value of api key for SMC

SMCvarcodes <- downloadSMCvarmetadata(api, type = "current")
SMChistvarcodes <- downloadSMCvarmetadata(api, type = "historical")
usethis::use_data(SMCvarcodes, SMChistvarcodes, internal= TRUE, overwrite= TRUE)

# Reload library