library(plumber)

port <- Sys.getenv('PORT')

# 'plumber.R' is the location of the file shown above
pr("plumber.R") %>%
  pr_run(host='0.0.0.0', port=as.numeric(port))
