r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

if(!require(microbenchmark)){
  install.packages("microbenchmark")
}

if(!require(TTR)){
  install.packages("TTR")
}

if(!require(xgboost)){
  install.packages("xgboost")
}

if(!require(data.table)){
  install.packages("data.table")
}

if(!require(RhpcBLASctl)){
  install.packages("RhpcBLASctl")
}

if(!require(magrittr)){
  install.packages("magrittr")
}



print("Packages installed successfully")
