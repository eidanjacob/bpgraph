systolic = function(time){
  if(time < 6){
    return(140 + rnorm(1)*3)
  }
  if(time < 12){
    return(160 + rnorm(1)*4)
  }
  return(130 + rnorm(1)*5)
}

diastolic = function(time){
  if(time < 6){
    return(100 + rnorm(1)*3)
  }
  if(time < 12){
    return(120 + rnorm(1)*4)
  }
  return(85 + rnorm(1)*5)
}

time = rep(c(8, 8, 12, 12, 16, 16, 20, 20), 7)
arm = c("left", "right")
last_sun = floor_date(Sys.Date(), "week")
day = rep(last_sun, length(time)) + c(rep(0, 8), rep(1, 8), rep(2, 8), rep(3,8), rep(4, 8), rep(5,8), rep(6,8))
day = c(day, day + 7)
template = data.frame("systolic" = sapply(c(time,time), systolic), 
                      "diastolic" = sapply(c(time,time), diastolic), 
                      time, arm, day)
write.csv(template, "bpTemplate.csv", row.names = FALSE)