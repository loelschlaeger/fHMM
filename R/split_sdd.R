split_sdd = function(sdd, all_sdds = c("t","gamma")) {
  
  fixed = list()
  
  s1 = unlist(strsplit(x, split = "[()]"))
  sub(".*[t|gamma]","",x)
  
  out = list("name" = name, fixed = fixed)
}