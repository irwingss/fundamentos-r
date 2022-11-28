fac <- function(x) {
  f=1
  for (i in 1:x) {
    f = f * i
    print(f)
  }
  print(f)
}

fac2 <- function(x) {
  f=1
  for (i in 1:x) {
    f = f * i
    print(f)
  }
  return(f)
}

fac3 <- function(x) {
  f=1
  for (i in 1:x) {
    f = f * i
    return(f)
  }
  return(f)
}

f1 <- fac(3)
f2 <- fac2(3)
f3 <- fac3(3)


f1
f2
f3
