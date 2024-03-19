i<-1
while(i<6){
  print(i)
  i<- i+1
}

i<-1
while(i<6){
  print(i)
  i<- i+1
  if(i==4){
    break
    
  }
}

i<-0
while(i<6){
  i<- i+1
  if(i==3){
    next
  }
  print(i)
}

for(x in 1:10){
  print(x)
}

for(x in 1:2){
  for(y in 1:3){
    print(x*y)
  }
}

sum <-function(){
  print("Hello")
}
sum()


