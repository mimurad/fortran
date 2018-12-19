 program emnei
 !implicit none
 
 integer x(100)
 read*,(x(i),i=1,10)
 print*,(x(i),i=1,10)
  !"comment section it is"
 
2 read*,a,b,c
 	if(a.eq.0)then
   	print*,"a can not be zero"
   	goto 2
   	end if
  print*,a,b,c
 
 end program emnei
 !!author murad-pc