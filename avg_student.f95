program avg
implicit none
character*15::name(50) 
integer::number(300),i,n,z,y,x1,x2,x3,x4,x5,x6
real::r(50),gpa(300),cgpa(50)
z=1
x1=1
x2=2
x3=3
x4=4
x5=5
x6=6
print*,'Enter How Many Student(s):'
READ(*,1)name(z),number(x1),number(x2),number(x3),number(x4),number(x5),number(x6)

1 format ( A4, 16X ,A7 , 5X , A5 , 5X , A7 , 5X , A7 , 5X , A7 , 5X , A6)
  
  
  r(z)=(number(x1)+number(x2)+number(x3)+number(x4)+number(x5)+number(x6))/6
  z=z+1
  x1=x1+6
  x2=x2+6
  x3=x3+6
  x4=x4+6
  x5=x5+6
  x6=x6+6
  
z=1
x1=1
x2=2
x3=3
x4=4
x5=5
x6=6
print*,""
print*,""
print*,"LIST OF STUDENTS RESULT :"
PRINT*,'------------------------------------------------------------------------------------'
WRITE(*,2)
2 format(4x,'Name',5x,'Physics',3x,'Maths',3x,'Biology',3x,'History',3x,'English',3x,'French',3x,'Average')
PRINT*,'------------------------------------------------------------------------------------'
print*,""
do i=1,n  
  !READ 2,name(z),number(x1),number(x2),number(x3),number(x4),number(x5),number(x6)
  !2 format(a5,i2,i2,i2,i2,i2,i2)
  print3,name(z),number(x1),number(x2),number(x3),number(x4),number(x5),number(x6),r(z)
  3 format(4x,a5,6x,i3,6x,i3,6x,i3,6x,i3,8x,i3,6x,i3,5x,f5.2)
  
    z=z+1
  x1=x1+6
  x2=x2+6
  x3=x3+6
  x4=x4+6
  x5=x5+6
  x6=x6+6
  end do

PRINT*,'------------------------------------------------------------------------------------'
z=1
x1=1
x2=2
x3=3
x4=4
x5=5
x6=6
print*,""
print*,""
print*,"CALCULATED CGPA :"
PRINT*,'------------------------------------------------------------------------------------'
WRITE(*,5)
5 format(4x,'Name',5x,'Physics',3x,'Maths',3x,'Biology',3x,'History',3x,'English',3x,'French',3x,'C.G.P.A')
PRINT*,'------------------------------------------------------------------------------------'
do i=1,n  
  if(number(x1)<40)then
    gpa(x1)=0.00
  else if(number(x1)<45.and.number(x1)>=40)then
    gpa(x1)=2.00 
  else if(number(x1)<50.and.number(x1)>=45)then
    gpa(x1)=2.25
  else if(number(x1)<55.and.number(x1)>=50)then
    gpa(x1)=2.50
  else if(number(x1)<60.and.number(x1)>=55)then
    gpa(x1)=2.75
  else if(number(x1)<65.and.number(x1)>=60)then
    gpa(x1)=3.00
  else if(number(x1)<70.and.number(x1)>=65)then
    gpa(x1)=3.25
  else if(number(x1)<75.and.number(x1)>=70)then
    gpa(x1)=3.50
  else if(number(x1)<80.and.number(x1)>=75)then
    gpa=3.75
  else if(number(x1)<=100.and.number(x1)>=80)then
    gpa=4.00
    
  end if 
  if(number(x2)<40)then
    gpa(x2)=0.00   
  else if(number(x2)<45.and.number(x2)>=40)then
    gpa(x2)=2.00 
  else if(number(x2)<50.and.number(x2)>=45)then
    gpa(x2)=2.25
  else if(number(x2)<55.and.number(x2)>=50)then
    gpa(x2)=2.50
  else if(number(x2)<60.and.number(x2)>=55)then
    gpa(x2)=2.75
  else if(number(x2)<65.and.number(x2)>=60)then
    gpa(x2)=3.00
  else if(number(x2)<70.and.number(x2)>=65)then
    gpa(x2)=3.25
  else if(number(x2)<75.and.number(x2)>=70)then
    gpa(x2)=3.50
  else if(number(x2)<80.and.number(x2)>=75)then
    gpa(x2)=3.75
  else if(number(x2)<=100.and.number(x2)>=80)then
    gpa(x2)=4.00
    
  end if  
  if(number(x3)<40)then
    gpa(x3)=0.00  
  else if(number(x3)<45.and.number(x3)>=40)then
    gpa(x3)=2.00 
  else if(number(x3)<50.and.number(x3)>=45)then
    gpa(x3)=2.25
  else if(number(x3)<55.and.number(x3)>=50)then
    gpa(x3)=2.50
  else if(number(x3)<60.and.number(x3)>=55)then
    gpa(x3)=2.75
  else if(number(x3)<65.and.number(x3)>=60)then
    gpa(x3)=3.00
  else if(number(x3)<70.and.number(x3)>=65)then
    gpa(x3)=3.25
  else if(number(x3)<75.and.number(x3)>=70)then
    gpa(x3)=3.50
  else if(number(x3)<80.and.number(x3)>=75)then
    gpa(x3)=3.75
  else if(number(x3)<=100.and.number(x3)>=80)then
    gpa(x3)=4.00
    
  end if 
  if(number(x4)<40)then
    gpa(x4)=0.00 
  else if(number(x4)<45.and.number(x4)>=40)then
    gpa(x4)=2.00 
  else if(number(x4)<50.and.number(x4)>=45)then
    gpa(x4)=2.25
  else if(number(x4)<55.and.number(x4)>=50)then
    gpa(x4)=2.50
  else if(number(x4)<60.and.number(x4)>=55)then
    gpa(x4)=2.75
  else if(number(x4)<65.and.number(x4)>=60)then
    gpa(x4)=3.00
  else if(number(x4)<70.and.number(x4)>=65)then
    gpa(x4)=3.25
  else if(number(x4)<75.and.number(x4)>=70)then
    gpa(x4)=3.50
  else if(number(x4)<80.and.number(x4)>=75)then
    gpa(x4)=3.75
  else if(number(x4)<=100.and.number(x4)>=80)then
    gpa(x4)=4.00
    
  end if
  if(number(x5)<40)then
    gpa(x5)=0.00      
  else if(number(x5)<45.and.number(x5)>=40)then
    gpa(x5)=2.00 
  else if(number(x5)<50.and.number(x5)>=45)then
    gpa(x5)=2.25
  else if(number(x5)<55.and.number(x5)>=50)then
    gpa(x5)=2.50
  else if(number(x5)<60.and.number(x5)>=55)then
    gpa(x5)=2.75
  else if(number(x5)<65.and.number(x5)>=60)then
    gpa(x5)=3.00
  else if(number(x5)<70.and.number(x5)>=65)then
    gpa(x5)=3.25
  else if(number(x5)<75.and.number(x5)>=70)then
    gpa(x5)=3.50
  else if(number(x5)<80.and.number(x5)>=75)then
    gpa(x5)=3.75
  else if(number(x5)<=100.and.number(x5)>=80)then
    gpa(x5)=4.00
    
  end if  

  if(number(x6)<40)then
    gpa(x6)=0.00      
  else if(number(x6)<45.and.number(x6)>=40)then
    gpa(x6)=2.00 
  else if(number(x6)<50.and.number(x6)>=45)then
    gpa(x6)=2.25
  else if(number(x6)<55.and.number(x6)>=50)then
    gpa(x6)=2.50
  else if(number(x6)<60.and.number(x6)>=55)then
    gpa(x6)=2.75
  else if(number(x6)<65.and.number(x6)>=60)then
    gpa(x6)=3.00
  else if(number(x6)<70.and.number(x6)>=65)then
    gpa(x6)=3.25
  else if(number(x6)<75.and.number(x6)>=70)then
    gpa(x6)=3.50
  else if(number(x6)<80.and.number(x6)>=75)then
    gpa(x6)=3.75
  else if(number(x6)<=100.and.number(x6)>=80)then
    gpa(x6)=4.00
    
  end if  
  cgpa(z)=(gpa(x1)+gpa(x2)+gpa(x3)+gpa(x4)+gpa(x5)+gpa(x6))/6
  print6,name(z),gpa(x1),gpa(x2),gpa(x3),gpa(x4),gpa(x5),gpa(x6),cgpa(z)
  6 format(4x,a5,6x,f5.2,3x,f5.2,3x,f5.2,6x,f5.2,5x,f5.2,4x,f5.2,4x,f5.2)
  z=z+1
  x1=x1+6
  x2=x2+6
  x3=x3+6
  x4=x4+6
  x5=x5+6
  x6=x6+6
 
   
end do
  
end avg
 !!author murad-pc