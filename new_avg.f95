program test_read

   implicit none
   integer, parameter :: VLI_K = selected_int_kind (18)
   integer, parameter :: DR_K = selected_real_kind (14)

   integer  :: i,j,k,l,m,n,suma
   !real (DR_K) :: a, b, c, d
   character *10:: a,b,c,d,e,f,g,h
   real :: f_ave,b_ave,w_ave,m_ave,c_ave,o,p,q,r,s,t
   real :: phy_ave,mat_ave,bio_ave,his_ave,eng_ave,fre_ave


   open (unit=15, file="data.dat", status='old',    &
             access='sequential', form='formatted', action='read' )

   open(12, file="test.txt", status="new", action="write")

   !read (15, 110)  i, a, b, c, d
   110 format (I16, 4(1X, F10.0) )
   !write (*, 120) i, a, b, c, d
   121 format ( A4, 16X ,A7 , 5X , A5 , 5X , A7 , 5X , A7 , 5X , A7 , 5X , A6)
   123 format ( A4, 16X ,A7 , 5X , A5 , 5X , A7 , 5X , A7 , 5X , A7 , 5X , A6, 3x ,'Average')
   120 format ( A6, 1X, A4, 9X ,I2 , 10X , I2 , 8X , I2 , 10X , I2 , 10X , I2 , 10X , I2,7x,f5.2 )

   122 format ( 'Average', 13X ,f5.2 , 7X , f5.2 , 5X , f5.2 , 7X , f5.2 , 7X , f5.2 , 7X , f5.2 )

   read (15, 121) a,b,c,d,e,f,g
   write (12, 123) a,b,c,d,e,f,g
   read (15, *) a,b,i,j,k,l,m,n
   o=i
   p=j
   q=k
   r=l
   s=m
   t=n
   f_ave=(o+p+((q/40)*100)+r+s+t)/6
   phy_ave=i
   mat_ave=j
   bio_ave=k
   his_ave=l
   eng_ave=m
   fre_ave=n
   write (12, 120) a,b,i,j,k,l,m,n,f_ave
   read (15, *) a,b,i,j,k,l,m,n
   phy_ave=phy_ave+i
   mat_ave=mat_ave+j
   bio_ave=bio_ave+k
   his_ave=his_ave+l
   eng_ave=eng_ave+m
   fre_ave=fre_ave+n

   o=i
   p=j
   q=k
   r=l
   s=m
   t=n
   b_ave=(o+p+((q/40)*100)+r+s+t)/6
   write (12, 120) a,b,i,j,k,l,m,n,b_ave
   read (15, *) a,b,i,j,k,l,m,n

   o=i
   p=j
   q=k
   r=l
   s=m
   t=n
   w_ave=(o+p+((q/40)*100)+r+s+t)/6

   phy_ave=phy_ave+i
   mat_ave=mat_ave+j
   bio_ave=bio_ave+k
   his_ave=his_ave+l
   eng_ave=eng_ave+m
   fre_ave=fre_ave+n
   write (12, 120) a,b,i,j,k,l,m,n,w_ave
   read (15, *) a,b,i,j,k,l,m,n

   phy_ave=phy_ave+i
   mat_ave=mat_ave+j
   bio_ave=bio_ave+k
   his_ave=his_ave+l
   eng_ave=eng_ave+m
   fre_ave=fre_ave+n
   o=i
   p=j
   q=k
   r=l
   s=m
   t=n
   m_ave=(o+p+((q/40)*100)+r+s+t)/6
   write (12, 120) a,b,i,j,k,l,m,n,m_ave
   read (15, *) a,b,i,j,k,l,m,n

   phy_ave=phy_ave+i
   mat_ave=mat_ave+j
   bio_ave=bio_ave+k
   his_ave=his_ave+l
   eng_ave=eng_ave+m
   fre_ave=fre_ave+n
   o=i
   p=j
   q=k
   r=l
   s=m
   t=n
   c_ave=(o+p+((q/40)*100)+r+s+t)/6
   write (12, 120) a,b,i,j,k,l,m,n,c_ave

   phy_ave=phy_ave/5
   mat_ave=mat_ave/5
   bio_ave=bio_ave/5
   his_ave=his_ave/5
   eng_ave=eng_ave/5
   fre_ave=fre_ave/5
   write (12, 122) phy_ave,mat_ave,bio_ave,his_ave,eng_ave,fre_ave


end program test_read
 !!author murad-pc