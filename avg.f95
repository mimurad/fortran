
 	PROGRAM LAB
    IMPLICIT NONE
    	CHARACTER*8::F_NAME(10),L_NAME(10),C
		REAL::A1(10),A2(10),A3(10),A4(10),A5(10),A6(10)
    	INTEGER::I,PERSON,Z,J
        REAL::AVG(10),R

        PRINT*,"ENTER THE NUBBER OF PERSON : "
        READ*,PERSON
		
		PRINT*,'ENTER THE DATA'
        PRINT*,'------------------------------------------------------------------------------------'
        WRITE(*,20)'NAME','PHYSICS','MATHS','BIOLOGY','HISTORY','ENGLISH','FRENCH'
        PRINT*,'------------------------------------------------------------------------------------'        
20		FORMAT(3X,A,4X,4X,A,4X,A,4X,A,4X,A,4X,A,4X,A)
		
        DO I=1,PERSON,1
            READ*,F_NAME(I),L_NAME(I),A1(I),A2(I),A3(I),A4(I),A5(I),A6(I)
            AVG(I)=(A1(I)+A2(I)+A3(I)+A4(I)+A5(I)+A6(I))/6
        END DO
        
        PRINT*,'-----------------------------------------------------------------------------------------'
		PRINT*,""
        PRINT*,""
        PRINT*,'AQUIRED MARKS'
        PRINT*,'--------------------------------------------------------------------------------------------'
        WRITE(*,30)'NAME','PHYSICS','MATHS','BIOLOGY','HISTORY','ENGLISH','FRENCH','AVERAGE-MARKS'
        PRINT*,'--------------------------------------------------------------------------------------------'
30		FORMAT(5X,A,8X,3X,A,3X,A,3X,A,3X,A,3X,A,3X,A,3X,A)
		
        DO I=1,PERSON,1
          	WRITE(*,40)F_NAME(I),L_NAME(I),INT(A1(I)),INT(A2(I)),INT(A3(I)),INT(A4(I)),INT(A5(I)),INT(A6(I)),AVG(I)
40          FORMAT(2X,A,1X,A,2X,I3,6X,I3,6X,I3,7X,I3,7X,I3,7X,I3,9X,F4.1)
        END DO
        
        PRINT*,'--------------------------------------------------------------------------------------------'
        
		DO I=1,PERSON,1
        	Z=6
            ! MARKS OUT OF 100
            IF(A1(I)<40) THEN
              	A1(I)=0.0
                Z=Z-1
            ELSE IF(A1(I)>39 .AND. A1(I)<45) THEN
              	A1(I)=2.0
            ELSE IF(A1(I)>44 .AND. A1(I)<50) THEN
              	A1(I)=2.25
            ELSE IF(A1(I)>49 .AND. A1(I)<55) THEN
              	A1(I)=2.50
            ELSE IF(A1(I)>54 .AND. A1(I)<60) THEN
              	A1(I)=2.75
            ELSE IF(A1(I)>59 .AND. A1(I)<65) THEN
              	A1(I)=3.0
            ELSE IF(A1(I)>64 .AND. A1(I)<70) THEN
              	A1(I)=3.25
            ELSE IF(A1(I)>69 .AND. A1(I)<75) THEN
              	A1(I)=3.50        
            ELSE IF(A1(I)>74 .AND. A1(I)<80) THEN
              	A1(I)=3.75
           	ELSE IF(A1(I)>79) THEN 
            	A1(I)=4.0  
            END IF

            ! MARKS OUT OF 100
            IF(A2(I)<40) THEN
              	A2(I)=0.0
                Z=Z-1
            ELSE IF(A2(I)>39 .AND. A2(I)<45) THEN
              	A2(I)=2.0
            ELSE IF(A2(I)>44 .AND. A2(I)<50) THEN
              	A2(I)=2.25
            ELSE IF(A2(I)>49 .AND. A2(I)<55) THEN
              	A2(I)=2.50
            ELSE IF(A2(I)>54 .AND. A2(I)<60) THEN
              	A2(I)=2.75
            ELSE IF(A2(I)>59 .AND. A2(I)<65) THEN
              	A2(I)=3.0
            ELSE IF(A2(I)>64 .AND. A2(I)<70) THEN
              	A2(I)=3.25
            ELSE IF(A2(I)>69 .AND. A2(I)<75) THEN
              	A2(I)=3.50        
            ELSE IF(A2(I)>74 .AND. A2(I)<80) THEN
              	A2(I)=3.75
           	ELSE IF(A2(I)>79) THEN 
               	A2(I)=4.0
            END IF

            ! MARKS OUT OF 40
            IF(A3(I)<16) THEN
              	A3(I)=0.0
                Z=Z-1
            ELSE IF(A3(I)>15 .AND. A3(I)<18) THEN
              	A3(I)=2.0
            ELSE IF(A3(I)>17 .AND. A3(I)<20) THEN
              	A3(I)=2.25
            ELSE IF(A3(I)>19 .AND. A3(I)<22) THEN
              	A3(I)=2.50
            ELSE IF(A3(I)>21 .AND. A3(I)<24) THEN
              	A3(I)=2.75
            ELSE IF(A3(I)>23 .AND. A3(I)<26) THEN
              	A3(I)=3.0
            ELSE IF(A3(I)>25 .AND. A3(I)<28) THEN
              	A3(I)=3.25
            ELSE IF(A3(I)>27 .AND. A3(I)<30) THEN
              	A3(I)=3.50        
            ELSE IF(A3(I)>29 .AND. A3(I)<32) THEN
              	A3(I)=3.75
           	ELSE IF(A3(I)>31) THEN 
            	A3(I)=4.0
            END IF

            ! MARKS OUT OF 100
            IF(A4(I)<40) THEN
              	A4(I)=0.0
                Z=Z-1
            ELSE IF(A4(I)>39 .AND. A4(I)<45) THEN
              	A4(I)=2.0
            ELSE IF(A4(I)>44 .AND. A4(I)<50) THEN
              	A4(I)=2.25
            ELSE IF(A4(I)>49 .AND. A4(I)<55) THEN
              	A4(I)=2.50
            ELSE IF(A4(I)>54 .AND. A4(I)<60) THEN
              	A4(I)=2.75
            ELSE IF(A4(I)>59 .AND. A4(I)<65) THEN
              	A4(I)=3.0
            ELSE IF(A4(I)>64 .AND. A4(I)<70) THEN
              	A4(I)=3.25
            ELSE IF(A4(I)>69 .AND. A4(I)<75) THEN
              	A4(I)=3.50        
            ELSE IF(A4(I)>74 .AND. A4(I)<80) THEN
              	A4(I)=3.75
           	ELSE IF(A4(I)>79) THEN 
            	A4(I)=4.0
           	END IF

            ! MARKS OUT OF 100
            IF(A5(I)<40) THEN
              	A5(I)=0.0
                Z=Z-1
            ELSE IF(A5(I)>39 .AND. A5(I)<45) THEN
              	A5(I)=2.0
            ELSE IF(A5(I)>44 .AND. A5(I)<50) THEN
              	A5(I)=2.25
            ELSE IF(A5(I)>49 .AND. A5(I)<55) THEN
              	A5(I)=2.50
            ELSE IF(A5(I)>54 .AND. A5(I)<50) THEN
              	A5(I)=2.75
            ELSE IF(A5(I)>59 .AND. A5(I)<65) THEN
              	A5(I)=3.0
            ELSE IF(A5(I)>64 .AND. A5(I)<70) THEN
              	A5(I)=3.25
            ELSE IF(A5(I)>69 .AND. A5(I)<75) THEN
              	A5(I)=3.50        
            ELSE IF(A5(I)>74 .AND. A5(I)<80) THEN
              	A5(I)=3.75
           	ELSE IF(A5(I)>79) THEN 
            	A5(I)=4.0
			END IF
                
			! MARKS OUT OF 100
            IF(A6(I)<40) THEN
              	A6(I)=0.0
                Z=Z-1
            ELSE IF(A6(I)>39 .AND. A6(I)<45) THEN
              	A6(I)=2.0
            ELSE IF(A6(I)>44 .AND. A6(I)<50) THEN
              	A6(I)=2.25
            ELSE IF(A6(I)>49 .AND. A6(I)<55) THEN
              	A6(I)=2.50
            ELSE IF(A6(I)>54 .AND. A6(I)<60) THEN
              	A6(I)=2.75
            ELSE IF(A6(I)>59 .AND. A6(I)<65) THEN
              	A6(I)=3.0
            ELSE IF(A6(I)>64 .AND. A6(I)<70) THEN
              	A6(I)=3.25
            ELSE IF(A6(I)>69 .AND. A6(I)<75) THEN
              	A6(I)=3.50        
            ELSE IF(A6(I)>74 .AND. A6(I)<80) THEN
              	A6(I)=3.75
           	ELSE IF(A6(I)>79) THEN 
            	A6(I)=4.0
            END IF
            
			IF(Z/=0) THEN
				AVG(I)=(A1(I)+A2(I)+A3(I)+A4(I)+A5(I)+A6(I))/Z    
            ELSE
              	AVG(I)=0.0
           	END IF
            
		END DO
        PRINT*,""
        PRINT*,""
        PRINT*,"AQUIRED CGPA BEFORE SORTING"
        PRINT*,'---------------------------------------------------------------------------------------------'
        WRITE(*,50)'NAME','PHYSICS','MATHS','BIOLOGY','HISTORY','ENGLISH','FRENCH','AVERAGE-CGPA'
        PRINT*,'---------------------------------------------------------------------------------------------'
50		FORMAT(5X,A,8X,3X,A,4X,A,4X,A,3X,A,3X,A,3X,A,4X,A)

        DO I=1,PERSON,1
          	WRITE(*,60)F_NAME(I),L_NAME(I),A1(I),A2(I),A3(I),A4(I),A5(I),A6(I),AVG(I)
60          FORMAT(2X,A,1X,A,2X,F4.2,6X,F4.2,6X,F4.2,6X,F4.2,6X,F4.2,6X,F4.2,6X,F7.2)
        END DO
        PRINT*,'---------------------------------------------------------------------------------------------'

        DO I=1,PERSON-1,1
          	DO J=2,PERSON,1
            	IF(AVG(J)>AVG(J-1)) THEN
                	! EXCHANGING ALL THE DATA OF THE TWO LINES
       				             
                    C=F_NAME(J)
                    F_NAME(J)=F_NAME(J-1)
                    F_NAME(J-1)=C
                    
                    C=L_NAME(J)
                    L_NAME(J)=L_NAME(J-1)
                    L_NAME(J-1)=C
                    
                    R=A1(J)
                    A1(J)=A1(J-1)
                    A1(J-1)=R
                    
                    R=A2(J)
                    A2(J)=A2(J-1)
                    A2(J-1)=R
                    
                    R=A3(J)
                    A3(J)=A3(J-1)
                    A3(J-1)=R
                    
                    R=A4(J)
                    A4(J)=A4(J-1)
                    A4(J-1)=R
                    
                    R=A5(J)
                    A5(J)=A5(J-1)
                    A5(J-1)=R
                    
                    R=A6(J)
                    A6(J)=A6(J-1)
                    A6(J-1)=R

                    R=AVG(J)
                    AVG(J)=AVG(J-1)
                    AVG(J-1)=R
               	END IF
          	END DO
     	END DO
      
        PRINT*,""
        PRINT*,""
        PRINT*,"AQUIRED CGPA AFTER SORTING"
        PRINT*,'---------------------------------------------------------------------------------------------'
        WRITE(*,70)'NAME','PHYSICS','MATHS','BIOLOGY','HISTORY','ENGLISH','FRENCH','AVERAGE-CGPA'
        PRINT*,'---------------------------------------------------------------------------------------------'
70		FORMAT(5X,A,8X,3X,A,4X,A,4X,A,3X,A,3X,A,3X,A,4X,A)

        DO I=1,PERSON,1
          	WRITE(*,80)F_NAME(I),L_NAME(I),A1(I),A2(I),A3(I),A4(I),A5(I),A6(I),AVG(I)
80          FORMAT(2X,A,1X,A,2X,F4.2,6X,F4.2,6X,F4.2,6X,F4.2,6X,F4.2,6X,F4.2,6X,F7.2)
        END DO
        PRINT*,'---------------------------------------------------------------------------------------------'

    END

!!author murad-pc