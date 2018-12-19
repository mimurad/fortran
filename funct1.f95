program bb
    
    parameter(symbol='*')
    parameter(width=30,hgt=9)
    parameter(xmin=-1.0,xmax=1.0,ymin=0.0,ymax=1.0)
    parameter (from=-1.0,to=1.0,incr=0.05)

    character:: graph(30,9)
    real x
    call clear(graph,width,hgt)

    do 10 x=from+incr,to-incr,0.05
      	y=x*x
        call plot(graph,width,hgt,xmin,xmax,ymin,ymax,x,x*x,symbol)
10  continue

    call prnt(graph,30,9)
    stop
    end

!-----------------------------------------------------------------!

    subroutine clear(plot,width,hgt)
    integer:: width,hgt
    character plot(width,hgt)

    do 10 i=1,width
      	do 20 j=1,hgt
        	plot(i,j)=' '
20      continue
10  continue

    return
    end

!-----------------------------------------------------------------!

    subroutine prnt(plot,width,hgt)
    integer::width,hgt
    character plot(width,hgt)

    do 10 j=hgt,1,-1
      	print*,(plot(i,j),i=1,width)
10  continue
	
	return
    end

!-----------------------------------------------------------------!

    subroutine plot(sitmap,width,hgt,xmin,xmax,ymin,ymax,x,y,symbol)
    integer::width,hgt
    character sitmap(width,hgt),symbol
    real::xmin,xmax,ymin,ymax,x,y

    i=int((x-xmin)/(xmax-xmin)*width)+1
    j=int((y-ymin)/(ymax-ymin)*hgt)+1

    if(i .ge. 1 .and. i .le. width .and. j .ge. 1 .and. j .le. hgt)	sitmap(i,j)=symbol
    return
    end
!!author murad-pc