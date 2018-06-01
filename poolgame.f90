Program Main
	!--------------Start of Declaration-------------------
    implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
    Integer :: n, i, t, collide_times, bounce_times
    Real( Kind = NP ) :: total_distance
	Real( Kind = NP ) :: radius                           ! ball radius  (Default by 1 cm)
	Real( Kind = NP ) :: hole_width                       ! width of the hole (Default by 5 cm)
    Real( Kind = NP ) :: width, height                    ! boundary condition (Default by 100 cm and 50 cm)
    Logical, Dimension(5) :: Inhole
	Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ), Dimension(5) :: Vx
    Real( Kind = NP ), Dimension(5) :: Vy
    Real( Kind = NP ), Dimension(5, 4) :: Position
    Real( Kind = NP ), Dimension(5, 2) :: delta_D
    Real( Kind = NP ), Parameter :: dt = 1E-6             ! delta time (sec)
    Real( Kind = NP ), Parameter :: f = 1.614E-6          ! friction (cm/sec)
    Real( Kind = NP ), Parameter :: e = 2.0d0             ! decrease of velocity each time a ball hit the boundary (cm/sec)
	!---------------End of Declaration--------------------

    !---------------Enviromental Settings-----------------
	radius = 1.0d0
	hole_width = 5.0d0
    width = 100.0d0
    height = 50.0d0
    Vx(1) = -43.2                      ! Velocity Component of Cue Ball (cm/sec)
    Vy(1) = -77.3                      ! Velocity Component of Cue Ball (cm/sec)
    Do i = 2, 5
    	Vx(i) = 0.0d0
        Vy(i) = 0.0d0
    EndDo
    x(1) = 80.0d0                              ! Cue Ball
    y(1) = 43.0d0                              ! Cue Ball
    x(2) = 35.0d0
    y(2) = 43.0d0
    x(3) = 33.0d0
    y(3) = 18.0d0
    x(4) = 63.0d0
    y(4) = 13.0d0
    x(5) = 77.0d0
    y(5) = 22.0d0
    Do i = 1, 5
		Position(i, 1) = x(i)
        Position(i, 2) = y(i)
		delta_D(i, 1) = x(i)
        delta_D(i, 2) = y(i)        
    EndDo
      
    Do i = 1, 5
    	Inhole(i) = .False.
    EndDo
    collide_times = 0
    bounce_times = 0
    total_distance = 0.0d0
	!-----------------------------------------------------
	Write(*,*) "Code Number: "
	Write(*,*) "Ball 1: Cue Ball"
    Write(*,*) "Ball 2: Object Ball 1"
    Write(*,*) "Ball 3: Object Ball 2"
    Write(*,*) "Ball 4: Object Ball 3"
    Write(*,*) "Ball 5: Object Ball 4"
    Write(*,*)
    Write(*,*)
    Write(*,*) "Game Start!"
    Write(*,*) "----------------------------------"
	!------------------Game Loop---------------------------
    t = 0
	Do While (.True.)
    	Do i = 1, 5
!	     	Get Ball_Position, Counting Total Moving Displacement
!     		Move -> Friction, Update Velocity
!     		Bounce, Update Velocity
!     		If bounce: ball in? Keep it Inhole
!     		Collide 
			Call Update_Position(i, x, y , delta_D, total_distance, Inhole)    
			Call Move(i, x, y, Vx, Vy, dt, Inhole)
            Call Friction(i, x, y, Vx, Vy, Position, f, Inhole)
        	Call Bounce(i, x, y, Vx, Vy, e, width, height, radius, hole_width, bounce_times, Inhole)
            Call Collide(i, x, y, Vx, Vy, radius, collide_times, Inhole)
		EndDo
   		
		! Counting time
		t = t + 1
        If (MOD(t, 1000000) == 0) Then
        	Write(*,'("Elapsed time: ", I5, "s")')  t/1000000
        EndIf
		

		n = 0
		Do i = 1, 5
        	! All balls stopped
        	If (Vx(i) == 0.0 .AND. Vy(i) == 0.0) Then
            	n = n + 1
            EndIf
        EndDo
        ! End, Exit, Show Results
        If (n == 5) Then
          	Write(*,*) "----------------------------------"
            Do i = 1, 5
            	If ( Inhole(i) ) Then
                	Write(*,'("Ball ", I1, " in!")') i
                Else
                	Write(*,'("Ball ", I1, " stop at: (", F10.6, ", ", F10.6, ")")')  i, x(i), y(i)
                EndIf
            EndDo                   
          	Write(*,'("Total Game Time: ", F9.6, " seconds")') t/1000000.0
            Write(*,'("Total Displacement: ", F12.6, " cm")') total_distance
          	Write(*,'("Total Collide Times: ", I3)') collide_times
          	Write(*,'("Total Bounce Times: ", I3)') bounce_times
          	Exit
        EndIF     
	End Do
	!------------------------------------------------------

    
Stop
End Program Main

Subroutine Update_Position(n, x, y, delta_D, total_distance, Inhole)
	implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
	Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ), Dimension(5, 2) :: delta_D
    Logical, Dimension(5) :: Inhole
    Real( Kind = NP ) move_distance, total_distance
	Integer :: n
	
	! Counting total distance
    If ( .NOT. Inhole(n) ) Then
  		move_distance = DSQRT( ( x(n) - delta_D(n, 1) )**2 + ( y(n) -delta_D(n, 2) )**2 )   
        total_distance = total_distance + move_distance
        delta_D(n, 1) = x(n)
        delta_D(n, 2) = y(n)
    EndIf
    
Return
End Subroutine Update_Position


Subroutine Move(n, x, y, Vx, Vy, dt, Inhole)
	implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
	Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ), Dimension(5) :: Vx
    Real( Kind = NP ), Dimension(5) :: Vy
    Logical, Dimension(5) :: Inhole
	Real( Kind = NP ) :: dt
    Integer :: n

	! Not inhole -> x - x0 = v*t
    If ( .NOT. Inhole(n) ) Then
    	x(n) = x(n) + Vx(n)*dt
    	y(n) = y(n) + Vy(n)*dt
    EndIf

Return
End Subroutine Move

Subroutine Bounce(n, x, y, Vx, Vy, e, w, h, r, hole, bounce_times, Inhole)
	implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
    Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ), Dimension(5) :: Vx
    Real( Kind = NP ), Dimension(5) :: Vy
    Real( Kind = NP ) :: w, h, r, hole, e, speed, speed_prime, product
    Logical, Dimension(5) :: Inhole
    Integer :: n, bounce_times
	
    If ( .NOT. Inhole(n) ) Then
        If ( x(n) > (w - r) ) Then
			Call BallIn(n, x, y, w, h, r, hole, Inhole)
            If (Inhole(n)) Then
                x(n) = -1.0
                y(n) = -1.0
                Vx(n) = 0
                Vy(n) = 0
            Else
              	Write(*,'("Right Bounce: ", I2)')  n
                bounce_times = bounce_times + 1
            	x(n) = 2*(w - r) - x(n)
       	     	Vx(n) = Vx(n)*(-1)
                speed = DSQRT(Vx(n)**2 + Vy(n)**2)
                speed_prime = speed - e
                If (speed_prime < 0) Then
                	Vx(n) = 0
                    Vy(n) = 0
              	Write(*,*)
                Write(*, '("Ball ", I2, " stop at: (", F10.6, ", ", F10.6, ")")')  n, x(n), y(n)
              	Write(*,*)
                Else
                	product = speed_prime / speed
                    Vx(n) = Vx(n)*product
                    Vy(n) = Vy(n)*product
                EndIf
            EndIF

        Else If ( x(n) < r ) Then
			Call BallIn(n, x, y, w, h, r, hole, Inhole)
            If (Inhole(n)) Then
                x(n) = -1.0
                y(n) = -1.0
                Vx(n) = 0
                Vy(n) = 0
            Else
              	Write(*,'("Left Bounce: ", I2)') n
                bounce_times = bounce_times + 1
            	x(n) = 2*r - x(n)
            	Vx(n) = Vx(n)*(-1)
                speed = DSQRT(Vx(n)**2 + Vy(n)**2)
                speed_prime = speed - e
                If (speed_prime < 0) Then
                	Vx(n) = 0
                    Vy(n) = 0
              	Write(*,*)
                Write(*, '("Ball ", I2, " stop at: (", F10.6, ", ", F10.6, ")")')  n, x(n), y(n)
              	Write(*,*)
                Else
                	product = speed_prime / speed
                    Vx(n) = Vx(n)*product
                    Vy(n) = Vy(n)*product
                EndIf
            EndIF

        EndIf

        If ( y(n) > (h - r) ) Then
			Call BallIn(n, x, y, w, h, r, hole, Inhole)
            If (Inhole(n)) Then
                x(n) = -1.0
                y(n) = -1.0
                Vx(n) = 0
                Vy(n) = 0
            Else
              	Write(*,'("Top Bounce: ", I2)') n
                bounce_times = bounce_times + 1
            	y(n) = 2*(h - r) - y(n)
            	Vy(n) = Vy(n)*(-1)
                speed = DSQRT(Vx(n)**2 + Vy(n)**2)
                speed_prime = speed - e
                If (speed_prime < 0) Then
                	Vx(n) = 0
                    Vy(n) = 0
              	Write(*,*)
                Write(*, '("Ball ", I2, " stop at: (", F10.6, ", ", F10.6, ")")')  n, x(n), y(n)
              	Write(*,*)
                Else
                	product = speed_prime / speed
                    Vx(n) = Vx(n)*product
                    Vy(n) = Vy(n)*product
                EndIf
            EndIF

        Else If ( y(n) < r ) Then
			Call BallIn(n, x, y, w, h, r, hole, Inhole)
            If (Inhole(n)) Then
                x(n) = -1.0
                y(n) = -1.0
                Vx(n) = 0
                Vy(n) = 0
            Else
              	Write(*,'("Bottom Bounce: ", I2)') n
                bounce_times = bounce_times + 1
            	y(n) = 2*r - y(n)
            	Vy(n) = Vy(n)*(-1)
                speed = DSQRT(Vx(n)**2 + Vy(n)**2)
                speed_prime = speed - e
                If (speed_prime < 0) Then
                	Vx(n) = 0
                    Vy(n) = 0
              	Write(*,*)
                Write(*, '("Ball ", I2, " stop at: (", F10.6, ", ", F10.6, ")")')  n, x(n), y(n)
              	Write(*,*)
                Else
                	product = speed_prime / speed
                    Vx(n) = Vx(n)*product
                    Vy(n) = Vy(n)*product
                EndIf
            EndIF

        EndIf
    EndIf

Return
End Subroutine Bounce

Subroutine BallIn(n, x, y, w, h, r, hole, Inhole)
	implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
    Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ) :: w, h, r, hole
    Logical, Dimension(5) :: Inhole
    Integer :: n

    If ( x(n) <= (0.0 + r) .OR. x(n) >= (w - r) ) Then
        If ( y(n) <= (0.0 + hole) .OR. y(n) >= (h - hole) ) Then
            Inhole(n) = .True.
            
            If ( x(n) <= (0.0 + r) .AND. y(n) <= (0.0 + hole) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') 0.0, 0.0, 0.0, (0 + hole)  
              	Write(*,*)
            Else If ( x(n) <= (0.0 + r) .AND. y(n) >= (h - hole) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') 0.0, (h - hole), 0.0, h  
              	Write(*,*)
                
            Else If ( x(n) >= (w - r) .AND. y(n) <= (0.0 + hole) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') w, 0.0, w, (0.0 + hole)
              	Write(*,*)
            Else
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') w, (h - hole), w, h 
              	Write(*,*) 
            EndIf
            
            Return
        EndIf
	Else If (x(n)<=(0.0 + hole).OR.x(n)>=(w - hole).OR.( (w/2+hole/2)>=x(n).AND.x(n)>=(w/2-hole/2) )) Then
        If ( y(n) <= (0.0 + r) .OR. y(n) >= (h - r) ) Then
			Inhole(n) = .True.
            If ( x(n)<=(0.0 + hole) .AND. y(n) <= (0.0 + r) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') 0.0, 0.0, (0.0 + hole), 0.0
              	Write(*,*)
            Else If ( x(n)<=(0.0 + hole) .AND. y(n) >= (h - r) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') 0.0, h, (0.0 + hole), h  
              	Write(*,*)
            
			Else If ( x(n)>=(w - hole) .AND. y(n) <= (0.0 + r) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') (w - hole), 0.0, w, 0.0    
              	Write(*,*)
            Else If ( x(n)>=(w - hole) .AND. y(n) >= (h - r) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') (w - hole), h, w, h
              	Write(*,*)
                
			Else If ( ( (w/2+hole/2)>=x(n).AND.x(n)>=(w/2-hole/2) ) .AND. y(n) <= (0.0 + r) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') (w/2-hole/2), 0.0, (w/2+hole/2), 0.0
              	Write(*,*)
            Else If ( ( (w/2+hole/2)>=x(n).AND.x(n)>=(w/2-hole/2) ) .AND. y(n) >= (h - r) ) Then
              	Write(*,*)
            	Write(*, '("Ball in: ", I3)') n
                Write(*, '( "Pocket: ( ", F4.1, ", ", F4.1, ") ~ ( ", F4.1, ", ", F4.1, ")" )') (w/2-hole/2), h, (w/2+hole/2), h 
              	Write(*,*)          
			EndIf
            Return
        EndIf
    EndIf

Return
End Subroutine BallIn

Subroutine Collide(i, x, y, Vx, Vy, r, collide_times, Inhole)
	implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
    Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ), Dimension(5) :: Vx
    Real( Kind = NP ), Dimension(5) :: Vy
    Real( Kind = NP ) :: r
    Logical, Dimension(5) :: Inhole
    Integer :: i, j, collide_times
    Real( Kind = NP ) :: dX, dY, distance, inner
    Real( Kind = NP ) :: dV1_x, dV1_y, dV2_x, dV2_y
	
	If ( .NOT. Inhole(i) ) Then
		If ( i /= 5) Then
    		Do j = i+1, 5
            	If ( .NOT. Inhole(j)) Then
					dX = x(i) - x(j)
   	     			dY = y(i) - y(j)
        			distance = DSQRT((dX**2 + dY**2))
					If ( distance < r + r ) Then
                		Write(*,'( "Collide: Ball " , I1, " and Ball ", I1)') i, j
                        collide_times = collide_times + 1
                        
						! Elastic collision
						inner = (Vx(i) - Vx(j))*(x(i) - x(j)) + (Vy(i) - Vy(j))*(y(i) - y(j))
                		dV1_x = -(inner/(dX**2+dY**2))*(x(i) - x(j))
                		dV1_y = -(inner/(dX**2+dY**2))*(y(i) - y(j))
                		dV2_x = -(inner/(dX**2+dY**2))*(x(j) - x(i))
                		dV2_y = -(inner/(dX**2+dY**2))*(y(j) - y(i))
				
                		Vx(i) = Vx(i) + dV1_x
                		Vy(i) = Vy(i) + dV1_y
                		Vx(j) = Vx(j) + dV2_x
                		Vy(j) = Vy(j) + dV2_y
                        
        			EndIf
                EndIf
    		EndDo
		EndIf
   	EndIf
	
Return
End Subroutine Collide

Subroutine Friction(n, x, y, Vx, Vy, P, f, Inhole)
	implicit none
    Integer , parameter :: NP = SELECTED_REAL_KIND(15)
    Real( Kind = NP ), Dimension(5) :: x
    Real( Kind = NP ), Dimension(5) :: y
    Real( Kind = NP ), Dimension(5) :: Vx
    Real( Kind = NP ), Dimension(5) :: Vy
    Real( Kind = NP ), Dimension(5, 4) :: P
    Logical, Dimension(5) :: Inhole
    Real( Kind = NP ) :: f
    Real( Kind = NP ) :: speed, speed_prime, product
    Integer :: n

	If ( .NOT. Inhole(n) ) Then
		P(n, 3) = x(n)
    	P(n, 4) = y(n)
    	If ( DSQRT((P(n, 3) - P(n, 1))**2 + (P(n, 4) - P(n, 2))**2) >= 1.00000000 ) Then
    		speed = DSQRT(Vx(n)**2 + Vy(n)**2)
    		speed_prime = speed - f
        	If ( speed_prime < 0 ) Then
        		Vx(n) = 0
            	Vy(n) = 0
              	Write(*,*)
                Write(*, '("Ball ", I2, " stop at: (", F10.6, ", ", F10.6, ")")')  n, x(n), y(n)
              	Write(*,*)
        	Else
        		product = speed_prime / speed
            	Vx(n) = Vx(n)*product
            	Vy(n) = Vy(n)*product
        	EndIf
        	P(n, 1) = x(n)
        	P(n, 2) = y(n)
    	EndIf
    EndIf

    
Return
End Subroutine Friction