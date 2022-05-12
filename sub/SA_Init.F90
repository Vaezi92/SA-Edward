!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Initializing the SA Turbulence Model                                    //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine SA_Init(Dim,NC,NFW1,NFW2,NF,IDS,X,Y,Xc,Yc,Mu0,R0,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,&
                    Cv1,Mut0,Nuhat0,WTNP1,DW,Mut)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NFW1,NFW2,NF,IDS,X,Y,Xc,Yc,Mu0,R0
 Intent(Out  )::Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mut0,Nuhat0,WTNP1,DW,Mut

 Integer::Dim,I,J,NC,NFW1,NFW2,NF,ME,NE,P1,P2
 Real(8)::Chi,Nut0,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mu0,R0,Xj,Yj,Xi,Yi,Dmin,Dis,DX,DY,Nu0,&
          Nuhat0,Mut0,Cv13,Chi3,Fv1
 Real(8),Dimension(1:Dim)::X,Y,Xc,Yc,WTNP1,DW,Mut
 Integer,Dimension(1:4,1:Dim)::IDS
!*******************************************************************************************	
!Part 1:
 Cb1   = 0.1355
 Cb2   = 0.622
 Kei   = 0.41
 Sigma = 2/3.
 Cw1   = cb1/(Kei*Kei) + (1+cb2)/sigma
 Cw2   = 0.3
 Cw3   = 2.
 Cv1   = 7.1

!Part 2:
 Chi = 3.

!Part 3:
 Nuhat0 = Chi*R0*Mu0

!Part 4:
 Cv13 = Cv1*Cv1*Cv1
 Chi3 = Chi*Chi*Chi
 Fv1  = Chi3/(Chi3+Cv13)  

!Part 5:
 Mut0 = Fv1*R0*Nuhat0 

!Part 6:
 Do I=1,NC
    WTNP1(I) = Nuhat0
	Mut(I)   = Mut0
 End do

!Part 7:
 Do J=1,NC
    
   !Part 8:
	Xj = Xc(J)
	Yj = Yc(J)
   
   !Part 9:    
    Dmin=1000000.0

   !Part 10:
    Do I=NFW1+1,NFW2
      
	  !Part 11:
       ME = IDS(1,I)
	   P1 = IDS(3,I)
	   P2 = IDS(4,I)
      
	  !Part 12:
       Xi = 0.5*(X(P1) + X(P2))
       Yi = 0.5*(Y(P1) + Y(P2))
      
	  !Part 13:  
	   DX = Xj-Xi
	   DY = Yj-Yi
       Dis = Dsqrt(DX*DX+DY*DY) 
      
	  !Part 14:
	   If(Dis<Dmin)then
	    Dmin = Dis
	   Endif

	End do
      
   !Part 15: 
    DW(j) =  Dmin  !abs( Yc(J) )  !100.00 *  ! 

 End do
!*******************************************************************************************
 End
!###########################################################################################
