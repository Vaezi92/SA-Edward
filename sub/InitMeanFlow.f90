!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description:To Initialize all of the Parameters Contribute in Solver Process         //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2014                                                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine InitMeanFlow(Dim,Init,NC,ALF,Minf,Rinf,Tt,MR,GM,PrL,PrT,R0,P0,C0,U0,V0,T0,Mu0,B0,WNP1)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,Init,NC,ALF,Minf,Rinf
 Intent(Out  )::MR,GM,PrL,PrT,R0,P0,C0,U0,V0,T0,Mu0,B0,WNP1

 Integer::Dim,Init,I,J,NC
 Real(8)::PI,ALF,Minf,Rinf,MR,GM,R0,P0,T0,Ts,Tt,C0,U0,V0,E0,Mu0,PrL,PrT,ALFA,B0,U,V
 Real(8),Dimension(1:Dim)::Mut
 Real(8),Dimension(1:4,1:Dim)::WNP1
!*******************************************************************************************
!Part 1:
 PI  = 4.0*Atan(1.0)
 ALFA= ALF*PI/180.

!Part 2:
 PrL = 0.72
 PrT=0.9
 GM  = 1.4

!Part 3:
 R0 = 1.0

!Part 4:
 P0 = 1.0/GM

!Part 5:
 T0 = GM*P0/R0

!Part 6:
 Ts = Tt/(1.0 + 0.5*(GM-1)*Minf*Minf)

!Part 5:
 B0 = 110.4/Ts

!Part 7:
 C0 = SQRT(GM*P0/R0)

!Part 8:
 U0 = Minf*C0*COS(ALFA)
 V0 = Minf*C0*SIN(ALFA)

!Part 9:
 E0 = P0/(R0*(GM-1))+ 0.5*(U0*U0 + V0*V0)

!Part 10:
 Mu0 = 1.0

!Part 11:
 DO I=1,NC
    WNP1(1,I) = R0
    WNP1(2,I) = R0*U0
    WNP1(3,I) = R0*V0
    WNP1(4,I) = R0*E0
 end do

!Part 12:
 MR = Minf/Rinf

!Part 13:
 IF(Init==1)Then

  Open(1,File='SolutionData.txt')
  Do I=1,NC
	 Read(1,*) WNP1(1,I),WNP1(2,I),WNP1(3,I),WNP1(4,I)
  End Do
  close(1)

 Endif
!*******************************************************************************************
 End
!###########################################################################################
