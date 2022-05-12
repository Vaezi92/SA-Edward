!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description:Calculate the Convection Terms of 2D Mean Flow Equations by Centeral Scheme!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2014                                                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine ConMeanFlow_ScalarDiss(Dim,NC,NF1,NF2,NF,IDS,NX,NY,DA,GM,K2,K4,WNP1,WB,P,Con)
 Implicit None
!*******************************************************************************************
 Intent (In   )::Dim,NC,NF1,NF2,NF,IDS,NX,NY,DA,GM,K2,K4,WNP1,WB,P
 Intent (Out  )::Con

 Integer::Dim,I,NC,NF1,NF2,NF,ME,NE
 Real(8)::U,V,NXX,NYY,F1,F2,F3,F4,Q,Pm,R,RU,RV,RE,GM,K2,K4
 Real(8),Dimension(1:4,1:Dim)::Con,Dis,WNP1
 Real(8),Dimension(1:5,1:Dim)::WB
 Real(8),Dimension(1:Dim)::NX,NY,DA,P
 Integer,Dimension(1:4,1:Dim)::IDS
!*******************************************************************************************	
!Part 1:
 DO I=1,NC
    Con(1,I) = 0.0
    Con(2,I) = 0.0
    Con(3,I) = 0.0
    Con(4,I) = 0.0
 End Do

!Part 2:
 DO I=NF2+1,NF
 
   !Part 3:
    ME = IDS(1,I)

   !Part 4:
    U = WB(2,I)/WB(1,I)
    V = WB(3,I)/WB(1,I)

   !Part 5:
    Q  = U*NX(I) + V*NY(I)
    Pm = WB(5,I)

   !Part 6:
    F1 = Q*Wb(1,I) 
    F2 = Q*Wb(2,I) + Pm*NX(I)
    F3 = Q*Wb(3,I) + Pm*NY(I)
    F4 = Q*Wb(4,I) + Pm*Q

   !Part 7:
    Con(1,ME) = Con(1,ME) + F1
    Con(2,ME) = Con(2,ME) + F2
    Con(3,ME) = Con(3,ME) + F3
    Con(4,ME) = Con(4,ME) + F4
	
 End Do

!Part 8:
 DO I=NF1+1,NF2

   !Part 9:
    ME = IDS(1,I)
    NE = IDS(2,I)

   !Part 10:
    NXX = NX(I)
    NYY = NY(I)

   !Part 11:
    R  = 0.5 * ( WNP1(1,ME) + WNP1(1,NE) )
	RU = 0.5 * ( WNP1(2,ME) + WNP1(2,NE) )
	RV = 0.5 * ( WNP1(3,ME) + WNP1(3,NE) )
	RE = 0.5 * ( WNP1(4,ME) + WNP1(4,NE) )
    Pm = 0.5 * ( P(ME)      + P(NE)      )

   !Part 12:
    U = RU / R
    V = RV / R

   !Part 13:
    Q = U*NXX + V*NYY

   !Part 14:
    F1 = Q * R 
    F2 = Q * RU + Pm*NXX
    F3 = Q * RV + Pm*NYY
    F4 = Q * RE + Pm*Q 

   !Part 15:
    Con(1,ME) = Con(1,ME) + F1
    Con(2,ME) = Con(2,ME) + F2
    Con(3,ME) = Con(3,ME) + F3
    Con(4,ME) = Con(4,ME) + F4

   !Part 16:
    Con(1,NE) = Con(1,NE) - F1
    Con(2,NE) = Con(2,NE) - F2
    Con(3,NE) = Con(3,NE) - F3
    Con(4,NE) = Con(4,NE) - F4
 
 End Do

!Part 17:
 Call DisJameson(Dim,NC,NF1,NF2,IDS,NX,NY,DA,GM,K2,K4,WNP1,P,Dis)

!Part 18:
 DO I=1,NC
    Con(1,I) = Con(1,I) - Dis(1,I)
    Con(2,I) = Con(2,I) - Dis(2,I)
    Con(3,I) = Con(3,I) - Dis(3,I)
    Con(4,I) = Con(4,I) - Dis(4,I)
 End Do
!*******************************************************************************************
 End
!###########################################################################################

