!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description:Calculate the Scalar Dissipation by Jameson Scheme                       //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2014                                                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine DisJameson(Dim,NC,NF1,NF2,IDS,NX,NY,DA,GM,K2,K4,WNP1,P,Dis)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF1,NF2,IDS,NX,NY,DA,GM,K2,K4,WNP1,P
 Intent(Out  )::Dis

 Integer::Dim,I,NC,NF1,NF2,ME,NE
 Real(8)::C2,GM,U,V,Ai,E2,E4,T1,T2,T3,T4,S1,S2,S3,S4,F1,F2,F3,F4,DW1,DW2,DW3,DW4,K2,K4
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::DW,Dis,WNP1
 Real(8),Dimension(1:Dim)::NX,NY,DA,P
!*******************************************************************************************
!Part 1:
 DO I=1,NC
    DW(1,I) = 0.0
    DW(2,I) = 0.0
    DW(3,I) = 0.0
    DW(4,I) = 0.0
    
	Dis(1,I) = 0.0
    Dis(2,I) = 0.0
    Dis(3,I) = 0.0
    Dis(4,I) = 0.0
 End Do

!Part 2:
 DO I=NF1+1,NF2

   !Part 3:
    ME = IDS(1,I)
    NE = IDS(2,I)

   !Part 4:
    DW1 = WNP1(1,NE)-WNP1(1,ME)
    DW2 = WNP1(2,NE)-WNP1(2,ME)
    DW3 = WNP1(3,NE)-WNP1(3,ME)
    DW4 = WNP1(4,NE)-WNP1(4,ME)  + P(NE)-P(ME)

   !Part 5:
    DW(1,ME) = DW(1,ME) + DW1
    DW(2,ME) = DW(2,ME) + DW2
    DW(3,ME) = DW(3,ME) + DW3
    DW(4,ME) = DW(4,ME) + DW4

   !Part 6:
    DW(1,NE) = DW(1,NE) - DW1
    DW(2,NE) = DW(2,NE) - DW2
    DW(3,NE) = DW(3,NE) - DW3
    DW(4,NE) = DW(4,NE) - DW4
 End Do

!Part 7:
 DO I=NF1+1,NF2
 
   !Part 8:
    ME = IDS(1,I)
    NE = IDS(2,I)

   !Part 9:
    C2 = GM*(P(ME)+P(NE))/(WNP1(1,ME)+WNP1(1,NE))

   !Part 10:
    U = (WNP1(2,ME)/WNP1(1,ME)+WNP1(2,NE)/WNP1(1,NE))*0.5
    V = (WNP1(3,ME)/WNP1(1,ME)+WNP1(3,NE)/WNP1(1,NE))*0.5

   !Part 11:
    Ai = ABS(U*NX(I)+V*NX(I)) + SQRT(C2*DA(I)*DA(I))

   !Part 13:
    E2 = K2*ABS((P(NE)-P(ME))/(P(NE)+P(ME)))
    E4 = DMAX1(0.0,(K4-E2))

   !Part 14:
    F1 = E4*(DW(1,ME)-DW(1,NE))
    F2 = E4*(DW(2,ME)-DW(2,NE))
    F3 = E4*(DW(3,ME)-DW(3,NE))
    F4 = E4*(DW(4,ME)-DW(4,NE))

    DW1 = WNP1(1,ME)-WNP1(1,NE)
    DW2 = WNP1(2,ME)-WNP1(2,NE)
    DW3 = WNP1(3,ME)-WNP1(3,NE)
    DW4 = WNP1(4,ME)-WNP1(4,NE) + P(ME)-P(NE)
    
	S1 = E2*DW1
    S2 = E2*DW2
    S3 = E2*DW3
    S4 = E2*DW4

    T1 = Ai*(F1-S1)
    T2 = Ai*(F2-S2)
    T3 = Ai*(F3-S3)
    T4 = Ai*(F4-S4)

   !Part 15:
    Dis(1,ME) = Dis(1,ME) + T1
    Dis(2,ME) = Dis(2,ME) + T2
    Dis(3,ME) = Dis(3,ME) + T3
    Dis(4,ME) = Dis(4,ME) + T4

   !Part 16:
    Dis(1,NE) = Dis(1,NE) - T1
    Dis(2,NE) = Dis(2,NE) - T2
    Dis(3,NE) = Dis(3,NE) - T3
    Dis(4,NE) = Dis(4,NE) - T4

 End Do 


	! pause
!*******************************************************************************************
 End
!###########################################################################################
