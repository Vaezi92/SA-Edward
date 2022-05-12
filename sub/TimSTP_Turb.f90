!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate Time Step of 2D Turbulent Flow (Explicit Scheme)              //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine TimSTP_Turb(Dim,NC,NF,NF1,NF2,IDS,NX,NY,DA,A,CFLx,GM,P,WNP1,WB,Mu,Mut,PrL,PrT,MR,DT)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF,NF1,NF2,IDS,NX,NY,DA,A,CFLx,GM,P,WNP1,WB,Mu,Mut,PrL,PrT,MR
 Intent(Out  )::DT

 Integer::Dim,I,NC,ME,NE,NF,NF1,NF2
 Real(8)::U,V,Ti,Tv,CFLx,GM,PrL,PrT,Muj,Rj,C,Sig,MR
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:5,1:Dim)::WB
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:Dim)::DTi,DTv,DT,A,NX,NY,DA,P,Mu,Mut
!*******************************************************************************************	
!Part 1:
 Do I=1,NC
    DTi(I) = 0.0
    DTv(I) = 0.0	
 End Do

!Part 2:
 DO I=NF2+1,NF

   !Part 3:
    ME = IDS(1,I)

   !Part 4:
    U = WB(2,I)/WB(1,I)
    V = WB(3,I)/WB(1,I)

   !Part 5:
    C = SQRT( ABS( GM*WB(5,I)/WB(1,I) ) )

   !Part 6:
    DTi(ME) = ABS(U*NX(I) + V*NY(I)) + C*DA(I)
    DTv(ME) = ( Mu(ME)/PrL+Mut(ME)/PrT ) * DA(I)*DA(I) / WB(1,I)

 End Do

!Part 7:
 DO I=NF1+1,NF2
 
   !Part 8:
    ME = IDS(1,I)
	NE = IDS(2,I)

   !Part 9:
    U = (WNP1(2,ME)/WNP1(1,ME)+WNP1(2,NE)/WNP1(1,NE)) / 2
    V = (WNP1(3,ME)/WNP1(1,ME)+WNP1(3,NE)/WNP1(1,NE)) / 2

   !Part 10:
    C = SQRT( ABS( GM*(P(ME)+P(NE))/(WNP1(1,ME)+WNP1(1,NE)) ) )

   !Part 11:
    Ti = ABS(U*NX(I) + V*NY(I)) + C*DA(I)

   !Part 12:
    DTi(ME) = DTi(ME) + Ti
    DTi(NE) = DTi(NE) + Ti

   !Part 13:
    Muj = 0.5*( (Mu(ME)/PrL+Mut(ME)/PrT) + (Mu(NE)/PrL+Mut(NE)/PrT) )  !eslah shode 10.10.95
    Rj  = 0.5*(WNP1(1,ME) + WNP1(1,NE))
    Tv  = Muj*DA(I)*DA(I)/Rj

   !Part 14:
    DTv(ME) = DTv(ME) + Tv
    DTv(NE) = DTv(NE) + Tv

 End Do

!Part 15:
 Sig = 0.15
 C = GM**1.5*MR
 DO I=1,NC
    DTi(I) = A(I)/DTi(I)
    DTv(I) = Sig*A(I)*A(I) / (C*DTv(I))

    DT(I) = CFLx*DTi(I)*DTv(I)/(DTi(I)+DTv(I))
 End Do
!*******************************************************************************************
 End
!###########################################################################################
