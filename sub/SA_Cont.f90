!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate the Convection Term of Spalar-Allmaras Turbulence Model       //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine SA_Cont(Dim,NC,NF1,NF2,NF,NX,NY,IDS,WTNP1,WNP1,WB,WTB,Cont)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF1,NF2,NF,NX,NY,IDS,WTNP1,WNP1,WB,WTB
 Intent(Out  )::Cont

 Integer::Dim,I,NC,NF1,NF2,NF,ME,NE,P1,P2
 Real(8)::U,V,Un,F1
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:Dim)::NX,NY,WTB,WTNP1,Cont
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:5,1:Dim)::WB
!*******************************************************************************************	
!Part 1:
 Do I=1,NC
    Cont(I) = 0.0
 End do
	
!Part 2:
 Do I=NF2+1,NF
 	
   !Part 3:
    ME = IDS(1,I)
    	
   !Part 4:
    U = WB(2,I)/WB(1,I)
    V = WB(3,I)/WB(1,I)

   !Part 5:
    Un = U*NX(I)+V*NY(I)
		
   !Part 6:	
    F1 = Un * WTB(I)

    Cont(ME) = Cont(ME) + F1

 End do
	
!Part 7:
 Do I=NF1+1,NF2
	
   !Part 8:
    ME = IDS(1,I)
    NE = IDS(2,I)
	
   !Part 9:
    U = 0.5*( WNP1(2,ME)/WNP1(1,ME) + WNP1(2,NE)/WNP1(1,NE) )
    V = 0.5*( WNP1(3,ME)/WNP1(1,ME) + WNP1(3,NE)/WNP1(1,NE) )
	
   !Part 10:
    Un = U*NX(I)+V*NY(I)
	
   !Part 11:
    IF(Un>0.)Then
     F1 = Un * WTNP1(ME)
	Else
     F1 = Un * WTNP1(NE)
	Endif
	
   !Part 12:
	Cont(ME) = Cont(ME) + F1
    Cont(NE) = Cont(NE) - F1

 End do
!*******************************************************************************************
 End
!###########################################################################################

