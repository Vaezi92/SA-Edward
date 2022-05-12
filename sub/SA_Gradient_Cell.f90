!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate the Gradient of SA Turbulence Model variable at Cell          //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine SA_Gradient_Cello(Dim,NC,NF1,NF2,NF,NX,NY,A,WTNP1,WNP1,WB,WTB,IDS,DNuXC,DNuYC,&
                             DRNuXC,DRNuYC,DUY,DVX)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF1,NF2,NF,NX,NY,A,WTNP1,WNP1,WB,WTB,IDS
 Intent(Out  )::DNuXC,DNuYC,DRNuXC,DRNuYC,DUY,DVX

 Integer::Dim,I,NC,NF1,NF2,NF,ME,NE,P1,P2
 Real(8)::NXX,NYY,AA,U,V,Nu,RNu,DNuXCj,DNuYCj
 Real(8),Dimension(1:Dim)::NX,NY,A,DNuXC,DNuYC,DRNuXC,DRNuYC,WTNP1,WTB,DUY,DVX
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:5,1:Dim)::WB
 Integer,Dimension(1:4,1:Dim)::IDS
!*******************************************************************************************
!Part 1:	
 DO I=1,NC
    DNuXC(I)  = 0.0
    DNuYC(I)  = 0.0

    DRNuXC(I) = 0.0
    DRNuYC(I) = 0.0

	DUY(I)    = 0.0
    DVX(I)    = 0.0
 End Do

!Part 2:
 DO I=NF2+1,NF
 
   !Part 3:
    ME = IDS(1,I)

	NXX = NX(I)    
	NYY = NY(I)
 
   !Part 4:
    U = WB(2,I)/WB(1,I)
    V = WB(3,I)/WB(1,I)
 
    Nu  = WTB(I)/WB(1,I)
    RNu = WTB(I)

   !Part 5:
    DVX(ME) = DVX(ME) + V*NXX
    DUY(ME) = DUY(ME) + U*NYY

    DNuXC(ME)  = DNuXC(ME)  + Nu  * NXX
    DNuYC(ME)  = DNuYC(ME)  + Nu  * NYY

    DRNuXC(ME) = DRNuXC(ME) + RNu * NXX
    DRNuYC(ME) = DRNuYC(ME) + RNu * NYY

 End Do
 
!Part 6:
 DO I=NF1+1,NF2
 
   !Part 7:
    ME = IDS(1,I)
	NE = IDS(2,I)

	NXX = NX(I)    
	NYY = NY(I)
 
   !Part 8:
    U = 0.5*( WNP1(2,ME)/WNP1(1,ME) + WNP1(2,NE)/WNP1(1,NE) )
	V = 0.5*( WNP1(3,ME)/WNP1(1,ME) + WNP1(3,NE)/WNP1(1,NE) )
 
    Nu  = 0.5*( WTNP1(ME)/WNP1(1,ME) + WTNP1(NE)/WNP1(1,NE) )
    RNu = 0.5*( WTNP1(ME) + WTNP1(NE) )

   !Part 9:
    DVX(ME) = DVX(ME) + V*NXX
    DUY(ME) = DUY(ME) + U*NYY

    DVX(NE) = DVX(NE) - V*NXX
    DUY(NE) = DUY(NE) - U*NYY

    DNuXC(ME)  = DNuXC(ME)  + Nu  * NXX
    DNuYC(ME)  = DNuYC(ME)  + Nu  * NYY

    DNuXC(NE)  = DNuXC(NE)  - Nu  * NXX
    DNuYC(NE)  = DNuYC(NE)  - Nu  * NYY

    DRNuXC(ME) = DRNuXC(ME) + RNu * NXX
    DRNuYC(ME) = DRNuYC(ME) + RNu * NYY

    DRNuXC(NE) = DRNuXC(NE) - RNu * NXX
    DRNuYC(NE) = DRNuYC(NE) - RNu * NYY

 End Do
 
!Part 10: 
 DO I=1,NC

    AA = A(I)

    DNuXC(I)  = DNuXC(I)  / AA
    DNuYC(I)  = DNuYC(I)  / AA

    DRNuXC(I) = DRNuXC(I) / AA
    DRNuYC(I) = DRNuYC(I) / AA

    DVX(I)    = DVX(I)    / AA
    DUY(I)    = DUY(I)    / AA
 End Do
!*******************************************************************************************
 End
!###########################################################################################