!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate the Gradient of SA Turbulence Model variable at Edges         //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine SA_Gradient_Face(Dim,NC,NF1,NF2,NFW1,NFW2,NFS1,NFS2,NF,NP,IDS,X,Y,Xc,Yc,WNP1,WB,&
                             WTNP1,WTB,DNuXF,DNuYF)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF1,NF2,NFW1,NFW2,NFS1,NFS2,NF,NP,IDS,X,Y,Xc,Yc,WNP1,WB,WTNP1,WTB
 Intent(Out  )::DNuXF,DNuYF

 Integer::Dim,I,NP,NC,NF1,NF2,NFW1,NFW2,NF,ME,NE,P1,P2,NFS1,NFS2
 Real(8)::DX,DY,A1,A2,A3,AREA,DX1,DX2,DX3,DX4,DY1,DY2,DY3,DY4,DXC,DYC,Nu,R,Nu1,Nu2,NuME,NuNE 
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:5,1:Dim)::WB
 Real(8),Dimension(1:Dim)::X,Y,XC,YC,NuP,DNuXF,DNuYF,WTNP1,WTB
 Integer,Dimension(1:4,1:Dim)::IDS
 Integer,Dimension(1:Dim)::NEC
!*******************************************************************************************
!Part 20:
 DO I=1,NF
    DNuXF(I) = 0.0
    DNuYF(I) = 0.0
 End Do

!Part 1:
 DO I=1,NP
    NuP(I)  = 0.0
    NEC(I)  = 0
 END DO

!Part 2:
 Do I=NF1+1,NF2
  
   !Part 3:
    ME = IDS(1,I)
    NE = IDS(2,I)
	P1 = IDS(3,I)
    P2 = IDS(4,I)
 
   !Part 4:
    Nu = 0.5*( WTNP1(ME)/WNP1(1,ME) + WTNP1(NE)/WNP1(1,NE) )

   !Part 5:
    NuP(P1) = NuP(P1) + Nu
    NuP(P2) = NuP(P2) + Nu

   !Part 6:
    NEC(P1) = NEC(P1) + 1
    NEC(P2) = NEC(P2) + 1

 End Do

!Part 7:
 Do I=NF2+1,NF
 
   !Part 8:
	P1 = IDS(3,I)
    P2 = IDS(4,I)
 
   !Part 9:
    Nu    = WTB(I)/WB(1,I)
 
   !Part 10:
    NuP(P1) = NuP(P1) + Nu
    NuP(P2) = NuP(P2) + Nu
 
   !Part 11:
    NEC(P1) = NEC(P1) + 1
    NEC(P2) = NEC(P2) + 1

 End Do

!Part 12:
 DO I=1,NP
    NuP(I)  = NuP(I) / NEC(I)
 END DO

!Part 13:
 DO I=NFW1+1,NFW2
    P1 = IDS(3,I)
    P2 = IDS(4,I)

    NuP(P1) = 0.0
    NuP(P2) = 0.0

 END DO
 
!Part 14:
 DO I=NF1+1,NF2
  
   !Part 15:        
    ME = IDS(1,I)        
	NE = IDS(2,I)
    P1 = IDS(3,I)
    P2 = IDS(4,I)
  
   !Part 16:
    DX   = X(P2)  - X(P1)
    DY   = Y(P2)  - Y(P1)
    DXC  = XC(ME) - XC(NE)
    DYC  = YC(ME) - YC(NE)
    AREA = ABS(0.5*(DX*DYC - DY*DXC))
  
    DX1 = XC(NE) - X (P1)
    DX2 = X (P2) - XC(NE)
    DX3 = XC(ME) - X (P2)
    DX4 = X (P1) - XC(ME)

    DY1 = YC(NE) - Y (P1)
    DY2 = Y (P2) - YC(NE)
    DY3 = YC(ME) - Y (P2)
    DY4 = Y (P1) - YC(ME)

   !Part 17:
    Nu1 = NuP(P1)
    Nu2 = NuP(P2)
    NuME = WTNP1(ME)/WNP1(1,ME)
    NuNE = WTNP1(NE)/WNP1(1,NE)
  
    DNuXF(I) =  0.5*( (Nu1+NuNE)*DY1 + (NuNE+Nu2)*DY2 + (NuME+Nu2)*DY3 + (Nu1+NuME)*DY4 )/AREA
    DNuYF(I) = -0.5*( (Nu1+NuNE)*DX1 + (NuNE+Nu2)*DX2 + (NuME+Nu2)*DX3 + (Nu1+NuME)*DX4 )/AREA

 END DO

!Part 18:
 DO I=NFW1+1,NFW2
  
   !Part 19:
    ME = IDS(1,I)
    P1 = IDS(3,I)
    P2 = IDS(4,I)
  
   !Part 20:
    DX1 = X (P2)-X(P1)
	DY1 = Y (P2)-Y(P1)
    DX2 = Xc(ME)-X(P1)
	DY2 = Yc(ME)-Y(P1)
    AREA= 0.5*Dabs( DX1*DY2 - DY1*DX2 )

   !Part 21:
    NuME = WTNP1(ME)/WNP1(1,ME)

    DNuXF(I) = -0.5*NuME*DY1/AREA
    DNuYF(I) =  0.5*NuME*DX1/AREA

 END DO
 
!Part 22:
 DO I=NFW2+1,NF
  
   !Part 23:
    ME = IDS(1,I)
    P1 = IDS(3,I)
    P2 = IDS(4,I)
  
   !Part 24:
    DX  = X (P2)-X (P1)
	DY  = Y (P2)-Y (P1)
    DX1 = Xc(ME)-X (P2)
	DY1 = Yc(ME)-Y (P2)
    DX2 = X (P1)-Xc(ME)
    DY2 = Y (P1)-Yc(ME)

    AREA = 0.5*Dabs( DX*DY2 - DY*DX2 )

   !Part 25:
    Nu1 = NuP(P1)
    Nu2 = NuP(P2)
    NuME = WTNP1(ME)/WNP1(1,ME)

    DNuXF(I) = 0.5*( (NuME+Nu2)*DY1 + (Nu1+NuME)*DY2 + (Nu1+Nu2)*DY )/AREA
    DNuYF(I) =-0.5*( (NuME+Nu2)*DX1 + (Nu1+NuME)*DX2 + (Nu1+Nu2)*DX )/AREA

 END DO

!Part 26:
 DO I=NFS1+1,NFS2
    DNuXF(I) = 0.0
    DNuYF(I) = 0.0
 END DO
!*******************************************************************************************
 End
!###########################################################################################

