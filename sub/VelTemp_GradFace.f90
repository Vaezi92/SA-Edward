!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate Gradient of Velocity and Temperature at each Face of a 2D Mesh//!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine VelTemp_GradFace(Dim,NC,NF1,NF2,NFW1,NFW2,NF,NP,IDS,X,Y,Xc,Yc,WNP1,WB,GM,P,&
                             DUX,DUY,DVX,DVY,DTX,DTY)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF1,NF2,NFW1,NFW2,NF,NP,IDS,X,Y,Xc,Yc,WNP1,WB,GM,P
 Intent(Out  )::DUX,DUY,DVX,DVY,DTX,DTY

 Integer::Dim,J,I,NP,NC,NF1,NF2,NFW1,NFW2,NF,ME,NE,P1,P2
 Real(8)::U,V,DX,DY,A1,A2,A3,AREA,UME,VME,TME,UNE,VNE,TNE,Temp,DX1,DX2,DX3,DX4,DY1,DY2,DY3,&
          GM,DY4,DXC,DYC,DL,U1,V1,T1,U2,V2,T2
 Integer,Dimension(1:4,1:Dim)::IDS
 Integer,Dimension(1:Dim)::NEC
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:5,1:Dim)::WB
 Real(8),Dimension(1:Dim)::X,Y,XC,YC,T,P,UP,VP,TP,DUX,DUY,DVX,DVY,DTX,DTY
!*******************************************************************************************
!Part 20:
 DO I=1,NF
    DUX(I) = 0.0
    DUY(I) = 0.0
    DVX(I) = 0.0
    DVY(I) = 0.0
    DTX(I) = 0.0
    DTY(I) = 0.0
 End Do


!Part 1:
 Do J=1,NC
    T(J) = GM*P(J)/WNP1(1,J)         
 End Do

!Part 2:
 DO I=1,NP
    UP(I) = 0.0
    VP(I) = 0.0
    TP(I) = 0.0
    NEC(I)= 0
 END DO

!Part 3:
 Do I=NF1+1,NF2
  
   !Part 4:
    ME = IDS(1,I)
    NE = IDS(2,I)
	P1 = IDS(3,I)
    P2 = IDS(4,I)
 
   !Part 5:
    U    = 0.5*( WNP1(2,ME)/WNP1(1,ME) + WNP1(2,NE)/WNP1(1,NE) )
    V    = 0.5*( WNP1(3,ME)/WNP1(1,ME) + WNP1(3,NE)/WNP1(1,NE) )
    Temp = 0.5*( T(ME) + T(NE) )
 
   !Part 6:
    UP(P1) = UP(P1) + U
    VP(P1) = VP(P1) + V
    TP(P1) = TP(P1) + Temp

    UP(P2) = UP(P2) + U
    VP(P2) = VP(P2) + V
    TP(P2) = TP(P2) + Temp

   !Part 7:
    NEC(P1) = NEC(P1) + 1
    NEC(P2) = NEC(P2) + 1

 End Do

!Part 8:
 Do I=NF2+1,NF
 
   !Part 9:
	P1 = IDS(3,I)
    P2 = IDS(4,I)
 
   !Part 10:
    U    = WB(2,I)/WB(1,I)
    V    = WB(3,I)/WB(1,I)
    Temp = GM*WB(5,I)/WB(1,I)
 
   !Part 11:
    UP(P1) = UP(P1) + U
    VP(P1) = VP(P1) + V
    TP(P1) = TP(P1) + Temp

    UP(P2) = UP(P2) + U
    VP(P2) = VP(P2) + V
    TP(P2) = TP(P2) + Temp

    NEC(P1) = NEC(P1) + 1
    NEC(P2) = NEC(P2) + 1

 End Do

!Part 12:
 DO I=1,NP
    IF(NEC(I)==0)Cycle
    UP(I) = UP(I)/NEC(I)
    VP(I) = VP(I)/NEC(I)
    TP(I) = TP(I)/NEC(I)
 END DO

!Part 13:
 DO I=NFW1+1,NFW2
    P1 = IDS(3,I)
    P2 = IDS(4,I)

    UP(P1) = 0.0
    VP(P1) = 0.0

    UP(P2) = 0.0
    VP(P2) = 0.0
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
  
   !Part 17:
    DX1 = XC(NE) - X (P1)
    DX2 = X (P2) - XC(NE)
    DX3 = XC(ME) - X (P2)
    DX4 = X (P1) - XC(ME)

    DY1 = YC(NE) - Y (P1)
    DY2 = Y (P2) - YC(NE)
    DY3 = YC(ME) - Y (P2)
    DY4 = Y (P1) - YC(ME)

   !Part 18:
    U1 = UP(P1)
    V1 = VP(P1)
    T1 = TP(P1)

    U2 = UP(P2)
    V2 = VP(P2)
    T2 = TP(P2)

    UME = WNP1(2,ME)/WNP1(1,ME)
    VME = WNP1(3,ME)/WNP1(1,ME)
    TME = T(ME)

    UNE = WNP1(2,NE)/WNP1(1,NE)
    VNE = WNP1(3,NE)/WNP1(1,NE)
    TNE = T(NE)
  
   !Part 19:
    DUX(I) = 0.5*( (U1+UNE)*DY1 + (UNE+U2)*DY2 + (UME+U2)*DY3 + (U1+UME)*DY4 )/AREA
    DUY(I) =-0.5*( (U1+UNE)*DX1 + (UNE+U2)*DX2 + (UME+U2)*DX3 + (U1+UME)*DX4 )/AREA

    DVX(I) = 0.5*( (V1+VNE)*DY1 + (VNE+V2)*DY2 + (VME+V2)*DY3 + (V1+VME)*DY4 )/AREA
    DVY(I) =-0.5*( (V1+VNE)*DX1 + (VNE+V2)*DX2 + (VME+V2)*DX3 + (V1+VME)*DX4 )/AREA

    DTX(I) = 0.5*( (T1+TNE)*DY1 + (TNE+T2)*DY2 + (TME+T2)*DY3 + (T1+TME)*DY4 )/AREA
    DTY(I) =-0.5*( (T1+TNE)*DX1 + (TNE+T2)*DX2 + (TME+T2)*DX3 + (T1+TME)*DX4 )/AREA

 END DO

!Part 20:
 DO I=NFW1+1,NFW2
  
   !Part 21:
    ME = IDS(1,I)
    P1 = IDS(3,I)
    P2 = IDS(4,I)
  
   !Part 22:
    DX1 = X (P2)-X(P1)
	DY1 = Y (P2)-Y(P1)
    DX2 = Xc(ME)-X(P1)
	DY2 = Yc(ME)-Y(P1)
    AREA= 0.5*Dabs( DX1*DY2 - DY1*DX2 )

   !Part 23:
    DX = X(P2)-X(P1)
    DY = Y(P2)-Y(P1)

   !Part 24:
    U = WNP1(2,ME)/WNP1(1,ME)
    V = WNP1(3,ME)/WNP1(1,ME)
  
   !Part 25:
    DUX(I) = -0.5*U*DY/AREA
    DUY(I) =  0.5*U*DX/AREA
    DVX(I) = -0.5*V*DY/AREA
    DVY(I) =  0.5*V*DX/AREA
    DTX(I) =  0.0
    DTY(I) =  0.0
  
 End Do
!*******************************************************************************************
 End
!###########################################################################################








 
!Part 20:
 !DO I=NF2+1,NF
  
   !Part 35:
    !ME = IDS(1,I) 
    !P1 = IDS(3,I)
    !P2 = IDS(4,I)
  
   !Part 36:
    !DX  = X (P2)-X (P1)
	!DY  = Y (P2)-Y (P1)
    !DX1 = Xc(ME)-X (P2)
	!DY1 = Yc(ME)-Y (P2)
    !DX2 = X (P1)-Xc(ME)
    !DY2 = Y (P1)-Yc(ME)
  
   !Part 37:
   ! AREA = 0.5*Dabs( DX*DY2 - DY*DX2 )

   !Part 38:
    !U1 = UP(P1)
   ! V1 = VP(P1)
    !T1 = TP(P1)

    !U2 = UP(P2)
    !V2 = VP(P2)
    !T2 = TP(P2)

    !UME = WNP1(2,ME)/WNP1(1,ME)
    !VME = WNP1(3,ME)/WNP1(1,ME)
    !TME = T(ME)
  
   !Part 39:
    !DUX(I) = 0.5*( (UME+U2)*DY1 + (U1+UME)*DY2 + (U1+U2)*DY )/AREA
    !DUY(I) =-0.5*( (UME+U2)*DX1 + (U1+UME)*DX2 + (U1+U2)*DX )/AREA

    !DVX(I) = 0.5*( (VME+V2)*DY1 + (V1+VME)*DY2 + (V1+V2)*DY )/AREA
   ! DVY(I) =-0.5*( (VME+V2)*DX1 + (V1+VME)*DX2 + (V1+V2)*DX )/AREA

    !DTX(I) = 0.5*( (TME+T2)*DY1 + (T1+TME)*DY2 + (T1+T2)*DY )/AREA
    !DTY(I) =-0.5*( (TME+T2)*DX1 + (T1+TME)*DX2 + (T1+T2)*DX )/AREA
    
 !END DO

