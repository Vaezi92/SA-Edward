!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate Geometrical Values of 2D Mesh.                                //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2014                                                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine GeoCal2D(Dim,NF1,NF2,NF,NC,IDS,X,Y,Xc,Yc,NX,NY,DA,A)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NF1,NF2,NF,NC,IDS,X,Y
 Intent(Out  )::Xc,Yc,NX,NY,DA,A

 Integer::Dim,I,P1,P2,P3,NF1,NF2,NC,NF,ME,NE
 Real(8)::SumX,SumY,DArea,DX,DY,DL,aa
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:Dim)::X,Y,A,Xc,Yc,NX,NY,DA
!*******************************************************************************************	
!Part 1:
 DO I=1,NC
    A(I)  = 0.0
    Xc(I) = 0.0
    Yc(I) = 0.0
 End do

!Part 2:
 DO I=NF1+1,NF2

   !Part 3:
    ME = IDS(1,I)
    NE = IDS(2,I)
	P1 = IDS(3,I)
    P2 = IDS(4,I)

   !Part 4:
    DArea = X(P1)*Y(P2) - X(P2)*Y(P1)

    SumX = X(P1) + X(P2)
    SumY = Y(P1) + Y(P2)

   !Part 5:
    A(ME) = A(ME) + DArea

    Xc(ME) = Xc(ME) + SumX*DArea
    Yc(ME) = Yc(ME) + SumY*DArea

   !Part 6:
    A(NE) = A(NE) - DArea

    Xc(NE) = Xc(NE) - SumX*DArea
    Yc(NE) = Yc(NE) - SumY*DArea

 End do

!Part 7:
 DO I=NF2+1,NF

    ME = IDS(1,I)
	P1 = IDS(3,I)
    P2 = IDS(4,I)

    DArea = X(P1)*Y(P2) - X(P2)*Y(P1)

    SumX = X(P1) + X(P2)
    SumY = Y(P1) + Y(P2)

    A(ME) = A(ME) + DArea

    Xc(ME) = Xc(ME) + SumX*DArea
    Yc(ME) = Yc(ME) + SumY*DArea

 End do

!Part 8:
 DO I=1,NC
    A(I)  = A(I)/ 2
    Xc(I) = Xc(I) / (6*A(I))
    Yc(I) = Yc(I) / (6*A(I))
 End do

!Part 9:
 DO I=1,NF

   !Part 10:
	P1 = IDS(3,I)
    P2 = IDS(4,I)

   !Part 11:
    NX(I) = Y(P2)-Y(P1)
    NY(I) = X(P1)-X(P2)
    DA(I) = Dsqrt(NX(I)*NX(I) + NY(I)*NY(I))

 End do
!*******************************************************************************************
 End
!###########################################################################################
