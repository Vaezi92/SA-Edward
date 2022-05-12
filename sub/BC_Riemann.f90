!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Apply Riemann Boundary Condition                                        //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!//                                                                                      //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine BC_Riemann(Dim,NFF1,NFF2,NX,NY,DA,IDS,GM,U0,V0,P0,R0,C0,WNP1,P,WB)
 Implicit None
!*******************************************************************************************
 Intent (In   )::Dim,NFF1,NFF2,GM,U0,V0,P0,R0,C0,IDS,Wnp1,NX,NY,DA,P
 Intent (Out  )::Wb

 Integer::Dim,I,NFF1,NFF2,ME,P1,P2
 Real(8)::GM,GM1,U,V,CC,MLoc,C0,U0B,V0B,P0B,R0B,C0B,S0,DX,DY,DH,NXX,NYY,QN0,QT0,RI0,PE,RE,CE,&
          VE,QNE,QTE,RIE,SE,UE,QNN,C,QTT,SB,UB,VB,RB,PB,REB,U0,V0,P0,R0
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:Dim)::NX,NY,DA,P
 Real(8),Dimension(1:5,1:Dim)::Wb
!*******************************************************************************************	
 GM1= GM-1.
 
!Part 1
 DO I=NFF1+1,NFF2

   !Part 2:
    ME  = IDS(1,I)
    P1  = IDS(3,I)
    P2  = IDS(4,I)
    NXX = NX(I)/DA(I)
    NYY = NY(I)/DA(I)

   !Part 3:
    U = WNP1(2,ME)/WNP1(1,ME)
    V = WNP1(3,ME)/WNP1(1,ME)
    CC = GM*P(ME)/WNP1(1,ME)
    MLoc = SQRT((U*U+V*V)/CC)

   !Part 4:
    U0B = U0
    V0B = V0
    P0B = P0
    R0B = R0
    C0B = SQRT(GM*P0B/R0B)

   !Part 5:
    RE = WNP1(1,ME)
    UE = WNP1(2,ME)/RE
    VE = WNP1(3,ME)/RE
    PE = P(ME)
    CE = SQRT(ABS(GM*PE/RE))
    
   !Part 6:
    QN0 = U0B*NXX+V0B*NYY
    QT0 =-U0B*NYY+V0B*NXX
    RI0 = QN0 - 2.*C0B/GM1
    S0  = P0B/R0B**GM

   !Part 7:
    QNE = UE*NXX+VE*NYY
    QTE =-UE*NYY+VE*NXX
    RIE = QNE+2.*CE/GM1
    SE  = PE/RE**GM

   !Part 8:
    IF(MLoc<=1.)then
     
    !Part 9:
     QNN = (RIE+RI0)/2.0
     C   = 0.25*GM1*(RIE-RI0)
	 
    !Part 10:
     QTT = QT0
     SB  = S0
	 
    !Part 11:
     IF(QNN>0.0)then
	  QTT = QTE
      SB  = SE
	 Endif
     
   !Part 12:
    Elseif(MLoc>1.)then
     
    !Part 13:
     QNN = QN0
     C   = C0
     QTT = QT0
     SB  = S0
     
    !Part 14:
	 IF(QNN>0.0)then
      QNN = QNE
      C   = CE
      QTT = QTE
      SB  = SE
	 Endif

	Endif
    
   !Part 15:
    RB = (C*C/GM/SB)**(1.0/GM1)
    UB = QNN*NXX-QTT*NYY
    VB = QTT*NXX+QNN*NYY
    PB = C*C*RB/GM

    REB= PB/GM1 + 0.5*RB*(UB*UB + VB*VB)

    Wb(1,I) = RB
    Wb(2,I) = RB*UB
    Wb(3,I) = RB*VB
    Wb(4,I) = REB
    Wb(5,I) = PB

 End do
!*******************************************************************************************
 End
!###########################################################################################
