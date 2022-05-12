!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Apply InFlow Boundary Condition                                         //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!//                                                                                      //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine	BC_InFlow(Dim,NFI1,NFI2,NX,NY,DA,IDS,GM,U0,V0,P0,R0,WNP1,P,ALF,Minf,WB)
 Implicit None
!*******************************************************************************************
 Intent (In   )::Dim,NFI1,NFI2,GM,U0,V0,P0,R0,IDS,Wnp1,NX,NY,DA,P,ALF,Minf
 Intent (Out  )::WB

 Integer::Dim,I,NFI1,NFI2,ME
 Real(8)::GM,GM1,Minf,ITT,ITP,ALF,CC,MLoc,NXX,NYY,RE,VE,UE,PE,CE,HTE,QNE,QTE,RIE,UB,VB,RB,PB,&
          REB,RIB,MB,TB,UBB,HTB,C,U0,V0,P0,R0,EP1,EP2,EP3,C1,C2
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:Dim)::NX,NY,DA,P
 Real(8),Dimension(1:5,1:Dim)::WB
!*******************************************************************************************
!Part 1: 
 ITT=(1.+0.2*Minf*Minf)
 ITP= (ITT**3.5)   * P0

!Part 2: 
 GM1= GM-1
 DO I=NFI1+1,NFI2

   !Part 3: 
    NXX = NX(I)/DA(I)
    NYY = NY(I)/DA(I)

   !Part 4: 
    ME  = IDS(1,I)

    RE = WNP1(1,ME)
    UE = WNP1(2,ME)/RE
    VE = WNP1(3,ME)/RE
    PE = P(ME)

   !Part 5: 
    CE = SQRT(ABS(GM*PE/RE))
    MLoc = SQRT(UE*UE+VE*VE) / CE

   !Part 6: 
	IF(MLoc<1.)then

    !Part 7:
     QNE = UE*NXX+VE*NYY
     QTE =-UE*NYY+VE*NXX
	
    !Part 8:
     HTE = (PE/RE)*(GM/GM1) + 0.5*(UE*UE+VE*VE)
     RIE =-QNE-2.*CE/GM1
	
    !Part 9:
	 HTB = HTE
	 RIB = RIE

    !Part 10:
	 EP1 = 1+2/GM1
	 EP2 = 2*RIB
	 EP3 = (GM1/2)*(RIB*RIB-2*HTB)

	 C1 = (-EP2+SQRT(EP2*EP2-4*EP1*EP3))/(2*EP1)
	 C2 = (-EP2-SQRT(EP2*EP2-4*EP1*EP3))/(2*EP1)

	 C  = MAX(C1,C2)

    !Part 11:
	 UBB= (2*C/GM1)+RIB

     MB = UBB/C

	 PB = ITP*(1+(GM1/2)*MB**2)**(-GM/GM1)

	 TB = ITT*(PB/ITP)**(GM1/GM)

	 RB = GM*PB/TB

     UB = Cos(ALF)*UBB
	 VB = Sin(ALF)*UBB

   !Part 12:
    Else

	 RB = R0
     UB = U0
	 VB = V0
	 PB = P0

	Endif

   !Part 13:
	REB= PB/GM1 + 0.5*RB*(UB*UB + VB*VB)

   !Part 14:
	WB(1,I) = RB
    WB(2,I) = RB*UB
    WB(3,I) = RB*VB
    WB(4,I) = REB
    WB(5,I) = PB

 End Do
!*******************************************************************************************
 End
!########################################################################################### 

