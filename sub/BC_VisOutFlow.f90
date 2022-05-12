!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Apply OutFlow Boundary Condition on faces ajacent to Viscous Wall       //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!//                                                                                      //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine	BC_VisOutFlow(Dim,NFO1,NFO2,IDS,GM,P0,WNP1,P,WB)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NFO1,NFO2,IDS,GM,P0,WNP1,P
 Intent(Out  )::WB

 Integer::Dim,I,ME,NFO1,NFO2
 Real(8)::GM,GM1,CC,MLoc,PE,RE,UE,VE,TE,RB,UB,VB,PB,TB,REB,P0
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:Dim)::P
 Real(8),Dimension(1:5,1:Dim)::WB
!*******************************************************************************************	
!Part 1: 
 GM1= GM-1
 DO I=NFO1+1,NFO2

   !Part 2: 
    ME  = IDS(1,I)

    RE = WNP1(1,ME)
    UE = WNP1(2,ME)/RE
    VE = WNP1(3,ME)/RE
    PE = P(ME)
	TE = PE*GM/RE

   !Part 3: 
    CC = GM*P(ME)/RE
	MLoc = SQRT( (UE*UE+VE*VE)/CC )

   !Part 4: 
	IF(MLoc<1.)then
	 PB=P0
	 UB=UE
	 VB=VE
	 TB=TE
	 RB=PB*GM/TB

   !Part 5: 
	Else
	 PB=PE
	 UB=UE
	 VB=VE
	 TB=TE
	 RB=PB*GM/TB

	Endif

   !Part 6: 
	REB= PB/GM1 + 0.5*RB*(UB*UB + VB*VB)

   !Part 7: 
	WB(1,I) = RB
    WB(2,I) = RB*UB
    WB(3,I) = RB*VB
    WB(4,I) = REB
    WB(5,I) = PB

 End Do
!*******************************************************************************************
 End
!###########################################################################################