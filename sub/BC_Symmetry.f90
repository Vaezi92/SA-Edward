!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Apply Symmetry Boundary Condition                                       //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!//                                                                                      //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine	BC_Symmetry(Dim,NFS1,NFS2,NX,NY,DA,IDS,GM,WNP1,P,WB)
 Implicit None
!*******************************************************************************************
 Intent (In   )::Dim,NFS1,NFS2,NX,NY,DA,IDS,GM,WNP1,P
 Intent (Out  )::WB

 Integer::Dim,I,NFS1,NFS2,ME
 Real(8)::GM1,GM,NXX,NYY,RE,UE,VE,PE,TE,QNE,QTE,RB,QNB,QTB,PB,TB,UB,VB,REB
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Real(8),Dimension(1:Dim)::NX,NY,DA,P
 Real(8),Dimension(1:5,1:Dim)::WB
!********************************************************************************************** 
!Part 1	
 GM1= GM-1
 
!Part 2
 DO I=NFS1+1,NFS2

   !Part 3
    ME  = IDS(1,I)
    NXX = NX(I)/DA(I)
    NYY = NY(I)/DA(I)

   !Part 4
    RE = WNP1(1,ME)
    UE = WNP1(2,ME)/RE
    VE = WNP1(3,ME)/RE
    PE = P(ME)
	TE=PE*GM/RE
	
   !Part 5
    QNE = UE*NXX+VE*NYY
    QTE =-UE*NYY+VE*NXX

   !Part 6
	RB=RE
	QNB=0
	QTB=QTE
	PB=PE
	TB=TE

   !Part 7
	UB=NXX*QNB-NYY*QTB
	VB=NYY*QNB+NXX*QTB
	
   !Part 8
	REB= PB/GM1 + 0.5*RB*(UB*UB + VB*VB)

   !Part 9
	WB(1,I) = RB
    WB(2,I) = RB*UB
    WB(3,I) = RB*VB
    WB(4,I) = REB
    WB(5,I) = PB

 END Do
!*******************************************************************************************
 End
!###########################################################################################