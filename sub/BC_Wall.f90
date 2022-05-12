!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Apply Wall Boundary Condition on Inviscid and Viscouse Wall Faces       //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!//                                                                                      //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine BC_Wall(Dim,NFW1,NFW2,IDS,GM,P,WB)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NFW1,NFW2,IDS,GM,P
 Intent(Out  )::WB

 Integer::Dim,I,NFW1,NFW2,ME
 Real(8)::GM,GM1,PB
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:Dim)::P
 Real(8),Dimension(1:5,1:Dim)::WB
!*******************************************************************************************	
!Part 1:  
 GM1= GM-1.

!Part 2:
 DO I=NFW1+1,NFW2

   !Part 3:
    ME = IDS(1,I)

   !Part 4:
    WB(1,I) = 1.0
    WB(2,I) = 0.0
    WB(3,I) = 0.0
    WB(4,I) = P(ME)/GM1
    WB(5,I) = P(ME)

 End do
!*******************************************************************************************
 End
!###########################################################################################
