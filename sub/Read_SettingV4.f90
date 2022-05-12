!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description:To Take all of the Necessary setting Parameters from User                //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2014                                                              //!
!// It May be Copied, Modified, and Redistributed For Non-Commercial Use.                //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine Read_SettingV4(Minf,Rinf,ALF,Tt,ERmx,CFLx,NRKS,NWrite,Init,K2,K4)
 Implicit None
!******************************************************************************************* 
 Intent(Out  )::Minf,Rinf,ALF,Tt,ERmx,CFLx,NRKS,NWrite,Init,K2,K4

 Real(8)::Minf,Rinf,ALF,Tt,ERmx,CFLx,K2,K4
 Integer::NRKS,NWrite,Init
!*******************************************************************************************	
 Open(1,File='Setting.Txt')

!Part 1:
 Read(1,*) Minf

!Part 2:
 Read(1,*) Rinf

!Part 3:
 Read(1,*) ALF

!Part 4:
 Read(1,*) Tt

!Part 5:
 Read(1,*) ERmx
 
!Part 6:
 Read(1,*) CFLx
 
!Part 7:
 Read(1,*) NRKS

!Part 8:
 Read(1,*) NWrite

!Part 9:
 Read(1,*) Init

!Part 8:
 Read(1,*) K2

!Part 9:
 Read(1,*) K4
  
 Close(1)
!*******************************************************************************************
 End
!###########################################################################################