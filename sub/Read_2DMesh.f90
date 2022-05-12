!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: To Read Edge Based Mesh From 'Mesh.Txt' File                            //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: November, 6, 2015                                                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine Read_2DMesh(Dim,NP,NC,NF,NR,NFR,BC,IDS,X,Y)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim
 Intent(Out  )::NP,NC,NF,NR,NFR,BC,IDS,X,Y

 Integer::Dim,I,J,J1,JJ,NP,NC,NF,NR,SFace,FaceType,MeshDim
 Integer,Dimension(1:100)::NFR,BC
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:Dim)::X,Y
!*******************************************************************************************
!Part 1:
 Open(1,File='Mesh.Txt')

!Part 2:
 Read(1,*) MeshDim
 IF(MeshDim/=2)Print*,'Please Check the Mesh File. It is not a 2D Mesh'

!Part 3:
 Read(1,*) NP    

!Part 4:
 Read(1,*) NC

!Part 5:
 Read(1,*) NF

!Part 6:
 Read(1,*) NR

!Part 7:
 Read(1,*)
 Do J=1,NR
    Read(1,*) NFR(J) , BC(J)
 End Do 

!Part 8:
 Read(1,*)
 Do J=1,NF  
    Read(1,*) FaceType,IDS(1,J),IDS(2,J),IDS(3,J),IDS(4,J)
 End Do
 
!Part 9:
 Read(1,*)        
 Do J=1,NP
    Read(1,*) X(J),Y(J)
 End Do

 Close(1)
!*******************************************************************************************
 End
!###########################################################################################
