!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Renumbering Faces According to Boundary Conditions and                  //!
!//              Determining the Index of first and last Faces of Each Boundary Conditions/!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2014                                                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine MeshBC(Dim,NR,NFR,BC,IDS,NF,NF1,NF2,NFW1,NFW2,NFF1,NFF2,NFI1,NFI2,NFS1,NFS2,&
                   NFO1,NFO2,NFIF1,NFIF2)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NR,NF
 Intent(Out  )::NF1,NF2,NFW1,NFW2,NFF1,NFF2,NFI1,NFI2,NFO1,NFO2,NFS1,NFS2,NFIF1,NFIF2
 Intent(InOut)::NFR,BC,IDS

 Integer::Dim,J,JJ,J1,I,SF,N,M,NR,NF,NFW1,NFW2,NFF1,NFF2,NFI1,NFI2,NFS1,NFS2,NFO1,NFO2,&
          NF1,NF2,NFIF1,NFIF2,NFN,NFW,NFF,NFI,NFO,NFS,NFIF
 Integer,Dimension(1:100)::NFR,TNFR,BC,TBC
 Integer,Dimension(1:4,1:Dim)::IDS,TIDS
!*******************************************************************************************

!Part 1:
 Do J=1,NF
	Do J1=1,4
       TIDS(J1,J) = IDS(J1,J)
    End do
 End Do

!Part 2:
 Do J=1,NR
    TNFR(J) = NFR(J)
	TBC(J)  = BC(J) 
 End do

!Part 3:
 N=0
 M=0
 Do JJ=1,10

   !Part 4:
    SF=0
    Do J=1,NR
       IF(TBC(J)==JJ)Then

	    Do I=SF+1,SF+TNFR(J)

           N=N+1
           Do J1=1,4
              IDS(J1,N) = TIDS(J1,I)
		   End do

        Enddo
		
	   !Part 5:		   
	    M=M+1
	    NFR(M) = TNFR(J)
		BC(M)  = TBC(J)  

	   Endif
	   SF=SF+TNFR(J)
    End Do

 End Do

!Part 6:
 NFN  = 0
 NFW  = 0
 NFF  = 0
 NFI  = 0
 NFO  = 0
 NFS  = 0
 NFIF = 0
 Do J=1,NR
    IF( BC(J)==1 ) NFN  = NFN  + NFR(J)
    IF( BC(J)==2 ) NFW  = NFW  + NFR(J)
    IF( BC(J)==3 ) NFF  = NFF  + NFR(J)
    IF( BC(J)==4 ) NFI  = NFI  + NFR(J)
    IF( BC(J)==5 ) NFO  = NFO  + NFR(J)
    IF( BC(J)==6 ) NFS  = NFS  + NFR(J)
    IF( BC(J)==7 ) NFIF = NFIF + NFR(J)
 End Do

!Part 7:
 NF1=0
 NF2=NF1+NFN

 NFW1=NF2
 NFW2=NFW1+NFW

 NFF1=NFW2
 NFF2=NFF1+NFF

 NFI1=NFF2
 NFI2=NFI1+NFI

 NFO1=NFI2
 NFO2=NFO1+NFO

 NFS1=NFO2
 NFS2=NFS1+NFS

 NFIF1=NFI2
 NFIF2=NFIF1+NFIF
!*******************************************************************************************
 End
!###########################################################################################

 
