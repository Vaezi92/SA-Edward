!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate the Eddy Viscousity Using Spalar-Allmaras Turbulence Model    //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine SA_Main(Dim,NFW1,NFW2,NF1,NF2,NFF1,NFF2,NFS1,NFS2,NF,NC,NP,IDS,NX,NY,X,Y,Xc,&
                    Yc,A,Dw,NRKS,MR,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mu0,Nuhat0,Mu,DT,WB,&
					WNP1,WTNP1,Mut)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NFW1,NFW2,NF1,NF2,NFF1,NFF2,NFS1,NFS2,NF,NC,NP,IDS,NX,NY,X,Y,Xc,Yc,A,&
                Dw,NRKS,MR,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mu0,Nuhat0,Mu,DT,WB,WNP1
 Intent(InOut)::WTNP1,Mut

 Integer::Dim,I,NFW1,NFW2,NF1,NF2,NFF1,NFF2,NF,NC,NP,NS,NFS1,NFS2,NRKS
 Real(8)::Cv1,Cw3,Kei,Cw1,Cw2,Cb1,Mu0,MR,RKco,RHS,Nu,Chi,Chi3,Cv13,Cb2,Sigma,Nuhat0,Fv1,Co,MutMax
 Real(8),Dimension(1:Dim)::NX,NY,X,Y,A,Dw,WCt,WTNP1,Mu,Mut,WTB,Cont,Prod,Dift,Dest,DUY,&
                           DVX,DT,Xc,Yc,DNuXF,DNuYF,DRNuX,DRNuY,DNuXc,DNuYc,DRNuXc,DRNuYc,DUX,DVY
 Real(8),Dimension(1:5,1:Dim)::WB
 Real(8),Dimension(1:4,1:Dim)::WNP1
 Integer,Dimension(1:4,1:Dim)::IDS
!*******************************************************************************************
!Part 1:
 Do I=1,NC
    WCt(I) = WTNP1(I)
 End Do

!Part 2:
 Do NS=1,NRKS

   !Part 3:
    Call SA_BC(Dim,NFW1,NFW2,NF,IDS,NX,NY,WB,WTNP1,Nuhat0,WTB)

   !Part 4:
    Call SA_Gradient_Cell(Dim,NC,NF1,NF2,NF,NX,NY,A,WTNP1,WNP1,WB,WTB,IDS,DNuXC,DNuYC,DRNuXC,DRNuYC,DUY,DVX,DUX,DVY)

   !Part 5:
    Call SA_Gradient_Face(Dim,NC,NF1,NF2,NFW1,NFW2,NFS1,NFS2,NF,NP,IDS,X,Y,Xc,Yc,WNP1,WB,WTNP1,WTB,DNuXF,DNuYF)

   !Part 6:
    Call SA_Cont(Dim,NC,NF1,NF2,NF,NX,NY,IDS,WTNP1,WNP1,WB,WTB,Cont)

   !Part 7:
    Call SA_Dift(Dim,NC,NF1,NF2,NF,MR,Cb2,Sigma,NX,NY,A,WTNP1,WNP1,Mu,WTB,IDS,DNuXF,DNuYF,DNuXc,DNuYc,DRNuXc,DRNuYc,Dift)

   !Part 8:
    Call SA_ProdDest(Dim,NC,A,Dw,MR,Cv1,Cb1,Cw1,Cw2,Cw3,Kei,WNP1,Mu,DUY,DVX,DUX,DVY,WTNP1,Dest,Prod)

   !Part 9:
    RKco=1.0/(NRKS-NS+1)
    DO I=1,NC

	   Co = RKco*DT(I)/A(I)
       WTNP1(I) = WCt(I) - Co*( Cont(I) - Prod(I) - Dift(I) + Dest(I))

       IF( WTNP1(I)<0. ) WTNP1(I) = WCt(I)

	   !IF( xc(I)<0. ) WTNP1(I) = Nuhat0
    End do

   !Part 10:
    Cv13=Cv1*Cv1*Cv1
    DO I=1,NC

	   Chi  = WTNP1(I)/Mu(I)
	   Chi3 = Chi*Chi*Chi

	   Fv1  = Chi3/(Chi3+Cv13)

	   Mut(I) = Fv1*WTNP1(I)
    End do

 End do


 MutMax=0.0
 DO I=1,NC
  !  if( Mut(I)>MutMax) MutMax=Mut(I)
 End do
    !print*,Mut(I)  !MutMax

!**********************************************************************************************
 End
!##############################################################################################






