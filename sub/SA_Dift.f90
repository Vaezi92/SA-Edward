!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: Calculate the Diffusion Term of Spalar-Allmaras Turbulence Model        //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Subroutine SA_Dift(Dim,NC,NF1,NF2,NF,MR,Cb2,Sigma,NX,NY,A,WTNP1,WNP1,Mu,WTB,IDS,DNuX,DNuY,&
                    DNuXc,DNuYc,DRNuXc,DRNuYc,Dift)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NC,NF1,NF2,NF,MR,Cb2,Sigma,NX,NY,A,WTNP1,WNP1,Mu,WTB,IDS,DNuX,DNuY,&
                DNuXc,DNuYc,DRNuXc,DRNuYc
 Intent(Out  )::Dift

 Integer::Dim,I,NC,NF1,NF2,NF,ME,NE
 Real(8)::T1,Mu_eff,Del_Nu,MR,Cb2,Sigma
 Real(8),Dimension(1:Dim)::NX,NY,A,DNuX,DNuY,DRNuX,DRNuY,DNuXc,DNuYc,DRNuXc,DRNuYc,WTNP1,WTB,Mu,Dift
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::WNP1
!*******************************************************************************************
!Part 1:
 Do I=1,NC
    Dift(I) = A(I) * Cb2 * ( DNuXc(I)*DRNuXc(I) + DNuYc(I)*DRNuYc(I) )
 End do

!Part 2:
 Do I=NF2+1,NF

   !Part 3:
    ME = IDS(1,I)

   !Part 4:
    Mu_eff = Mu(ME) + WTB(I)
    T1 = Mu_eff * ( DNuX(I)*NX(I) + DNuY(I)*NY(I) )

    Dift(ME) = Dift(ME) + T1

 End do

!Part 5:
 Do I=NF1+1,NF2

   !Part 6:
    ME = IDS(1,I)
	NE = IDS(2,I)

   !Part 7:
    Mu_eff = 0.5*( Mu(ME)+WTNP1(ME) + Mu(NE)+WTNP1(NE) )

    T1 = Mu_eff * ( DNuX(I)*NX(I) + DNuY(I)*NY(I) )

   !Part 8:
    Dift(ME) = Dift(ME) + T1
    Dift(NE) = Dift(NE) - T1

 End do

!Part 9:
 Do I=1,NC
    Dift(I) = MR * Dift(I) / Sigma
 End do
!**********************************************************************************************
 End
!##############################################################################################


