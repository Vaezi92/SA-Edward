!//////////////////////////////////////////////////////////////////////////////////////////!
!// Date :         Febreury/2/2020                                                       //!
!// Developed by : Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com              //!
!//                                                                                      //!                                                                      //
!// A Turbulent 2D Flow Solver                                                           //!
!// Features: 1- 2D                                                                      //!
!//           2- Using Unstructured Mesh                                                 //!
!//           3- Edge Based Data Structured                                              //!
!//           4- Cell Center Conrol Volume                                               //!
!//           5- Laminar Flow                                                            //!
!//           6- Convection Terms is Discritized by AUSM Scheme                          //!
!//           7- Transient Term is Discritized by Runge-Kutta Explicit                   //!
!//           8- Spalart-Allmaras Tulbulence Model                                       //!
!//                                                                                      //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************
 Program AirFlow_Turb
 Implicit None
!===============================
 Integer,Parameter::Dim=200000
!===============================

 Integer::I,J,NC,NP,NF,NF1,NF2,NFW1,NFW2,NFF1,NFF2,NFI1,NFI2,NFS1,NFS2,NFO1,NFO2,NFIF1,NFIF2,&
          NR,NRKS,NWrite,Init,Ncyc,NS
 Real(8)::GM,Co,ALF,R0,P0,T0,C0,U0,V0,E0,B0,ERmx,CFLx,Minf,Rinf,Tt,MR,PrL,PrT,Rm,U,V,RKco,Mu0,&
          Mut0,Temp ,K2,K4
 Integer,Dimension(1:100)::NFR,BC
 Integer,Dimension(1:4,1:Dim)::IDS
 Real(8),Dimension(1:4,1:Dim)::WNP1,WC,Con,Dif
 Real(8),Dimension(1:Dim)::X,Y,XC,YC,A,NX,NY,DA,DW,DT,P,Mu,Mut,DUX,DUY,DVX,DVY,DTX,DTY,Taukk
 Real(8),Dimension(1:5,1:Dim)::WB
 Integer,Dimension(1:Dim)::INW
 Real(8),Dimension(1:Dim)::Wtnp1
 Real(8)::Chi,Nut0,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Nu0,Nuhat0,Cv13,Chi3,Fv1
!***************************************** Main ********************************************
!Part 1:
 Call Read_2DMesh(Dim,NP,NC,NF,NR,NFR,BC,IDS,X,Y)

!Part 2:
 Call Read_SettingV2(Minf,Rinf,ALF,Tt,ERmx,CFLx,NRKS,NWrite,Init)

!Part 3:
 Call MeshBC(Dim,NR,NFR,BC,IDS,NF,NF1,NF2,NFW1,NFW2,NFF1,NFF2,NFI1,NFI2,NFS1,NFS2,NFO1,NFO2,NFIF1,NFIF2)

!Part 4:
 Call GeoCal2D(Dim,NF1,NF2,NF,NC,IDS,X,Y,Xc,Yc,NX,NY,DA,A)

!Part 5:
 Call InitMeanFlow(Dim,Init,NC,ALF,Minf,Rinf,Tt,MR,GM,PrL,PrT,R0,P0,C0,U0,V0,T0,Mu0,B0,WNP1)

 Do J=1,NC

   !Part 6:
	U = WNP1(2,J)/WNP1(1,J)
    V = WNP1(3,J)/WNP1(1,J)
    P(J) = (GM-1)*(WNP1(4,J)-0.5*WNP1(1,J)*(U*U+V*V))

   !Part 7:
    Temp = GM*P(J)/WNP1(1,J)
    Mu(j) = (Temp**1.5)*(1.0+B0)/(Temp+B0)

 End Do

!Part 8:
 Call BC_Wall(Dim,NFW1,NFW2,IDS,GM,P,WB)
 Call BC_Riemann(Dim,NFF1,NFF2,NX,NY,DA,IDS,GM,U0,V0,P0,R0,C0,WNP1,P,WB)
 Call BC_InFlow(Dim,NFI1,NFI2,NX,NY,DA,IDS,GM,U0,V0,P0,R0,WNP1,P,ALF,Minf,WB)
 Call BC_VisOutFlow(Dim,NFO1,NFO2,IDS,GM,P0,WNP1,P,WB)
 Call BC_Symmetry(Dim,NFS1,NFS2,NX,NY,DA,IDS,GM,WNP1,P,WB)

!Part 9:
 Call SA_Init(Dim,NC,NFW1,NFW2,NF,IDS,X,Y,Xc,Yc,Mu0,R0,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mut0,Nuhat0,WTNP1,DW,Mut)

!Part 10:
 IF(Init==1)Call SA_Main(Dim,NFW1,NFW2,NF1,NF2,NFF1,NFF2,NFS1,NFS2,NF,NC,NP,IDS,NX,NY,X,Y,Xc,Yc,A,Dw,&
                         NRKS,MR,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mu0,Nuhat0,Mu,DT,WB,WNP1,WTNP1,Mut)

!Part 11:
 Ncyc = 0
 Rm   = 10.0

!Part 12:
 Do While(Rm > ERmx)

   !Part 13:
    Ncyc=Ncyc+1

   !Part 14:
    Do J=1,NC
       WC(1,J) = WNP1(1,J)
       WC(2,J) = WNP1(2,J)
       WC(3,J) = WNP1(3,J)
       WC(4,J) = WNP1(4,J)
    End Do

   !Part 15:
	Call TimSTP_Turb(Dim,NC,NF,NF1,NF2,IDS,NX,NY,DA,A,CFLx,GM,P,WNP1,WB,Mu,Mut,PrL,PrT,MR,DT)

   !Part 16:
    Do NS=1,NRKS

      !Part 17:
	   RKco=1.0/(NRKS-NS+1)

      !Part 18:
	   Call ConMeanFlow_AUSM_Plus(Dim,NC,NF1,NF2,NF,IDS,NX,NY,DA,GM,WNP1,WB,P,Con)

      !Part 19:
       Call VelTemp_GradFace(Dim,NC,NF1,NF2,NFW1,NFW2,NF,NP,IDS,X,Y,Xc,Yc,WNP1,WB,GM,P,DUX,DUY,DVX,DVY,DTX,DTY)

      !Part 20:
	   Call DifMeanFlow_TurbWithk(Dim,NC,NFW1,NFW2,NF,NF1,NF2,IDS,GM,PrL,PrT,NX,NY,MR,Mu,Mut,&
                                WNP1,Wtnp1,WB,DUX,DUY,DVX,DVY,DTX,DTY,Dif)

      !Part 21:
       Do J=1,NC

		  Co = RKco*DT(J)/A(J)

          WNP1(1,J) = WC(1,J) - Co*( Con(1,J)          )
		  WNP1(2,J) = WC(2,J) - Co*( Con(2,J)+Dif(2,J) )
          WNP1(3,J) = WC(3,J) - Co*( Con(3,J)+Dif(3,J) )
          WNP1(4,J) = WC(4,J) - Co*( Con(4,J)+Dif(4,J) )

         !Part 22:
	      U    = WNP1(2,J)/WNP1(1,J)
          V    = WNP1(3,J)/WNP1(1,J)
          P(J) = (GM-1)*(WNP1(4,J)-0.5*WNP1(1,J)*(U*U+V*V))

         !Part 23:
          Temp = GM*P(J)/WNP1(1,J)
          Mu(j) = (Temp**1.5)*(1.0+B0)/(Temp+B0)

       End Do

      !Part 24:
       Call BC_Wall(Dim,NFW1,NFW2,IDS,GM,P,WB)
       Call BC_Riemann(Dim,NFF1,NFF2,NX,NY,DA,IDS,GM,U0,V0,P0,R0,C0,WNP1,P,WB)
       Call BC_InFlow(Dim,NFI1,NFI2,NX,NY,DA,IDS,GM,U0,V0,P0,R0,WNP1,P,ALF,Minf,WB)
       Call BC_VisOutFlow(Dim,NFO1,NFO2,IDS,GM,P0,WNP1,P,WB)
       Call BC_Symmetry(Dim,NFS1,NFS2,NX,NY,DA,IDS,GM,WNP1,P,WB)

     End Do !Ns

    !Part 25:
     Call SA_Main(Dim,NFW1,NFW2,NF1,NF2,NFF1,NFF2,NFS1,NFS2,NF,NC,NP,IDS,NX,NY,X,Y,Xc,Yc,A,Dw,&
                  NRKS,MR,Cb1,Cb2,Kei,Sigma,Cw1,Cw2,Cw3,Cv1,Mu0,Nuhat0,Mu,DT,WB,WNP1,WTNP1,Mut)

    !Part 26:
 	 Call ResMass(Dim,NC,WNP1,WC,DT,Ncyc,Rm)
     Print*,Ncyc,Rm,Mut(1000)

    !Part 27:
     IF( Mod(Ncyc,NWrite)==0 )Then
      Print*,'Writing Results... ',Ncyc,Rm
	  Call Write_ResultsV2(Dim,NFW1,NFW2,NF,NC,NP,IDS,X,Y,Xc,Yc,GM,Minf,Rinf,WNP1,P,Mu,Mut,DUY)
	 End If

 End Do !Do While
!*******************************************************************************************
 End
!###########################################################################################

