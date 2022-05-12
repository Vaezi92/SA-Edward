!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//////////////////////////////////////////////////////////////////////////////////////////!
!// Description: To Write the Results                                                    //!
!//                                                                                      //!
!// Version: V1                                                                          //!
!// Date: October, 12, 2020                                                              //!
!// Developed by: Mohammad Vaezi, Iran, Tehran, mohammadvaeziphd@gmail.com               //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                 //!
!//////////////////////////////////////////////////////////////////////////////////////////!
!*******************************************************************************************5739
 Subroutine Write_ResultsV2(Dim,NFW1,NFW2,NF,NC,NP,IDS,X,Y,Xc,Yc,GM,Minf,Rinf,WNP1,P,Mu,Mut,DUY)
 Implicit None
!*******************************************************************************************
 Intent(In)::Dim,NFW1,NFW2,NF,NC,NP,IDS,X,Y,Xc,Yc,GM,Minf,Rinf,WNP1,P,Mu,Mut,DUY

 Integer::Dim,I,J,P1,P2,P3,P4,ME,NP,NC,NFW1,NFW2,NF,IDC,IDF
 Real(8)::Rt,Ut,Vt,Tt,Pt,VTt,Mt,Minf,Rinf,GM,CP,CCP,DX,DY,DL,Xm,Ym,Yk,TAUW,CFC,ALF,X21,X10,&
          Y21,Y10,Et,Rex,CF,CFexactT , CFexactL,Yp,UpP,Utau,Dis,XD,Dmin,U,V,CC,Mach,Teta
 Integer,Dimension(1:4,1:Dim)::Corn,IDS
 Real(8),Dimension(1:Dim)::X,Y,P,Xc,Yc,Mu,Mut,DUY
 Real(8),Dimension(1:4,1:Dim)::WNP1
!*******************************************************************************************2500149633	
!Part 1:
 Open(1,File='Contours.Plt')
 Open(2,File='CP.Plt')
 Open(3,File='CF.Plt')
 Open(4,File='SolutionData.txt')
 Open(5,File='UpYp.Plt')

!Part 2:
 Call EdgeToCell(Dim,NF,NC,IDS,Corn)

!Part 3:
 Write(1,*) 'Variables="X","Y","Ro","U","V","P","Mach","Mut"'
 Write(1,*) 'ZONE N=' ,   NP , ' E=' ,  NC  
 Write(1,*) ' ZONETYPE=FEQUADRILATERAL DATAPACKING=BLOCK VARLOCATION=([3-8]=CELLCENTERED)'

 Do J=1,NP
	Write(1,*) X(J)
 End Do
 Do J=1,NP
	Write(1,*) Y(J) 
 End Do

 Do J=1,NC
	Write(1,*) WNP1(1,J)
 End Do
 Do J=1,NC
	Write(1,*) WNP1(2,J)/WNP1(1,J)
 End Do
 Do J=1,NC
	Write(1,*) WNP1(3,J)/WNP1(1,J)
 End Do
 Do J=1,NC
	Write(1,*) P(J)
 End Do   
 Do J=1,NC
    U = WNP1(2,J)/WNP1(1,J)
    V = WNP1(3,J)/WNP1(1,J)
    CC = GM*P(J)/WNP1(1,J)
    Mach = SQRT((U*U+V*V)/CC)
	Write(1,*) Mach
 End Do
 Do J=1,NC
	Write(1,*) Mut(J)
 End Do

 Do I=1,NC
    P1 = Corn(1,I) 
    P2 = Corn(2,I) 
    P3 = Corn(3,I)
    P4 = Corn(4,I)
    if(P4==0) P4=P3

	Write(1,*) P1,P2,P3,P4
 End Do

!Part 4:
 Do I=1,NC
	Write(4,*) WNP1(1,I),WNP1(2,I),WNP1(3,I),WNP1(4,I)
 End Do

!Part 5:
 CCP = 0.5*GM*Minf*Minf
 WRITE(2,*)'VARIABLES="X","CP"'  
 WRITE(2,*)'ZONE'

 DO J=NFW1+1,NFW2
    ME  = IDS(1,J)
    P1  = IDS(3,J)
    P2  = IDS(4,J)

    Xm = 0.5*( X(P1)+X(P2) )
    CP = (GM*P(ME)-1.0)/CCP

	Write(2,*) Xm,CP
 End do

!Part 6:
 WRITE(3,*) 'zone'
 CFC = 2/(Rinf*Minf)
 DO I=NFW1+1,NFW2

    ME = IDS(1,I)
    P1 = IDS(3,I)
    P2 = IDS(4,I)

    DX = X(P2) - X(P1)
    DY = Y(P2) - Y(P1)
	DL = SQRT(DX*DX + DY*DY)


    UT = WNP1(2,ME)/WNP1(1,ME)*DX/DL + WNP1(3,ME)/WNP1(1,ME)*DY/DL
    CF = CFC * Mu(ME)*DUY(I)

    WRITE(3,*) Xc(ME) , CF

 End do

!Goto 100    !Comment This Comand Just for Simulation the Flow Field Over Flat Plate 6237

!Part 7:
 WRITE(3,*) 'zone'
 DO I=NFW1+1,NFW2

    ME = IDS(1,I)

	Rex= Xc(ME)* Rinf
    CFexactL =  0.664/Dsqrt(Rex) 
		 		
    WRITE(3,*) Xc(ME) , CFexactL

 End do

!Part 8:
 WRITE(3,*) 'zone'
 DO I=NFW1+1,NFW2

    ME = IDS(1,I)

	Rex= Xc(ME)* Rinf
    CFexactT = 0.025*Rex**(-1/7.)
	
    WRITE(3,*) Xc(ME) , CFexactT

 End do

!Part 9:
 XD = 0.5

 Dmin = 1000
 Do I=NFW1+1,NFW2
    P1 = IDS(3,I)
    P2 = IDS(4,I)
    Xm = 0.5*( X(P1)+X(P2) )
	Dis = Dabs(Xm-XD)
    IF( Dis<Dmin )Then
	 Dmin=Dis
	 IDF=I
	Endif
 Enddo
 IDC = IDS(1,IDF)

!Analytical Solution of 'Viscous Sub Layer'
 WRITE(5,*) 'zone'
 Do Yp=0.1,20,0.2
	   Upp=Yp
	   WRITE(5,*) Yp,Upp
 End do

!Analytical Solution of 'Logaritmic Layer'
 WRITE(5,*) 'zone'
 Do Yp=1,1000
         Upp=(1/0.41)*dlog(9.0*Yp)
	   WRITE(5,*) Yp,Upp
 End do

!Numerical Solution
 WRITE(5,*) 'zone'
 Do I=1,NC
    IF(abs(XC(I)-XC(IDC))<0.0000001)then
 
     Utau=Sqrt(Mu(IDC)*DUY(IDF)/WNP1(1,IDC)) 

     Yp=Yc(I)*Utau*WNP1(1,IDC)/Mu(IDC) * Sqrt(Rinf/Minf)
	 

	 Upp=WNP1(2,I)/Utau * Sqrt(Rinf/Minf)

	 write(5,*) Yp,Upp

    End if
 End do

100 continue

!Part 10:
 Close(1)
 Close(2)
 Close(3)
 Close(4)
 Close(5)
!**********************************************************************************************
 End
!##############################################################################################
!*******************************************************************************************
 Subroutine EdgeToCell(Dim,NF,NC,IDS,Corn)
 Implicit None
!*******************************************************************************************
 Intent(In   )::Dim,NF,NC,IDS
 Intent(Inout)::Corn
 
 Integer::Dim,NF,NC,ME,NE,I,J,J1,J2,E,E1,E2,E3,P1_E2,P2_E1,P
 Integer,Dimension(1:4, 1:Dim)::IDS
 Integer,Dimension(1:4, 1:Dim)::CELL_EDGE,Corn
 Integer,Dimension(1:Dim)::NCELL_EDGE
!*******************************************************************************************
 Do I=1,NC
    NCELL_EDGE(I)=0
 End Do
    Corn=0

!Part 1:
 Do I=1,NF

   !Part 2:
    ME = IDS(1, I)
    NE = IDS(2, I)
	
    !Part 3:
    IF(ME/=0)Then

    !Part 4:
	 NCELL_EDGE(ME) = NCELL_EDGE(ME) + 1
     CELL_EDGE(NCELL_EDGE(ME),ME)=I

    EndIF

	!Part 5:
    IF(NE/=0)Then

     !Part 6:
	 NCELL_EDGE(NE) = NCELL_EDGE(NE) + 1
     CELL_EDGE(NCELL_EDGE(NE),NE)=-I

    EndIF

 End Do


!Part 7:
 Do I=1,NC

    Do J1=1,NCELL_EDGE(I)-1

       E1 = CELL_EDGE(J1,I)

    !Part 9:
    IF( E1>0 )Then
     P2_E1 = IDS(4,E1)
    Else
     P2_E1 = IDS(3,-E1)
	EndIF

	   Do J2=2,NCELL_EDGE(I)
    !Part 8:
    E2 = CELL_EDGE(J2,I)

    !Part 10:
    IF( E2>0 )Then
     P1_E2 = IDS(3,E2)
    Else
     P1_E2 = IDS(4,-E2)
    EndIF


    !Part 11:
    IF( P2_E1==P1_E2 )Then
	 E                 = CELL_EDGE(J1+1,I)
	 CELL_EDGE(J1+1,I) = CELL_EDGE(J2,I)
     CELL_EDGE(J2,I)   = E
    EndIF

       End Do

    End Do

 End Do 



 do I=1,NC

    Do J=1,NCELL_EDGE(I)

       E = CELL_EDGE(J,I)

       IF( E>0 )Then
        P = IDS(3,E)
       Else
        P = IDS(4,-E)
       EndIF

       Corn(J,I) = P

    End Do
 End Do
!*******************************************************************************************
 End
!###########################################################################################



 
 
 
