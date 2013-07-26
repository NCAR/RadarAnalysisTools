c
c----------------------------------------------------------------------X
c
      FUNCTION ESAT(T)
C     
C     SATURATION VAPOR PRESSURE FOR INPUT (T)
C     ESAT(MILLIBARS),T(KELVIN)
C     
      DATA ABZ/273.16/
C     
      TC=T-ABZ
      ESAT=6.1078*EXP((17.2693882*TC)/(TC+237.3))

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION OD(T, P)                                                
C     
C     OD(T,P) DRY ADIABATIC LINE THROUGH T,P (KELVIN)
C     OUTPUT POTENTIAL TEMPERATURE FROM INPUT (T,P)
C     OD AND T(KELVIN), P(MILLIBARS)
C     
      OD = T*(1000./P)**.286
      RETURN                                                            
      END                                                               
c
c----------------------------------------------------------------------X
c
      FUNCTION OS(T,P)
C     
C     OS(T,P) SATURATED ADIABAT LINE THROUGH T,P (KELVIN)
C     OUTPUT EQUIVALENT POTENTIAL TEMPERATURE FROM INPUT (T,P)
C     OS AND T(KELVIN), P(MILLIBARS)
C     
      OS=T*((1000./P)**.286)/(EXP(-2.6518986*W(T,P)/T))
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION TDRY(OD, P)                                                
C     
C     AIR TEMPERATURE ALONG A DRY ADIABAT
C     OUTPUT AIR TEMPERATURE TDRY (KELVIN)
C     FROM INPUT (OD,P) in Kelvin, millibars
C     Note: OD is potential temperature
C     
      A = OD
      TDRY = A*(P/1000.)**.286
      RETURN                                                            
      END                                                               
c
c----------------------------------------------------------------------X
c
      FUNCTION TMR(W,P)
C
C     TMR(AW,P) TEMPERATURE ON MIXING RATIO AW AT LEVEL P (KELVIN)              
C     TMR(KELVIN),W(GRAMS WATER VAPOR/KILOGRAM DRY AIR),P(MILLIBAR)             
C     ALOG10 IS LOG TO THE BASE TEN.                                        
C
      IF(W.LE.0.0)W=0.01
      X =  ALOG10(   W*P/(622.+ W)  )
      TMR=10.**(.0498646455*X+2.4082965)-7.07475+38.9114*((10.**(
     1     .0915*X ) - 1.2035 )**2 )
      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION TSA(OS,P)
C     
C     TSA(OS,P) TEMPERATURE ON SATURATED ADIABAT AOS AT LEVEL P (KELVIN)
C     SIGN(A,B) REPLACES THE ALGEBRAIC SIGN OF A WITH THE SIGN OF B
C     TSA AND OS(KELVIN), P(MILLIBARS)
C     
      A=OS
      TQ=253.16
      D=120
C     
      DO 1 I=1,50
         D=D/2
C     
C     IF THE TEMPERATURE DIFFERENCE,X, IS SMALL, EXIT THIS LOOP
C     
         X=A*EXP(-2.6518986*W(TQ,P)/TQ)-TQ*((1000./P)**.286)
         IF (ABS(X).LT.0.001) GO TO 2
         TQ=TQ+SIGN(D,X)
 1    CONTINUE
 2    TSA=TQ

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION W(T,P)
C     
C     W(T,P) MIXING RATIO LINE THROUGH T,P (GM/KG)
C     W(GRAMS WATER VAPOR/KILOGRAM DRY AIR), P(MILLIBARS)
C     
      IF (T.GE.999.) GO TO 10
      X=ESAT(T)
      W=622.*X/(P-X)

      RETURN

 10   W=10.0

      RETURN
      END
c
c----------------------------------------------------------------------X
c
      FUNCTION Z(PT,P,T,TD,N)
C
C     Z(PTOP,P,T,TD,N) THICKNESS IN METERS FROM P(1) TO PTOP                    
C     P IS PRESSURE IN MILLIBARS                                                
C     T IS TEMPERATURE IN KELVIN                                                
C     TD IS DEWPOINT IN KELVIN                                                  
C
      DIMENSION T(1),P(1),TD(1)

      Z = 0.0
      IF(PT.LT.P(N))GO TO 20
      I =  0

 9    I = I+1
      J  = I+1
      IF(PT.GE.P(J)) GO TO 10
      A1=T(J)*(1.+.0006078*W(TD(J),P(J)))
      A2=T(I)*(1.+.0006078*W(TD(I),P(I)))
      Z = Z+14.64285*(A1+A2)*(ALOG(P(I)/P(J)))
      GO TO 9

 10   CONTINUE
      A1=T(J)*(1.+.0006078*W(TD(J),P(J)))
      A2=T(I)*(1.+.0006078*W(TD(I),P(I)))
      Z = Z+14.64285*(A1+A2)*(ALOG(P(I)/PT ))
      RETURN

 20   Z=-1.0

      RETURN
      END



