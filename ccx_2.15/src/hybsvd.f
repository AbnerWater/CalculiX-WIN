      SUBROUTINE HYBSVD(NA, NU, NV, NZ, NB, M, N, A, W, MATU, U, MATV,
     * V, Z, B, IRHS, IERR, RV1)
      INTEGER NA, NU, NV, NZ, M, N, IRHS, IERR, MIN0
      REAL*8 A(NA,1), W(1), U(NU,1), V(NV,1), Z(NZ,1), B(NB,IRHS)
      REAL*8 RV1(1)
      LOGICAL MATU, MATV
C
C     THIS ROUTINE IS A MODIFICATION OF THE GOLUB-REINSCH PROCEDURE (1)
C                                                           T
C     FOR COMPUTING THE SINGULAR VALUE DECOMPOSITION A = UWV  OF A
C     REAL M BY N RECTANGULAR MATRIX. U IS M BY MIN(M,N) CONTAINING
C     THE LEFT SINGULAR VECTORS, W IS A MIN(M,N) BY MIN(M,N) DIAGONAL
C     MATRIX CONTAINING THE SINGULAR VALUES, AND V IS N BY MIN(M,N)
C     CONTAINING THE RIGHT SINGULAR VECTORS.
C
C     THE ALGORITHM IMPLEMENTED IN THIS
C     ROUTINE HAS A HYBRID NATURE.  WHEN M IS APPROXIMATELY EQUAL TO N,
C     THE GOLUB-REINSCH ALGORITHM IS USED, BUT WHEN EITHER OF THE RATIOS
C     M/N OR N/M IS GREATER THAN ABOUT 2,
C     A MODIFIED VERSION OF THE GOLUB-REINSCH
C     ALGORITHM IS USED.  THIS MODIFIED ALGORITHM FIRST TRANSFORMS A
C                                                                T
C     INTO UPPER TRIANGULAR FORM BY HOUSEHOLDER TRANSFORMATIONS L
C     AND THEN USES THE GOLUB-REINSCH ALGORITHM TO FIND THE SINGULAR
C     VALUE DECOMPOSITION OF THE RESULTING UPPER TRIANGULAR MATRIX R.
C     WHEN U IS NEEDED EXPLICITLY IN THE CASE M.GE.N (OR V IN THE CASE
C     M.LT.N), AN EXTRA ARRAY Z (OF SIZE AT LEAST
C     MIN(M,N)**2) IS NEEDED, BUT OTHERWISE Z IS NOT REFERENCED
C     AND NO EXTRA STORAGE IS REQUIRED.  THIS HYBRID METHOD
C     SHOULD BE MORE EFFICIENT THAN THE GOLUB-REINSCH ALGORITHM WHEN
C     M/N OR N/M IS LARGE.  FOR DETAILS, SEE (2).
C
C     WHEN M .GE. N,
C     HYBSVD CAN ALSO BE USED TO COMPUTE THE MINIMAL LENGTH LEAST
C     SQUARES SOLUTION TO THE OVERDETERMINED LINEAR SYSTEM A*X=B.
C     IF M .LT. N (I.E. FOR UNDERDETERMINED SYSTEMS), THE RHS B
C     IS NOT PROCESSED.
C
C     NOTICE THAT THE SINGULAR VALUE DECOMPOSITION OF A MATRIX
C     IS UNIQUE ONLY UP TO THE SIGN OF THE CORRESPONDING COLUMNS
C     OF U AND V.
C
C     THIS ROUTINE HAS BEEN CHECKED BY THE PFORT VERIFIER (3) FOR
C     ADHERENCE TO A LARGE, CAREFULLY DEFINED, PORTABLE SUBSET OF
C     AMERICAN NATIONAL STANDARD FORTRAN CALLED PFORT.
C
C     REFERENCES:
C
C     (1) GOLUB,G.H. AND REINSCH,C. (1970) 'SINGULAR VALUE
C         DECOMPOSITION AND LEAST SQUARES SOLUTIONS,'
C         NUMER. MATH. 14,403-420, 1970.
C
C     (2) CHAN,T.F. (1982) 'AN IMPROVED ALGORITHM FOR COMPUTING
C         THE SINGULAR VALUE DECOMPOSITION,' ACM TOMS, VOL.8,
C         NO. 1, MARCH, 1982.
C
C     (3) RYDER,B.G. (1974) 'THE PFORT VERIFIER,' SOFTWARE -
C         PRACTICE AND EXPERIENCE, VOL.4, 359-377, 1974.
C
C     ON INPUT:
C
C        NA MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETER A AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.  NOTE THAT NA MUST BE AT LEAST
C          AS LARGE AS M.
C
C        NU MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY U AS DECLARED IN THE CALLING PROGRAM DIMENSION
C          STATEMENT. NU MUST BE AT LEAST AS LARGE AS M.
C
C        NV MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETER V AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT. NV MUST BE AT LEAST AS LARGE AS N.
C
C        NZ MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETER Z AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.  NOTE THAT NZ MUST BE AT LEAST
C          AS LARGE AS MIN(M,N).
C
C        NB MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETER B AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT. NB MUST BE AT LEAST AS LARGE AS M.
C
C        M IS THE NUMBER OF ROWS OF A (AND U).
C
C        N IS THE NUMBER OF COLUMNS OF A (AND NUMBER OF ROWS OF V).
C
C        A CONTAINS THE RECTANGULAR INPUT MATRIX TO BE DECOMPOSED.
C
C        B CONTAINS THE IRHS RIGHT-HAND-SIDES OF THE OVERDETERMINED
C         LINEAR SYSTEM A*X=B. IF IRHS .GT. 0 AND M .GE. N,
C         THEN ON OUTPUT, THE FIRST N COMPONENTS OF THESE IRHS COLUMNS
C                       T
C         WILL CONTAIN U B. THUS, TO COMPUTE THE MINIMAL LENGTH LEAST
C                                               +
C         SQUARES SOLUTION, ONE MUST COMPUTE V*W  TIMES THE COLUMNS OF
C                   +                        +
C         B, WHERE W  IS A DIAGONAL MATRIX, W (I)=0 IF W(I) IS
C         NEGLIGIBLE, OTHERWISE IS 1/W(I). IF IRHS=0 OR M.LT.N,
C         B IS NOT REFERENCED.
C
C        IRHS IS THE NUMBER OF RIGHT-HAND-SIDES OF THE OVERDETERMINED
C         SYSTEM A*X=B. IRHS SHOULD BE SET TO ZERO IF ONLY THE SINGULAR
C         VALUE DECOMPOSITION OF A IS DESIRED.
C
C        MATU SHOULD BE SET TO .TRUE. IF THE U MATRIX IN THE
C          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
C
C        MATV SHOULD BE SET TO .TRUE. IF THE V MATRIX IN THE
C          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
C
C        WHEN HYBSVD IS USED TO COMPUTE THE MINIMAL LENGTH LEAST
C        SQUARES SOLUTION TO AN OVERDETERMINED SYSTEM, MATU SHOULD
C        BE SET TO .FALSE. , AND MATV SHOULD BE SET TO .TRUE.  .
C
C     ON OUTPUT:
C
C        A IS UNALTERED (UNLESS OVERWRITTEN BY U OR V).
C
C        W CONTAINS THE (NON-NEGATIVE) SINGULAR VALUES OF A (THE
C          DIAGONAL ELEMENTS OF W).  THEY ARE SORTED IN DESCENDING
C          ORDER.  IF AN ERROR EXIT IS MADE, THE SINGULAR VALUES
C          SHOULD BE CORRECT AND SORTED FOR INDICES IERR+1,...,MIN(M,N).
C
C        U CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE
C          DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE.  IF MATU IS
C          FALSE, THEN U IS EITHER USED AS A TEMPORARY STORAGE (IF
C          M .GE. N) OR NOT REFERENCED (IF M .LT. N).
C          U MAY COINCIDE WITH A IN THE CALLING SEQUENCE.
C          IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING
C          TO INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT.
C
C        V CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF
C          MATV HAS BEEN SET TO .TRUE.  IF MATV IS
C          FALSE, THEN V IS EITHER USED AS A TEMPORARY STORAGE (IF
C          M .LT. N) OR NOT REFERENCED (IF M .GE. N).
C          IF M .GE. N, V MAY ALSO COINCIDE WITH A.  IF AN ERROR
C          EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO INDICES OF
C          CORRECT SINGULAR VALUES SHOULD BE CORRECT.
C
C        Z CONTAINS THE MATRIX X IN THE SINGULAR VALUE DECOMPOSITION
C                  T
C          OF R=XSY,  IF THE MODIFIED ALGORITHM IS USED. IF THE
C          GOLUB-REINSCH PROCEDURE IS USED, THEN IT IS NOT REFERENCED.
C          IF MATU HAS BEEN SET TO .FALSE. IN THE CASE M.GE.N (OR
C          MATV SET TO .FALSE. IN THE CASE M.LT.N), THEN Z IS NOT
C          REFERENCED AND NO EXTRA STORAGE IS REQUIRED.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          K          IF THE K-TH SINGULAR VALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C          -1         IF IRHS .LT. 0 .
C          -2         IF M .LT. 1 .OR. N .LT. 1
C          -3         IF NA .LT. M .OR. NU .LT. M .OR. NB .LT. M.
C          -4         IF NV .LT. N .
C          -5         IF NZ .LT. MIN(M,N).
C
C        RV1 IS A TEMPORARY STORAGE ARRAY OF LENGTH AT LEAST MIN(M,N).
C
C     PROGRAMMED BY : TONY CHAN
C                     BOX 2158, YALE STATION,
C                     COMPUTER SCIENCE DEPT, YALE UNIV.,
C                     NEW HAVEN, CT 06520.
C     LAST MODIFIED : JANUARY, 1982.
C
C     HYBSVD USES THE FOLLOWING FUNCTIONS AND SUBROUTINES.
C       INTERNAL  GRSVD, MGNSVD, SRELPR
C       FORTRAN   MIN0,DABS,DSQRT,DFLOAT,DSIGN,DMAX1
C       BLAS      DSWAP
C
C     -----------------------------------------------------------------
C     ERROR CHECK.
C
      IERR = 0
      IF (IRHS.GE.0) GO TO 10
      IERR = -1
      RETURN
   10 IF (M.GE.1 .AND. N.GE.1) GO TO 20
      IERR = -2
      RETURN
   20 IF (NA.GE.M .AND. NU.GE.M .AND. NB.GE.M) GO TO 30
      IERR = -3
      RETURN
   30 IF (NV.GE.N) GO TO 40
      IERR = -4
      RETURN
   40 IF (NZ.GE.MIN0(M,N)) GO TO 50
      IERR = -5
      RETURN
   50 CONTINUE
C
C     FIRST COPIES A INTO EITHER U OR V ACCORDING TO WHETHER
C     M .GE. N OR M .LT. N, AND THEN CALLS SUBROUTINE MGNSVD
C     WHICH ASSUMES THAT NUMBER OF ROWS .GE. NUMBER OF COLUMNS.
C
      IF (M.LT.N) GO TO 80
C
C       M .GE. N  CASE.
C
      DO 70 I=1,M
        DO 60 J=1,N
          U(I,J) = A(I,J)
   60   CONTINUE
   70 CONTINUE
C
      CALL MGNSVD(NU, NV, NZ, NB, M, N, W, MATU, U, MATV, V, Z, B,
     * IRHS, IERR, RV1)
      RETURN
C
   80 CONTINUE
C                              T
C       M .LT. N CASE. COPIES A  INTO V.
C
      DO 100 I=1,M
        DO 90 J=1,N
          V(J,I) = A(I,J)
   90   CONTINUE
  100 CONTINUE
      CALL MGNSVD(NV, NU, NZ, NB, N, M, W, MATV, V, MATU, U, Z, B, 0,
     * IERR, RV1)
      RETURN
      END
C                                                                       MGN   10
      SUBROUTINE MGNSVD(NU, NV, NZ, NB, M, N, W, MATU, U, MATV, V, Z,   MGN   20
     * B, IRHS, IERR, RV1)
C
C     THE DESCRIPTION OF SUBROUTINE MGNSVD IS ALMOST IDENTICAL
C     TO THAT FOR SUBROUTINE HYBSVD ABOVE, WITH THE EXCEPTION
C     THAT MGNSVD ASSUMES M .GE. N.
C     IT ALSO ASSUMES THAT A COPY OF THE MATRIX A IS IN THE ARRAY U.
C
      INTEGER NU, NV, NZ, M, N, IRHS, IERR, IP1, I, J, K, IM1, IBACK
      REAL*8 W(1), U(NU,1), V(NV,1), Z(NZ,1), B(NB,IRHS), RV1(1)
      REAL*8 XOVRPT, C, R, G, SCALE, DSIGN, DABS, DSQRT, F, S, H
      REAL*8 DFLOAT
      LOGICAL MATU, MATV
C
C     SET VALUE FOR C. THE VALUE FOR C DEPENDS ON THE RELATIVE
C     EFFICIENCY OF FLOATING POINT MULTIPLICATIONS, FLOATING POINT
C     ADDITIONS AND TWO-DIMENSIONAL ARRAY INDEXINGS ON THE
C     COMPUTER WHERE THIS SUBROUTINE IS TO BE RUN.  C SHOULD
C     USUALLY BE BETWEEN 2 AND 4.  FOR DETAILS ON CHOOSING C, SEE
C     (2).  THE ALGORITHM IS NOT SENSITIVE TO THE VALUE OF C
C     ACTUALLY USED AS LONG AS C IS BETWEEN 2 AND 4.
C
      C = 4.d0
C
C     DETERMINE CROSS-OVER POINT
C
      IF (MATU .AND. MATV) XOVRPT = (C+7.d0/3.d0)/C
      IF (MATU .AND. .NOT.MATV) XOVRPT = (C+7.d0/3.d0)/C
      IF (.NOT.MATU .AND. MATV) XOVRPT = 5.d0/3.d0
      IF (.NOT.MATU .AND. .NOT.MATV) XOVRPT = 5.d0/3.d0
C
C     DETERMINE WHETHER TO USE GOLUB-REINSCH OR THE MODIFIED
C     ALGORITHM.
C
      R = DFLOAT(M)/DFLOAT(N)
      IF (R.GE.XOVRPT) GO TO 10
C
C     USE GOLUB-REINSCH PROCEDURE
C
      CALL GRSVD(NU, NV, NB, M, N, W, MATU, U, MATV, V, B, IRHS, IERR,
     * RV1)
      GO TO 330
C
C     USE MODIFIED ALGORITHM
C
   10 CONTINUE
C
C     TRIANGULARIZE U BY HOUSEHOLDER TRANSFORMATIONS, USING
C     W AND RV1 AS TEMPORARY STORAGE.
C
      DO 110 I=1,N
        G = 0.d0
        S = 0.d0
        SCALE = 0.d0
C
C         PERFORM SCALING OF COLUMNS TO AVOID UNNECSSARY OVERFLOW
C         OR UNDERFLOW
C
        DO 20 K=I,M
          SCALE = SCALE + DABS(U(K,I))
   20   CONTINUE
        IF (SCALE.EQ.0.d0) GO TO 110
        DO 30 K=I,M
          U(K,I) = U(K,I)/SCALE
          S = S + U(K,I)*U(K,I)
   30   CONTINUE
C
C         THE VECTOR E OF THE HOUSEHOLDER TRANSFORMATION I + EE'/H
C         WILL BE STORED IN COLUMN I OF U. THE TRANSFORMED ELEMENT
C         U(I,I) WILL BE STORED IN W(I) AND THE SCALAR H IN
C         RV1(I).
C
        F = U(I,I)
        G = -DSIGN(DSQRT(S),F)
        H = F*G - S
        U(I,I) = F - G
        RV1(I) = H
        W(I) = SCALE*G
C
        IF (I.EQ.N) GO TO 70
C
C         APPLY TRANSFORMATIONS TO REMAINING COLUMNS OF A
C
        IP1 = I + 1
        DO 60 J=IP1,N
          S = 0.d0
          DO 40 K=I,M
            S = S + U(K,I)*U(K,J)
   40     CONTINUE
          F = S/H
          DO 50 K=I,M
            U(K,J) = U(K,J) + F*U(K,I)
   50     CONTINUE
   60   CONTINUE
C
C         APPLY TRANSFORMATIONS TO COLUMNS OF B IF IRHS .GT. 0
C
   70   IF (IRHS.EQ.0) GO TO 110
        DO 100 J=1,IRHS
          S = 0.d0
          DO 80 K=I,M
            S = S + U(K,I)*B(K,J)
   80     CONTINUE
          F = S/H
          DO 90 K=I,M
            B(K,J) = B(K,J) + F*U(K,I)
   90     CONTINUE
  100   CONTINUE
  110 CONTINUE
C
C     COPY R INTO Z IF MATU = .TRUE.
C
      IF (.NOT.MATU) GO TO 290
      DO 130 I=1,N
        DO 120 J=I,N
          Z(J,I) = 0.d0
          Z(I,J) = U(I,J)
  120   CONTINUE
        Z(I,I) = W(I)
  130 CONTINUE
C
C     ACCUMULATE HOUSEHOLDER TRANSFORMATIONS IN U
C
      DO 240 IBACK=1,N
        I = N - IBACK + 1
        IP1 = I + 1
        G = W(I)
        H = RV1(I)
        IF (I.EQ.N) GO TO 150
C
        DO 140 J=IP1,N
          U(I,J) = 0.d0
  140   CONTINUE
C
  150   IF (H.EQ.0.d0) GO TO 210
        IF (I.EQ.N) GO TO 190
C
        DO 180 J=IP1,N
          S = 0.d0
          DO 160 K=IP1,M
            S = S + U(K,I)*U(K,J)
  160     CONTINUE
          F = S/H
          DO 170 K=I,M
            U(K,J) = U(K,J) + F*U(K,I)
  170     CONTINUE
  180   CONTINUE
C
  190   S = U(I,I)/H
        DO 200 J=I,M
          U(J,I) = U(J,I)*S
  200   CONTINUE
        GO TO 230
C
  210   DO 220 J=I,M
          U(J,I) = 0.d0
  220   CONTINUE
  230   U(I,I) = U(I,I) + 1.d0
  240 CONTINUE
C
C     COMPUTE SVD OF R (WHICH IS STORED IN Z)
C
      CALL GRSVD(NZ, NV, NB, N, N, W, MATU, Z, MATV, V, B, IRHS, IERR,
     * RV1)
C
C                                      T
C     FORM L*X TO OBTAIN U (WHERE R=XWY ). X IS RETURNED IN Z
C     BY GRSVD. THE MATRIX MULTIPLY IS DONE ONE ROW AT A TIME,
C     USING RV1 AS SCRATCH SPACE.
C
      DO 280 I=1,M
        DO 260 J=1,N
          S = 0.d0
          DO 250 K=1,N
            S = S + U(I,K)*Z(K,J)
  250     CONTINUE
          RV1(J) = S
  260   CONTINUE
        DO 270 J=1,N
          U(I,J) = RV1(J)
  270   CONTINUE
  280 CONTINUE
      GO TO 330
C
C     FORM R IN U BY ZEROING THE LOWER TRIANGULAR PART OF R IN U
C
  290 IF (N.EQ.1) GO TO 320
      DO 310 I=2,N
        IM1 = I - 1
        DO 300 J=1,IM1
          U(I,J) = 0.d0
  300   CONTINUE
        U(I,I) = W(I)
  310 CONTINUE
  320 U(1,1) = W(1)
C
      CALL GRSVD(NU, NV, NB, N, N, W, MATU, U, MATV, V, B, IRHS, IERR,
     * RV1)
  330 CONTINUE
      IERRP1 = IERR + 1
      IF (IERR.LT.0 .OR. N.LE.1 .OR. IERRP1.EQ.N) RETURN
C
C     SORT SINGULAR VALUES AND EXCHANGE COLUMNS OF U AND V ACCORDINGLY.
C     SELECTION SORT MINIMIZES SWAPPING OF U AND V.
C
      NM1 = N - 1
      DO 360 I=IERRP1,NM1
C...    FIND INDEX OF MAXIMUM SINGULAR VALUE
        ID = I
        IP1 = I + 1
        DO 340 J=IP1,N
          IF (W(J).GT.W(ID)) ID = J
  340   CONTINUE
        IF (ID.EQ.I) GO TO 360
C...    SWAP SINGULAR VALUES AND VECTORS
        T = W(I)
        W(I) = W(ID)
        W(ID) = T
        IF (MATV) CALL DSWAP(N, V(1,I), 1, V(1,ID), 1)
        IF (MATU) CALL DSWAP(M, U(1,I), 1, U(1,ID), 1)
        IF (IRHS.LT.1) GO TO 360
        DO 350 KRHS=1,IRHS
          T = B(I,KRHS)
          B(I,KRHS) = B(ID,KRHS)
          B(ID,KRHS) = T
  350   CONTINUE
  360 CONTINUE
      RETURN
C     ************** LAST CARD OF HYBSVD *****************
      END
      SUBROUTINE GRSVD(NU, NV, NB, M, N, W, MATU, U, MATV, V, B, IRHS,  GRS   10
     * IERR, RV1)
C
      INTEGER I, J, K, L, M, N, II, I1, KK, K1, LL, L1, MN, NU, NV, NB,
     * ITS, IERR, IRHS
      REAL*8 W(1), U(NU,1), V(NV,1), B(NB,IRHS), RV1(1)
      REAL*8 C, F, G, H, S, X, Y, Z, EPS, SCALE, SRELPR, DUMMY
      REAL*8 DSQRT, DMAX1, DABS, DSIGN
      LOGICAL MATU, MATV
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE SVD,
C     NUM. MATH. 14, 403-420(1970) BY GOLUB AND REINSCH.
C     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).
C
C     THIS SUBROUTINE DETERMINES THE SINGULAR VALUE DECOMPOSITION
C          T
C     A=USV  OF A REAL M BY N RECTANGULAR MATRIX.  HOUSEHOLDER
C     BIDIAGONALIZATION AND A VARIANT OF THE QR ALGORITHM ARE USED.
C     GRSVD ASSUMES THAT A COPY OF THE MATRIX A IS IN THE ARRAY U. IT
C     ALSO ASSUMES M .GE. N.  IF M .LT. N, THEN COMPUTE THE SINGULAR
C                             T       T    T             T
C     VALUE DECOMPOSITION OF A .  IF A =UWV  , THEN A=VWU  .
C
C     GRSVD CAN ALSO BE USED TO COMPUTE THE MINIMAL LENGTH LEAST SQUARES
C     SOLUTION TO THE OVERDETERMINED LINEAR SYSTEM A*X=B.
C
C     ON INPUT-
C
C        NU MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS U AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.  NOTE THAT NU MUST BE AT LEAST
C          AS LARGE AS M,
C
C        NV MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C          ARRAY PARAMETER V AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.  NV MUST BE AT LEAST AS LARGE AS N,
C
C        NB MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS B AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.  NOTE THAT NB MUST BE AT LEAST
C          AS LARGE AS M,
C
C        M IS THE NUMBER OF ROWS OF A (AND U),
C
C        N IS THE NUMBER OF COLUMNS OF A (AND U) AND THE ORDER OF V,
C
C        A CONTAINS THE RECTANGULAR INPUT MATRIX TO BE DECOMPOSED,
C
C        B CONTAINS THE IRHS RIGHT-HAND-SIDES OF THE OVERDETERMINED
C          LINEAR SYSTEM A*X=B.  IF IRHS .GT. 0,  THEN ON OUTPUT,
C          THE FIRST N COMPONENTS OF THESE IRHS COLUMNS OF B
C                        T
C          WILL CONTAIN U B.  THUS, TO COMPUTE THE MINIMAL LENGTH LEAST
C                                                +
C          SQUARES SOLUTION, ONE MUST COMPUTE V*W  TIMES THE COLUMNS OF
C                    +                        +
C          B, WHERE W  IS A DIAGONAL MATRIX, W (I)=0 IF W(I) IS
C          NEGLIGIBLE, OTHERWISE IS 1/W(I).  IF IRHS=0, B MAY COINCIDE
C          WITH A OR U AND WILL NOT BE REFERENCED,
C
C        IRHS IS THE NUMBER OF RIGHT-HAND-SIDES OF THE OVERDETERMINED
C          SYSTEM A*X=B.  IRHS SHOULD BE SET TO ZERO IF ONLY THE SINGULA
C          VALUE DECOMPOSITION OF A IS DESIRED,
C
C        MATU SHOULD BE SET TO .TRUE. IF THE U MATRIX IN THE
C          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE,
C
C        MATV SHOULD BE SET TO .TRUE. IF THE V MATRIX IN THE
C          DECOMPOSITION IS DESIRED, AND TO .FALSE. OTHERWISE.
C
C     ON OUTPUT-
C
C        W CONTAINS THE N (NON-NEGATIVE) SINGULAR VALUES OF A (THE
C          DIAGONAL ELEMENTS OF S).  THEY ARE UNORDERED.  IF AN
C          ERROR EXIT IS MADE, THE SINGULAR VALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,IERR+2,...,N,
C
C        U CONTAINS THE MATRIX U (ORTHOGONAL COLUMN VECTORS) OF THE
C          DECOMPOSITION IF MATU HAS BEEN SET TO .TRUE.  OTHERWISE
C          U IS USED AS A TEMPORARY ARRAY.
C          IF AN ERROR EXIT IS MADE, THE COLUMNS OF U CORRESPONDING
C          TO INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT,
C
C        V CONTAINS THE MATRIX V (ORTHOGONAL) OF THE DECOMPOSITION IF
C          MATV HAS BEEN SET TO .TRUE.  OTHERWISE V IS NOT REFERENCED.
C          IF AN ERROR EXIT IS MADE, THE COLUMNS OF V CORRESPONDING TO
C          INDICES OF CORRECT SINGULAR VALUES SHOULD BE CORRECT,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          K          IF THE K-TH SINGULAR VALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS,
C          -1         IF IRHS .LT. 0 ,
C          -2         IF M .LT. N ,
C          -3         IF NU .LT. M .OR. NB .LT. M,
C          -4         IF NV .LT. N .
C
C        RV1 IS A TEMPORARY STORAGE ARRAY.
C
C        THIS SUBROUTINE HAS BEEN CHECKED BY THE PFORT VERIFIER
C        (RYDER, B.G. 'THE PFORT VERIFIER', SOFTWARE - PRACTICE AND
C        EXPERIENCE, VOL.4, 359-377, 1974) FOR ADHERENCE TO A LARGE,
C        CAREFULLY DEFINED, PORTABLE SUBSET OF AMERICAN NATIONAL STANDAR
C        FORTRAN CALLED PFORT.
C
C        ORIGINAL VERSION OF THIS CODE IS SUBROUTINE SVD IN RELEASE 2 OF
C        EISPACK.
C
C        MODIFIED BY TONY F. CHAN,
C                    COMP. SCI. DEPT, YALE UNIV.,
C                    BOX 2158, YALE STATION,
C                    CT 06520
C        LAST MODIFIED : JANUARY, 1982.
C
C     ------------------------------------------------------------------
C
C     ********** SRELPR IS A MACHINE-DEPENDENT FUNCTION SPECIFYING
C                THE RELATIVE PRECISION OF FLOATING POINT ARITHMETIC.
C
C                **********
C
      IERR = 0
      IF (IRHS.GE.0) GO TO 10
      IERR = -1
      RETURN
   10 IF (M.GE.N) GO TO 20
      IERR = -2
      RETURN
   20 IF (NU.GE.M .AND. NB.GE.M) GO TO 30
      IERR = -3
      RETURN
   30 IF (NV.GE.N) GO TO 40
      IERR = -4
      RETURN
   40 CONTINUE
C
C     ********** HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM **********
      G = 0.d0
      SCALE = 0.d0
      X = 0.d0
C
      DO 260 I=1,N
        L = I + 1
        RV1(I) = SCALE*G
        G = 0.d0
        S = 0.d0
        SCALE = 0.d0
C
C     COMPUTE LEFT TRANSFORMATIONS THAT ZERO THE SUBDIAGONAL ELEMENTS
C     OF THE I-TH COLUMN.
C
        DO 50 K=I,M
          SCALE = SCALE + DABS(U(K,I))
   50   CONTINUE
C
        IF (SCALE.EQ.0.d0) GO TO 160
C
        DO 60 K=I,M
          U(K,I) = U(K,I)/SCALE
          S = S + U(K,I)**2
   60   CONTINUE
C
        F = U(I,I)
        G = -DSIGN(DSQRT(S),F)
        H = F*G - S
        U(I,I) = F - G
        IF (I.EQ.N) GO TO 100
C
C     APPLY LEFT TRANSFORMATIONS TO REMAINING COLUMNS OF A.
C
        DO 90 J=L,N
          S = 0.d0
C
          DO 70 K=I,M
            S = S + U(K,I)*U(K,J)
   70     CONTINUE
C
          F = S/H
C
          DO 80 K=I,M
            U(K,J) = U(K,J) + F*U(K,I)
   80     CONTINUE
   90   CONTINUE
C
C      APPLY LEFT TRANSFORMATIONS TO THE COLUMNS OF B IF IRHS .GT. 0
C
  100   IF (IRHS.EQ.0) GO TO 140
        DO 130 J=1,IRHS
          S = 0.d0
          DO 110 K=I,M
            S = S + U(K,I)*B(K,J)
  110     CONTINUE
          F = S/H
          DO 120 K=I,M
            B(K,J) = B(K,J) + F*U(K,I)
  120     CONTINUE
  130   CONTINUE
C
C     COMPUTE RIGHT TRANSFORMATIONS.
C
  140   DO 150 K=I,M
          U(K,I) = SCALE*U(K,I)
  150   CONTINUE
C
  160   W(I) = SCALE*G
        G = 0.d0
        S = 0.d0
        SCALE = 0.d0
        IF (I.GT.M .OR. I.EQ.N) GO TO 250
C
        DO 170 K=L,N
          SCALE = SCALE + DABS(U(I,K))
  170   CONTINUE
C
        IF (SCALE.EQ.0.d0) GO TO 250
C
        DO 180 K=L,N
          U(I,K) = U(I,K)/SCALE
          S = S + U(I,K)**2
  180   CONTINUE
C
        F = U(I,L)
        G = -DSIGN(DSQRT(S),F)
        H = F*G - S
        U(I,L) = F - G
C
        DO 190 K=L,N
          RV1(K) = U(I,K)/H
  190   CONTINUE
C
        IF (I.EQ.M) GO TO 230
C
        DO 220 J=L,M
          S = 0.d0
C
          DO 200 K=L,N
            S = S + U(J,K)*U(I,K)
  200     CONTINUE
C
          DO 210 K=L,N
            U(J,K) = U(J,K) + S*RV1(K)
  210     CONTINUE
  220   CONTINUE
C
  230   DO 240 K=L,N
          U(I,K) = SCALE*U(I,K)
  240   CONTINUE
C
  250   X = DMAX1(X,DABS(W(I))+DABS(RV1(I)))
  260 CONTINUE
C     ********** ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS **********
      IF (.NOT.MATV) GO TO 350
C     ********** FOR I=N STEP -1 UNTIL 1 DO -- **********
      DO 340 II=1,N
        I = N + 1 - II
        IF (I.EQ.N) GO TO 330
        IF (G.EQ.0.d0) GO TO 310
C
        DO 270 J=L,N
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
          V(J,I) = (U(I,J)/U(I,L))/G
  270   CONTINUE
C
        DO 300 J=L,N
          S = 0.d0
C
          DO 280 K=L,N
            S = S + U(I,K)*V(K,J)
  280     CONTINUE
C
          DO 290 K=L,N
            V(K,J) = V(K,J) + S*V(K,I)
  290     CONTINUE
  300   CONTINUE
C
  310   DO 320 J=L,N
          V(I,J) = 0.d0
          V(J,I) = 0.d0
  320   CONTINUE
C
  330   V(I,I) = 1.d0
        G = RV1(I)
        L = I
  340 CONTINUE
C     ********** ACCUMULATION OF LEFT-HAND TRANSFORMATIONS **********
  350 IF (.NOT.MATU) GO TO 470
C     **********FOR I=MIN(M,N) STEP -1 UNTIL 1 DO -- **********
      MN = N
      IF (M.LT.N) MN = M
C
      DO 460 II=1,MN
        I = MN + 1 - II
        L = I + 1
        G = W(I)
        IF (I.EQ.N) GO TO 370
C
        DO 360 J=L,N
          U(I,J) = 0.d0
  360   CONTINUE
C
  370   IF (G.EQ.0.d0) GO TO 430
        IF (I.EQ.MN) GO TO 410
C
        DO 400 J=L,N
          S = 0.d0
C
          DO 380 K=L,M
            S = S + U(K,I)*U(K,J)
  380     CONTINUE
C     ********** DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
          F = (S/U(I,I))/G
C
          DO 390 K=I,M
            U(K,J) = U(K,J) + F*U(K,I)
  390     CONTINUE
  400   CONTINUE
C
  410   DO 420 J=I,M
          U(J,I) = U(J,I)/G
  420   CONTINUE
C
        GO TO 450
C
  430   DO 440 J=I,M
          U(J,I) = 0.d0
  440   CONTINUE
C
  450   U(I,I) = U(I,I) + 1.d0
  460 CONTINUE
C     ********** DIAGONALIZATION OF THE BIDIAGONAL FORM **********
  470 EPS = SRELPR(DUMMY)*X
C     ********** FOR K=N STEP -1 UNTIL 1 DO -- **********
      DO 650 KK=1,N
        K1 = N - KK
        K = K1 + 1
        ITS = 0
C     ********** TEST FOR SPLITTING.
C                FOR L=K STEP -1 UNTIL 1 DO -- **********
  480   DO 490 LL=1,K
          L1 = K - LL
          L = L1 + 1
          IF (DABS(RV1(L)).LE.EPS) GO TO 550
C     ********** RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP **********
          IF (DABS(W(L1)).LE.EPS) GO TO 500
  490   CONTINUE
C     ********** CANCELLATION OF RV1(L) IF L GREATER THAN 1 **********
  500   C = 0.d0
        S = 1.d0
C
        DO 540 I=L,K
          F = S*RV1(I)
          RV1(I) = C*RV1(I)
          IF (DABS(F).LE.EPS) GO TO 550
          G = W(I)
          H = DSQRT(F*F+G*G)
          W(I) = H
          C = G/H
          S = -F/H
C
C     APPLY LEFT TRANSFORMATIONS TO B IF IRHS .GT. 0
C
          IF (IRHS.EQ.0) GO TO 520
          DO 510 J=1,IRHS
            Y = B(L1,J)
            Z = B(I,J)
            B(L1,J) = Y*C + Z*S
            B(I,J) = -Y*S + Z*C
  510     CONTINUE
  520     CONTINUE
C
          IF (.NOT.MATU) GO TO 540
C
          DO 530 J=1,M
            Y = U(J,L1)
            Z = U(J,I)
            U(J,L1) = Y*C + Z*S
            U(J,I) = -Y*S + Z*C
  530     CONTINUE
C
  540   CONTINUE
C     ********** TEST FOR CONVERGENCE **********
  550   Z = W(K)
        IF (L.EQ.K) GO TO 630
C     ********** SHIFT FROM BOTTOM 2 BY 2 MINOR **********
        IF (ITS.EQ.30) GO TO 660
        ITS = ITS + 1
        X = W(L)
        Y = W(K1)
        G = RV1(K1)
        H = RV1(K)
        F = ((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.d0*H*Y)
        G = DSQRT(F*F+1.0)
        F = ((X-Z)*(X+Z)+H*(Y/(F+DSIGN(G,F))-H))/X
C     ********** NEXT QR TRANSFORMATION **********
        C = 1.0
        S = 1.0
C
        DO 620 I1=L,K1
          I = I1 + 1
          G = RV1(I)
          Y = W(I)
          H = S*G
          G = C*G
          Z = DSQRT(F*F+H*H)
          RV1(I1) = Z
          C = F/Z
          S = H/Z
          F = X*C + G*S
          G = -X*S + G*C
          H = Y*S
          Y = Y*C
          IF (.NOT.MATV) GO TO 570
C
          DO 560 J=1,N
            X = V(J,I1)
            Z = V(J,I)
            V(J,I1) = X*C + Z*S
            V(J,I) = -X*S + Z*C
  560     CONTINUE
C
  570     Z = DSQRT(F*F+H*H)
          W(I1) = Z
C     ********** ROTATION CAN BE ARBITRARY IF Z IS ZERO **********
          IF (Z.EQ.0.d0) GO TO 580
          C = F/Z
          S = H/Z
  580     F = C*G + S*Y
          X = -S*G + C*Y
C
C     APPLY LEFT TRANSFORMATIONS TO B IF IRHS .GT. 0
C
          IF (IRHS.EQ.0) GO TO 600
          DO 590 J=1,IRHS
            Y = B(I1,J)
            Z = B(I,J)
            B(I1,J) = Y*C + Z*S
            B(I,J) = -Y*S + Z*C
  590     CONTINUE
  600     CONTINUE
C
          IF (.NOT.MATU) GO TO 620
C
          DO 610 J=1,M
            Y = U(J,I1)
            Z = U(J,I)
            U(J,I1) = Y*C + Z*S
            U(J,I) = -Y*S + Z*C
  610     CONTINUE
C
  620   CONTINUE
C
        RV1(L) = 0.d0
        RV1(K) = F
        W(K) = X
        GO TO 480
C     ********** CONVERGENCE **********
  630   IF (Z.GE.0.d0) GO TO 650
C     ********** W(K) IS MADE NON-NEGATIVE **********
        W(K) = -Z
        IF (.NOT.MATV) GO TO 650
C
        DO 640 J=1,N
          V(J,K) = -V(J,K)
  640   CONTINUE
C
  650 CONTINUE
C
      GO TO 670
C     ********** SET ERROR -- NO CONVERGENCE TO A
C                SINGULAR VALUE AFTER 30 ITERATIONS **********
  660 IERR = K
  670 RETURN
C     ********** LAST CARD OF GRSVD **********
      END
c      SUBROUTINE DSWAP(N, SX, INCX, SY, INCY)                           SSW   10
cC
cC     INTERCHANGES TWO VECTORS.
cC     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.
cC     JACK DONGARRA, LINPACK, 3/11/78.
cC
c      REAL*8 SX(1), SY(1), STEMP
c      INTEGER I, INCX, INCY, IX, IY, M, MP1, N
cC
c      IF (N.LE.0) RETURN
c      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
cC
cC       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
cC         TO 1
cC
c      IX = 1
c      IY = 1
c      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
c      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
c      DO 10 I=1,N
c        STEMP = SX(IX)
c        SX(IX) = SY(IY)
c        SY(IY) = STEMP
c        IX = IX + INCX
c        IY = IY + INCY
c   10 CONTINUE
c      RETURN
cC
cC       CODE FOR BOTH INCREMENTS EQUAL TO 1
cC
cC
cC       CLEAN-UP LOOP
cC
c   20 M = MOD(N,3)
c      IF (M.EQ.0) GO TO 40
c      DO 30 I=1,M
c        STEMP = SX(I)
c        SX(I) = SY(I)
c        SY(I) = STEMP
c   30 CONTINUE
c      IF (N.LT.3) RETURN
c   40 MP1 = M + 1
c      DO 50 I=MP1,N,3
c        STEMP = SX(I)
c        SX(I) = SY(I)
c        SY(I) = STEMP
c        STEMP = SX(I+1)
c        SX(I+1) = SY(I+1)
c        SY(I+1) = STEMP
c        STEMP = SX(I+2)
c        SX(I+2) = SY(I+2)
c        SY(I+2) = STEMP
c   50 CONTINUE
c      RETURN
c      END
      REAL*8 FUNCTION SRELPR(DUMMY)                                       SRE   10
      REAL*8 DUMMY
C
C     SRELPR COMPUTES THE RELATIVE PRECISION OF THE FLOATING POINT
C     ARITHMETIC OF THE MACHINE.
C
C     IF TROUBLE WITH AUTOMATIC COMPUTATION OF THESE QUANTITIES,
C     THEY CAN BE SET BY DIRECT ASSIGNMENT STATEMENTS.
C     ASSUME THE COMPUTER HAS
C
C        B = BASE OF ARITHMETIC
C        T = NUMBER OF BASE  B  DIGITS
C
C     THEN
C
C        SRELPR = B**(1-T)
C
      REAL*8 S
C
      SRELPR = 1.d0
   10 SRELPR = SRELPR/2.d0
      S = 1.d0 + SRELPR
      IF (S.GT.1.d0) GO TO 10
      SRELPR = 2.d0*SRELPR
      RETURN
      END
