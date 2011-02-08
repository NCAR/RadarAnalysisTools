c
c----------------------------------------------------------------------X
c
      SUBROUTINE USTOPO(DAT,IOUT,C1,C2,C3,C4,NSCTP,BDVAL,MNGATE,MXGATE,
     X     MANG,RNG,AZA,ELA,AZROT,ITPOLD,MXR,MXA,MXF,X0,Y0,H0,NGRD,
     X     FXOLD,OLAT,OLON)
C
C     Note: Modified from the original version (topint.f within GEMINI 
C           and adapted to getting topographic values at radar sampling
C           locations of range and azimuth.
C
C     THIS ROUTINE ACCESSES A TOPO DATABASE ON THE MASS STORE AND
C     INTERPOLATES THE ELEVATIONS OF THE TOPO DATA TO EACH OUTPUT
C     GRID POINT. CURRENTLY, THE DATASET ACCESSED IS ONLY FOR THE
C     CONTINENTAL US AND HAS A RESOLUTION OF 30 SECONDS.
C
C     C1,C2     - Origin latitude and longitude
C     C3        - First range (km) for calculations
C     C4        - If printing out values, do so for fixed angle .le. C4
C     NGRD      - 'REG' output at the regular range-azimuth grid
C                 '   ' output at the sampling range-azimuth locations
C     RNG       - Distance (km) to the range gates
C     AZA       - Angle which varies during the scan (azimuth or elevation)
C     FXOLD     - Fixed angle during scan.
C     ITPOLD    - Scanning mode [RHI (3): AZA contains elevation angle;
C                              otherwise, AZA contains azimuth angle]
C     X0,Y0,HO  - (X,Y,Z) COORDINATES FOR THIS RADAR
C     AZROT     - Array of angle corrections (usually all 0s)
C     CBUF      - Array to hold ASCII record
C
      DIMENSION DAT(MXR,MXA,MXF)
      DIMENSION RNG(MXR,2),AZA(MXA,2),ELA(MXA,2)
      DIMENSION IARRAY(29,71),ITOP(121,121),TOP(121,121)
      DIMENSION AZROT(8)
      CHARACTER*3 NGRD
      CHARACTER*8 NSCTP
      CHARACTER*59100 CBUF
      LOGICAL PRINT

      DATA RE,REI/17000.0,1.17647E-04/
      DATA TORAD,TODEG/0.017453293,57.29577951/

      IF(C1.NE.0.0)THEN
         ORLAT=C1
      ELSE
         ORLAT=OLAT
      END IF
      IF(C2.NE.0.0)THEN
         ORLON=C2
      ELSE
         ORLON=OLON
      END IF
 
      FRST_RNG=C3
      IF(FRST_RNG.LE.0.0)FRST_RNG=1.0
      GRIDANG=90.0-AZROT(ITPOLD)
      LSTREC=-1
      FXMAX=C3

      PRINT=.TRUE.
      IF(NSCTP(1:4).EQ.'PRNT'.AND.FXOLD.LE.FXMAX)THEN
         PRINT=.TRUE.
         READ(NSCTP(5:8),11)IMOD,JMOD
 11      FORMAT(2I2)
         IF(IMOD.EQ.0)IMOD=9999
         IF(JMOD.EQ.0)JMOD=9999
      END IF

      IF(PRINT)THEN
         PRINT *,'USTOPO - ',NGRD,' ',ORLAT,ORLON,INT(GRIDANG),
     +        INT(FRST_RNG),NSCTP
      END IF

      DO 7 J=1,29
         DO 5 I=1,71
            IARRAY(I,J)=-1
 5       CONTINUE
 7    CONTINUE
C     
C     THE FOLLOWING ASSIGNMENTS MATCH (LAT,LON) WITH CORRESPONDING
C     RECORD NUMBERS IN THE TOPO DATABASE. EACH RECORD CONTAINS DATA
C     FOR A 1 DEGREE BY 1 DEGREE SQUARE (121 POINTS IN SQUARE).
C
      IARRAY(27, 3)=1018
      IARRAY(28, 3)=1062
      IARRAY(27, 4)=1019
      IARRAY(28, 4)=1063
      IARRAY(26, 5)= 974
      IARRAY(27, 5)=1020
      IARRAY(28, 5)=1064
      IARRAY(18, 6)= 514
      IARRAY(19, 6)= 567
      IARRAY(20, 6)= 623
      IARRAY(21, 6)= 678
      IARRAY(22, 6)= 738
      IARRAY(23, 6)= 801
      IARRAY(24, 6)= 866
      IARRAY(25, 6)= 923
      IARRAY(26, 6)= 975
      IARRAY(27, 6)=1021
      IARRAY(28, 6)=1065
      IARRAY(15, 7)= 365
      IARRAY(16, 7)= 414
      IARRAY(17, 7)= 464
      IARRAY(18, 7)= 515
      IARRAY(19, 7)= 568
      IARRAY(20, 7)= 624
      IARRAY(21, 7)= 679
      IARRAY(22, 7)= 739
      IARRAY(23, 7)= 802
      IARRAY(24, 7)= 867
      IARRAY(25, 7)= 924
      IARRAY(26, 7)= 976
      IARRAY(27, 7)=1022
      IARRAY(28, 7)=1066
      IARRAY(14, 8)= 317
      IARRAY(15, 8)= 366
      IARRAY(16, 8)= 415
      IARRAY(17, 8)= 465
      IARRAY(18, 8)= 516
      IARRAY(19, 8)= 569
      IARRAY(20, 8)= 625
      IARRAY(21, 8)= 680
      IARRAY(22, 8)= 740
      IARRAY(23, 8)= 803
      IARRAY(24, 8)= 868
      IARRAY(25, 8)= 925
      IARRAY(26, 8)= 977
      IARRAY(27, 8)=1023
      IARRAY(28, 8)=1067
      IARRAY(13, 9)= 270
      IARRAY(14, 9)= 318
      IARRAY(15, 9)= 367
      IARRAY(16, 9)= 416
      IARRAY(17, 9)= 466
      IARRAY(18, 9)= 517
      IARRAY(19, 9)= 570
      IARRAY(20, 9)= 626
      IARRAY(21, 9)= 681
      IARRAY(22, 9)= 741
      IARRAY(23, 9)= 804
      IARRAY(24, 9)= 869
      IARRAY(25, 9)= 926
      IARRAY(26, 9)= 978
      IARRAY(27, 9)=1024
      IARRAY(28, 9)=1068
      IARRAY(11,10)= 181
      IARRAY(12,10)= 225
      IARRAY(13,10)= 271
      IARRAY(14,10)= 319
      IARRAY(15,10)= 368
      IARRAY(16,10)= 417
      IARRAY(17,10)= 467
      IARRAY(18,10)= 518
      IARRAY(19,10)= 571
      IARRAY(20,10)= 627
      IARRAY(21,10)= 682
      IARRAY(22,10)= 742
      IARRAY(23,10)= 805
      IARRAY(24,10)= 870
      IARRAY(25,10)= 927
      IARRAY(26,10)= 979
      IARRAY(27,10)=1025
      IARRAY(28,10)=1069
      IARRAY(11,11)= 182
      IARRAY(12,11)= 226
      IARRAY(13,11)= 272
      IARRAY(14,11)= 320
      IARRAY(15,11)= 369
      IARRAY(16,11)= 418
      IARRAY(17,11)= 468
      IARRAY(18,11)= 519
      IARRAY(19,11)= 572
      IARRAY(20,11)= 628
      IARRAY(21,11)= 683
      IARRAY(22,11)= 743
      IARRAY(23,11)= 806
      IARRAY(24,11)= 871
      IARRAY(25,11)= 928
      IARRAY(26,11)= 980
      IARRAY(27,11)=1026
      IARRAY(10,12)= 141
      IARRAY(11,12)= 183
      IARRAY(12,12)= 227
      IARRAY(13,12)= 273
      IARRAY(14,12)= 321
      IARRAY(15,12)= 370
      IARRAY(16,12)= 419
      IARRAY(17,12)= 469
      IARRAY(18,12)= 520
      IARRAY(19,12)= 573
      IARRAY(20,12)= 629
      IARRAY(21,12)= 684
      IARRAY(22,12)= 744
      IARRAY(23,12)= 807
      IARRAY(24,12)= 872
      IARRAY(25,12)= 929
      IARRAY(26,12)= 981
      IARRAY(27,12)=1027
      IARRAY(10,13)= 142
      IARRAY(11,13)= 184
      IARRAY(12,13)= 228
      IARRAY(13,13)= 274
      IARRAY(14,13)= 322
      IARRAY(15,13)= 371
      IARRAY(16,13)= 420
      IARRAY(17,13)= 470
      IARRAY(18,13)= 521
      IARRAY(19,13)= 574
      IARRAY(20,13)= 630
      IARRAY(21,13)= 685
      IARRAY(22,13)= 745
      IARRAY(23,13)= 808
      IARRAY(24,13)= 873
      IARRAY(25,13)= 930
      IARRAY(26,13)= 982
      IARRAY(27,13)=1028
      IARRAY(28,13)=1070
      IARRAY( 8,14)=  68
      IARRAY( 9,14)= 104
      IARRAY(10,14)= 143
      IARRAY(11,14)= 185
      IARRAY(12,14)= 229
      IARRAY(13,14)= 275
      IARRAY(14,14)= 323
      IARRAY(15,14)= 372
      IARRAY(16,14)= 421
      IARRAY(17,14)= 471
      IARRAY(18,14)= 522
      IARRAY(19,14)= 575
      IARRAY(20,14)= 631
      IARRAY(21,14)= 686
      IARRAY(22,14)= 746
      IARRAY(23,14)= 809
      IARRAY(24,14)= 874
      IARRAY(25,14)= 931
      IARRAY(26,14)= 983
      IARRAY(27,14)=1029
      IARRAY(28,14)=1071
      IARRAY( 8,15)=  69
      IARRAY( 9,15)= 105
      IARRAY(10,15)= 144
      IARRAY(11,15)= 186
      IARRAY(12,15)= 230
      IARRAY(13,15)= 276
      IARRAY(14,15)= 324
      IARRAY(15,15)= 373
      IARRAY(16,15)= 422
      IARRAY(17,15)= 472
      IARRAY(18,15)= 523
      IARRAY(19,15)= 576
      IARRAY(20,15)= 632
      IARRAY(21,15)= 687
      IARRAY(22,15)= 747
      IARRAY(23,15)= 810
      IARRAY(24,15)= 875
      IARRAY(25,15)= 932
      IARRAY(26,15)= 984
      IARRAY(27,15)=1030
      IARRAY(28,15)=1072
      IARRAY( 8,16)=  70
      IARRAY( 9,16)= 106
      IARRAY(10,16)= 145
      IARRAY(11,16)= 187
      IARRAY(12,16)= 231
      IARRAY(13,16)= 277
      IARRAY(14,16)= 325
      IARRAY(15,16)= 374
      IARRAY(16,16)= 423
      IARRAY(17,16)= 473
      IARRAY(18,16)= 524
      IARRAY(19,16)= 577
      IARRAY(20,16)= 633
      IARRAY(21,16)= 688
      IARRAY(22,16)= 748
      IARRAY(23,16)= 811
      IARRAY(24,16)= 876
      IARRAY(25,16)= 933
      IARRAY(26,16)= 985
      IARRAY(27,16)=1031
      IARRAY(28,16)=1073
      IARRAY( 8,17)=  71
      IARRAY( 9,17)= 107
      IARRAY(10,17)= 146
      IARRAY(11,17)= 188
      IARRAY(12,17)= 232
      IARRAY(13,17)= 278
      IARRAY(14,17)= 326
      IARRAY(15,17)= 375
      IARRAY(16,17)= 424
      IARRAY(17,17)= 474
      IARRAY(18,17)= 525
      IARRAY(19,17)= 578
      IARRAY(20,17)= 634
      IARRAY(21,17)= 689
      IARRAY(22,17)= 749
      IARRAY(23,17)= 812
      IARRAY(24,17)= 877
      IARRAY(25,17)= 934
      IARRAY(26,17)= 986
      IARRAY(27,17)=1032
      IARRAY(28,17)=1074
      IARRAY( 8,18)=  72
      IARRAY( 9,18)= 108
      IARRAY(10,18)= 147
      IARRAY(11,18)= 189
      IARRAY(12,18)= 233
      IARRAY(13,18)= 279
      IARRAY(14,18)= 327
      IARRAY(15,18)= 376
      IARRAY(16,18)= 425
      IARRAY(17,18)= 475
      IARRAY(18,18)= 526
      IARRAY(19,18)= 579
      IARRAY(20,18)= 635
      IARRAY(21,18)= 690
      IARRAY(22,18)= 750
      IARRAY(23,18)= 813
      IARRAY(24,18)= 878
      IARRAY(25,18)= 935
      IARRAY(26,18)= 987
      IARRAY(27,18)=1033
      IARRAY(28,18)=1075
      IARRAY( 8,19)=  73
      IARRAY( 9,19)= 109
      IARRAY(10,19)= 148
      IARRAY(11,19)= 190
      IARRAY(12,19)= 234
      IARRAY(13,19)= 280
      IARRAY(14,19)= 328
      IARRAY(15,19)= 377
      IARRAY(16,19)= 426
      IARRAY(17,19)= 476
      IARRAY(18,19)= 527
      IARRAY(19,19)= 580
      IARRAY(20,19)= 636
      IARRAY(21,19)= 691
      IARRAY(22,19)= 751
      IARRAY(23,19)= 814
      IARRAY(24,19)= 879
      IARRAY(25,19)= 936
      IARRAY(26,19)= 988
      IARRAY(27,19)=1034
      IARRAY(28,19)=1076
      IARRAY( 8,20)=  74
      IARRAY( 9,20)= 110
      IARRAY(10,20)= 149
      IARRAY(11,20)= 191
      IARRAY(12,20)= 235
      IARRAY(13,20)= 281
      IARRAY(14,20)= 329
      IARRAY(15,20)= 378
      IARRAY(16,20)= 427
      IARRAY(17,20)= 477
      IARRAY(18,20)= 528
      IARRAY(19,20)= 581
      IARRAY(20,20)= 637
      IARRAY(21,20)= 692
      IARRAY(22,20)= 752
      IARRAY(23,20)= 815
      IARRAY(24,20)= 880
      IARRAY(25,20)= 937
      IARRAY(26,20)= 989
      IARRAY(27,20)=1035
      IARRAY(28,20)=1077
      IARRAY( 8,21)=  75
      IARRAY( 9,21)= 111
      IARRAY(10,21)= 150
      IARRAY(11,21)= 192
      IARRAY(12,21)= 236
      IARRAY(13,21)= 282
      IARRAY(14,21)= 330
      IARRAY(15,21)= 379
      IARRAY(16,21)= 428
      IARRAY(17,21)= 478
      IARRAY(18,21)= 529
      IARRAY(19,21)= 582
      IARRAY(20,21)= 638
      IARRAY(21,21)= 693
      IARRAY(22,21)= 753
      IARRAY(23,21)= 816
      IARRAY(24,21)= 881
      IARRAY(25,21)= 938
      IARRAY(26,21)= 990
      IARRAY(27,21)=1036
      IARRAY(28,21)=1078
      IARRAY( 8,22)=  76
      IARRAY( 9,22)= 112
      IARRAY(10,22)= 151
      IARRAY(11,22)= 193
      IARRAY(12,22)= 237
      IARRAY(13,22)= 283
      IARRAY(14,22)= 331
      IARRAY(15,22)= 380
      IARRAY(16,22)= 429
      IARRAY(17,22)= 479
      IARRAY(18,22)= 530
      IARRAY(19,22)= 583
      IARRAY(20,22)= 639
      IARRAY(21,22)= 694
      IARRAY(22,22)= 754
      IARRAY(23,22)= 817
      IARRAY(24,22)= 882
      IARRAY(25,22)= 939
      IARRAY(26,22)= 991
      IARRAY(27,22)=1037
      IARRAY(28,22)=1079
      IARRAY( 6,23)=  28
      IARRAY( 7,23)=  44
      IARRAY( 8,23)=  77
      IARRAY( 9,23)= 113
      IARRAY(10,23)= 152
      IARRAY(11,23)= 194
      IARRAY(12,23)= 238
      IARRAY(13,23)= 284
      IARRAY(14,23)= 332
      IARRAY(15,23)= 381
      IARRAY(16,23)= 430
      IARRAY(17,23)= 480
      IARRAY(18,23)= 531
      IARRAY(19,23)= 584
      IARRAY(20,23)= 640
      IARRAY(21,23)= 695
      IARRAY(22,23)= 755
      IARRAY(23,23)= 818
      IARRAY(24,23)= 883
      IARRAY(25,23)= 940
      IARRAY(26,23)= 992
      IARRAY(27,23)=1038
      IARRAY(28,23)=1080
      IARRAY( 6,24)=  29
      IARRAY( 8,24)=  78
      IARRAY( 9,24)= 114
      IARRAY(10,24)= 153
      IARRAY(11,24)= 195
      IARRAY(12,24)= 239
      IARRAY(13,24)= 285
      IARRAY(14,24)= 333
      IARRAY(15,24)= 382
      IARRAY(16,24)= 431
      IARRAY(17,24)= 481
      IARRAY(18,24)= 532
      IARRAY(19,24)= 585
      IARRAY(20,24)= 641
      IARRAY(21,24)= 696
      IARRAY(22,24)= 756
      IARRAY(23,24)= 819
      IARRAY(24,24)= 884
      IARRAY(25,24)= 941
      IARRAY(26,24)= 993
      IARRAY(27,24)=1039
      IARRAY(28,24)=1081
      IARRAY( 6,25)=  30
      IARRAY( 7,25)=  45
      IARRAY( 8,25)=  79
      IARRAY( 9,25)= 115
      IARRAY(10,25)= 154
      IARRAY(11,25)= 196
      IARRAY(12,25)= 240
      IARRAY(13,25)= 286
      IARRAY(14,25)= 334
      IARRAY(15,25)= 383
      IARRAY(16,25)= 432
      IARRAY(17,25)= 482
      IARRAY(18,25)= 533
      IARRAY(19,25)= 586
      IARRAY(20,25)= 642
      IARRAY(21,25)= 697
      IARRAY(22,25)= 757
      IARRAY(23,25)= 820
      IARRAY(24,25)= 885
      IARRAY(25,25)= 942
      IARRAY(26,25)= 994
      IARRAY(27,25)=1040
      IARRAY(28,25)=1082
      IARRAY( 6,26)=  31
      IARRAY( 7,26)=  46
      IARRAY( 8,26)=  80
      IARRAY( 9,26)= 116
      IARRAY(10,26)= 155
      IARRAY(11,26)= 197
      IARRAY(12,26)= 241
      IARRAY(13,26)= 287
      IARRAY(14,26)= 335
      IARRAY(15,26)= 384
      IARRAY(16,26)= 433
      IARRAY(17,26)= 483
      IARRAY(18,26)= 534
      IARRAY(19,26)= 587
      IARRAY(20,26)= 643
      IARRAY(21,26)= 698
      IARRAY(22,26)= 758
      IARRAY(23,26)= 821
      IARRAY(24,26)= 886
      IARRAY(25,26)= 943
      IARRAY(26,26)= 995
      IARRAY(27,26)=1041
      IARRAY(28,26)=1083
      IARRAY( 6,27)=  32
      IARRAY( 7,27)=  47
      IARRAY( 8,27)=  81
      IARRAY( 9,27)= 117
      IARRAY(10,27)= 156
      IARRAY(11,27)= 198
      IARRAY(12,27)= 242
      IARRAY(13,27)= 288
      IARRAY(14,27)= 336
      IARRAY(15,27)= 385
      IARRAY(16,27)= 434
      IARRAY(17,27)= 484
      IARRAY(18,27)= 535
      IARRAY(19,27)= 588
      IARRAY(20,27)= 644
      IARRAY(21,27)= 699
      IARRAY(22,27)= 759
      IARRAY(23,27)= 822
      IARRAY(24,27)= 887
      IARRAY(25,27)= 944
      IARRAY(26,27)= 996
      IARRAY(27,27)=1042
      IARRAY(28,27)=1084
      IARRAY( 6,28)=  33
      IARRAY( 7,28)=  48
      IARRAY( 8,28)=  82
      IARRAY( 9,28)= 118
      IARRAY(10,28)= 157
      IARRAY(11,28)= 199
      IARRAY(12,28)= 243
      IARRAY(13,28)= 289
      IARRAY(14,28)= 337
      IARRAY(15,28)= 386
      IARRAY(16,28)= 435
      IARRAY(17,28)= 485
      IARRAY(18,28)= 536
      IARRAY(19,28)= 589
      IARRAY(20,28)= 645
      IARRAY(21,28)= 700
      IARRAY(22,28)= 760
      IARRAY(23,28)= 823
      IARRAY(24,28)= 888
      IARRAY(25,28)= 945
      IARRAY(26,28)= 997
      IARRAY(27,28)=1043
      IARRAY(28,28)=1085
      IARRAY( 3,29)=   7
      IARRAY( 4,29)=  12
      IARRAY( 5,29)=  20
      IARRAY( 6,29)=  34
      IARRAY( 7,29)=  49
      IARRAY( 8,29)=  83
      IARRAY( 9,29)= 119
      IARRAY(10,29)= 158
      IARRAY(11,29)= 200
      IARRAY(12,29)= 244
      IARRAY(13,29)= 290
      IARRAY(14,29)= 338
      IARRAY(15,29)= 387
      IARRAY(16,29)= 436
      IARRAY(17,29)= 486
      IARRAY(18,29)= 537
      IARRAY(19,29)= 590
      IARRAY(20,29)= 646
      IARRAY(21,29)= 701
      IARRAY(22,29)= 761
      IARRAY(23,29)= 824
      IARRAY(24,29)= 889
      IARRAY(25,29)= 946
      IARRAY(26,29)= 998
      IARRAY(27,29)=1044
      IARRAY(28,29)=1086
      IARRAY( 3,30)=   8
      IARRAY( 4,30)=  13
      IARRAY( 5,30)=  21
      IARRAY( 6,30)=  35
      IARRAY( 7,30)=  50
      IARRAY( 8,30)=  84
      IARRAY( 9,30)= 120
      IARRAY(10,30)= 159
      IARRAY(11,30)= 201
      IARRAY(12,30)= 245
      IARRAY(13,30)= 291
      IARRAY(14,30)= 339
      IARRAY(15,30)= 388
      IARRAY(16,30)= 437
      IARRAY(17,30)= 487
      IARRAY(18,30)= 538
      IARRAY(19,30)= 591
      IARRAY(20,30)= 647
      IARRAY(21,30)= 702
      IARRAY(22,30)= 762
      IARRAY(23,30)= 825
      IARRAY(24,30)= 890
      IARRAY(25,30)= 947
      IARRAY(26,30)= 999
      IARRAY(27,30)=1045
      IARRAY(28,30)=1087
      IARRAY( 3,31)=   9
      IARRAY( 4,31)=  14
      IARRAY( 5,31)=  22
      IARRAY( 6,31)=  36
      IARRAY( 7,31)=  51
      IARRAY( 8,31)=  85
      IARRAY( 9,31)= 121
      IARRAY(10,31)= 160
      IARRAY(11,31)= 202
      IARRAY(12,31)= 246
      IARRAY(13,31)= 292
      IARRAY(14,31)= 340
      IARRAY(15,31)= 389
      IARRAY(16,31)= 438
      IARRAY(17,31)= 488
      IARRAY(18,31)= 539
      IARRAY(19,31)= 592
      IARRAY(20,31)= 648
      IARRAY(21,31)= 703
      IARRAY(22,31)= 763
      IARRAY(23,31)= 826
      IARRAY(24,31)= 891
      IARRAY(25,31)= 948
      IARRAY(26,31)=1000
      IARRAY(27,31)=1046
      IARRAY(28,31)=1088
      IARRAY( 3,32)=  10
      IARRAY( 4,32)=  15
      IARRAY( 5,32)=  23
      IARRAY( 6,32)=  37
      IARRAY( 7,32)=  52
      IARRAY( 8,32)=  86
      IARRAY( 9,32)= 122
      IARRAY(10,32)= 161
      IARRAY(11,32)= 203
      IARRAY(12,32)= 247
      IARRAY(13,32)= 293
      IARRAY(14,32)= 341
      IARRAY(15,32)= 390
      IARRAY(16,32)= 439
      IARRAY(17,32)= 489
      IARRAY(18,32)= 540
      IARRAY(19,32)= 593
      IARRAY(20,32)= 649
      IARRAY(21,32)= 704
      IARRAY(22,32)= 764
      IARRAY(23,32)= 827
      IARRAY(24,32)= 892
      IARRAY(25,32)= 949
      IARRAY(26,32)=1001
      IARRAY(27,32)=1047
      IARRAY(28,32)=1089
      IARRAY( 2,33)=   5
      IARRAY( 3,33)=  11
      IARRAY( 4,33)=  16
      IARRAY( 5,33)=  24
      IARRAY( 6,33)=  38
      IARRAY( 7,33)=  53
      IARRAY( 8,33)=  87
      IARRAY( 9,33)= 123
      IARRAY(10,33)= 162
      IARRAY(11,33)= 204
      IARRAY(12,33)= 248
      IARRAY(13,33)= 294
      IARRAY(14,33)= 342
      IARRAY(15,33)= 391
      IARRAY(16,33)= 440
      IARRAY(17,33)= 490
      IARRAY(18,33)= 541
      IARRAY(19,33)= 594
      IARRAY(20,33)= 650
      IARRAY(21,33)= 705
      IARRAY(22,33)= 765
      IARRAY(23,33)= 828
      IARRAY(24,33)= 893
      IARRAY(25,33)= 950
      IARRAY(26,33)=1002
      IARRAY(27,33)=1048
      IARRAY(28,33)=1090
      IARRAY( 6,34)=  39
      IARRAY( 7,34)=  54
      IARRAY( 8,34)=  88
      IARRAY( 9,34)= 124
      IARRAY(10,34)= 163
      IARRAY(11,34)= 205
      IARRAY(12,34)= 249
      IARRAY(13,34)= 295
      IARRAY(14,34)= 343
      IARRAY(15,34)= 392
      IARRAY(16,34)= 441
      IARRAY(17,34)= 491
      IARRAY(18,34)= 542
      IARRAY(19,34)= 595
      IARRAY(20,34)= 651
      IARRAY(21,34)= 706
      IARRAY(22,34)= 766
      IARRAY(23,34)= 829
      IARRAY(24,34)= 894
      IARRAY(25,34)= 951
      IARRAY(26,34)=1003
      IARRAY(27,34)=1049
      IARRAY(28,34)=1091
      IARRAY( 6,35)=  40
      IARRAY( 7,35)=  55
      IARRAY( 8,35)=  89
      IARRAY( 9,35)= 125
      IARRAY(10,35)= 164
      IARRAY(11,35)= 206
      IARRAY(12,35)= 250
      IARRAY(13,35)= 296
      IARRAY(14,35)= 344
      IARRAY(15,35)= 393
      IARRAY(16,35)= 442
      IARRAY(17,35)= 492
      IARRAY(18,35)= 543
      IARRAY(19,35)= 596
      IARRAY(20,35)= 652
      IARRAY(21,35)= 707
      IARRAY(22,35)= 767
      IARRAY(23,35)= 830
      IARRAY(24,35)= 895
      IARRAY(25,35)= 952
      IARRAY(26,35)=1004
      IARRAY(27,35)=1050
      IARRAY(28,35)=1092
      IARRAY( 7,36)=  56
      IARRAY( 8,36)=  90
      IARRAY( 9,36)= 126
      IARRAY(10,36)= 165
      IARRAY(11,36)= 207
      IARRAY(12,36)= 251
      IARRAY(13,36)= 297
      IARRAY(14,36)= 345
      IARRAY(15,36)= 394
      IARRAY(16,36)= 443
      IARRAY(17,36)= 493
      IARRAY(18,36)= 544
      IARRAY(19,36)= 597
      IARRAY(20,36)= 653
      IARRAY(21,36)= 708
      IARRAY(22,36)= 768
      IARRAY(23,36)= 831
      IARRAY(24,36)= 896
      IARRAY(25,36)= 953
      IARRAY(26,36)=1005
      IARRAY(27,36)=1051
      IARRAY(28,36)=1093
      IARRAY( 7,37)=  57
      IARRAY( 8,37)=  91
      IARRAY( 9,37)= 127
      IARRAY(10,37)= 166
      IARRAY(11,37)= 208
      IARRAY(12,37)= 252
      IARRAY(13,37)= 298
      IARRAY(14,37)= 346
      IARRAY(15,37)= 395
      IARRAY(16,37)= 444
      IARRAY(17,37)= 494
      IARRAY(18,37)= 545
      IARRAY(19,37)= 598
      IARRAY(20,37)= 654
      IARRAY(21,37)= 709
      IARRAY(22,37)= 769
      IARRAY(23,37)= 832
      IARRAY(24,37)= 897
      IARRAY(25,37)= 954
      IARRAY(26,37)=1006
      IARRAY(27,37)=1052
      IARRAY(28,37)=1094
      IARRAY( 7,38)=  58
      IARRAY( 8,38)=  92
      IARRAY( 9,38)= 128
      IARRAY(10,38)= 167
      IARRAY(11,38)= 209
      IARRAY(12,38)= 253
      IARRAY(13,38)= 299
      IARRAY(14,38)= 347
      IARRAY(15,38)= 396
      IARRAY(16,38)= 445
      IARRAY(17,38)= 495
      IARRAY(18,38)= 546
      IARRAY(19,38)= 599
      IARRAY(20,38)= 655
      IARRAY(21,38)= 710
      IARRAY(22,38)= 770
      IARRAY(23,38)= 833
      IARRAY(24,38)= 898
      IARRAY(25,38)= 955
      IARRAY(26,38)=1007
      IARRAY(27,38)=1053
      IARRAY(28,38)=1095
      IARRAY( 7,39)=  59
      IARRAY( 8,39)=  93
      IARRAY( 9,39)= 129
      IARRAY(10,39)= 168
      IARRAY(11,39)= 210
      IARRAY(12,39)= 254
      IARRAY(13,39)= 300
      IARRAY(14,39)= 348
      IARRAY(15,39)= 397
      IARRAY(16,39)= 446
      IARRAY(17,39)= 496
      IARRAY(18,39)= 547
      IARRAY(19,39)= 600
      IARRAY(20,39)= 656
      IARRAY(21,39)= 711
      IARRAY(22,39)= 771
      IARRAY(23,39)= 834
      IARRAY(24,39)= 899
      IARRAY(25,39)= 956
      IARRAY(26,39)=1008
      IARRAY(27,39)=1054
      IARRAY(28,39)=1096
      IARRAY( 7,40)=  60
      IARRAY( 8,40)=  94
      IARRAY( 9,40)= 130
      IARRAY(10,40)= 169
      IARRAY(11,40)= 211
      IARRAY(12,40)= 255
      IARRAY(13,40)= 301
      IARRAY(14,40)= 349
      IARRAY(15,40)= 398
      IARRAY(16,40)= 447
      IARRAY(17,40)= 497
      IARRAY(18,40)= 548
      IARRAY(19,40)= 601
      IARRAY(20,40)= 657
      IARRAY(21,40)= 712
      IARRAY(22,40)= 772
      IARRAY(23,40)= 835
      IARRAY(24,40)= 900
      IARRAY(25,40)= 957
      IARRAY(26,40)=1009
      IARRAY(27,40)=1055
      IARRAY(28,40)=1097
      IARRAY( 7,41)=  61
      IARRAY( 8,41)=  95
      IARRAY( 9,41)= 131
      IARRAY(10,41)= 170
      IARRAY(11,41)= 212
      IARRAY(12,41)= 256
      IARRAY(13,41)= 302
      IARRAY(14,41)= 350
      IARRAY(15,41)= 399
      IARRAY(16,41)= 448
      IARRAY(17,41)= 498
      IARRAY(18,41)= 549
      IARRAY(19,41)= 602
      IARRAY(20,41)= 658
      IARRAY(21,41)= 713
      IARRAY(22,41)= 773
      IARRAY(23,41)= 836
      IARRAY(24,41)= 901
      IARRAY(25,41)= 958
      IARRAY(26,41)=1010
      IARRAY(27,41)=1056
      IARRAY(28,41)=1098
      IARRAY( 8,42)=  96
      IARRAY( 9,42)= 132
      IARRAY(10,42)= 171
      IARRAY(11,42)= 213
      IARRAY(12,42)= 257
      IARRAY(13,42)= 303
      IARRAY(14,42)= 351
      IARRAY(15,42)= 400
      IARRAY(16,42)= 449
      IARRAY(17,42)= 499
      IARRAY(18,42)= 550
      IARRAY(19,42)= 603
      IARRAY(20,42)= 659
      IARRAY(21,42)= 714
      IARRAY(22,42)= 774
      IARRAY(23,42)= 837
      IARRAY(24,42)= 902
      IARRAY(25,42)= 959
      IARRAY(26,42)=1011
      IARRAY(27,42)=1057
      IARRAY(28,42)=1099
      IARRAY( 8,43)=  97
      IARRAY( 9,43)= 133
      IARRAY(10,43)= 172
      IARRAY(11,43)= 214
      IARRAY(12,43)= 258
      IARRAY(13,43)= 304
      IARRAY(14,43)= 352
      IARRAY(15,43)= 401
      IARRAY(16,43)= 450
      IARRAY(17,43)= 500
      IARRAY(18,43)= 551
      IARRAY(19,43)= 604
      IARRAY(20,43)= 660
      IARRAY(21,43)= 715
      IARRAY(22,43)= 775
      IARRAY(23,43)= 838
      IARRAY(24,43)= 903
      IARRAY(25,43)= 960
      IARRAY(26,43)=1012
      IARRAY(27,43)=1058
      IARRAY( 8,44)=  98
      IARRAY( 9,44)= 134
      IARRAY(10,44)= 173
      IARRAY(11,44)= 215
      IARRAY(12,44)= 259
      IARRAY(13,44)= 305
      IARRAY(14,44)= 353
      IARRAY(15,44)= 402
      IARRAY(16,44)= 451
      IARRAY(17,44)= 501
      IARRAY(18,44)= 552
      IARRAY(19,44)= 605
      IARRAY(20,44)= 661
      IARRAY(21,44)= 716
      IARRAY(22,44)= 776
      IARRAY(23,44)= 839
      IARRAY(24,44)= 904
      IARRAY(26,44)=1013
      IARRAY(27,44)=1059
      IARRAY( 7,45)=  62
      IARRAY( 8,45)=  99
      IARRAY( 9,45)= 135
      IARRAY(10,45)= 174
      IARRAY(11,45)= 216
      IARRAY(12,45)= 260
      IARRAY(13,45)= 306
      IARRAY(14,45)= 354
      IARRAY(15,45)= 403
      IARRAY(16,45)= 452
      IARRAY(17,45)= 502
      IARRAY(18,45)= 553
      IARRAY(19,45)= 606
      IARRAY(20,45)= 662
      IARRAY(21,45)= 717
      IARRAY(22,45)= 777
      IARRAY(23,45)= 840
      IARRAY(24,45)= 905
      IARRAY(25,45)= 961
      IARRAY(26,45)=1014
      IARRAY(27,45)=1060
      IARRAY( 7,46)=  63
      IARRAY( 8,46)= 100
      IARRAY( 9,46)= 136
      IARRAY(10,46)= 175
      IARRAY(11,46)= 217
      IARRAY(12,46)= 261
      IARRAY(13,46)= 307
      IARRAY(14,46)= 355
      IARRAY(15,46)= 404
      IARRAY(16,46)= 453
      IARRAY(17,46)= 503
      IARRAY(18,46)= 554
      IARRAY(19,46)= 607
      IARRAY(20,46)= 663
      IARRAY(21,46)= 718
      IARRAY(22,46)= 778
      IARRAY(23,46)= 841
      IARRAY(24,46)= 906
      IARRAY(25,46)= 962
      IARRAY(26,46)=1015
      IARRAY(27,46)=1061
      IARRAY( 1,47)=   1
      IARRAY( 7,47)=  64
      IARRAY( 8,47)= 101
      IARRAY( 9,47)= 137
      IARRAY(10,47)= 176
      IARRAY(11,47)= 218
      IARRAY(12,47)= 262
      IARRAY(13,47)= 308
      IARRAY(14,47)= 356
      IARRAY(15,47)= 405
      IARRAY(16,47)= 454
      IARRAY(17,47)= 504
      IARRAY(18,47)= 555
      IARRAY(19,47)= 608
      IARRAY(20,47)= 664
      IARRAY(21,47)= 719
      IARRAY(22,47)= 779
      IARRAY(23,47)= 842
      IARRAY(24,47)= 907
      IARRAY(25,47)= 963
      IARRAY(26,47)=1016
      IARRAY( 1,48)=   2
      IARRAY( 4,48)=  17
      IARRAY( 5,48)=  25
      IARRAY( 6,48)=  41
      IARRAY( 7,48)=  65
      IARRAY( 8,48)= 102
      IARRAY( 9,48)= 138
      IARRAY(10,48)= 177
      IARRAY(11,48)= 219
      IARRAY(12,48)= 263
      IARRAY(13,48)= 309
      IARRAY(14,48)= 357
      IARRAY(15,48)= 406
      IARRAY(16,48)= 455
      IARRAY(17,48)= 505
      IARRAY(18,48)= 556
      IARRAY(19,48)= 609
      IARRAY(20,48)= 665
      IARRAY(21,48)= 720
      IARRAY(22,48)= 780
      IARRAY(23,48)= 843
      IARRAY(24,48)= 908
      IARRAY(25,48)= 964
      IARRAY(26,48)=1017
      IARRAY( 2,49)=   6
      IARRAY( 4,49)=  18
      IARRAY( 5,49)=  26
      IARRAY( 6,49)=  42
      IARRAY( 7,49)=  66
      IARRAY( 8,49)= 103
      IARRAY( 9,49)= 139
      IARRAY(10,49)= 178
      IARRAY(11,49)= 220
      IARRAY(12,49)= 264
      IARRAY(13,49)= 310
      IARRAY(14,49)= 358
      IARRAY(15,49)= 407
      IARRAY(16,49)= 456
      IARRAY(17,49)= 506
      IARRAY(18,49)= 557
      IARRAY(19,49)= 610
      IARRAY(20,49)= 666
      IARRAY(21,49)= 721
      IARRAY(22,49)= 781
      IARRAY(23,49)= 844
      IARRAY(24,49)= 909
      IARRAY(25,49)= 965
      IARRAY( 1,50)=   3
      IARRAY( 4,50)=  19
      IARRAY( 5,50)=  27
      IARRAY( 6,50)=  43
      IARRAY( 7,50)=  67
      IARRAY( 9,50)= 140
      IARRAY(10,50)= 179
      IARRAY(11,50)= 221
      IARRAY(12,50)= 265
      IARRAY(13,50)= 311
      IARRAY(14,50)= 359
      IARRAY(15,50)= 408
      IARRAY(16,50)= 457
      IARRAY(17,50)= 507
      IARRAY(18,50)= 558
      IARRAY(19,50)= 611
      IARRAY(20,50)= 667
      IARRAY(21,50)= 722
      IARRAY(22,50)= 782
      IARRAY(23,50)= 845
      IARRAY(24,50)= 910
      IARRAY(25,50)= 966
      IARRAY( 1,51)=   4
      IARRAY(10,51)= 180
      IARRAY(11,51)= 222
      IARRAY(12,51)= 266
      IARRAY(13,51)= 312
      IARRAY(14,51)= 360
      IARRAY(15,51)= 409
      IARRAY(16,51)= 458
      IARRAY(17,51)= 508
      IARRAY(18,51)= 559
      IARRAY(19,51)= 612
      IARRAY(20,51)= 668
      IARRAY(21,51)= 723
      IARRAY(22,51)= 783
      IARRAY(23,51)= 846
      IARRAY(11,52)= 223
      IARRAY(12,52)= 267
      IARRAY(13,52)= 313
      IARRAY(14,52)= 361
      IARRAY(15,52)= 410
      IARRAY(16,52)= 459
      IARRAY(17,52)= 509
      IARRAY(18,52)= 560
      IARRAY(19,52)= 613
      IARRAY(20,52)= 669
      IARRAY(21,52)= 724
      IARRAY(22,52)= 784
      IARRAY(23,52)= 847
      IARRAY(11,53)= 224
      IARRAY(12,53)= 268
      IARRAY(13,53)= 314
      IARRAY(14,53)= 362
      IARRAY(15,53)= 411
      IARRAY(16,53)= 460
      IARRAY(17,53)= 510
      IARRAY(18,53)= 561
      IARRAY(19,53)= 614
      IARRAY(20,53)= 670
      IARRAY(21,53)= 725
      IARRAY(22,53)= 785
      IARRAY(23,53)= 848
      IARRAY(12,54)= 269
      IARRAY(13,54)= 315
      IARRAY(14,54)= 363
      IARRAY(15,54)= 412
      IARRAY(16,54)= 461
      IARRAY(17,54)= 511
      IARRAY(18,54)= 562
      IARRAY(19,54)= 615
      IARRAY(20,54)= 671
      IARRAY(21,54)= 726
      IARRAY(22,54)= 786
      IARRAY(23,54)= 849
      IARRAY(13,55)= 316
      IARRAY(14,55)= 364
      IARRAY(15,55)= 413
      IARRAY(16,55)= 462
      IARRAY(17,55)= 512
      IARRAY(18,55)= 563
      IARRAY(19,55)= 616
      IARRAY(20,55)= 672
      IARRAY(21,55)= 727
      IARRAY(22,55)= 787
      IARRAY(23,55)= 850
      IARRAY(16,56)= 463
      IARRAY(17,56)= 513
      IARRAY(18,56)= 564
      IARRAY(19,56)= 617
      IARRAY(20,56)= 673
      IARRAY(21,56)= 728
      IARRAY(22,56)= 788
      IARRAY(23,56)= 851
      IARRAY(18,57)= 565
      IARRAY(19,57)= 618
      IARRAY(20,57)= 674
      IARRAY(21,57)= 729
      IARRAY(22,57)= 789
      IARRAY(23,57)= 852
      IARRAY(24,57)= 911
      IARRAY(18,58)= 566
      IARRAY(19,58)= 619
      IARRAY(20,58)= 675
      IARRAY(21,58)= 730
      IARRAY(22,58)= 790
      IARRAY(23,58)= 853
      IARRAY(24,58)= 912
      IARRAY(19,59)= 620
      IARRAY(20,59)= 676
      IARRAY(21,59)= 731
      IARRAY(22,59)= 791
      IARRAY(23,59)= 854
      IARRAY(24,59)= 913
      IARRAY(19,60)= 621
      IARRAY(20,60)= 677
      IARRAY(21,60)= 732
      IARRAY(22,60)= 792
      IARRAY(23,60)= 855
      IARRAY(24,60)= 914
      IARRAY(19,61)= 622
      IARRAY(21,61)= 733
      IARRAY(22,61)= 793
      IARRAY(23,61)= 856
      IARRAY(24,61)= 915
      IARRAY(25,61)= 967
      IARRAY(21,62)= 734
      IARRAY(22,62)= 794
      IARRAY(23,62)= 857
      IARRAY(24,62)= 916
      IARRAY(25,62)= 968
      IARRAY(22,63)= 795
      IARRAY(23,63)= 858
      IARRAY(24,63)= 917
      IARRAY(25,63)= 969
      IARRAY(21,64)= 735
      IARRAY(22,64)= 796
      IARRAY(23,64)= 859
      IARRAY(24,64)= 918
      IARRAY(25,64)= 970
      IARRAY(21,65)= 736
      IARRAY(22,65)= 797
      IARRAY(23,65)= 860
      IARRAY(21,66)= 737
      IARRAY(23,66)= 861
      IARRAY(22,67)= 798
      IARRAY(23,67)= 862
      IARRAY(24,67)= 919
      IARRAY(22,68)= 799
      IARRAY(23,68)= 863
      IARRAY(24,68)= 920
      IARRAY(25,68)= 971
      IARRAY(22,69)= 800
      IARRAY(23,69)= 864
      IARRAY(24,69)= 921
      IARRAY(25,69)= 972
      IARRAY(23,70)= 865
      IARRAY(24,70)= 922
      IARRAY(25,70)= 973
C
C  DO THE INTERPOLATION PROCEDURE FOR EACH POINT ON THE GRID:
C
C     Note: If ISW=1, coordinates only at original sampling locations
C              ISW=2, coordinates at regular range-angle grid
C     RHI (3) - AZA contains elevation angle and FXOLD = azimuth
C     Others  - AZA contains azimuth   angle and FXOLD = elevation
C
      IF(NGRD.EQ.'REG')THEN
         ISW=2
      ELSE
         ISW=1
      END IF
c      write(*,*)'ustopo: ',ngrd,isw,itpold,mang,mngate,mxgate

      DO 720 J=1,MANG

C        RHI scan:
C
         IF(ITPOLD.EQ.3)THEN
            IF(ISW.EQ.2)THEN
               AZRAD=FXOLD-AZROT(ITPOLD)
            ELSE
               AZRAD=ELA(J,ISW)
            END IF
            ELRAD=AZA(J,ISW)

C        All other scans:
C
         ELSE
            AZRAD=AZA(J,ISW)-AZROT(ITPOLD)
            IF(ISW.EQ.2)THEN
               ELRAD=FXOLD
            ELSE
               ELRAD=ELA(J,ISW)
            END IF
            IF(AZRAD.LT.0.0)AZRAD=AZRAD+360.0
         END IF
         SINA=SIN(AZRAD*TORAD)
         COSA=COS(AZRAD*TORAD)
         SINE=SIN(ELRAD*TORAD)
         COSE=COS(ELRAD*TORAD)

         DO 710 I=MNGATE,MXGATE
            DAT(I,J,IOUT)=BDVAL
            IF(RNG(I,ISW).LT.FRST_RNG)GO TO 710
            HRNG=RNG(I,ISW)*COSE
            X=X0+HRNG*SINA
            Y=Y0+HRNG*COSA
            Z=H0+RNG(I,ISW)*SINE+0.5*HRNG*HRNG*REI
C
C     CALL THE XY-GRID TO LAT-LON CONVERTER ROUTINE:
C
            XTP=X
            YTP=Y
            CALL XY2LLDRV(DLAT,DLON,XTP,YTP,ORLAT,ORLON,GRIDANG)
            DLON=ABS(DLON)
C
C     CONVERT (LAT,LON) TO BE IN SAME SYSTEM AS DATABASE
C
            DLAT=DLAT-22.
            DLON=360.0-DLON-229
            IREC=IARRAY(INT(DLAT),INT(DLON))
            IF (IREC.LE.0 .OR. IREC.GT.1099) THEN
C
C     THERE IS NO RECORD FOR THE DESIRED (LAT,LON). THIS MEANS THAT
C     THAT REGION IS ALL MISSING OR ALL AT SEA LEVEL.  SET HEIGHT
C     TO BAD AND DO NEXT GRID POINT.  Calls RDTOPREC inside topo.c
C
               DAT(I,J,IOUT)=BDVAL
               GOTO 710
            END IF
            IF (IREC.EQ.LSTREC) GOTO 70
            LSTREC=IREC
            CALL RDTOPREC(IREC, CBUF)
            READ(CBUF,50)LAT,LON
 50         FORMAT(2I6)
            LAT=LAT-90-22
            LON=LON-229
            IF (LAT.NE.INT(DLAT)) THEN
               WRITE(6,20)DLAT,LAT
 20            FORMAT(/,5X,'+++ERROR READING TOPOGRAPHY DATA. ',
     X                'REQUESTED LAT=',F8.2,' ACTUAL=',I6)
               RETURN
            ELSE IF (LON.NE.INT(DLON)) THEN
               WRITE(6,30)DLON,LON
 30            FORMAT(/,5X,'+++ERROR READING TOPOGRAPHY DATA. ',
     X                'REQUESTED LON=',F8.2,' ACTUAL=',I6)
               RETURN
            END IF
C
C     NOW UNPACK ELEVATIONS AND CONVERT TO METERS
C
            READ(CBUF,60)ITOP
 60         FORMAT(44X,488(30I4,1X),I4)
            DO 25 JJ=1,121
               DO 27 II=1,121
                  IF (ITOP(II,JJ).EQ.0) THEN
                     TOP(II,JJ)=BDVAL
                  ELSE
                     TOP(II,JJ)=(ITOP(II,JJ)*20. - 4000.)*12.*2.54/100.0
                  END IF
 27            CONTINUE
 25         CONTINUE
            
C
C     FIND 30 SEC INDEX INTO ARRAY (DLAT AND DLON ARE IN DECIMAL FORM,
C     NOT MINUTES AND SECONDS FORM)
C
 70         CONTINUE
            ILAT=INT((DLAT-LAT)*120.0) + 1
            ILON=INT((DLON-LON)*120.0) + 1
            RLAT=((DLAT-LAT)*120.0) + 1.0
            RLON=((DLON-LON)*120.0) + 1.0
            
C
C     NOW DO THE BILINEAR INTERPOLATION
C
            WSUM=0.0
            DAT(I,J,IOUT)=0.0
            IF (TOP(ILON,  ILAT  ).NE.BDVAL .AND. 
     X          TOP(ILON+1,ILAT  ).NE.BDVAL .AND.
     X          TOP(ILON,  ILAT+1).NE.BDVAL .AND. 
     X          TOP(ILON+1,ILAT+1).NE.BDVAL) THEN
               DAT(I,J,IOUT)=DAT(I,J,IOUT)
     X              + TOP(ILON,ILAT)*(ILAT+1-RLAT)*
     X              (ILON+1-RLON) + TOP(ILON+1,ILAT)*(RLON-ILON)*
     X              (ILAT+1-RLAT) + TOP(ILON,ILAT+1)*(ILON+1-RLON)*
     X              (RLAT-ILAT)   + TOP(ILON+1,ILAT+1)*
     X              (RLAT-ILAT)*(RLON-ILON)
            ELSE
               DAT(I,J,IOUT)=BDVAL
            END IF

            IF(PRINT)THEN
               ZDAT=DAT(I,J,IOUT)
               IF(MOD(J,JMOD).EQ.0 .AND. MOD(I,IMOD).EQ.0)THEN
                  WRITE(6,709)J,I,RNG(I,ISW),AZRAD,ELRAD,ZDAT
 709              FORMAT(' USTOPO: rae=',2I4,3F8.3,'  Z(m)=',F8.3)
               END IF
            END IF

 710     CONTINUE
 720  CONTINUE

      RETURN

      END

