SUBROUTINE read_eastriver_param(nout,nx,ny,nz)

USE crunchtype
USE medium
USE CrunchFunctions
USE params
USE concentration
USE transport
USE flow
USE strings
USE RunTime
USE io

IMPLICIT NONE

!  External variables and arrays

INTEGER(I4B), INTENT(IN)                                   :: nout
INTEGER(I4B), INTENT(IN)                                   :: nx
INTEGER(I4B), INTENT(IN)                                   :: ny
INTEGER(I4B), INTENT(IN)                                   :: nz

!  Internal variables and arrays

INTEGER(I4B)                                                :: id
INTEGER(I4B)                                                :: iff
INTEGER(I4B)                                                :: ids
INTEGER(I4B)                                                :: ls
INTEGER(I4B)                                                :: lzs
INTEGER(I4B)                                                :: nlen1

REWIND nout

!! Time discretization (in years), nb of pumps and geochemical conditions of the wells

10 READ(nout,'(a)',END=500) zone
nlen1 = LEN(zone)
CALL majuscules(zone,nlen1)
id = 1
iff = mls
CALL sschaine(zone,id,iff,ssch,ids,ls)

IF(ls /= 0) THEN
  lzs=ls
  CALL convan(ssch,lzs,res)
  IF (ssch == 'east_river') THEN
    east_river=.TRUE.

    id = ids + ls
    CALL sschaine(zone,id,iff,ssch,ids,ls)
    IF(ls /= 0) THEN
      lzs=ls
      CALL convan(ssch,lzs,res)
      IF (res == 'n') THEN
        thres_OM = DNUM(ssch)
      ELSE                !  An ascii string--so bag it.
        WRITE(*,*)
        WRITE(*,*) ' Cant interpret string following "east_river"'
        WRITE(*,*) ' Looking for numerical value (thres_OM)'
        WRITE(*,*)
        READ(*,*)
        STOP
      END IF

      id = ids + ls
    CALL sschaine(zone,id,iff,ssch,ids,ls)
      IF(ls /= 0) THEN
        lzs=ls
        CALL convan(ssch,lzs,res)
        IF (res == 'n') THEN
          exp_OM = DNUM(ssch)
        ELSE                !  An ascii string--so bag it.
          WRITE(*,*)
          WRITE(*,*) ' Cant interpret string following length pump time series'
          WRITE(*,*) ' Looking for numerical value'
          WRITE(*,*)
          READ(*,*)
          STOP
        END IF

                 !  Now, look for geochemical condition following pumping rate (only used if rate is positive)





    ELSE
      WRITE(*,*)
      WRITE(*,*) ' No exponent OM release given'
      WRITE(*,*) 'stoping'
      WRITE(*,*)
      STOP
      GO TO 10
    END IF
  ELSE
    WRITE(*,*)
      WRITE(*,*) ' No east_river param provided'
      WRITE(*,*)
      STOP
    GO TO 10
  END IF
  ELSE
    GO TO 10
  END IF
ELSE
  GO TO 10
END IF

GO TO 10
500 CONTINUE









RETURN

END SUBROUTINE read_eastriver_param