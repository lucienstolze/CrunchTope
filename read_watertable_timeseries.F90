SUBROUTINE read_watertable_timeseries(nout)

USE crunchtype
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

!  Internal variables and arrays

INTEGER(I4B)                                                :: id
INTEGER(I4B)                                                :: iff
INTEGER(I4B)                                                :: ids
INTEGER(I4B)                                                :: ls
INTEGER(I4B)                                                :: lzs
INTEGER(I4B)                                                :: nlen1
REAL(DP)                                                    :: tslength
CHARACTER (LEN=mls)                                           :: FileTemp
INTEGER(I4B)                                                   :: tp
INTEGER(I4B)                                                  :: FileNameLength
LOGICAL(LGT)                                               :: ext
CHARACTER (LEN=mls)                          :: watertablefile
INTEGER(I4B)                                  :: lfile


watertablefile = ' '
REWIND nout

10  READ(nout,'(a)',END=1000) zone
nlen1 = LEN(zone)
!!CALL majuscules(zone,nlen1)
id = 1
iff = mls
CALL sschaine(zone,id,iff,ssch,ids,ls)
IF(ls /= 0) THEN
  lzs=ls
  CALL convan(ssch,lzs,res)
  
  IF (ssch == 'read_watertable_timeseries') THEN
    watertabletimeseries = .true.

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!Looking for time series length
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    id = ids + ls
    CALL sschaine(zone,id,iff,ssch,ids,ls)
    IF(ls /= 0) THEN
      lzs=ls
      CALL convan(ssch,lzs,res)
      IF (res == 'n') THEN
        tslength = DNUM(ssch)
      ELSE                !  An ascii string--so bag it.
        WRITE(*,*)
        WRITE(*,*) ' Cant interpret string following "read_watertable_timeseries"'
        WRITE(*,*) ' Looking for length time_series' !should provide the length of the time series
        WRITE(*,*)
        READ(*,*)
        STOP
      END IF

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!Looking for file name
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    id = ids + ls
    CALL sschaine(zone,id,iff,ssch,ids,ls)
    IF(ls /= 0) THEN
      lzs=ls
      CALL stringtype(ssch,lzs,res)
      watertablefile = ssch
      lfile = ls
    ELSE
      WRITE(*,*)
      WRITE(*,*) ' No file name provided in "read_watertable_timeseries"'
      WRITE(*,*)
      READ(*,*)
      STOP
    END IF


    ELSE
      WRITE(*,*)
      WRITE(*,*) ' No information provided on time series water table'
      WRITE(*,*) ' Stopping'
      WRITE(*,*)
      STOP
    END IF
    
    IF (watertabletimeseries) THEN

      WRITE(iunit2,*)
      WRITE(iunit2,*) '  Reading watertabletimeseries from file: ',watertablefile(1:lfile)
      WRITE(iunit2,*)
      
      
      
        IF (ALLOCATED(wattab_t)) THEN
          DEALLOCATE(wattab_t)
          ALLOCATE(wattab_t(1:int(tslength)))
        ELSE
          ALLOCATE(wattab_t(1:int(tslength)))
        END IF
      
        IF (ALLOCATED(wattab_ts)) THEN
          DEALLOCATE(wattab_ts)
          ALLOCATE(wattab_ts(1:int(tslength))) !! only work for a 2D model XY
        ELSE
          ALLOCATE(wattab_ts(1:int(tslength)))
        END IF
      
      
          INQUIRE(FILE=watertablefile,EXIST=ext)
          IF (.NOT. ext) THEN
            WRITE(*,*)
            WRITE(*,*) 'watertabletimeseries file not found: ',watertablefile(1:lfile)
            WRITE(*,*)
            READ(*,*)
            STOP
          END IF
      
      
          OPEN(UNIT=23,FILE=watertablefile,STATUS='old',ERR=8005)
          FileTemp = watertablefile
          CALL stringlen(FileTemp,FileNameLength)
                DO tp = 1,int(tslength)
                READ(23,*,END=1020) wattab_t(tp),wattab_ts(tp)
              END DO
      
          !       DO jz = 1,nz
          !         DO jy = 1,ny
          !           DO jx = 0,0
          !       if (pressurebct(tp,jx,jy,jz)/=0) then
          !       pressurebct(tp,jx,jy,jz)=(10**pressurebct(tp,jx,jy,jz))
          !       else
          !       permx(jx,jy,jz)=0  
          !       end if
                  
          !         pres(jx,jy,jz)=pressurebct(1,jx,jy,jz)
          !         activecellPressure(jx,jy,jz) = 0
          !     END DO
          !   END DO
          ! END DO
          CLOSE(UNIT=23,STATUS='keep')
      END IF 
    
  ELSE
    GO TO 10
  END IF
  GO TO 10
  
ELSE         ! No string found
  GO TO 10
END IF

GO TO 10
!!!!!!!!!!!!!!!!!!!!





RETURN

1020  WRITE(*,*) ' End of file during read'
WRITE(*,*) ' Trying to read the file: ', FileTemp(1:FileNameLength)
READ(*,*)
STOP

8005   WRITE(*,*) ' Error opening watertabletimeseries file'
        READ(*,*)
STOP

1000 RETURN
END SUBROUTINE read_watertable_timeseries