	Program testit

	integer i, j, nargs, template
	Character arg1*70
	Character arg2*70

	Open (unit = 100, file = 'grid2ps.default', status = 'unknown')

	Rea
	close(100)
	nargs = iargc()

C	DO i = 1, nargs
C	   Call getarg(i, arg1)
C	   IF (i.EQ.1) THEN
C	      Call char2int(arg1, template)
C	      print*, 'test1: ', template
C	   ELSE
C	   ENDIF
C	enddo

	End
