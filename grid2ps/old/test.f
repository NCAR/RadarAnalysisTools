	Program testit

        Implicit none

	Include 'common1.h'
	Include 'common2.h'
	Include 'common3.h'
	Include 'structure.h'

	print*, '**************'
	scan.mode = "PRI"
	print*, 'PRI: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "PPI"
	print*, 'PPI: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "COP"
	print*, 'COP: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "FIX"
	print*, 'FIX: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "VAD"
	print*, 'VAD: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "CPL"
	print*, 'CPL: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "CPN"
	print*, 'CPN: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "CRT"
	print*, 'CRT: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "AIR"
	print*, 'AIR: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'
	scan.mode = "POLA"
	print*, 'POLA: '
	print*, '16: ', scan.id16
	print*, '17: ', scan.id17
	print*, '**************'

	End

