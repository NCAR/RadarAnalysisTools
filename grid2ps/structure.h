C This structure converts the integers id(16) & id(17) to characters
        structure /scanmode/
           union
              map
                 Integer*2 id16
                 Integer*2 id17
              end map
              map
                 Character*4 mode 
	      end map
           end union
        end structure

	Record /scanmode/ scan

	Common scan
