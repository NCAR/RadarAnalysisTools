C This common file contains the character variables

	Character*1 input_flag,fix_axis,horiz_axis,vert_axis
	Character*1 math_flag, color_flag
C	Character*1 orient,fill_flag,level_flag,pscommand(5000)
C	Character*1 orient,fill_flag,level_flag,pscommand(20000)
	Character*1 orient,fill_flag,level_flag,pscommand(40000)
        Character*2 fld_name(5,5)
	Character*3 hkm, vkm
        Character*4 title_flag,scale_flag,scale_pos
	Character*5 grid_flag
	Character*6 strx(10),stry(10)
	Character*4 scan_mode
	Character*8 str(4)
	Character*25 font_name
	Character*100 catalog_name,data_file,panel_title
	Character*100 scale_title, title, output_name, ct_fname

C Common block PsName contains the names & titles used in the PostScript file
C Common block Flag contains the flags (usually Y or N) 
C Common block ScanMode contains the scan mode (ie: AIR,COP,CPL..etc) 
C Common block Titl contains the title of the plot and the scale title 
C Coomon block Str contains the names of the position designators 

	Common/PsName/output_name,fld_name
	Common/Flag/input_flag,fix_axis,horiz_axis,vert_axis,orient,
     +   title_flag,grid_flag,fill_flag,level_flag,scale_pos,
     +   color_flag,math_flag,scale_flag
	Common/ScnMode/scan_mode
	Common/Titl/scale_title,title
	Common/Str/strx,stry,hkm,vkm,str
	Common/Command/pscommand
	Common/Font/font_name
	Common/Size80/catalog_name,data_file,panel_title, ct_fname
	
