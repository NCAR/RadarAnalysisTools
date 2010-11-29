#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define FALSE 0
#define TRUE  1
#define BUFSIZE 100

/* Routine to read the input file for grid2ps */
void readinppt1_(panel, inp_fname, ps_fname, in_flag,
   data_fname, color_flag, axis, cont, f1, f2, f3, f4, plot_flag,
   fill, fill_beg, fill_end, fill_inc, nofill_beg, nofill_end,
   nofill_inc, panel_title, scale_title, title, level_flag,
   level, dist, cat_fname, plane_inc)

   int *panel; 
   int cont, plot_flag, level, plane_inc;
   char inp_fname[20], ps_fname[], in_flag[];
   char data_fname[], color_flag[], axis[];
   char f1[], f2[], f3[], f4[], fill[];
   char panel_title[], scale_title[], title[];
   char level_flag[], cat_fname[];
   float *fill_beg, *fill_end, *fill_inc;
   float *nofill_beg, *nofill_end, *nofill_inc;
   float *dist;

{

   /* READ THE FIRST PART OF THE INPUT FILE */

   FILE *fp;
   char buf[BUFSIZE];
   char ch;
   int i;

   /* PUT A NULL CHARACTER AT END OF FILE NAME */
   if (*panel < 10) {
      inp_fname[4] = '\0';
   }


   /* OPEN THE INPUT FILE */
   if ( (fp = fopen(inp_fname,"r"))==NULL) {
      printf("Can't open %s\n",inp_fname);
      exit(-1);
   }

   while ( !feof(fp) ) {

      fgets(buf, BUFSIZE, fp);

      if ( strstr(buf, "OUTPUT_NAME") && *panel == 1 ) {
         fgets(ps_fname, BUFSIZE, fp);
	 ps_fname[strlen(ps_fname)-1] = '$';
	 ps_fname[strlen(ps_fname)] = '$';

      } else if ( strstr(buf, "INPUT_FLAG")  ) {
	 ch = getc(fp);
	 in_flag[0] = toupper(ch);

      } else if ( strstr(buf, "DATA_FILE") ) {
         fgets(data_fname, BUFSIZE, fp);
	 data_fname[strlen(data_fname)-1] = '$';
	 data_fname[strlen(data_fname)] = '$';

      } else if ( strstr(buf, "COLOR")  ) {
	 ch = getc(fp);
	 color_flag[0] = toupper(ch);

      } else if ( strstr(buf, "CONTOUR_TYPE")  ) {
	 fscanf (fp,"%d", cont); 

      } else if ( strstr(buf, "FIX_AXIS")  ) {
	 ch = getc(fp);
	 axis[0] = toupper(ch);

      } else if ( strstr(buf, "FIELD1")  ) {
	 fscanf (fp,"%s", f1); 
	 f1[strlen(f1)] = '\0';

      } else if ( strstr(buf, "FIELD2")  ) {
	 fscanf (fp,"%s", f2); 
	 f2[strlen(f2)] = '\0';

      } else if ( strstr(buf, "FIELD3")  ) {
	 fscanf (fp,"%s", f3); 
	 f3[strlen(f3)] = '\0';

      } else if ( strstr(buf, "FIELD4")  ) {
	 fscanf (fp,"%s", f4); 
	 f4[strlen(f4)] = '\0';

      } else if ( strstr(buf, "PLOT_TYPE")  ) {
	 fscanf (fp,"%d", plot_flag); 

      } else if ( strstr(buf, "FILL_FLAG")  ) {
	 ch = getc(fp);
	 fill[0] = toupper(ch);

      } else if ( strstr(buf, "#FILL_BEG")  ) {
	 fscanf (fp,"%f", fill_beg); 

      } else if ( strstr(buf, "#FILL_END")  ) {
	 fscanf (fp,"%f", fill_end); 

      } else if ( strstr(buf, "#FILL_INC")  ) {
	 fscanf (fp,"%f", fill_inc); 

      } else if ( strstr(buf, "NOFILL_BEG")  ) {
	 fscanf (fp,"%f", nofill_beg); 

      } else if ( strstr(buf, "NOFILL_END")  ) {
	 fscanf (fp,"%f", nofill_end); 

      } else if ( strstr(buf, "NOFILL_INC")  ) {
	 fscanf (fp,"%f", nofill_inc); 

      } else if ( strstr(buf, "PANEL_TITLE") ) {
         fgets(panel_title, BUFSIZE, fp);
	 panel_title[strlen(panel_title)] = '\0';

      } else if ( strstr(buf, "SCALE_TITLE") && *panel == 1 ) {
         fgets(scale_title, BUFSIZE, fp);
	 scale_title[strlen(scale_title)] = '\0';

      } else if ( strstr(buf, "#TITLE !") && *panel == 1 ) {
         fgets(title, BUFSIZE, fp);
	 title[strlen(title)] = '\0';

      } else if ( strstr(buf, "LEVEL_FLAG")  ) {
	 ch = getc(fp);
	 level_flag[0] = toupper(ch);

      } else if ( strstr(buf, "DISTANCE OR LEVEL") || 
		  strstr(buf, "PLOTS") ) {
	 if ( strstr(level_flag, "L") ) {
	    fscanf (fp,"%d", level); 
	 } else if ( strstr(level_flag, "D") ) {
	    fscanf (fp,"%f", dist); 
	 }

      } else if ( strstr(buf, "CATALOG_NAME")  ) {
         fgets(cat_fname, BUFSIZE, fp);
	 cat_fname[strlen(cat_fname)] = '\0';

      } else if ( strstr(buf, "PLANE_INC")  ) {
	 fscanf (fp,"%d", plane_inc); 

      }

   } /* end while */

   fclose(fp);


}
/****************************************************/
void readinppt2_(panel, inp_fname, h1, h2, v1, v2, pri1,
   pri2, sec1, sec2, scale_flag, scale_pos, scale_w, scale_h, zlinew,
   linew, dash_on, dash_off, dash_beg, skip, vec_speed, vec_len,
   arrow_h, arrow_hh, arrow_ht, font_title, font_num, font_label,
   stretch_horiz, stretch_vert, sp_text, sp_scale, sp_row, sp_col,
   sp_title, title_flag, grid_flag, panel_w, panel_h, page_w,
   page_h, l_marg, r_marg, t_marg, b_marg)

   int *panel, pri1, pri2, sec1, sec2; 
   int dash_on, dash_off, dash_beg;
   int skip, font_title, font_num, font_label;
   char inp_fname[], scale_pos[];
   char title_flag[], grid_flag[], scale_flag[];
   float *h1, *h2, *v1, *v2;
   float *scale_w, *scale_h;
   float *zlinew, *linew;
   float *vec_speed, *vec_len;
   float *arrow_h, *arrow_hh, *arrow_ht;
   float *stretch_horiz, *stretch_vert;
   float *sp_text, *sp_scale, *sp_row, *sp_title, *sp_col;
   float *panel_w, *panel_h;
   float *page_w, *page_h;
   float *l_marg, *r_marg, *t_marg, *b_marg;

{


   /* READ THE SECOND PART OF THE INPUT FILE */


   FILE *fp;
   char buf[BUFSIZE];
   int int_val;
   float float_val;
   char ch_val;
   char str_val[BUFSIZE];


   /* PUT A NULL CHARACTER AT END OF FILE NAME */
   if (*panel < 10) {
      inp_fname[4] = '\0';
   }

   /* OPEN THE INPUT FILE */
   if ( (fp = fopen(inp_fname,"r"))==NULL) {
      printf("Can't open %s\n",inp_fname);
      exit(-1);
   }

   while ( !feof(fp) ) {

      fgets(buf, BUFSIZE, fp);

      /* Only change values if line doesn't
	 have a '#' (comment) preceeding it */

      if ( strstr(buf, "HORIZ_BEG") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", h1);}

      } else if ( strstr(buf, "HORIZ_END") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", h2);}

      } else if ( strstr(buf, "VERT_BEG") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", v1);}

      } else if ( strstr(buf, "VERT_END") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", v2);}

      } else if ( strstr(buf, "PRI_GRID1") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", pri1);}

      } else if ( strstr(buf, "PRI_GRID2") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", pri2);}

      } else if ( strstr(buf, "SEC_GRID1") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", sec1);}

      } else if ( strstr(buf, "SEC_GRID2") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", sec2);}

      } else if ( strstr(buf, "SCALE_FLAG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {
	    sscanf(str_val, "%4s", scale_flag);
	    scale_flag[strlen(scale_flag)] = '\0';
         }

      } else if ( strstr(buf, "SCALE_POS") && *panel == 1) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {
	    sscanf(str_val, "%4s", scale_pos);
	    scale_flag[strlen(scale_flag)] = '\0';
         }

      } else if ( strstr(buf, "SCALE_W") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", scale_w);}

      } else if ( strstr(buf, "SCALE_H") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", scale_h);}

      } else if ( strstr(buf, "ZERO_LINEWIDTH") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", zlinew);}

      } else if ( strstr(buf, "#LINEWIDTH") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", linew);}

      } else if ( strstr(buf, "DASH_ON") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", dash_on);}

      } else if ( strstr(buf, "DASH_OFF") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", dash_off);}

      } else if ( strstr(buf, "PTSKIP") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", skip);}

      } else if ( strstr(buf, "VEC_SPEED") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", vec_speed);}

      } else if ( strstr(buf, "VEC_LENGTH") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", vec_len);}

      } else if ( strstr(buf, "ARROW_H ") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", arrow_h);}

      } else if ( strstr(buf, "ARROW_HH") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", arrow_hh);}

      } else if ( strstr(buf, "ARROW_HT") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", arrow_ht);}

      } else if ( strstr(buf, "FONT_TITLE") && *panel == 1) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", font_title);}

      } else if ( strstr(buf, "FONT_NUM") && *panel == 1) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", font_num);}

      } else if ( strstr(buf, "FONT_LABEL") && *panel == 1) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%d", font_label);}

      } else if ( strstr(buf, "STRETCH_HORIZ") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", stretch_horiz);}

      } else if ( strstr(buf, "STRETCH_VERT") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", stretch_vert);}

      } else if ( strstr(buf, "SP_TEXT") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", sp_text);}

      } else if ( strstr(buf, "SP_SCALE") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", sp_scale);}

      } else if ( strstr(buf, "SP_ROW") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", sp_row);}

      } else if ( strstr(buf, "SP_COL") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", sp_col);}

      } else if ( strstr(buf, "SP_TITLE") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", sp_title);}

      } else if ( strstr(buf, "TITLE_FLAG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {
	    sscanf(str_val, "%4s", title_flag);
	    title_flag[strlen(title_flag)] = '\0';
         }

      } else if ( strstr(buf, "GRID_FLAG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {
	    sscanf(str_val, "%5s", grid_flag);
	    grid_flag[strlen(grid_flag)] = '\0';
         }

      } else if ( strstr(buf, "PANEL_W") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", panel_w);}

      } else if ( strstr(buf, "PANEL_H") ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", panel_h);}

      } else if ( strstr(buf, "PAGE_W") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", page_w);}

      } else if ( strstr(buf, "PAGE_H") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", page_h);}

      } else if ( strstr(buf, "L_MARG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", l_marg);}

      } else if ( strstr(buf, "R_MARG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", r_marg);}

      } else if ( strstr(buf, "T_MARG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", t_marg);}

      } else if ( strstr(buf, "B_MARG") && *panel == 1 ) {
         fgets(str_val, BUFSIZE, fp);
	 if (!strstr(str_val, "#") ) {sscanf (str_val, "%f", b_marg);}
      }

   } /* end while */

   fclose(fp);
}
