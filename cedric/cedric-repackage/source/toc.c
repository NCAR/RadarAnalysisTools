#include <stdio.h>
#include "cedric.h"

/*
 * This program reads a cedric format file and gives a brief
 * summary of its contents.
 */
main(argc,argv) 
     int argc;
     char *argv[];
{
  char string[57];
  char ident[5];
  char exp_name[5];
  char radar_name[7];
  int i, j, count;
  int ival, rval, addr;
  char start_year[3], start_month[3], start_day[3], start_hour[3];
  char start_minute[3], start_second[3], end_year[3], end_month[3];
  char end_day[3], end_hour[3], end_minute[3], end_second[3];
  FILE *fp;

  ident[4] = '\0';
  string[56] = '\0';
  exp_name[5] = '\0';
  radar_name[6] = '\0';
  start_year[2] = '\0';
  start_month[2] = '\0';
  start_day[2] = '\0';
  start_hour[2] = '\0';
  start_minute[2] = '\0';
  start_second[2] = '\0';
  end_year[2] = '\0';
  end_month[2] = '\0';
  end_day[2] = '\0';
  end_hour[2] = '\0';
  end_minute[2] = '\0';
  end_second[2] = '\0';


  if (argc < 2) { /* too few arguments */
    printf("usage: %s filename\n",argv[0]);
    exit(-1);
  }

  i = 1;
  printf("   TABLE OF CONTENTS OF CEDRIC FORMAT FILES\n\n");

  while( i < argc) {
    fp = fopen(argv[i],"r");
    if (fp == NULL) {
      printf("+++ERROR OPENING %s FOR READING \n",argv[i]);
      exit(-1);
    }
    rval = fread(ident, 4, 1, fp);
    if (rval <= 0) {
      printf("+++ERROR READING FROM %s\n",argv[i]);
      exit(-1);
    }
    
    if (strcmp(ident, CED) != 0) {
      printf("+++INPUT FILE FORMAT NOT RECOGNIZED+++\n");
      exit(-1);
    }

    printf("CONTENTS OF FILE:%s\n",argv[i]);
    printf("VOL. #  EXPERIMENT NAME  RADAR NAME  STARTING DATE/TIME    ENDING DATE/TIME\n");
    count = 0;
    fseek(fp,16,0);  /* seek to start of volume addresses */
    for (j = 0; j < 25; j++) {
      ival = fread(&addr, 4, 1, fp);
      if (ival <= 0) {
	printf("error reading %s\n",argv[i]);
	exit(-1);
      }
      if (addr > 0) count++;
    }

    for (j = 0; j < count; j++) {
      ival = fread(string, 1, 56, fp);
      if (ival <= 0) {
	printf("error reading %s\n",argv[i]);
	exit(-1);
      }
      exp_name[0] = string[0];
      exp_name[1] = string[1];
      exp_name[2] = string[2];
      exp_name[3] = string[3];

      radar_name[0] = string[4];
      radar_name[1] = string[5];
      radar_name[2] = string[6];
      radar_name[3] = string[7];
      radar_name[4] = string[8];
      radar_name[5] = string[9];

      start_year[0] = string[10];
      start_year[1] = string[11];
      start_month[0] = string[12];
      start_month[1] = string[13];
      start_day[0] = string[14];
      start_day[1] = string[15];
      start_hour[0] = string[16];
      start_hour[1] = string[17];
      start_minute[0] = string[18];
      start_minute[1] = string[19];
      start_second[0] = string[20];
      start_second[1] = string[21];
      if (start_month[0] == ' ') start_month[0] = '0';
      if (start_day[0] == ' ') start_day[0] = '0';
      if (start_hour[0] == ' ') start_hour[0] = '0';
      if (start_minute[0] == ' ') start_minute[0] = '0';
      if (start_second[0] == ' ') start_second[0] = '0';

      end_year[0] = string[22];
      end_year[1] = string[23];
      end_month[0] = string[24];
      end_month[1] = string[25];
      end_day[0] = string[26];
      end_day[1] = string[27];
      end_hour[0] = string[28];
      end_hour[1] = string[29];
      end_minute[0] = string[30];
      end_minute[1] = string[31];
      end_second[0] = string[32];
      end_second[1] = string[33];
      if (end_month[0] == ' ') end_month[0] = '0';
      if (end_day[0] == ' ') end_day[0] = '0';
      if (end_hour[0] == ' ') end_hour[0] = '0';
      if (end_minute[0] == ' ') end_minute[0] = '0';
      if (end_second[0] == ' ') end_second[0] = '0';

      printf("  %d         %s         %s       %s/%s/%s %s:%s:%s     %s/%s/%s %s:%s:%s\n", 
              j+1,exp_name, radar_name, start_day, start_month, start_year, 
              start_hour, start_minute, start_second, end_day,end_month,
	      end_year,end_hour,end_minute,end_second);
    }

    i++;
  }
      
      
      
      
      


  
		       
}
