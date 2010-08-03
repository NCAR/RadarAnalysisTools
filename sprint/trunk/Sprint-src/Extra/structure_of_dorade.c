void init_dorade_struct()
void free_dorade_struct()
void julday(juldat, iyr, imon, iday)
void print_vold(struct vold_blk vold,FILE *fpw,int iprint)
void print_radd(struct radd_blk radd,FILE *fpw, int iprint)
void print_parm(struct parm_blk parm,FILE *fpw,int iprint)
void print_celv(struct celv_blk celv,FILE *fpw, int iprint,float spacing)
void print_cfac(struct cfac_blk cfac,FILE *fpw, int iprint)
void print_swib(struct swib_blk swib,FILE *fpw, int iprint)
void  print_ryib(struct ryib_blk ryib,FILE *fpw, int iprint)
void  print_asib(struct asib_blk asib,FILE *fpw, int iprint)
  void print_asib();
void read_block(int    fd,
  void check_radar_name();
void rdbeam(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
void RDBEAM(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
void rdbeam_(inunit, irewnd, istat, ivolnum, iyr, mon,iday,
  void rdbeam2();
void rdbeam2(cradnam, inunit, irewnd, istat, ivolnum, iyr,
  void   radar_angles();
  void get_mode(int themode[100],int *modevalue)
  void GET_MODE(int themode[100],int *modevalue)
  void get_mode__(int themode[100],int *modevalue)
  void get_mode_(int themode[100],int *modevalue)
    void swack_short();
void swack_short(ss, tt, nn)
void set_radar(int rightradar)
void set_process()
void check_radar_name(char reqrad[9],int len)
void radar_angles( asib,rotation_angle,azimuth,elevation,the_tilt)
void rewnddor_()
void usrflds_(char name[8],int *j) 
void endtime_()
