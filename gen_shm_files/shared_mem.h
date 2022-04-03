#include <string.h>
#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
//#include <sys/ipc.h>
//#include <sys/shm.h>
//#include <semaphore.h>


  #define MAX_TYPE         15
  #define MAX_SBN        1500       //Make sure this is 1 more than and particular type (e.g. 01499xxxx.cm)
                                    //bc sbn numbers are from 1-1499, but arrays are [0],...,[1499]
  #define MAX_HRU          14
  //                                 MAX_TYPE* MAX_SBN * MAX_HRU=294,000 x 64 = 18,816,000
  #define MAX_SBNxMAX_HRU     82000    // 1276 x 64 = 81,664
  #define MAX_TYPE_FILE       35000    // 34,534
  #define MAX_FILE           250000    // 249,

  #define MAX_FILE_CHAR          14
  #define MAX_TYPE_CHAR           4

  #define MAX_LINE              500


const char  shm_meta_name[]="/swat_meta";
const char  shm_data_name[]="/swat_data";
//# of files 1=19, 2=1K  3=5K    3     3       3     1      3     2     1     2     3       3      3     2     2     2     2  swat
int            file_counts[];
//ng        file_max_sizes[]={ 1792,   1393,  3427, 2653,  1999, 4541, 3104, 1782,  485,   2015,  1647,  4672, 2968, 1208,  570  };
long        file_max_sizes[]={ 1793,   1395,  3428, 2653,  2000, 4541, 3104, 1782,  486,   2016,  1648,  9972, 2968, 1208,  570  };
enum              file_suffs { chm ,    gw ,  hru , lwq ,  mgt , pnd , res , rte ,  sdr ,   sep,  sol ,  sub , swq , wgn , wus , N_TYPES  };
const char * file_suffixes[]={"chm",   "gw", "hru","lwq", "mgt","pnd","res","rte", "sdr", "sep", "sol", "sub","swq","wgn","wus" };
//                                0       1     2     3      4     5     6     7      8      9     10     11    12    13    14
// HRU_MAX                       64      64    64     1     64     1     1     1     64     64     64      1     1     1     1 
// SBN_MAX=1276 (all)except                        1261               1261
//    No of files            {34535,  34534, 34534,   19, 34534, 1276,   19, 1276, 34534, 34534, 34534,  1276, 1276, 1276, 1276  };
//int   type_file_ct[N_TYPES];
//long  type_data_sz[N_TYPES];

//char  *Directory            = "/scratch/00770/milfeld/SWAT/HRU/Data/";
//char  *Directory            = "/scratch/00770/milfeld/Data/";
//char  *Directory_info_file  = "/scratch/00770/milfeld/Data/dir_info";

// commented out 12/30/2021
//char  *Data_dir            = "/scratch/00770/milfeld/SWAT2/modeldata/Wabash/Data/";
//char  *Data_info_file      = "/scratch/00770/milfeld/SWAT2/modeldata/Wabash/Data/dir_info";

  char  *Data_dir            = "/scratch/00770/milfeld/SWAT_DATA/TEST18/";
  char  *Data_info_file      = "/scratch/00770/milfeld/SWAT_DATA/TEST18/dir_info";

//char  *Data_dir            = "/scratch/00770/milfeld/SWAT2/modeldata/Wabash/Data.chm/";
//char  *Data_info_file      = "/scratch/00770/milfeld/SWAT2/modeldata/Wabash/Data.chm/dir_info";

//char  *Data_dir            = "/scratch/00770/milfeld/Data3/";
//char  *Data_info_file      = "/scratch/00770/milfeld/Data3/dir_info";
//char  *Data_dir            = "/scratch/00770/milfeld/SWAT15/modeldata/Wabash/Data";
//char  *Data_info_file      = "/scratch/00770/milfeld/SWAT15/modeldata/Wabash/Data/dir_info";

//char  *Data_dir            = "/scratch/00770/milfeld/KENT/modeldata/TxtInOut_Wabash/Data/";
//char  *Data_info_file      = "/scratch/00770/milfeld/KENT/modeldata/TxtInOut_Wabash/Data/dir_info";
//char  *Data_dir            = "/home1/00770/milfeld/IO/GEN10/TEST/";
//char  *Data_info_file      = "/home1/00770/milfeld/IO/GEN10/TEST/dir_info";
//                               directory   MUST END IN   "/"

int c_verbose;

//   No need to change anything below

//long put_shm_meta(int file_cnt, long *Offset, long *Offndx);
long put_shm_meta(int file_ct[N_TYPES], long  Offset[N_TYPES][MAX_SBN*MAX_HRU+1], long Offndx[N_TYPES][MAX_SBN*MAX_HRU]);

//int  put_shm_data(              long *Offset, long *Offndx);

long put_shm_data(
    long    Offset[N_TYPES][MAX_SBN*MAX_HRU+1], long Offndx[N_TYPES][MAX_SBN*MAX_HRU], int FN_cnt,
    char *Data_dir,
    int     hrumax[MAX_TYPE],
    int type_found[MAX_TYPE],
    int    bsn_cnt[MAX_TYPE],
    int    hru_cnt[MAX_TYPE][MAX_SBN ],
    char   data_fn[MAX_TYPE][MAX_TYPE_FILE][MAX_FILE_CHAR],
    long   data_sz[MAX_TYPE],
    int    file_ct[MAX_TYPE]); 

void   get_shm_meta(int *file_cnt, long *Offset, long *Offndx);
char * get_shm_data_ptr(               int *shm_data_fd);
void   close_shm_data(char *shm_data_ptr, int shm_data_fd);
void   ulink_shm_data(char *shm_data_name);

void   get_file_nums(char *file_name, int *type_no, int *sbn_no, int *hru_no);

char * get_file_data(char *shm_data_ptr,   long int *Offset, long int *Offndx,
                     char *file_name,      long int *start,  long int *end, int *ln_cnt,  int *type_no);
//                   char *file_name,      long int *start,       int *ln_cnt,  int *type_no);

void set_c_verbosity(int f_verbose);




void print_data(char *shm_data_ptr);

#define     N_SUFFIXES  (sizeof(file_suffixes)/sizeof(const char *))
//#define  OFFNDX(i,j,k)  Offndx[MAX_SBNxMAX_HRU*(i) + MAX_HRU*(j) + (k)]
  #define  OFFNDX(i,j,k)  Offndx[i][MAX_HRU*(j) + (k)]

#define error_handler(msg)  do { perror(msg); exit(EXIT_FAILURE); } while (0)

int get_data_FNs(char *path_name,                   int  hrumax[MAX_TYPE],
                 int type_found[MAX_TYPE],          int bsn_cnt[MAX_TYPE],
                 int    hru_cnt[MAX_TYPE][MAX_SBN],
                 char type_data_fn[MAX_TYPE][MAX_TYPE_FILE][MAX_FILE_CHAR],
                 long type_data_sz[MAX_TYPE],
                 int  type_file_ct[MAX_TYPE]);

int  get_db_policy();
long get_shm_size(char * Data_dir, char * dir_info_path);
