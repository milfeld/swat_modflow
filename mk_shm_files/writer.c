//  3420*8 = 27360 -> 28000
#include  <stdio.h>
#include <stdlib.h>
#include <string.h>
#include  <ctype.h>

#include  <dirent.h>
#include <stdbool.h>

#include "shared_mem.h"

#include <omp.h>

#define SWAT_DEBUG_READ_FILE_NAMES 0
#define SWAT_DEBUG_WRITE_DATA 0
#define SWAT_DEBUG_WRITE_META 0

double gtod_timer();
void get_fn_cmd(char *fn_cmd, char *Data_dir, const char *suffix);

int main(void){

  double t0, t1;

  //------------------------------------ file metadata
  int  file_cnt;
  long shm_data_sizeB, shm_meta_sizeB;

  long Offset[N_TYPES][MAX_SBN*MAX_HRU+1];
  long Offndx[N_TYPES][MAX_SBN*MAX_HRU];

  int  ndx_size  = MAX_SBN*MAX_HRU;
  int  set_size  = MAX_SBN*MAX_HRU + 1;
  //--------------------------------------------------

  //--------------------------------------------------
  int     hrumax[MAX_TYPE];
  int type_found[MAX_TYPE];
  int    bsn_cnt[MAX_TYPE];
  int    hru_cnt[MAX_TYPE][MAX_SBN];
  char  type_str[MAX_TYPE][MAX_TYPE_CHAR];
  char   data_fn[MAX_TYPE][MAX_TYPE_FILE][MAX_FILE_CHAR];
  long   data_sz[MAX_TYPE];
  int    file_ct[MAX_TYPE];
  //--------------------------------------------------

  for(int j=0; j<N_TYPES; j++){
     for(int i=0; i<ndx_size; i++) Offndx[j][i] =(long)-1;
     for(int i=0; i<set_size; i++) Offset[j][i] =(long) 0;
  }

  //v=========================================================
    file_cnt=                                                \
        get_data_FNs( Data_dir,   
                      hrumax,
                      type_found, 
                      bsn_cnt, 
                      hru_cnt,    
                      data_fn,
                      data_sz,
                      file_ct);
    printf(" ->  Done:  File names collected   (%11d  )\n", file_cnt);
  //^=========================================================


  //v=========================================================
t0=gtod_timer();
    shm_data_sizeB=
        put_shm_data( Offset,     
                      Offndx, 
                      file_cnt,
                      Data_dir,   
                      hrumax,
                      type_found, 
                      bsn_cnt,  
                      hru_cnt,
                      data_fn,
                      data_sz,
                      file_ct);
t1=gtod_timer();

   for(int isbn=1; isbn<1277; isbn++){
      int ioffndx=OFFNDX(11,isbn, 0);
      printf("Zitype= 11, is= %4d, ioffndx=%4d,  OFFNDX= % 5ld,  Offset== %8ld\n", \
                          isbn, ioffndx, Offndx[11][MAX_HRU*isbn], Offset[11][ioffndx]);
   }
   for(int isbn=1; isbn<1277; isbn++){
      int ioffndx=OFFNDX(10,isbn, 1);
      printf("Titype= 10, isbn= %05d, ioffndx=%5d,  OFFNDX= % 5ld,  Offset== %8ld\n", isbn, ioffndx, Offndx[10][MAX_HRU*isbn+1], Offset[10][ioffndx]);
   }
   

//  printf(" ->  Done:      data in shared mem (%11ld B)\n", \
//                                   shm_data_sizeB);
    printf(" ->  Reserved storage for data     (%11ld B)\n", \
                                     shm_data_sizeB);
    printf(" ->  Done:      data in shared mem (%11ld B)\n", \
                                     Offset[file_cnt]);
printf(" -> time(s) = put_shm_data  %8.3f\n",t1-t0);
  //^=========================================================

  //v=========================================================
    shm_meta_sizeB=                                          \
        put_shm_meta(file_ct, Offset, Offndx);
    printf(" ->  Done:  Metadata in shared mem (%11ld B)\n", \
                                         shm_meta_sizeB);
  //^=========================================================

  return 0;

}

//v---------------------------PARALLEL------------------------------v//

long put_shm_data(
    long    Offset[N_TYPES][MAX_SBN*MAX_HRU+1], long Offndx[N_TYPES][MAX_SBN*MAX_HRU], int FN_cnt,
    char *Data_dir,
    int     hrumax[MAX_TYPE],
    int type_found[MAX_TYPE],
    int    bsn_cnt[MAX_TYPE], 
    int    hru_cnt[MAX_TYPE][MAX_SBN ],
    char   data_fn[MAX_TYPE][MAX_TYPE_FILE][MAX_FILE_CHAR],
    long   data_sz[MAX_TYPE],
    int    file_ct[MAX_TYPE])
{

  long lfile_cnt;

  DIR   *directory = NULL;
  struct dirent *ent;

  long   shm_data_size=0;

//long shm_data_size=get_shm_size(Data_dir, Data_info_file);
//     shm_data_size+=FN_cnt*8;                           //bc \0 added at end of each file

//                                    if(SWAT_DEBUG_WRITE_DATA == 1)
//                                    printf(" -> Raw file storage in Data Dir=%ldB, need + %dB for EOFs: Reserving %ldB for shm data\n", \
//                                    shm_data_size-FN_cnt*8,FN_cnt*8,shm_data_size);

   directory = opendir (Data_dir);    // Directory in shared_mem.h
                                       if(directory == NULL) exit(1);

  #pragma omp parallel num_threads(N_TYPES) reduction( + : shm_data_size )
 {
  int itype = omp_get_thread_num();

  char  shm_data_p_name[128];   // only needs 14, just in case "swat_data" name is changed, using 128

  int   shm_data_p_fd;     // file descriptor
  char *shm_data_p_ptr;    // base address, from mmap()
  long  shm_data_p_size;   // (Over-) Estimated storage (B) for all files of a type


  char   path_p_name[          512];
  char          p_fn[MAX_FILE_CHAR];
  FILE  *p_fd ;
  int   file_p_cnt;

  int   sbn_base,     hru_base;
  char  sbn_chars[8], hru_chars[8];

  char *suffix_ptr, *suffix_end_ptr;
  int   suffix_len;
  int   type_no;

  long   shm_p_pos, file_p_size, total_p_size=0;

   char * ptr;
   shm_data_p_name[0]='\0';
   strcat(shm_data_p_name,  shm_data_name);
   ptr=index(shm_data_p_name,'\0');
   *ptr++='_';
   strcpy(ptr, file_suffixes[itype]);
   shm_data_p_size = data_sz[itype] + 8*file_ct[itype];  //+8x#files: because FS character written at end of each file.

   printf("type:%2d   shm_data_p_size= %ld  data_sz:%5d   file_ct:%5d\n", itype, shm_data_p_size, data_sz[itype], file_ct[itype] );

   shm_data_p_fd = shm_open(shm_data_p_name, (int)(O_CREAT | O_RDWR), (mode_t)0666);
                                                                                    if (shm_data_p_fd == -1) 
                                                                                    {printf("shm_open  failed: %s\n", strerror(errno)); exit(1);}
   ftruncate(shm_data_p_fd, shm_data_p_size); // set size of shm segment

   shm_data_p_ptr = mmap(0, shm_data_p_size, PROT_READ | PROT_WRITE, MAP_SHARED, shm_data_p_fd, 0);
                                                                                                   if (shm_data_p_ptr == MAP_FAILED) 
                                                                                    {printf("shm mmap failed:   %s\n", strerror(errno)); exit(1);}

    sbn_base = 1;            // location of start of subbasin # in filename
    hru_base = sbn_base+4;   // location of start hru # in filename

    shm_p_pos=0;
    file_p_cnt=0;

    int knt=0;
    int ihru;

                                                                 //for(   int itype=0; itype< N_SUFFIXES;  itype++) if (type_found[itype]=1)
    if (type_found[itype]=1)
    {
      int max_hru=0; for(int itr=0; itr<=bsn_cnt[itype] ; itr++) if ( hru_cnt[itype][itr] > max_hru ) max_hru=hru_cnt[itype][itr];
      printf(" -> Starting:  type=  %2d  sbn=%6d  max_hru=%3d  suffix= %3s\n", itype, bsn_cnt[itype], max_hru, file_suffixes[itype]);
//     if(itype == 0){ for(int ii=0; ii<1500; ii++) printf("[%4d:%3d]\n",ii,hru_cnt[itype][ii]); }

        for(int isbn=1;  isbn <=bsn_cnt[itype];       isbn++)
        for(int jhru=1;  jhru <=hru_cnt[itype][isbn]; jhru++)
        {
          if(hrumax[itype] == 0){ ihru = 0;} else{ ihru=jhru; }  //some hrus have count of 1 and start at zero
                                                                 // for hrumax number = 0 set isbn to zero
    
                                                                 if(file_p_cnt >MAX_FILE){perror(" -> ERROR: FILE_MAX too small");  exit(EXIT_FAILURE);}

              sprintf(p_fn,"%0.5d%0.4d.%s",isbn,ihru,file_suffixes[itype]);
              strcpy(path_p_name,Data_dir);                                // Form full name of file: dir. + file_name
              strcat(path_p_name,"data_");                                // Form full name of file: dir. + file_name
              strcat(path_p_name,file_suffixes[itype]);                                // Form full name of file: dir. + file_name
              strcat(path_p_name,"/");                                // Form full name of file: dir. + file_name
              strcat(path_p_name,p_fn);
//printf("THREAD/TYPE: %0.3d   Data_dir=%s p_fn=%-15s path_p_name=%s\n",itype,Data_dir,p_fn,path_p_name );
    
              p_fd = fopen(path_p_name, "rb");
              fseek(p_fd, 0, SEEK_END);
              file_p_size = ftell(p_fd);
//printf(" ***THREAD %0.3d   SIZE %10ld file: %s\n",itype,file_p_size,path_p_name);
              fseek(p_fd, 0, SEEK_SET);
    
//     
              fread(&shm_data_p_ptr[shm_p_pos            ], 1, (size_t)file_p_size, p_fd);
                     shm_data_p_ptr[shm_p_pos + file_p_size]=(char)28;
    
                                                                    //   printf("shm_p_pos:%d %s: \n%s\n",shm_p_pos, p_fn, &shm_data_p_ptr[shm_p_pos]);
                  if(SWAT_DEBUG_WRITE_DATA == 1)
                     printf(" File# sbn hru ( %3d %3d %3d ) No_files: %5d, File_size: %5d, shm_p_pos: %9d  %s\n", \
                                      itype, isbn, ihru,    file_p_cnt,   file_p_size,      shm_p_pos,      p_fn);
    
//
              OFFNDX(itype,isbn,ihru)   = (long)file_p_cnt;  //  #define  OFFNDX(i,j,k)  Offndx[i][MAX_HRU*(j) + (k)]
              Offset[itype][file_p_cnt] =        shm_p_pos;
if(itype == 11){
   int ioffndx=OFFNDX(itype,isbn,ihru);
   printf("Aitype= %02d, isbn= %05d ihru= %04d, file_p_cnt= %05d, OFFNDX= %05d,  shm_p_pos= %8ld\n", itype,isbn,ihru,file_p_cnt,ioffndx,shm_p_pos);
}
    
              fclose(p_fd);
              total_p_size += file_p_size+1;
              shm_p_pos    += file_p_size+1;
                                                                   //printf("HERE1 file_p_cnt=%d %0.4ld %0.4ld\n",file_p_cnt,isbn,ihru);
              file_p_cnt++;
    
        } // end jhru loop

    }  // end if type found

                                             if(SWAT_DEBUG_WRITE_DATA == 1)
                                             printf(" File# sbn hru (             ) No_files:      , File_size:      , shm_p_pos: %9d\n", shm_p_pos);


// pOffset[itype,file_p_cnt] =shm_p_pos;
/// Offset[file_p_cnt] = shm_p_pos;                       // points to position after last '\0' (for finding len of last data file)

                                                          //unmap shm from process address space
    if (munmap(shm_data_p_ptr, shm_data_p_size) == -1)  { printf("shm munmap failed: %s\n", strerror(errno)); exit(1); }
  
    if (close(shm_data_p_fd) == -1)                     { printf("shm close failed:  %s\n", strerror(errno)); exit(1); }
    shm_data_size += total_p_size;

printf("THREAD %d   SIZE %ld\n",itype,total_p_size);


 }  //end OMP parallel block

    return shm_data_size;

//if (shm_unlink(shm_data_name) == -1)   { printf("shm_unlink %s: %s\n", shm_data_name, strerror(errno)); exit(1); }
 
}

//^---------------------------PARALLEL------------------------------^//


long put_shm_meta(int  file_ct[N_TYPES],
                  long  Offset[N_TYPES][MAX_SBN*MAX_HRU+1], 
                  long  Offndx[N_TYPES][MAX_SBN*MAX_HRU])
{
                  int   ndx_size  = MAX_SBN*MAX_HRU;
                  int   set_size  = MAX_SBN*MAX_HRU + 1;
  long meta_bsize = sizeof(long) + sizeof(long)*(set_size) + sizeof(long)*ndx_size;

 #pragma omp parallel num_threads(N_TYPES) // end omp parallel
 {
   int itype = omp_get_thread_num();

   long lcnt = 0;

   char shm_meta_p_name[128];   // only needs 14, just in case "swat_data" name is changed, using 128

   int   shm_meta_fd;     // file descriptor
   long *shm_meta_ptr;    // base address, from mmap()
   char * ptr;

   shm_meta_p_name[0]='\0';
   strcat(shm_meta_p_name,  shm_meta_name);
   ptr=index(shm_meta_p_name,'\0');
   *ptr++='_';
   strcpy(ptr, file_suffixes[itype]);

   printf("META_SIZE=%ld(B)  %ld(element) pname:%s \n",meta_bsize, meta_bsize/sizeof(long), shm_meta_p_name );

   shm_meta_fd = shm_open(shm_meta_p_name, (int)(O_CREAT | O_RDWR), (mode_t)0666);
                                                    if (shm_meta_fd == -1) 
                                                    { printf("shm_open failed: %s\n", strerror(errno)); exit(1); }

   ftruncate(shm_meta_fd, (size_t)meta_bsize);   // set size of shm segment
   shm_meta_ptr = (long *)mmap(0, (size_t)meta_bsize, PROT_READ | PROT_WRITE, MAP_SHARED, shm_meta_fd, 0);
                                                    if (shm_meta_ptr == MAP_FAILED)
                                                    { printf("shm mmap failed:   %s\n", strerror(errno)); exit(1); }
   //****************************************************************************************{

   lcnt=(long) file_ct[itype];

         /*if(SWAT_DEBUG_WRITE_META == 1) 
             {
             printf("  lcnt    =%4d type=%2d sizeof(long)=%2d\n",lcnt,itype,sizeof(long));
             printf("  ndx_size=%8d  ndx_size*8=%8d\n",ndx_size,ndx_size*8);
             printf("  set_size=%8d  set_size*8=%8d\n",set_size,set_size*8);

             }
         */
          //for(int k=0;k<ndx_size;k++){ printf("  %8ld   Offndx=%ld\n",k,Offndx[k]);}

          printf("  type=%2d  lcnt=%5d  meta_bsize=%8d  sizeof(long)=%2d  ndx_size=%8d  set_size=%8d \n",
                    itype,    lcnt,      meta_bsize,     sizeof(long),    ndx_size,     set_size);
/*
   memcpy( shm_meta_ptr,                  &lcnt,        (size_t)sizeof(long)         );
   memcpy(&shm_meta_ptr[           1 ],  Offndx[itype], (size_t)sizeof(long)*ndx_size);
   memcpy(&shm_meta_ptr[ndx_size + 1 ],  Offset[itype], (size_t)sizeof(long)*set_size);
*/
   memcpy( shm_meta_ptr,                  &lcnt,             (size_t)sizeof(long)         );
   memcpy(&shm_meta_ptr[           1 ],   &Offndx[itype][0], (size_t)sizeof(long)*ndx_size);
   memcpy(&shm_meta_ptr[ndx_size + 1 ],   &Offset[itype][0], (size_t)sizeof(long)*set_size);

if(itype == 11){
   for(int ii=0; ii<1276;ii++){
   int Index= MAX_HRU*ii;
   printf("Xitype= %02d, Offndx,Offset= %05d,%08d,     Index=%d\n",itype,Offndx[itype][Index],Offset[itype][ii], Index);
   }
}

   //****************************************************************************************{

   if (munmap(shm_meta_ptr, (size_t)meta_bsize) == -1) { printf("shm munmap failed: %s\n", strerror(errno)); exit(1); }
   if (close( shm_meta_fd) == -1)                      { printf("shm close failed:  %s\n", strerror(errno)); exit(1); }

 } // end omp parallel

   return meta_bsize;

}// put_shm_meta 

//struct stat file_stat;
/*

  file names:   0sssshhhh.tttt
                 ssss           4   digits for subbasin
                     hhhh       4   digits for hru
                          tttt  3-4 digits for type ("suffix")

 Reads a set of files in a Directory and inserts them in shared memory
 Metadata is also written to share memory.

 Directory:     Directory path of data files = "/.../" (with / at end)
 shm_data_name: File name for Shared Memory     data = "/swat_data"
 shm_meta_name: File name for Shared Memory metadata = "/swat_meta"

 total size:    Size of all files in shared memory + 1 extra byte for each file

 Space:
 Files are stored as stream, as is, and appended with '\0' at end.

 swat_data size(bytes) for N files is:  N + sum of files sizes in bytes (total_size)

 swat_meta size(bytes) is: (1 +  MAX_TYPE*MAX_SBN*MAX_HRU + MAX_FILE + 1 )*sizeof(long)
                            ^- no of elements is written at beginning

                 ndx_t    ndx_s   ndx_h
     for OFFSET(MAX_TYPE,MAX_SBN,MAX_HRU)
     for OFFLEN(MAX_TYPE,MAX_SBN,MAX_HRU)

     file suffix name = specifies the type of file
     enum file_suffix relates suffix(the type) to a number {~15 different suffixes}

     type        no   = ndx_t
     subbasin    no   = ndx_s
     hru         no   = ndx_h

*/


/*
int main(){

    int FN_cnt;
    int        hrumax[MAX_TYPE];
    int    type_found[MAX_TYPE];  
    int       bsn_cnt[MAX_TYPE];
    int       hru_cnt[MAX_TYPE][MAX_SBN ];
    char type_data_fn[MAX_TYPE][MAX_TYPE_FILE][MAX_FILE_CHAR];

FN_cnt = get_data_FNs(
             Data_dir,
             hrumax,
             type_found,
             bsn_cnt,
             hru_cnt,
             type_data_fn,
             type_data_sz);

for(int i=0; i<N_TYPES; i++) printf(" %15s %8ld\n", type_data_fn[i],type_data_sz[i]);

}
*/

int get_data_FNs(char *Data_dir,
                 int         hrumax[MAX_TYPE], 
                 int     type_found[MAX_TYPE],
                 int        bsn_cnt[MAX_TYPE], 
                 int        hru_cnt[MAX_TYPE][MAX_SBN],
                 char  type_data_fn[MAX_TYPE][MAX_TYPE_FILE][MAX_FILE_CHAR],
                 long  type_data_sz[MAX_TYPE],
                 int   type_file_ct[MAX_TYPE]){

               //char   data_fn[MAX_FILE][MAX_FILE_CHAR]) {


  int FN_cnt=0;   // Return total file count (for all types)


// scratch/00770/milfeld/SWAT2/modeldata/Wabash  prg.c
// Get files here:

  for(int it=0; it<MAX_TYPE;it++) {   hrumax[it]=0 ;  
                                  type_found[it]=-1;   //initialized to "not found" = -1
                                     bsn_cnt[it]=0 ; }
  for(int it=0; it<MAX_TYPE;it++)
  for(int is=0; is<MAX_SBN; is++) {  hru_cnt[it][is]=0;}

//int hru_count[MAX_SBN];


//#pragma omp parallel num_threads(15) reduction(+:FN_cnt) private(hru_count)
//#pragma omp for schedule(dynamic)
  for(int type=0; type<N_TYPES; type++)
  {
   //for(int is=0; is<MAX_SBN; is++) {hru_count[is]=0;}

     int  sub_no, hru_no, not_found;
     char sub_str[6],      hru_str[5];
          sub_str[5]='\0'; hru_str[4]='\0';

     char fn_cmd[512];
     get_fn_cmd(fn_cmd, Data_dir, file_suffixes[type]);
//   printf(" HERE0 type= %d,  Data_dir= %s,  fn_cmd=%s\n",type, Data_dir, fn_cmd);

     FILE *file_ptr = popen(fn_cmd, "r");
     char file_name[256];

     int  bsn_max=0;
     int  hru_max=0;
     int type_cnt=0;

     while (fgets(file_name, sizeof(file_name), file_ptr) != 0) {


        file_name[strcspn(file_name,"\n")] = 0;  // removes LF
      //printf("%d >%s<\n",type, file_name);
   
        int is_data=1;
        for (int i=0; i<8; i++){ if( ! isdigit(file_name[i]) ) is_data=0;}
   
        if(is_data){
           char *ptr=strstr(file_name,".");   ptr++;
   
           if(type_cnt == MAX_TYPE_FILE-1){ printf(" -> ERROR: Increase size of MAX_TYPE_FILE(%d).\n",MAX_TYPE_FILE); printf(" %d \n",type_cnt); exit(1);}
           strcpy(type_data_fn[type][type_cnt++],file_name);
   
           strncpy(sub_str,file_name,5);
           for(int k=0; k<4;k++) hru_str[k]=file_name[k+5];
           sub_no= atoi(sub_str);
           hru_no= atoi(hru_str);
   
           if( sub_no >  bsn_max )  bsn_max = sub_no;  // sub_no always begins with 1
           if( hru_no >  hru_max )  hru_max = hru_no;  // hru_no may start with 0 or 1
   
           hru_cnt[type][sub_no]++;
   
           //if(SWAT_DEBUG_READ_FILE_NAMES == 1)
    //printf("get_data_FN--------------------------- %15s %4d  bsnMax:%4d nruMax:%3d\n",file_name,sub_no,bsn_max,hru_max);
        }
     }//while end FN
     pclose(file_ptr);

     FN_cnt += type_cnt;

     type_file_ct[type] = type_cnt;
     type_data_sz[type] = type_cnt * file_max_sizes[type];

     if(type_cnt >0 ) type_found[type] = 1;

      hrumax[type] = hru_max;
     bsn_cnt[type] = bsn_max;
   //for(int isn=0; isn < MAX_SBN; isn++) hru_cnt[type][isn] = hru_count[isn];

     printf("type=%0.3d type=%4s bsn_max=%0.6d hru_max=%0.6d type_cnt=%0.6d\n", type,file_suffixes[type],bsn_max,hru_max,type_cnt);

  }//end type loop

       if(SWAT_DEBUG_READ_FILE_NAMES == 1) for(int i=0;i<N_SUFFIXES;i++) for(int type=0; i<N_TYPES; i++)
      { if(type_found[type]==1) printf(" found file type: %3d %s\n",type,file_suffixes[type]); }
  return FN_cnt;
}


void get_fn_cmd(char *fn_cmd, char *Data_dir, const char *suffix){

   // construct command to extract specific type files:
   // e.g.  \ls -A Data.chm | grep chm\$ | sort >Data.chm/chm

//printf("INSIDE FN_CMD %s \n",suffix);

   char *cptr;
   char cmd_ls[]       = "\\ls -A ";
   char cmd_p_grp[]    = "| grep ";
   char cmd_end[]      = "\\$ ";
 //char cmd_end_sort[] = "\\$ | sort ";
 //char cmd_redirect[] = ">";
 //char cmd_path_sep[] = "/";

  cptr =strcpy(fn_cmd,        cmd_ls );
  cptr =strcat(fn_cmd,      Data_dir );
  cptr =strcat(fn_cmd,      "data_" );
  cptr =strcat(fn_cmd,      suffix );
  cptr =strcat(fn_cmd,      "/" );
  cptr =strcat(fn_cmd,     cmd_p_grp );
  cptr =strcat(fn_cmd,        suffix );
  cptr =strcat(fn_cmd,       cmd_end );

/*
  cptr =strcat(fn_cmd,  cmd_end_sort );
  cptr =strcat(fn_cmd,  cmd_redirect );
//cptr =strcat(fn_cmd,      Data_dir );
//cptr =strcat(fn_cmd,  cmd_path_sep );
  cptr =strcat(fn_cmd,      "." );
  cptr =strcat(fn_cmd,  cmd_path_sep );
  cptr =strcat(fn_cmd,        suffix );
  cptr =strcat(fn_cmd,       "_list" );
*/

     printf("fn_cmd %s \n",fn_cmd);

/*
     printf("%s \n",fn_cmd);
     exit(0);
  // \ls -A /scratch/00770/milfeld/SWAT2/modeldata/Wabash/Data/| grep chm\$  
*/

}

//https://c-for-dummies.com/blog/?p=1418
