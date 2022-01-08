#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "shared_mem.h"
#define  OFFSET(i,j,k) Offset[MAX_SBNxMAX_HRU*(i) + MAX_HRU*(j) + (k)]

#define SWAT_DEBUG_READ_META 1

long get_dev_shm_file_bsize(char * data_or_meta, int itype);

void set_c_verbosity(int f_verbose){ c_verbose=f_verbose; }

void get_file_nums(char *file_name, int *type_no, int *sbn_no, int *hru_no){
  // From file name get the three numbers to be used in Offndx to get data location.

  //----------------------------------------- variables: medadata no's from filenames
  int   k;
  const int       nchars_sbn=5,             nchars_hru=4;
  char  sbn_chars[nchars_sbn+1],  hru_chars[nchars_hru+1];

  char *suffix_ptr, *suffix_end_ptr;

  // Needs global file_suffixes; could use enum
  //-----------------------------------------


   suffix_ptr     = strrchr(file_name,'.');
   suffix_end_ptr = &file_name[ strlen(file_name) ];

   *type_no = -1;              // look for suffix in known suffixes (file_suffixes in shared_mem.h)
   suffix_ptr++;               // move pointer after dot (.)
   for(k=0; k<N_SUFFIXES; k++){ if ( ! strcmp(suffix_ptr,file_suffixes[k]) ){ *type_no=k; break; } }

   if(*type_no == -1){ printf("Non-existent file type (%s)  %s\n", suffix_ptr, strerror(errno)); exit(1); }

   sbn_chars[nchars_sbn]='\0'; hru_chars[nchars_hru]='\0';   // Set end of suffix string
                                                             // Copy suffix, then form integer 
   for(k=0;k<nchars_sbn;k++) sbn_chars[k] = file_name[           k]; *sbn_no = atoi(sbn_chars);
   for(k=0;k<nchars_hru;k++) hru_chars[k] = file_name[nchars_sbn+k]; *hru_no = atoi(hru_chars);
}

//char * get_file_data(char *shm_data_ptr,   long int *Offset, long int *Offndx,
  char * get_file_data(char *shm_data_ptr,   long  Offset[N_TYPES][MAX_SBN*MAX_HRU+1], 
                                             long Offndx[N_TYPES][MAX_SBN*MAX_HRU],
                       char *file_name,      long int *start,  long int *end,
                        int *ln_cnt,              int *typeno)
{
  //  returns pointer to shared memory location of file, and start and end for each line.

  //----------------------------- vars for file index values (type, sbn, hru)

  int type_no, sbn_no, hru_no;
  int ndx;

  //----------------------------- vars for start and line count determination

  char *data, *base, *ptr;
  int  i, tmp, single;

  //-----------------------------------------------------------------------------------------------
                               if(c_verbose>2) printf("v---@ (%d) %s\n",__LINE__, __FILE__);
                               if(c_verbose>0) printf(" -> get_file_data: filename=%s\n",file_name);
 
   get_file_nums(file_name, &type_no, &sbn_no, &hru_no);

                               if(c_verbose>1) printf(" -> type:sbn:hru (%0.2d %0.4d %0.4d) ",type_no,sbn_no,hru_no);
 
   ndx = Offndx[type_no][MAX_HRU*sbn_no + hru_no];

                               if(c_verbose>0) printf(" -> NDX=%d\n",ndx);
 
   if(ndx == -1){ 
      printf(" -> DB search for type:sbn:hru %d:%d:%d -- ndx:%d\n",type_no,sbn_no,hru_no,ndx);
      printf(" -> DB doesn't know about about file %s\n",file_name); exit(1); 
   }
 
 
   data = &shm_data_ptr[ Offndx[type_no][MAX_HRU*sbn_no + hru_no] ];

                               if(c_verbose>3) printf(" ->FIRST 100 CHARs at offset %ld\n",Offset[ndx]);
                               if(c_verbose>3){ for(int i=0;i<100;i++)  printf("%c",data[i]); printf("\n");}
 
   //-----------------------------------------------------------------------------------------------
 
   start[0] = 1;                   // first character index is 1 (Fortran numbering)
   *ln_cnt  = 0;                   // Initialize counter for number of lines
         i  = 1;
   base= data; ptr = data;       // Use pointers for counting characters to subsequent line returns
 
   //while(*++ptr != '\0') if(*ptr == '\n') {start[i++] = (int)(ptr - base)+2; *ln_cnt+=1;}
   //while(*++ptr != '\0')
   //  while(*++ptr != (char)255)
   while(*++ptr != (char)28)
   if(*ptr == '\n')
   {
      
      tmp =  (int)(ptr - base)+1;
 /*     if(i==11){
       //printf("HERE>\n");
       //for(int i=0;i<565;i++){ printf("%c",*(base+i) ); }
         printf("<HERE\n");
         printf("-2=%c\n",*(ptr-2));
         printf("-1=%c\n",*(ptr-1));
         printf(" 0=%c\n",*(ptr));
          
         for(int i=0;i<128;i++){ if( *(ptr+2) == (char)i ); } // printf("FOUND character %d\n",i); }
         for(int i=0;i<256;i++){ if( EOF == (char)i );      } // printf("FOUND EOF %d\n",i); }
      }
 */
 
      if(single==0) { end[i-1] = tmp  ; single=1;     } // printf("if single0 i=%d end[%d]=%d\n",i,i-1,tmp);}
      else          { end[i-1] = tmp-2;               } //printf("           i=%d end[%d]=%d\n",i,i-1,tmp-2);}
 
      if(*(ptr+1) == '\r'){ start[i]=tmp+2; single=0; } //printf(" r         i=%d start[%d]=%d\n",i,i,tmp+2);}
      else                { start[i]=tmp+1;           } //printf("           i=%d start[%d]=%d\n",i,i,tmp+1);}
 
      *ln_cnt+=1;
      i++;
   }
 
   end[i-1] = start[i-1];
   *typeno=type_no;
 
   if(*ln_cnt > MAX_LINE +1){ printf("Increase MAX_LINE above %d:  %s\n", *ln_cnt,strerror(errno)); exit(1); }

                               if(c_verbose>0) printf(" ->Line Start count =%d\n",*ln_cnt);
                               if(c_verbose>3) for(i=0;i<*ln_cnt; i++){ printf("   ln=%0.5d start=%.8d\n",i,start[i]); }
  return data;


  /* -----------------------------------------------------------------------------------------------
    123456789012345678901234567890123
    |         |          ||         | start
    whateverMNwhateverMNMNwhateverMN0
           |         |   |      |   | end
    123456789012345678901234567890123


   loop    i  ln_cnt  end[i-1]    start[i]
           0    0                     0    1

           1    1         0   8       1   11
           2    2         1  18       2   22
           3    3         2  22       3   23
           4    4         3  30       4   33

           5    5
                          4  33
     ----------------------------------------------------------------------------------------------- */


                                   if(c_verbose>0) printf(" ->Line Start count =%d\n",*ln_cnt);
                                   if(c_verbose>3) for(i=0;i<*ln_cnt; i++){printf("   ln=%0.5d start=%.8d\n",i,start[i]);}
                                   if(c_verbose>2) printf("^---@ (%d) %s\n",__LINE__, __FILE__);
}


char * get_shm_data_ptr(int *shm_data_fd){

  char *shm_data_ptr;
  char data_or_meta[]="data";
  long data_bsize = get_dev_shm_file_bsize(data_or_meta,-1);

  *shm_data_fd = shm_open(shm_data_name, (int)(O_RDONLY), (mode_t)0666);
                                          if (*shm_data_fd == -1)
                                          { printf("shm_open failed: %s\n", strerror(errno)); exit(1); }

  shm_data_ptr = mmap(0, data_bsize,PROT_READ, MAP_SHARED, *shm_data_fd, 0);
                                          if (shm_data_ptr == MAP_FAILED)
                                          { printf("shm mmap failed:   %s\n", strerror(errno)); exit(1); }

                                   if(c_verbose>3) printf(" -> get_shm_data_ptr: FIRST 100 CHARs of SHARED MEMORY:\n");
                                   if(c_verbose>3){for(int i=0;i<100;i++)  printf("%c",shm_data_ptr[i]);printf("\n");}
  return shm_data_ptr;
}

char * get_shm_data_type_ptr(int *shm_data_fd, int itype){

  char *shm_data_ptr;
  char data_or_meta[]="data";
  long data_bsize = get_dev_shm_file_bsize(data_or_meta,itype);

   if(SWAT_DEBUG_READ_META == 1)printf(" ->  file   swat_%4s_%-3s size (%10ld B)\n",
                                       data_or_meta,file_suffixes[itype],data_bsize);

   char  shm_data_p_name[128];   // only needs 14, just in case "swat_data" name is changed, using 128
   char * ptr;
   shm_data_p_name[0]='\0';
   strcat(shm_data_p_name,  shm_data_name);
   ptr=index(shm_data_p_name,'\0');
   *ptr++='_';
   strcpy(ptr, file_suffixes[itype]);

   *shm_data_fd = shm_open(shm_data_p_name, (int)(O_RDONLY), (mode_t)0666);

                                           if (*shm_data_fd == -1)
                                           { printf("shm_open failed: %s\n", strerror(errno)); exit(1); }

  shm_data_ptr  = mmap(0, data_bsize,PROT_READ, MAP_SHARED, *shm_data_fd, 0);

                                           if (shm_data_ptr == MAP_FAILED)
                                           { printf("shm mmap failed:   %s\n", strerror(errno)); exit(1); }

                                   if(c_verbose>3) printf(" -> get_shm_data_ptr: FIRST 100 CHARs of SHARED MEMORY:\n");
                                   if(c_verbose>3){for(int i=0;i<100;i++)  printf("%c",shm_data_ptr[i]);printf("\n");}
  return shm_data_ptr;
}


void get_shm_meta(int *file_cnt, long *Offset, long *Offndx, int type){
                                                                //int get_shm_meta(long *Offset, long *Offndx){
  int   shm_meta_fd;     // file descriptor
//long long *shm_meta_ptr;    // base address, from mmap()
  int  long *shm_meta_ptr;    // base address, from mmap()

  int  ndx_size  = MAX_SBN*MAX_HRU;
  int  set_size  = MAX_SBN*MAX_HRU+1;
  int long meta_bsize = sizeof(long) + sizeof(long)*(set_size) + sizeof(long)*ndx_size;

  int long lfile_cnt;

   char shm_meta_p_name[128];   // only needs 14, just in case "swat_data" name is changed, using 128
   char * ptr;

   *file_cnt = 0;

   shm_meta_p_name[0]='\0';
   strcat(shm_meta_p_name,  shm_meta_name);
   strcat(shm_meta_p_name, "_");
   strcat(shm_meta_p_name, file_suffixes[type]);

   shm_meta_fd = shm_open(shm_meta_p_name, (int)(O_RDONLY), (mode_t)0666);
                                           if (shm_meta_fd == -1) 
                                          {printf("shm_open failed (in get_shm_meta): %s\n",strerror(errno)); exit(1);}

   if(SWAT_DEBUG_READ_META == 1 )printf(" ->  opened /dev/shm/swat_meta_(%10s  )\n",file_suffixes[type]);


   shm_meta_ptr = (long *)mmap(0, (size_t)meta_bsize, PROT_READ, MAP_SHARED, shm_meta_fd, 0);
                                           if (shm_meta_ptr == MAP_FAILED)  
                                           {printf("shm mmap failed (in get_shm_meta): %s\n",strerror(errno)); exit(1);}

   if(SWAT_DEBUG_READ_META == 1 )printf(" ->  metadata size             (%10ld B)\n",meta_bsize);


   memcpy(&lfile_cnt, &shm_meta_ptr[0],          (size_t)sizeof(long)         );

   memcpy(Offndx,     &shm_meta_ptr[         1], (size_t)sizeof(long)*ndx_size);

   memcpy(Offset,     &shm_meta_ptr[ndx_size+1], (size_t)sizeof(long)*set_size);

   if(SWAT_DEBUG_READ_META == 1 )printf(" ->  metadata index   elements (%10d  )\n",ndx_size  );
   if(SWAT_DEBUG_READ_META == 1 )printf(" ->  metadata pointer elements (%10d  )=1 more than max # files\n",set_size  );

   if (munmap(shm_meta_ptr, meta_bsize) == -1)  { printf("shm munmap failed: %s\n", strerror(errno)); exit(1); }

   if (close(shm_meta_fd) == -1)                { printf("shm close failed:  %s\n", strerror(errno)); exit(1); }

   *file_cnt = (int) lfile_cnt;

} // end get_shm_meta



void close_shm_data(char *shm_data_ptr, int shm_data_fd, int itype){
                                              // unmap shm from process address space
                                              // "close" shm segment
  char data_or_meta[]="data";
  long data_bsize = get_dev_shm_file_bsize(data_or_meta,itype);
  if (munmap(shm_data_ptr, data_bsize) == -1) { printf("shm munmap failed: %s\n", strerror(errno)); exit(1); }
  if (close(shm_data_fd) == -1)               { printf("shm close failed:  %s\n", strerror(errno)); exit(1); }
}

void ulink_shm_data(char *shm_data_name){     //delete shm segment
  if (shm_unlink(shm_data_name) == -1)        {printf("shm_unlink %s: %s\n",shm_data_name,strerror(errno)); exit(1);}
}

void print_data(char *shm_data_ptr){
printf(" ->INSIDE PRINT_DATA shm_data_ptr start\n");
for(int i=0;i<100;i++)  printf("%c",shm_data_ptr[i]);
printf(" ->\nINSIDE PRINT_DATA shm_data_ptr   end\n\n");
}


long get_dev_shm_file_bsize(char * data_or_meta, int itype){
    long data_bsize;
    char size_char[12];
    FILE *p;
    char ch;
    int pos=0;

    char *cptr;
    char ls_cmd[512];
    char ls[] = "\\ls -l ";
    char pipe[] = "| awk '{print $5}' ";
    char cmd_end[]      = "\\$ ";

/*      Save for later use
    char swat_data_dir[512];
    const char* env_ptr = getenv("SWAT_DATA");
 
    if (env_ptr != NULL) {
       if(strlen(env_ptr) > sizeof(swat_data_dir)-1)
          { printf("ERROR: SWAT_DATA directory path name is %d characters.\n",strlen(env_ptr));
            printf("       Increase swat_data_dir size to %d or greater in %s.\n",sizeof(swat_data_dir)+1,__FILE__);
            exit(1);
          }
          strcpy(swat_data_dir, env_ptr);
          strcat(swat_data_dir, "/")    ;
       }
    else{
          swat_data_dir[0]='.'   ;
          swat_data_dir[1]='/'   ;
          swat_data_dir[2]='\0'  ;
    }
*/

    cptr =strcpy(ls_cmd, ls       );
    cptr =strcat(ls_cmd, "/dev/shm/" );
    cptr =strcat(ls_cmd, "swat_"  );
    cptr =strcat(ls_cmd, data_or_meta  );
    cptr =strcat(ls_cmd, "_"  );
    cptr =strcat(ls_cmd, file_suffixes[itype]);
    cptr =strcat(ls_cmd, pipe     );
    p = popen(ls_cmd, "r"); /* Unix */
    if( p == NULL) { puts("Unable to open process"); return(1); }
    while( (ch=fgetc(p)) != EOF) size_char[pos++]=ch;
                                 size_char[pos]='\0';
    data_bsize=atol(size_char);
                  //printf(" got /dev/shm/swat_%4s_%-3s  size for type=%d   data_bsize: %ld\n", 
                  //         data_or_meta, file_suffixes[itype], itype, data_bsize);    
    pclose(p);

    return(data_bsize);
}
