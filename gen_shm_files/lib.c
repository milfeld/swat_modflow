#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#include <stdbool.h>

#define MAX_DB_ENV 512


int get_db_policy()
{
  int  count;
  char env_data_db[MAX_DB_ENV];
  char *ptmp;
  enum swat_Data_DB{        SYNC,   USE_WO_SYNCING,   ABORT_IF_NOT_SYNCED } db_policy;
  const char *data_db[] = {"SYNC", "USE_WO_SYNCING", "ABORT_IF_NOT_SYNCED", NULL};
  const char **pptr = data_db;

  db_policy=SYNC;
  if( (ptmp=getenv("SWAT_DATA_DB")) != NULL )
  {
     strncpy( env_data_db, ptmp, MAX_DB_ENV-1 );
     count=0;
     db_policy=-1;
     while( *pptr != NULL ){
       if( strcmp(*pptr,env_data_db) == 0 ){ db_policy=count;}
       count++;
       pptr++;
     }
     if(db_policy==-1) {
        printf("  WARNING: SWAT_DATA_DB variable value %s is not valid\n",env_data_db);
        printf("          Using SWAT_DATA_DB SYNC policy.\n");
        db_policy=0;
     }
  }
  return db_policy;
}


long get_shm_size(char * Data_dir, char * dir_info_path)
{
  char file_line_func[512];

  char cmd_beg[]=" du -b --max-depth=0 ";
  char cmd_end[]=" | sed s@/.*@@ ";
  char du_cmd[512];

  long data_dir_size_B;    //  For extracting 
  char size_char[12];
  FILE *p, *fp;
  char *cptr;
  char ch;
  int  pos=0;
  bool can_use_USE_WO_SYNCING=true;

  struct stat stat_Data_dir_info;
  struct stat stat_Data_dir;
  int    tdiff;
  enum   swat_Data_DB{ SYNC, USE_WO_SYNCING, ABORT_IF_NOT_SYNCED } db_policy;

  if( stat(Data_dir, &stat_Data_dir) != 0 ){
     sprintf(file_line_func,"stat on:  %s  file: %s  line: %d  call: %s",Data_dir, __FILE__, __LINE__, __func__);
     perror(file_line_func); exit(EXIT_FAILURE);
  }

  if( stat(dir_info_path, &stat_Data_dir_info) != 0 ){
     printf(" -> Warning: Did not find the Data_DB file %s.\n",dir_info_path);
     printf("          File dir_info contains the storage (in bytes) needed for shm caching Data files.\n");
     printf("          Default policy will update the file.\n");
     printf("          Set Env. Var.  SWAT_DATA_DB to ABORT_IF_NOT_SYNCED to abort in this case.\n");
     can_use_USE_WO_SYNCING=false;

     tdiff= (int) (stat_Data_dir.st_ctime - 0);
  }
  else{
     fp = fopen(dir_info_path, "r"); /* Unix */

     if( fp == NULL) {
       sprintf(file_line_func,"Something went wrong!:  %s  file: %s  line: %d  call: %s",dir_info_path, __FILE__, __LINE__, __func__);
       perror(file_line_func); exit(EXIT_FAILURE);
     }


     while( (ch=fgetc(fp)) != EOF) size_char[pos++]=ch;  // Read dir_info file
     size_char[pos]='\0';
     data_dir_size_B=atol(size_char);

     fclose(fp);

     tdiff= (int) (stat_Data_dir.st_ctime - stat_Data_dir_info.st_ctime);
  }

  cptr =strcpy(du_cmd,   cmd_beg);
  cptr =strcat(du_cmd, Data_dir);
  cptr =strcat(du_cmd,   cmd_end);

  db_policy=get_db_policy();

  if( tdiff > 1 ){

    if(db_policy == SYNC ){
       printf(" WARNING: files in %s Data directory\n",Data_dir);
       printf("          were changed after dir_info file was updated (or dir_info does not exist).\n");
       printf("          The dir_info contains the storage (in bytes) needed for shm caching Data files.\n");
       printf("          SWAT program will update the file, causing a delay in execution.\n");

       p = popen(du_cmd, "r"); /* Unix */
       if( p == NULL) { puts("Unable to open process"); return(1); }

       pos=0;
       while( (ch=fgetc(p)) != EOF) size_char[pos++]=ch;
       size_char[pos]='\0';
       data_dir_size_B=atol(size_char);  // du -b may be off.
       pclose(p);
       //remove(dir_info_path);
       fp=fopen(dir_info_path,"w"); fprintf(fp,"%s\n",size_char); fclose(fp);

    }

    if(db_policy ==  USE_WO_SYNCING ){
       if (! can_use_USE_WO_SYNCING) {
          printf(" ERROR: Cannot have Env. Var. SWAT_DATA_DB be USE_WO_SYNCING when dir_info does not exist.\n");
          printf("        Use  %s > %sdir_info\n",du_cmd,Data_dir);
          printf("        to update dir_info, and don't change any files in the Data directory.\n");
          sprintf(file_line_func," dir_info file::  %s  file: %s  line: %d  call: %s",dir_info_path, __FILE__, __LINE__, __func__);
          perror(file_line_func); exit(EXIT_FAILURE);
       }

       data_dir_size_B*=1.10;

       printf(" WARNING: files in %s Data directory\n",Data_dir);
       printf("          were changed after dir_info file was updated.\n");
       printf("          USE_WO_SYNCING policy has been set, so will try to accommodate changes:\n");
       printf("          with a 10%% increase in total storage for shm caching: now %ldB.\n",data_dir_size_B);
       printf("          At some point use:\n");
       printf("           %s > %sdir_info\n",du_cmd,Data_dir);
       printf("          to update dir_info.\n");

    }

    if(db_policy == ABORT_IF_NOT_SYNCED ){
       printf(" ERROR: Files in %s Data directory\n",Data_dir);
       printf("        were changed after dir_info file was updated (or dir_info does not exist).\n");
       printf("        ABORT_IF_NOT_SYNCED policy has been set:\n");
       printf("        Use  %s > %sdir_info\n",du_cmd,Data_dir);
       printf("        to update dir_info, and don't change any files in the Data directory.\n");
       printf("        If you will, then run with the ABORT_IF_NOT_SYNCED policy.\n");
       exit(EXIT_FAILURE);
    }

  } // tdiff>1
  else{
    printf(" ->  Done:  DB file Data/dir_info  (      valid  )\n");
  }
// TODO du is alway bigger than direct file size
//      But, every file gets a \0 at the end so
//      data_dir_size_B, for safety, should add MAX_FILE*8
  return data_dir_size_B;

}

//http://c-faq.com/aryptr/aryptrequiv.html
