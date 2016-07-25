///----------------------------------------------------------------------
/// Copyright: (c) 2011-2013 MoreloSoft - Hova Networks S.A.P.I. de C.V.  
/// All rights reserved.
///
/// Redistribution and use in any form, with or without modification, 
/// is strictly prohibited.
///
/// Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
///----------------------------------------------------------------------

#include <syslog.h>
#include <stdio.h>
#include <string.h>
#include "helpers.h"
#include <dpfj.h>
#include "erl_driver.h"

#define SYS_LOG "/var/log/messages"
#define FINGERLPRINT_YES '1'
#define FINGERLPRINT_NOT '0'
#define FINGERLPRINT_ERROR '2'
#define FINGERLPRINT_CANNOT_ALLOCATE_MEMMORY 'M'

/*
 * \brief Extract a single FMD for the entire char array, knowning the size and 
 *        offset, size is the size for the FMD and offset is where starts to read the fmd in 
 *	  the given char array 
 *
 * \param size 	        [int] the size for the fmd 
 * \param fmds	        [char *] the string containing the FMD's
 * \param offset	[int] where starts to read the string 'fmds'
 *
 * \return              [char *] returns the fmd extracted, as string
 */
char *extract_fmd(int size, char *fmds, int offset) {
    int i = 0;
    char* fmd;
    if ((fmd = malloc(size)) == NULL) {
	return NULL;
    } else {
    while(i < size){
	fmd[i]=fmds[offset];
	offset ++;
	i ++;
    }
    return fmd;
    }
}

/* Enable this function for debugging, the next function 
   retrieves a size of a FMD given, FMD must be a string */
/*
 * int sizing(char *fmd, int buffer) {
 *   int x = 0;
 *   int i;
 *   for (i = 0; i < MAX_FMD_SIZE; i ++)
 *	x ++;
 *   return MAX_FMD_SIZE;
}*/

/*
 * \brief Write size of FMD's to the system log (only for UNIX systems)
 *	 The path to the system logs by default is set to '/var/log/messages'
 *
 * \param datax		[int] the size for the first valid FMD 
 * \param datay		[int] the size for the second valid FMD
 *
 * \return 		[void] return not applicable
 */
void write_to_log(int datax, int datay) {
  setlogmask(LOG_UPTO (LOG_NOTICE));
  openlog(SYS_LOG, LOG_CONS | LOG_PID | LOG_NDELAY, LOG_LOCAL1);
  syslog(LOG_NOTICE, "[fingerlprint-tools] size for fmd x %d", datax);
  syslog(LOG_NOTICE, "[fingerlprint-tools] size for fmd y %d", datay);
  closelog();
}

/*
 * \brief Compares two FMD's provided by erlang port driver as binary,
 *	  we receive the FMD's as a single char array, the first two elements in the unsigned char *fmds
 *	  array is the size for the first FMD, the next two elements are the size for the second FMD, the next bytes
 *	  are the FMD's example:
 *		
 *	  unsigned char *fmds = [ 0, 138, 1, 132, ..., ...]
 *				  |   |   |   |    |    |
 *				    |       |      |	|
 *				  size    size    FMD  FMD
 *
 *        we extract the FMD's knowning their size, and compares between them
 *
 * \param size_fmd_x         [int] the size for the first valid FMD
 * \param size_fmd_y         [int] the size for the second valid FMD
 * \param *fmds		     [unsigned char] the entire char array containing FMD's and their size 
 * \param buffer	     [unsigned int] the size of the char array '*fmds'
 *
 * \return              FINGERLPRINT_YES:			when comparison success (FMDs equal)
 * \return		FINGERLPRINT_CANNOT_ALLOCATE_MEMMORY:	when cannot allocate memmory for the FMD's
 * \return		FINGERLPRINT_NOT:			when comparison fails (FMDs not equal)
 * \return		FINGERLPRINT_ERROR:			when comparison fails (unknown error)
 */
char run_comparison(int size_fmd_x, int size_fmd_y, unsigned char *fmds, unsigned int buffer) {       
       unsigned char* ptrFmdx;
       unsigned char* ptrFmdy;
       if (((ptrFmdx = extract_fmd(size_fmd_x, fmds, 4)) == NULL) || (ptrFmdy = extract_fmd(size_fmd_y, fmds, size_fmd_x+4)) == NULL) {
       		return FINGERLPRINT_CANNOT_ALLOCATE_MEMMORY;
       } else {
       unsigned int falsematch_rate = 0;
       int result = dpfj_compare(DPFJ_FMD_ISO_19794_2_2005, ptrFmdx, MAX_FMD_SIZE, 0,
                                 DPFJ_FMD_ISO_19794_2_2005, ptrFmdy, MAX_FMD_SIZE, 0, &falsematch_rate);
       if(DPFJ_SUCCESS == result){
                const unsigned int target_falsematch_rate = DPFJ_PROBABILITY_ONE / 100000; //target rate is 0.00001
                if(falsematch_rate < target_falsematch_rate){
                        // fingerprints matched!
			// printf("dissimilarity score: 0x%x.\n", falsematch_rate);
                        // printf("false match rate: %e.\n\n\n", (double)(falsematch_rate / DPFJ_PROBABILITY_ONE));
                        free(ptrFmdx);
			free(ptrFmdy);
			return FINGERLPRINT_YES;
                }
                else { 
		 	// fingerprints did not matchi
			return FINGERLPRINT_NOT;
               	     }
                } else
			// unknown error 
			return FINGERLPRINT_ERROR;
}}
