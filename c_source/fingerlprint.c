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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
} port_s;

/*
 * \brief Calculate correct size for the FMD, decoding a unsigned binary from erlang
 *
 * \param x         [int] the size for the FMD (as encode unsigned bytes)
 * \param y         [int] the size for the FMD (as encode unsigned bytes)
 *
 * \return              [int] the size for the fmd
 */
int carry(int x, int y) {
    if (x == 0) {
        // write_to_log(*x, *y);
        return (256 + y);
    } else if (x == 1){
	if (y > 0) {
		return (256 + y);
	} else {
        	return (512 + y);
	}
    }
}
/* ================================================================================= */
// 
// Here starts the port driver functions related to erlang, for more info :
//		http://www.erlang.org/doc/tutorial/c_portdriver.html
//
/* ================================================================================= */
static ErlDrvData fingerlprint_driver_start(ErlDrvPort port, char *buff)
{
    port_s* to_erl_port = (port_s*)driver_alloc(sizeof(port));
    to_erl_port->port = port;
    return (ErlDrvData)to_erl_port;
}

static void fingerlprint_driver_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

static void fingerlprint_driver_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen)
{
    port_s* to_erl_port = (port_s*)handle;
    int x1 = (int)buff[0], x2 = (int)buff[1];
    int y1 = (int)buff[2], y2 = (int)buff[3];
    int size_fmd_x = carry(x1, x2);
    int size_fmd_y = carry(y1, y2);
    /* enable logs to debugging */
    write_to_log(size_fmd_x, size_fmd_y);
    char toolchain = run_comparison(size_fmd_x, size_fmd_y, buff, bufflen);
    driver_output(to_erl_port->port, &toolchain, sizeof(char));
}


ErlDrvEntry fingerlprint_driver_entry = {
    NULL,                        /* F_PTR init, N/A */
    fingerlprint_driver_start,   /* L_PTR start, called when port is opened */
    fingerlprint_driver_stop,    /* F_PTR stop, called when port is closed */
    fingerlprint_driver_output,  /* F_PTR output, called when erlang has sent */
    NULL,                        /* F_PTR ready_input, called when input descriptor ready */
    NULL,                        /* F_PTR ready_output, called when output descriptor ready */
    "fingerlprint",              /* char *driver_name, the argument to open_port */
    NULL,                        /* F_PTR finish, called when unloaded */
    NULL,
    NULL,                        /* F_PTR control, port_command callback */
    NULL,                        /* F_PTR timeout, reserved */
    NULL,			 /* F_PTR outputv, reserved*/
    NULL,                       /* F_PTR ready_async, only for async drivers */
    NULL,                       /* F_PTR flush, called when port is about 
				   to be closed, but there is data in driver 
				   queue */
    NULL,                       /* F_PTR call, much like control, sync call
				   to driver */
    NULL,                       /* F_PTR event, called when an event selected 
				   by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be 
				   set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
				       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
				       set to this value */
    0,                          /* int driver_flags, see documentation */
    NULL,                       /* void *handle2, reserved for VM use */
    NULL,                       /* F_PTR process_exit, called when a 
				   monitored process dies */
    NULL                        /* F_PTR stop_select, called to close an 
				   event object */
};

DRIVER_INIT(fingerlprint) /* must match name in driver_entry */
{
    return &fingerlprint_driver_entry;
}
