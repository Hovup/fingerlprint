###----------------------------------------------------------------------
### Copyright: (c) 2011-2013 MoreloSoft - Hova Networks S.A.P.I. de C.V.  
### All rights reserved.
###
### Redistribution and use in any form, with or without modification, 
### is strictly prohibited.
###
### Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
###----------------------------------------------------------------------

##
## Necessary includes for build a port driver
##
ERL_ROOT=/usr/local/lib/erlang

##
## Include flags on gcc compiler time, arch x86
## on cygwin it must be change.
## Load dinamically instead one by one -l options tells to the 
## gcc what libraries load
##
LDFLAGS = -lm -lc -ldpfpdd -ldpfj $(CFLAGS)

##
## Path to sampler libraries, we need helpers.c, maybe removal
## in next releases
##
ISAMPLERFLAGSUaU = /opt/DigitalPersona/UareUSDK/Linux/Samples/UareUSample/

##
## This flags represents libraries include, provided for 
## the DigitalPersona FingerPrint, and used for the port driver creation
##
IFLAGSUaU = /opt/DigitalPersona/UareUSDK/Include/

##
## Libraries provided by SDK, poiting to x86, change as you need!!
##
LFLAGSUaU = /opt/DigitalPersona/UareUSDK/Linux/lib/x86

.PHONY: demo

all: compile_c compile

compile:
	@mkdir -p ebin
	erlc -o ebin/ src/*.erl

clean:
	@rm -rf ebin/*.beam
	@rm -rf lib

demo:
	@erl -pa ebin/ -eval 'ok = application:start(fingerlprint).'

compile_c: check_lib_name
	@mkdir -p lib
	gcc -o priv/$(lib).so -L$(LFLAGSUaU) $(LDFLAGS) -I$(ISAMPLERFLAGSUaU)  -I$(ERL_ROOT)/usr/include/ -I$(IFLAGSUaU) -shared -fpic -rdynamic c_source/*.c
	@ldconfig $(PWD)

check_lib_name:
ifndef lib
	$(error LIB is unset)
endif
