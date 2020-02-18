USER_SML_LIB?=${HOME}/SML

all:
	@echo "make targets: poly mlton clean"
	@echo "make depends && make USER_SML_LIB=lib poly mlton"

poly: os-constants.sml 
	env USER_SML_LIB=${USER_SML_LIB} polyc -o redis-sharding-poly redis-sharding.mlp

mlton: os-constants.sml
	mlton -mlb-path-var 'USER_SML_LIB ${USER_SML_LIB}' -default-ann 'allowFFI true' -link-opt -lz -output redis-sharding-mlton redis-sharding.mlb ${USER_SML_LIB}/mlton-string-concat/mlton-string-concat.c

os-constants.sml: os-constants.c
	cc -o os-constants os-constants.c && ./os-constants > os-constants.sml && rm os-constants


depends: lib lib/ev lib/sparcl lib/mlton-string-concat

lib:
	mkdir lib

lib/ev:
	git clone https://github.com/kni/sml-ev.git lib/ev

lib/sparcl:
	git clone https://github.com/kni/sparcl.git  lib/sparcl

lib/mlton-string-concat:
	git clone https://github.com/kni/mlton-string-concat.git lib/mlton-string-concat

clean:
	rm -rf lib redis-sharding-poly redis-sharding-mlton os-constants.sml
