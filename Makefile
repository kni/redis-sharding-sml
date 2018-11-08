all:
	@echo "make targets: poly, mlton, clean."

poly: ev sparcl os-constants.sml 
	polyc -o redis-sharding-poly redis-sharding.mlp

mlton: ev sparcl os-constants.sml mlton-string-concat
	mlton -default-ann 'allowFFI true' -link-opt -lz -output redis-sharding-mlton redis-sharding.mlb mlton-string-concat/mlton-string-concat.c

os-constants.sml: os-constants.c
	cc -o os-constants os-constants.c && ./os-constants > os-constants.sml && rm os-constants

ev:
	git clone https://github.com/kni/sml-ev.git ev

sparcl:
	git clone https://github.com/kni/sparcl.git

mlton-string-concat:
	git clone https://github.com/kni/mlton-string-concat.git

clean:
	rm -rf redis-sharding-poly redis-sharding-mlton os-constants.sml
