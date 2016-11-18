all:
	@echo "make targets: poly, mlton, clean."

poly: ev sparcl
	polyc -o redis-sharding-poly redis-sharding.mlp

mlton: ev sparcl mlton-string-concat
	mlton -default-ann 'allowFFI true' -link-opt -lz -output redis-sharding-mlton redis-sharding.mlb mlton-string-concat/mlton-string-concat.c with-mlton.c

ev:
	git clone https://github.com/kni/sml-ev.git ev

sparcl:
	git clone https://github.com/kni/sparcl.git

mlton-string-concat:
	git clone https://github.com/kni/mlton-string-concat.git

clean:
	rm -rf redis-sharding-poly redis-sharding-mlton
