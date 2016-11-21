#include <stdio.h>
#include <sys/socket.h>

int main () {
	printf("structure OS_Constants =\n");
	printf("struct\n");
	printf("  val SOL_SOCKET   = %zi\n", SOL_SOCKET);
	printf("  val SO_REUSEPORT = %zi\n", SO_REUSEPORT);
	printf("end\n");
	return 0;
}
