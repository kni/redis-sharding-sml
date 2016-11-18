#include <sys/socket.h>

int reuseport = 1;
socklen_t int_size = sizeof(int);

int setsockopt_REUSEPORT(int s) {
     return setsockopt(s, SOL_SOCKET, SO_REUSEPORT, (void *) &reuseport, int_size);
}
