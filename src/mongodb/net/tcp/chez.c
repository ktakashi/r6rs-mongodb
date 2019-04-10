#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

/* For now only considers Linux... */
int make_client_socket(const char *host, const char *service)
{
  struct addrinfo hints, *ai, *results;
  int ret, fd;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_V4MAPPED & AI_ADDRCONFIG;
  hints.ai_protocol = 0;
  
  do {
    ret = getaddrinfo(host, service, &hints, &results);
  } while (EAI_AGAIN == ret);
  
  if (ret != 0) return -1;

  for (ai = results; ai != NULL; ai = ai->ai_next) {
    fd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol);
    if (fd == -1) continue;
    if (connect(fd, ai->ai_addr, ai->ai_addrlen) != -1) break;
    close(fd);
  }
  freeaddrinfo(results);
  return fd;
}

int socket_shutdown(int fd)
{
  return shutdown(fd, SHUT_RDWR);
}
