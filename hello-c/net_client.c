#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>

#define MAXRCVLEN 500

int main(void) {
  struct sockaddr_in dest;
  memset(&dest, 0, sizeof(dest));
  dest.sin_family = AF_INET;
  dest.sin_addr.s_addr = inet_addr("127.0.0.1");
  dest.sin_port = htons(2300);

  printf("Connecting...\n");
  int mysocket = socket(AF_INET, SOCK_STREAM, 0);
  connect(mysocket, (struct sockaddr *) &dest, sizeof(struct sockaddr_in));

  char buffer[MAXRCVLEN + 1];
  int len = recv(mysocket, buffer, MAXRCVLEN, 0);
  buffer[len] = '\0';
  printf("Received %s (%d bytes).\n", buffer, len);

  close(mysocket);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc net_client.c -o net_client && net_client" */
/* End: */
