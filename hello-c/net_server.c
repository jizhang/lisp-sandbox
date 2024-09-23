#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>

int main(void) {
  struct sockaddr_in serv;
  memset(&serv, 0, sizeof(serv));
  serv.sin_family = AF_INET;
  serv.sin_addr.s_addr = htonl(INADDR_ANY);
  serv.sin_port = htons(2300);

  printf("Listening...\n");
  int mysocket = socket(AF_INET, SOCK_STREAM, 0);
  bind(mysocket, (struct sockaddr *) &serv, sizeof(struct sockaddr));
  listen(mysocket, 1);

  char *msg = "hello world";
  struct sockaddr_in dest;
  socklen_t socksize = sizeof(struct sockaddr_in);
  int consocket = accept(mysocket, (struct sockaddr *) &dest, &socksize);
  while (consocket) {
    printf("Incoming connection from %s - sending welcome\n", inet_ntoa(dest.sin_addr));
    send(consocket, msg, strlen(msg), 0);
    close(consocket);
    consocket = accept(mysocket, (struct sockadrr *) &dest, &socksize);
  }

  close(mysocket);
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc net_server.c -o net_server && net_server" */
/* End: */
