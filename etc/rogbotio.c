/* $Id: rogbotio.c,v 1.4 2010/05/04 13:56:00 deraugla Exp $ */
/* interface to rogbot for a rogue close written in C */

#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>

static char robot_magic[] = "RGBT0001";

int rogbotio_socket(char *fname)
{
  struct stat st;
  struct sockaddr_un addr;
  struct sockaddr addr_accepted;
  int server_sock, sock;
  int addrlen;

  if (stat(fname, &st) == 0) {
    if (S_ISSOCK(st.st_mode)) unlink(fname);
    else {
      fprintf(stderr, "Error: file '%s' still exists\n", fname);
      fflush(stderr);
      return(-1);
    }
  };
  server_sock = socket(PF_UNIX, SOCK_STREAM, 0);
  if (server_sock < 0) {
    fprintf(stderr, "'Socket' error\n");
    fflush(stderr);
    return(-1);
  };
  setsockopt(server_sock, SOL_SOCKET, SO_REUSEADDR, (void *)1, sizeof(int));
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, fname);
  bind(server_sock, (struct sockaddr *)&addr, sizeof(struct sockaddr_un));
  listen(server_sock, 1);
  fprintf(stderr, "Waiting for socket connection...\n");
  fflush(stderr);
  addrlen = sizeof(struct sockaddr);
  sock = accept(server_sock, &addr_accepted, &addrlen);
  close(server_sock);
  return(sock);
}

int rogbotio_getchar(int nrow, int ncol, int sock)
{
  char line[ncol], txt[5];
  int i, j;

  write(sock, &robot_magic[0], strlen(&robot_magic[0]));
  write(sock, "\n", 1);
  sprintf(txt, "%d\n", nrow);
  write(sock, txt, strlen(txt));
  sprintf(txt, "%d\n", ncol);
  write(sock, txt, strlen(txt));
  for (i = 0; i < nrow; i++) {
    for (j = 0; j < ncol; j++) line[j] = mvinch(i, j);
    write(sock, &line[0], ncol);
    write(sock, "\n", 1);
  };
  read(sock, &line[0], 1);
  return(line[0]);
}
