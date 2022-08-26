/* $Id: rogbot.c,v 1.2 2010/05/04 12:55:21 deraugla Exp $ */
/* skeleton of a robot playing rogue, communicating with a socket */
/* algorithm must be set in function 'play' below */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>

static void home(void) {
  printf("\033[H");
}

static void clear_scr(void) {
  printf("\033[J");
}

static char rogbot_magic[] = "RGBT0001";

static int check_magic(int s) {
  int ilen = strlen(rogbot_magic), len, r;
  char buff[ilen+1];
  len = read(s, buff, ilen);
  if (len != ilen) r = 0;
  else {
    if (strncmp(buff, rogbot_magic, ilen) == 0) r = 1;
    else {
      printf("bad rogbot magic\n");
      fflush(stdout);
      r = 0;
    }
  }
  return(r);
}

static void skip_newline(int s) {
  int len;
  char buff[1];

  len = read(s, buff, 1);
  if (len == 1 && buff[0] == '\n') {
  }
  else {
    fprintf(stderr, "newline expected in protocol\n");
    fflush(stderr);
    exit(2);
  }
}

static int input_int(int s) {
  int n;
  char buff[1];

  n = 0;
  for (;;) {
    (void)read(s, buff, 1);
    if (isdigit(buff[0])) {
      n = 10 * n + buff[0] - '0';
    }
    else if (buff[0] == '\n') break;
    else {
      fprintf(stderr, "unexpected char '%c' in protocol\n", buff[0]);
      fflush(stderr);
      exit(2);
    }
  }
  return n;
}

static char **input_dungeon(int s, int *nrow, int *ncol) {
  static char **tab = 0;
  char *p;
  int row;
  if (check_magic(s)) {
    skip_newline(s);
    *nrow = input_int(s);
    *ncol = input_int(s);
    if (tab == 0) {
      tab = (char **)malloc((*nrow + 1) * sizeof(char *) +
                            *nrow * (*ncol + 1));
      tab[*nrow] = 0;
      p = (char *)(tab + (*nrow + 1));
      for (row = 0; row < *nrow; row++) {
        tab[row] = p;
        tab[row][*ncol] = 0;
        p += *ncol + 1;
      };
    }
    for (row = 0; row < *nrow; row++) {
      (void)read(s, tab[row], *ncol);
      skip_newline(s);
    }
  }
  else {
    if (tab) free(tab);
    tab = 0;
  }
  return(tab);
}

static char play(char **tab, int nrow, int ncol) {
  static char buff[] = "hjkl ";
  char p;
  p = buff[random() % strlen(buff)];
  return(p);
}

static void play_loop(int s) {
  char **tab, buff[1];
  int nrow, ncol, row;
  tab = input_dungeon(s, &nrow, &ncol);
  while (tab != 0) {
    home();
    for (row = 0; row < nrow; row++) {
      printf("%s", tab[row]);
      if (row < nrow - 1) printf("\n");
    }
    fflush(stdout);
    usleep(100000);
    buff[0] = play(tab, nrow, ncol);
    write(s, buff, 1);
    tab = input_dungeon(s, &nrow, &ncol);
  }
}

char *arg_addr = 0;

int main(int argc, char **argv) {
  struct sockaddr_un addr;
  int r, s;

  arg_addr = argv[1];
  printf("addr %s\n", arg_addr);
  fflush(stdout);
  addr.sun_family = AF_UNIX;
  strcpy(addr.sun_path, arg_addr);
  s = socket(PF_UNIX, SOCK_STREAM, 0);
  for (;;) {
    r = connect(s, (struct sockaddr *)&addr, sizeof(addr));
    if (r < 0 && (errno == ECONNREFUSED || errno == ENOENT)) sleep(1);
    else if (r < 0) {
      fprintf(stderr, "errno %d\n", errno);
      fflush(stderr);
      exit(2);
    }
    else break;
  }
  home();
  clear_scr();
  play_loop(s);
  close(s);
  return(0);
}
