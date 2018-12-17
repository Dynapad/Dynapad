#include "server.h"
#include <errno.h>
#include <signal.h>

char *argv0;

usage () {
  fprintf(stderr, "USAGE: %s [option=value ...] infile outfile\n", argv0);
  fprintf(stderr, "  filter={none, lanczos, gaussian}\n");
  fprintf(stderr, "  thumb_max_size, thumb_min_width, thumb_min_height\n");
  fprintf(stderr, "  thumb_src_min_width, thumb_src_min_height\n");
  fprintf(stderr, "  thumb_src_min_colors\n");
  fprintf(stderr, "  image_mime_type={gif, jpeg}\n");
  exit(0);
}

main (argc, argv)
int argc;
char **argv;
{
  static Thumb thumb;
  static Image image;
  int cnt;
  char *url = "-";
  char *thumb_name = "-";

  if (argv0 = strrchr(*argv, '/'))
    argv0++;
  else
    argv0 = *argv;
  
  bzero(&thumb, sizeof(Thumb));  /*  set all defaults to zero  */
  bzero(&image, sizeof(Image));  /*  set all defaults to zero  */
  argc--, argv++;
  cnt = parse_thumb_request(argv, &thumb, &image);
  argc -= cnt, argv += cnt;
  if (argc > 0) {
    if (!strcmp(*argv, "-help") || !strcmp(*argv, "-?"))
      usage();
    url = *argv;
  }
  if (argc > 1)
    thumb_name = argv[1];
  make_thumb(url, thumb_name, &image, &thumb);
}
