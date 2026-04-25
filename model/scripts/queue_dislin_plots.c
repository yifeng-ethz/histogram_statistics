#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "dislin.h"

#define MAX_POINTS 300
#define MAX_SERIES 8

typedef struct {
  char key[64];
  char label[96];
  int n;
  float x[MAX_POINTS];
  float y[MAX_POINTS];
} series_t;

static const char *output_format_from_path(const char *path) {
  const char *dot = strrchr(path, '.');
  if (dot == NULL) {
    return "PNG";
  }
  if (strcasecmp(dot, ".svg") == 0) {
    return "SVG";
  }
  if (strcasecmp(dot, ".pdf") == 0) {
    return "PDF";
  }
  return "PNG";
}

static void trim(char *s) {
  size_t len;
  while (*s == ' ' || *s == '\t' || *s == '\n' || *s == '\r') {
    memmove(s, s + 1, strlen(s));
  }
  len = strlen(s);
  while (len > 0 && (s[len - 1] == ' ' || s[len - 1] == '\t' ||
                     s[len - 1] == '\n' || s[len - 1] == '\r')) {
    s[--len] = '\0';
  }
}

static int find_or_add_series(series_t *series, int *nseries, const char *key, const char *label) {
  for (int i = 0; i < *nseries; i++) {
    if (strcmp(series[i].key, key) == 0) {
      return i;
    }
  }
  if (*nseries >= MAX_SERIES) {
    return -1;
  }
  snprintf(series[*nseries].key, sizeof(series[*nseries].key), "%s", key);
  snprintf(series[*nseries].label, sizeof(series[*nseries].label), "%s", label);
  series[*nseries].n = 0;
  (*nseries)++;
  return *nseries - 1;
}

static int read_loss_csv(const char *path, series_t *series, int *nseries) {
  FILE *fp = fopen(path, "r");
  char line[1024];
  int first = 1;
  *nseries = 0;

  if (fp == NULL) {
    fprintf(stderr, "could not open CSV: %s\n", path);
    return 0;
  }

  while (fgets(line, sizeof(line), fp) != NULL) {
    char *tokens[10] = {0};
    int ntok = 0;
    char *tok;
    char traffic[64];
    char legend[96];
    int depth;
    float loss;
    int idx;

    if (first) {
      first = 0;
      continue;
    }

    tok = strtok(line, ",");
    while (tok != NULL && ntok < 10) {
      tokens[ntok++] = tok;
      tok = strtok(NULL, ",");
    }
    if (ntok < 4) {
      continue;
    }

    snprintf(traffic, sizeof(traffic), "%s", tokens[0]);
    snprintf(legend, sizeof(legend), "%s", tokens[1]);
    trim(traffic);
    trim(legend);
    depth = atoi(tokens[2]);
    loss = (float) atof(tokens[3]);

    idx = find_or_add_series(series, nseries, traffic, legend);
    if (idx < 0 || series[idx].n >= MAX_POINTS) {
      continue;
    }
    series[idx].x[series[idx].n] = (float) depth;
    series[idx].y[series[idx].n] = loss;
    series[idx].n++;
  }

  fclose(fp);
  return 1;
}

static void set_series_style(int idx) {
  static const char *colors[] = {"blue", "red", "green", "orange", "magenta", "cyan"};
  color(colors[idx % 6]);
  if (idx == 0) {
    solid();
  } else if (idx == 1) {
    dash();
  } else {
    dot();
  }
  linwid(7);
}

static void draw_legend_entry(float x, float y, float dx, const char *label, int idx) {
  float xs[2] = {x, x + dx};
  float ys[2] = {y, y};
  set_series_style(idx);
  curve(xs, ys, 2);
  color("fore");
  solid();
  linwid(1);
  height(28);
  rlmess(label, x + dx * 1.15f, y);
}

static void render_loss_plot(const char *csv_path, const char *out_path) {
  series_t series[MAX_SERIES];
  int nseries = 0;
  float cap_x[2] = {160.0f, 160.0f};
  float cap_y[2] = {0.0f, 1.0f};

  if (!read_loss_csv(csv_path, series, &nseries)) {
    exit(1);
  }

  metafl(output_format_from_path(out_path));
  setfil(out_path);
  filmod("delete");
  setpag("da4l");
  if (strcasecmp(output_format_from_path(out_path), "PNG") == 0) {
    winsiz(2048, 1448);
  }
  scrmod("reverse");
  disini();
  pagera();
  complx();
  titlin("Coalescing Queue Loss at 8-MuTRiG Link Cap", 2);
  titlin("Model/TLM, 256 channels, 200 Mhit/s aggregate, 256-cycle drain stall", 4);
  name("coalescing queue depth [entries]", "x");
  name("lost-hit fraction", "y");
  intax();
  labdig(0, "x");
  labdig(2, "y");
  axspos(430, 1600);
  axslen(2050, 970);
  graf(0.0f, 256.0f, 0.0f, 32.0f, 0.0f, 1.05f, 0.0f, 0.15f);
  grid(1, 1);

  for (int i = 0; i < nseries; i++) {
    set_series_style(i);
    curve(series[i].x, series[i].y, series[i].n);
  }

  color("black");
  dashl();
  linwid(3);
  curve(cap_x, cap_y, 2);
  solid();
  color("fore");
  linwid(1);

  for (int i = 0; i < nseries; i++) {
    draw_legend_entry(34.0f, 0.96f - 0.075f * i, 22.0f, series[i].label, i);
  }
  height(24);
  rlmess("Default preset depth 160", 166.0f, 0.98f);
  title();

  disfin();
}

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: %s loss_csv output_plot\n", argv[0]);
    return 2;
  }
  render_loss_plot(argv[1], argv[2]);
  return 0;
}
