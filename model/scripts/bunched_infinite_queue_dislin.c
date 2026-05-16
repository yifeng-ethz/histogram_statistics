#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "dislin.h"

#define MAX_POINTS 256

typedef struct {
  int n;
  float x[MAX_POINTS];
  float min_cells[MAX_POINTS];
  float p50_cells[MAX_POINTS];
  float max_cells[MAX_POINTS];
  float max_latency_us[MAX_POINTS];
  float n_bins;
  float clock_hz;
  float drain_cells_per_cycle;
} infinite_sweep_t;

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

static int split_csv(char *line, char **tokens, int max_tokens) {
  int ntok = 0;
  char *tok = strtok(line, ",");
  while (tok != NULL && ntok < max_tokens) {
    trim(tok);
    tokens[ntok++] = tok;
    tok = strtok(NULL, ",");
  }
  return ntok;
}

static int read_infinite_csv(const char *path, infinite_sweep_t *data) {
  FILE *fp = fopen(path, "r");
  char line[4096];
  int first = 1;

  memset(data, 0, sizeof(*data));
  if (fp == NULL) {
    fprintf(stderr, "could not open CSV: %s\n", path);
    return 0;
  }

  while (fgets(line, sizeof(line), fp) != NULL) {
    char *tokens[32] = {0};
    int ntok;
    int idx;

    if (first) {
      first = 0;
      continue;
    }

    ntok = split_csv(line, tokens, 32);
    if (ntok < 24 || data->n >= MAX_POINTS) {
      continue;
    }

    idx = data->n;
    data->x[idx] = (float) atof(tokens[4]);
    data->min_cells[idx] = (float) atof(tokens[8]);
    data->p50_cells[idx] = (float) atof(tokens[9]);
    data->max_cells[idx] = (float) atof(tokens[10]);
    data->max_latency_us[idx] = (float) atof(tokens[15]);
    data->n_bins = (float) atof(tokens[18]);
    data->clock_hz = (float) atof(tokens[19]);
    data->drain_cells_per_cycle = (float) atof(tokens[21]);
    data->n++;
  }

  fclose(fp);
  return data->n > 1;
}

static void start_page(const char *out_path) {
  const char *fmt = output_format_from_path(out_path);
  metafl(fmt);
  setfil(out_path);
  filmod("delete");
  if (strcasecmp(fmt, "PNG") == 0) {
    winsiz(1800, 1112);
  }
  page(2970, 1835);
  scrmod("reverse");
  disini();
  pagera();
  complx();
}

static void finish_page(void) {
  disfin();
}

static void draw_band(const infinite_sweep_t *data) {
  float xb[2 * MAX_POINTS];
  float yb[2 * MAX_POINTS];
  int n = data->n;
  for (int i = 0; i < n; i++) {
    xb[i] = data->x[i];
    yb[i] = data->max_cells[i];
    xb[2 * n - 1 - i] = data->x[i];
    yb[2 * n - 1 - i] = data->min_cells[i];
  }
  color("cyan");
  shdpat(16L);
  rlarea(xb, yb, 2 * n);
  color("fore");
  solid();
  shdpat(0L);
}

static void draw_vline(float x, float ytop, const char *label, const char *color_name, int dashed, float label_y_frac) {
  float xs[2] = {x, x};
  float ys[2] = {0.0f, ytop};
  color(color_name);
  if (dashed) {
    dash();
  } else {
    dot();
  }
  linwid(3);
  curve(xs, ys, 2);
  solid();
  color("fore");
  linwid(1);
  height(24);
  rlmess(label, x + 1.5f, ytop * label_y_frac);
}

static float nice_ytick(float ymax) {
  if (ymax <= 512.0f) {
    return 64.0f;
  }
  if (ymax <= 4096.0f) {
    return 512.0f;
  }
  if (ymax <= 16384.0f) {
    return 2048.0f;
  }
  return 4096.0f;
}

static void render_infinite(const char *csv_path, const char *out_path) {
  infinite_sweep_t data;
  float xmax = 0.0f;
  float ymax = 0.0f;
  float xaxis_max;
  float yaxis_max;
  float ytick;
  float marker_100k_mps;
  float service_mps;

  if (!read_infinite_csv(csv_path, &data)) {
    exit(1);
  }

  for (int i = 0; i < data.n; i++) {
    if (data.x[i] > xmax) {
      xmax = data.x[i];
    }
    if (data.max_cells[i] > ymax) {
      ymax = data.max_cells[i];
    }
  }

  xaxis_max = ceilf((xmax * 1.04f) / 25.0f) * 25.0f;
  yaxis_max = ceilf((ymax * 1.08f) / 1024.0f) * 1024.0f;
  if (yaxis_max < 512.0f) {
    yaxis_max = 512.0f;
  }
  ytick = nice_ytick(yaxis_max);
  marker_100k_mps = data.n_bins * 100000.0f / 1.0e6f;
  service_mps = data.clock_hz * data.drain_cells_per_cycle / 1.0e6f;

  start_page(out_path);
  titlin("Infinite-Capacity Bunched Update Queue", 2);
  titlin("D/Det/1/infinity reference; cyan band = sampled min..max, red = p50, blue dashed = max", 4);
  name("offered push rate [M bin-updates/s]", "x");
  name("queued update cells [entries]", "y");
  labdig(0, "xy");
  axspos(420, 1390);
  axslen(2180, 890);
  height(30);
  graf(0.0f, xaxis_max, 0.0f, 25.0f, 0.0f, yaxis_max, 0.0f, ytick);

  draw_band(&data);
  grid(1, 1);

  color("red");
  solid();
  linwid(7);
  curve(data.x, data.p50_cells, data.n);
  color("blue");
  dash();
  linwid(4);
  curve(data.x, data.max_cells, data.n);
  solid();
  color("fore");
  linwid(1);

  draw_vline(marker_100k_mps, yaxis_max, "100 kHz/bin", "black", 1, 0.86f);
  draw_vline(service_mps, yaxis_max, "service limit", "green", 0, 0.78f);

  color("fore");
  linwid(1);
  height(30);
  title();
  finish_page();
}

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: %s infinite_csv output_plot\n", argv[0]);
    return 2;
  }
  render_infinite(argv[1], argv[2]);
  return 0;
}
