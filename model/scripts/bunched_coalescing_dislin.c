#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "dislin.h"

#define MAX_POINTS 256

typedef struct {
  char key[64];
  char label[96];
  int n;
  float x[MAX_POINTS];
  float min_cells[MAX_POINTS];
  float p50_cells[MAX_POINTS];
  float max_cells[MAX_POINTS];
} sweep_series_t;

typedef struct {
  sweep_series_t per_bin;
  sweep_series_t cb;
  float n_bins;
  float clock_hz;
  float drain_cells_per_cycle;
} sweep_data_t;

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

static sweep_series_t *series_for_key(sweep_data_t *data, const char *key) {
  if (strcmp(key, "per_bin_counter") == 0) {
    return &data->per_bin;
  }
  if (strcmp(key, "tag_addressable_cb") == 0) {
    return &data->cb;
  }
  return NULL;
}

static int read_sweep_csv(const char *path, sweep_data_t *data, int metric_is_kick) {
  FILE *fp = fopen(path, "r");
  char line[4096];
  int first = 1;

  memset(data, 0, sizeof(*data));
  snprintf(data->per_bin.key, sizeof(data->per_bin.key), "per_bin_counter");
  snprintf(data->per_bin.label, sizeof(data->per_bin.label), "per-bin counters");
  snprintf(data->cb.key, sizeof(data->cb.key), "tag_addressable_cb");
  snprintf(data->cb.label, sizeof(data->cb.label), "tag-addressable CB");

  if (fp == NULL) {
    fprintf(stderr, "could not open CSV: %s\n", path);
    return 0;
  }

  while (fgets(line, sizeof(line), fp) != NULL) {
    char *tokens[32] = {0};
    int ntok;
    sweep_series_t *series;
    int idx;

    if (first) {
      first = 0;
      continue;
    }

    ntok = split_csv(line, tokens, 32);
    if (ntok < 30) {
      continue;
    }

    series = series_for_key(data, tokens[1]);
    if (series == NULL || series->n >= MAX_POINTS) {
      continue;
    }

    idx = series->n;
    snprintf(series->label, sizeof(series->label), "%s", tokens[2]);
    series->x[idx] = (float) atof(tokens[5]);
    if (metric_is_kick) {
      series->min_cells[idx] = (float) atof(tokens[27]);
      series->p50_cells[idx] = (float) atof(tokens[28]);
      series->max_cells[idx] = (float) atof(tokens[29]);
    } else {
      series->min_cells[idx] = (float) atof(tokens[9]);
      series->p50_cells[idx] = (float) atof(tokens[10]);
      series->max_cells[idx] = (float) atof(tokens[11]);
    }
    series->n++;

    data->n_bins = (float) atof(tokens[21]);
    data->clock_hz = (float) atof(tokens[22]);
    data->drain_cells_per_cycle = (float) atof(tokens[24]);
  }

  fclose(fp);
  return data->per_bin.n > 1 && data->cb.n > 1;
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

static int bands_equal(const sweep_series_t *a, const sweep_series_t *b) {
  if (a->n != b->n) {
    return 0;
  }
  for (int i = 0; i < a->n; i++) {
    if (fabsf(a->x[i] - b->x[i]) > 1.0e-3f ||
        fabsf(a->min_cells[i] - b->min_cells[i]) > 1.0e-3f ||
        fabsf(a->p50_cells[i] - b->p50_cells[i]) > 1.0e-3f ||
        fabsf(a->max_cells[i] - b->max_cells[i]) > 1.0e-3f) {
      return 0;
    }
  }
  return 1;
}

static void draw_band(const sweep_series_t *series, const char *color_name, long pattern) {
  float xb[2 * MAX_POINTS];
  float yb[2 * MAX_POINTS];
  int n = series->n;
  if (n < 2) {
    return;
  }
  for (int i = 0; i < n; i++) {
    xb[i] = series->x[i];
    yb[i] = series->max_cells[i];
    xb[2 * n - 1 - i] = series->x[i];
    yb[2 * n - 1 - i] = series->min_cells[i];
  }
  color(color_name);
  shdpat(pattern);
  rlarea(xb, yb, 2 * n);
  color("fore");
  solid();
  shdpat(0L);
}

static void draw_boundaries(const sweep_series_t *series, const char *color_name, int dashed) {
  color(color_name);
  if (dashed) {
    dash();
  } else {
    solid();
  }
  linwid(3);
  curve(series->x, series->min_cells, series->n);
  curve(series->x, series->max_cells, series->n);
  solid();
  color("fore");
  linwid(1);
}

static void draw_vline(float x, float ytop, const char *label, const char *color_name, int dashed) {
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
  height(24);
  rlmess(label, x + 1.5f, ytop * 0.88f);
}

static void draw_line_legend(float x, float y, const char *label, const char *color_name, int dashed) {
  float xs[2] = {x, x + 24.0f};
  float ys[2] = {y, y};
  color(color_name);
  if (dashed) {
    dash();
  } else {
    solid();
  }
  linwid(6);
  curve(xs, ys, 2);
  solid();
  color("fore");
  linwid(1);
  height(24);
  rlmess(label, x + 29.0f, y);
}

static void draw_fill_legend(float x, float y, const char *label) {
  float xs[4] = {x, x + 24.0f, x + 24.0f, x};
  float ys[4] = {y + 4.0f, y + 4.0f, y - 4.0f, y - 4.0f};
  color("cyan");
  shdpat(16L);
  rlarea(xs, ys, 4);
  color("fore");
  linwid(1);
  height(24);
  rlmess(label, x + 29.0f, y - 2.0f);
}

static void render_sweep(const char *csv_path, const char *out_path, int metric_is_kick, const char *traffic_label) {
  sweep_data_t data;
  int same_bands;
  float xmax = 0.0f;
  float ymax = 0.0f;
  float xaxis_max;
  float yaxis_max;
  float ytick;
  float marker_100k_mps;
  float service_mps;

  if (!read_sweep_csv(csv_path, &data, metric_is_kick)) {
    exit(1);
  }
  same_bands = bands_equal(&data.per_bin, &data.cb);

  for (int i = 0; i < data.per_bin.n; i++) {
    if (data.per_bin.x[i] > xmax) {
      xmax = data.per_bin.x[i];
    }
    if (data.per_bin.max_cells[i] > ymax) {
      ymax = data.per_bin.max_cells[i];
    }
  }
  for (int i = 0; i < data.cb.n; i++) {
    if (data.cb.x[i] > xmax) {
      xmax = data.cb.x[i];
    }
    if (data.cb.max_cells[i] > ymax) {
      ymax = data.cb.max_cells[i];
    }
  }

  xaxis_max = ceilf((xmax * 1.04f) / 25.0f) * 25.0f;
  if (metric_is_kick) {
    float padded = ymax * 1.12f;
    if (padded <= 8.0f) {
      yaxis_max = 8.0f;
      ytick = 2.0f;
    } else if (padded <= 16.0f) {
      yaxis_max = 16.0f;
      ytick = 4.0f;
    } else if (padded <= 32.0f) {
      yaxis_max = 32.0f;
      ytick = 8.0f;
    } else {
      yaxis_max = ceilf(padded / 32.0f) * 32.0f;
      ytick = 16.0f;
    }
  } else {
    yaxis_max = ceilf((ymax * 1.08f) / 32.0f) * 32.0f;
    if (yaxis_max < 64.0f) {
      yaxis_max = 64.0f;
    }
    ytick = 32.0f;
  }

  marker_100k_mps = data.n_bins * 100000.0f / 1.0e6f;
  service_mps = data.clock_hz * data.drain_cells_per_cycle / 1.0e6f;

  start_page(out_path);
  if (metric_is_kick) {
    char title[160];
    snprintf(title, sizeof(title), "%s Coalescer Kick Count", traffic_label);
    titlin(title, 2);
    titlin("cyan = per-bin min..max; red dashed = CB min/max; solid lines = p50", 4);
  } else {
    char title[160];
    snprintf(title, sizeof(title), "%s Coalescer Queue Occupancy", traffic_label);
    titlin(title, 2);
    titlin("cyan = per-bin min..max; red dashed = CB min/max; solid lines = p50", 4);
  }
  name("offered push rate [M bin-updates/s]", "x");
  name(metric_is_kick ? "kick_count per live cell" : "pending update cells [entries]", "y");
  labdig(0, "xy");
  axspos(420, 1390);
  axslen(2180, 890);
  height(30);
  graf(0.0f, xaxis_max, 0.0f, 25.0f, 0.0f, yaxis_max, 0.0f, ytick);

  if (same_bands) {
    draw_band(&data.per_bin, "cyan", 16L);
  } else {
    draw_band(&data.per_bin, "cyan", 16L);
  }

  grid(1, 1);
  draw_boundaries(&data.per_bin, "blue", 0);
  draw_boundaries(&data.cb, "red", 1);

  color("blue");
  solid();
  linwid(6);
  curve(data.per_bin.x, data.per_bin.p50_cells, data.per_bin.n);
  color("red");
  solid();
  linwid(6);
  curve(data.cb.x, data.cb.p50_cells, data.cb.n);
  solid();
  color("fore");
  linwid(1);

  if (!metric_is_kick) {
    draw_vline(marker_100k_mps, yaxis_max, "100 kHz/bin", "black", 1);
    draw_vline(service_mps, yaxis_max, "0.5 cell/cyc", "green", 0);
  }

  if (!metric_is_kick) {
    draw_fill_legend(xaxis_max * 0.63f, yaxis_max * 0.37f, same_bands ? "shared min..max band" : "per-bin min..max band");
    draw_line_legend(xaxis_max * 0.63f, yaxis_max * 0.29f, "per-bin p50", "blue", 0);
    draw_line_legend(xaxis_max * 0.63f, yaxis_max * 0.21f, "CB p50", "red", 0);
    if (!same_bands) {
      draw_line_legend(xaxis_max * 0.63f, yaxis_max * 0.13f, "CB min/max", "red", 1);
    }
  }
  if (same_bands && !metric_is_kick) {
    height(20);
    rlmess("bands overlap for deterministic all-bin bunches", xaxis_max * 0.63f, yaxis_max * 0.14f);
  }

  color("fore");
  linwid(1);
  height(30);
  title();
  finish_page();
}

int main(int argc, char **argv) {
  if (argc != 5) {
    fprintf(stderr, "usage: %s sweep_csv output_plot occupancy|kick traffic_label\n", argv[0]);
    return 2;
  }
  render_sweep(argv[1], argv[2], strcmp(argv[3], "kick") == 0, argv[4]);
  return 0;
}
