#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "dislin.h"

#define MAX_POINTS 5000
#define MAX_SERIES 8

typedef struct {
  char key[64];
  char label[96];
  int n;
  float x[MAX_POINTS];
  float y[MAX_POINTS];
} series_t;

typedef struct {
  int n;
  float cycle[MAX_POINTS];
  float tlm_occ[MAX_POINTS];
  float rtl_occ[MAX_POINTS];
  float tlm_ovf[MAX_POINTS];
  float rtl_ovf[MAX_POINTS];
  int mismatches;
} trace_t;

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

static int read_series_csv(
    const char *path,
    int key_col,
    int label_col,
    int x_col,
    int y_col,
    series_t *series,
    int *nseries) {
  FILE *fp = fopen(path, "r");
  char line[2048];
  int first = 1;
  *nseries = 0;

  if (fp == NULL) {
    fprintf(stderr, "could not open CSV: %s\n", path);
    return 0;
  }

  while (fgets(line, sizeof(line), fp) != NULL) {
    char *tokens[16] = {0};
    int ntok;
    int idx;

    if (first) {
      first = 0;
      continue;
    }
    ntok = split_csv(line, tokens, 16);
    if (ntok <= key_col || ntok <= label_col || ntok <= x_col || ntok <= y_col) {
      continue;
    }

    idx = find_or_add_series(series, nseries, tokens[key_col], tokens[label_col]);
    if (idx < 0 || series[idx].n >= MAX_POINTS) {
      continue;
    }
    series[idx].x[series[idx].n] = (float) atof(tokens[x_col]);
    series[idx].y[series[idx].n] = (float) atof(tokens[y_col]);
    series[idx].n++;
  }

  fclose(fp);
  return 1;
}

static int read_trace_column_csv(const char *path, float *cycle, float *occ, float *ovf, int *n) {
  FILE *fp = fopen(path, "r");
  char line[2048];
  int first = 1;
  *n = 0;

  if (fp == NULL) {
    fprintf(stderr, "could not open trace CSV: %s\n", path);
    return 0;
  }

  while (fgets(line, sizeof(line), fp) != NULL) {
    char *tokens[16] = {0};
    int ntok;
    if (first) {
      first = 0;
      continue;
    }
    ntok = split_csv(line, tokens, 16);
    if (ntok < 10 || *n >= MAX_POINTS) {
      continue;
    }
    cycle[*n] = (float) atof(tokens[0]);
    occ[*n] = (float) atof(tokens[7]);
    ovf[*n] = (float) atof(tokens[9]);
    (*n)++;
  }

  fclose(fp);
  return 1;
}

static int read_trace_pair(const char *tlm_path, const char *rtl_path, trace_t *trace) {
  int ntlm = 0;
  int nrtl = 0;
  float rtl_cycle[MAX_POINTS];

  trace->mismatches = 0;
  if (!read_trace_column_csv(tlm_path, trace->cycle, trace->tlm_occ, trace->tlm_ovf, &ntlm)) {
    return 0;
  }
  if (!read_trace_column_csv(rtl_path, rtl_cycle, trace->rtl_occ, trace->rtl_ovf, &nrtl)) {
    return 0;
  }

  trace->n = ntlm < nrtl ? ntlm : nrtl;
  if (ntlm != nrtl) {
    trace->mismatches++;
  }
  for (int i = 0; i < trace->n; i++) {
    if (trace->cycle[i] != rtl_cycle[i] ||
        trace->tlm_occ[i] != trace->rtl_occ[i] ||
        trace->tlm_ovf[i] != trace->rtl_ovf[i]) {
      trace->mismatches++;
    }
  }
  return 1;
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

static void set_series_style(int idx) {
  static const char *colors[] = {"blue", "red", "green", "magenta", "black", "cyan"};
  color(colors[idx % 6]);
  if (idx == 0) {
    solid();
  } else if (idx == 1) {
    dash();
  } else if (idx == 2) {
    dot();
  } else {
    dashl();
  }
  linwid(6);
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

static void draw_legend_box(float x, float y_top, float width, float height) {
  float xs[4] = {x, x + width, x + width, x};
  float ys[4] = {y_top, y_top, y_top - height, y_top - height};
  color("white");
  solid();
  rlarea(xs, ys, 4);
  color("fore");
}

static void draw_common_axes(void) {
  axspos(420, 1390);
  axslen(2180, 890);
  height(30);
}

static void render_loss_plot(const char *csv_path, const char *out_path) {
  series_t series[MAX_SERIES];
  int nseries = 0;
  float cap_x[2] = {160.0f, 160.0f};
  float cap_y[2] = {0.0f, 1.0f};

  if (!read_series_csv(csv_path, 0, 1, 2, 3, series, &nseries)) {
    exit(1);
  }

  start_page(out_path);
  titlin("Queue Loss at the 8-MuTRiG Link Cap", 2);
  titlin("256 channels, 200 Mhit/s aggregate, 256-cycle saturated stall", 4);
  name("coalescing queue depth [entries]", "x");
  name("lost offered-hit fraction", "y");
  labdig(0, "x");
  labdig(2, "y");
  draw_common_axes();
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

  draw_legend_box(165.0f, 1.00f, 88.0f, 0.24f);
  for (int i = 0; i < nseries; i++) {
    draw_legend_entry(170.0f, 0.96f - 0.075f * i, 18.0f, series[i].label, i);
  }
  height(24);
  rlmess("preset depth 160", 166.0f, 0.09f);
  title();
  finish_page();
}

static void render_quantile_plot(const char *csv_path, const char *out_path) {
  series_t series[MAX_SERIES];
  int nseries = 0;
  float preset_x[2] = {0.0f, 256.0f};
  float preset_y[2] = {160.0f, 160.0f};

  if (!read_series_csv(csv_path, 0, 1, 2, 3, series, &nseries)) {
    exit(1);
  }

  start_page(out_path);
  titlin("Analytical Queue Depth Regions", 2);
  titlin("iid tags, one hit/cycle, 256-cycle no-drain window", 4);
  name("active histogram channels", "x");
  name("required queue depth [entries]", "y");
  labdig(0, "xy");
  draw_common_axes();
  graf(0.0f, 256.0f, 0.0f, 32.0f, 0.0f, 270.0f, 0.0f, 30.0f);
  grid(1, 1);

  for (int i = 0; i < nseries; i++) {
    set_series_style(i);
    curve(series[i].x, series[i].y, series[i].n);
  }

  color("black");
  dashl();
  linwid(3);
  curve(preset_x, preset_y, 2);
  solid();
  color("fore");
  linwid(1);

  draw_legend_box(8.0f, 263.0f, 112.0f, 70.0f);
  for (int i = 0; i < nseries; i++) {
    draw_legend_entry(10.0f, 255.0f - 18.0f * i, 22.0f, series[i].label, i);
  }
  height(24);
  rlmess("preset depth 160", 12.0f, 151.0f);
  title();
  finish_page();
}

static void render_trace_plot(const char *tlm_path, const char *rtl_path, const char *out_path) {
  trace_t trace;
  float xmax;
  float ytop = 180.0f;

  if (!read_trace_pair(tlm_path, rtl_path, &trace)) {
    exit(1);
  }
  xmax = trace.n > 0 ? trace.cycle[trace.n - 1] : 1.0f;

  start_page(out_path);
  titlin("TLM and RTL Cycle Trace Agreement", 2);
  titlin("occupancy and cumulative overflow count, queue depth 160", 4);
  name("cycle", "x");
  name("entries / overflow count", "y");
  labdig(0, "xy");
  draw_common_axes();
  graf(0.0f, xmax, 0.0f, 150.0f, 0.0f, ytop, 0.0f, 20.0f);
  grid(1, 1);

  color("blue");
  solid();
  linwid(7);
  curve(trace.cycle, trace.tlm_occ, trace.n);
  color("red");
  dash();
  linwid(5);
  curve(trace.cycle, trace.rtl_occ, trace.n);
  color("green");
  solid();
  linwid(7);
  curve(trace.cycle, trace.tlm_ovf, trace.n);
  color("magenta");
  dash();
  linwid(5);
  curve(trace.cycle, trace.rtl_ovf, trace.n);

  draw_legend_box(705.0f, 174.0f, 390.0f, 66.0f);
  draw_legend_entry(720.0f, 168.0f, 80.0f, "TLM occupancy", 0);
  draw_legend_entry(720.0f, 156.0f, 80.0f, "RTL occupancy", 1);
  draw_legend_entry(720.0f, 144.0f, 80.0f, "TLM overflow", 2);
  draw_legend_entry(720.0f, 132.0f, 80.0f, "RTL overflow", 3);
  height(24);
  if (trace.mismatches == 0) {
    rlmess("0 mismatches", 900.0f, 116.0f);
  } else {
    rlmess("mismatch detected", 900.0f, 116.0f);
  }
  title();
  finish_page();
}

int main(int argc, char **argv) {
  if (argc == 4 && strcmp(argv[1], "loss") == 0) {
    render_loss_plot(argv[2], argv[3]);
    return 0;
  }
  if (argc == 4 && strcmp(argv[1], "quantiles") == 0) {
    render_quantile_plot(argv[2], argv[3]);
    return 0;
  }
  if (argc == 5 && strcmp(argv[1], "trace") == 0) {
    render_trace_plot(argv[2], argv[3], argv[4]);
    return 0;
  }

  fprintf(
      stderr,
      "usage:\n"
      "  %s loss loss_csv output_plot\n"
      "  %s quantiles quantile_csv output_plot\n"
      "  %s trace tlm_csv rtl_csv output_plot\n",
      argv[0],
      argv[0],
      argv[0]);
  return 2;
}
