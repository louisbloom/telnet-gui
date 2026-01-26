#ifndef HISTOGRAM_H
#define HISTOGRAM_H

#include <stddef.h>
#include <stdint.h>

#define HISTOGRAM_DEFAULT_CAPACITY 100
#define HISTOGRAM_DEFAULT_PRINT_INTERVAL 10

typedef struct {
    size_t count;
    uint64_t min, max, p50, p90, p99;  /* nanoseconds */
} HistogramStats;

typedef struct Histogram Histogram;

Histogram *histogram_create(const char *name, size_t capacity, size_t print_interval);
void histogram_destroy(Histogram *h);
int histogram_record(Histogram *h, uint64_t sample_ns);  /* returns 1 if auto-printed */
void histogram_get_stats(const Histogram *h, HistogramStats *stats);
void histogram_print(const Histogram *h);
void histogram_reset(Histogram *h);

#endif
