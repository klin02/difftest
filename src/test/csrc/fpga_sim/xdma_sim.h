
#ifndef __XDMA_SIM_H__
#define __XDMA_SIM_H__
#include <pthread.h>
// perror, printf
#include <stdio.h>

// exit, malloc, free
#include <stdlib.h>

// size_t
#include <stddef.h>

// memset, memcpy
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/mman.h>
#define XDMA_SHM_DEV "/xdma_sim_shm"
#define BUFFER_SIZE 10000

typedef struct {
    pthread_mutex_t lock;
    pthread_cond_t read_cond;

    bool read_waiting;
    size_t read_size;
    size_t write_size;
    char buffer[BUFFER_SIZE];
} xdma_shm_dev;

void xdma_sim_init(bool is_host);
void xdma_sim_finish(bool is_host);
int xdma_sim_read(char* buf, size_t size);
int xdma_sim_write(const char* buf, size_t size);
int xdma_sim_tready();

#endif // __XDMA_SIM_H__