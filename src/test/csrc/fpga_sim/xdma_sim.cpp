#include "xdma_sim.h"


static int shm_fd = -1;
static xdma_shm_dev *xdev = nullptr;

// API for shared XDMA Dev
void xdma_sim_init(bool is_host) {
  shm_fd = shm_open(XDMA_SHM_DEV, O_CREAT | O_RDWR, 0666);
  if (shm_fd == -1) {
    perror("XDMA_SIM: Failed to open shared memory device\n");
    exit(-1);
  }
  ftruncate(shm_fd, sizeof(xdma_shm_dev));
  xdev = (xdma_shm_dev *)mmap(NULL, sizeof(xdma_shm_dev), PROT_READ | PROT_WRITE, MAP_SHARED, shm_fd, 0);
  if (is_host) {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED); // 跨进程共享
    pthread_mutex_init(&xdev->lock, &attr);

    pthread_condattr_t cattr;
    pthread_condattr_init(&cattr);
    pthread_condattr_setpshared(&cattr, PTHREAD_PROCESS_SHARED);
    pthread_cond_init(&xdev->read_cond, &cattr);
  }
}

void xdma_sim_finish(bool is_host) {
  munmap(xdev, sizeof(xdma_shm_dev));
  close(shm_fd);
  if (is_host) {
    // shm_unlink(XDMA_SHM_DEV);
  }
}

int xdma_sim_tready() {
  if (shm_fd == -1) return 0;
  return xdev->read_waiting;
  // return 0;
}

int xdma_sim_read(char* buf, size_t size) {
    pthread_mutex_lock(&xdev->lock);

    xdev->read_waiting = true;
    xdev->write_size = 0;
    xdev->read_size = size;

    // printf("Read wait: %d\n", size);
    while (xdev->write_size < size) {
        pthread_cond_wait(&xdev->read_cond, &xdev->lock);
    }
    // printf("Read unlock: %d\n", xdev->write_size);
    size_t to_copy = size < xdev->write_size ? size : xdev->write_size;
    memcpy(buf, xdev->buffer, to_copy);

    pthread_mutex_unlock(&xdev->lock);

    return to_copy;
}

int xdma_sim_write(const char* buf, size_t size) {
    pthread_mutex_lock(&xdev->lock);
    // printf("write %d %d\n", size, xdev->write_size);
    // if (xdev->read_waiting == false) {
    //   perror("XDMA_SIM: Writing when not ready");
    //   exit(-1);
    // }
    while (!xdev->read_waiting) {
      pthread_mutex_unlock(&xdev->lock);
      pthread_mutex_lock(&xdev->lock);
    }
    size_t space = BUFFER_SIZE - xdev->write_size;
    size_t to_write = size < space ? size : space;

    memcpy(xdev->buffer + xdev->write_size, buf, to_write);
    xdev->write_size += to_write;
    if (xdev->write_size >= xdev->read_size) {
      xdev->read_waiting = false;
      pthread_cond_signal(&xdev->read_cond); // 唤醒 reader
    }
    pthread_mutex_unlock(&xdev->lock);

    return to_write;
}

extern "C" unsigned char v_xdma_tready() {
  // return xdma_sim_tready();
  return 1;
}
extern "C" void v_xdma_write(const char* axi_tdata) {
  xdma_sim_write(axi_tdata, 64);
}