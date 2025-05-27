
#include "xdma_sim.h"
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

static int xdma_sim_fd = -1;
static struct pollfd pfd;
static int shm_fd = -1;

#define SHM_NAME "/my_shm_struct"
#define SHM_SIZE sizeof(struct shared_data)

struct shared_data {
  int ready;
};
void xdma_sim_init() {
  xdma_sim_fd = open(XDMA_SIM_DEV, O_RDWR | O_NONBLOCK);
  if (xdma_sim_fd == -1) {
    perror("XDMA_SIM: Failed to open\n");
    exit(-1);
  } else {
    pfd.fd = xdma_sim_fd;
    pfd.events = POLLOUT;
  }
  printf(">>>>>>> fd %d\n", xdma_sim_fd);
    int fd = shm_open(SHM_NAME, O_CREAT | O_RDWR, 0666);
    if (fd == -1) {
        perror("shm_open");
    }

    struct shared_data *ptr = (struct shared_data *)mmap(NULL, SHM_SIZE,
                                  PROT_READ | PROT_WRITE,
                                  MAP_SHARED, fd, 0);
    printf("shm valu: %d\n", ptr->ready);
}

void xdma_sim_finish() {
  close(xdma_sim_fd);
}

extern "C" int xdma_sim_tready() {
  if (xdma_sim_fd < 0)
    return 0;
  int ret = poll(&pfd, 1, 0);
  // printf("TREADY: %d %d %d\n", ret, pfd.revents, POLLIN);
  if ((ret > 0) && (pfd.revents & POLLIN)) {
    printf("tready\n");
  }
  return (ret > 0) && (pfd.revents & POLLIN);
}