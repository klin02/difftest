#define FUSE_USE_VERSION 35
#include <fuse3/fuse.h>
#include <cstring>
#include <mutex>
#include <condition_variable>
#include <vector>
#include <poll.h>
#include <atomic>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#define SHM_NAME "/my_shm_struct"
#define SHM_SIZE sizeof(struct shared_data)

constexpr size_t BUFFER_SIZE = 4096;

struct shared_data {
  int ready;
};

class XDMADevice {
public:
    std::mutex mutex;
    std::condition_variable read_cond, write_cond;

    std::atomic<bool> read_waiting = false;
    std::vector<char> buffer;

    bool is_ready() {
        return read_waiting && buffer.size() < BUFFER_SIZE;
    }

    static XDMADevice& instance() {
        static XDMADevice dev;
        return dev;
    }
};

int xdma_getattr(const char* path, struct stat* stbuf,
                 struct fuse_file_info* fi) {
    memset(stbuf, 0, sizeof(struct stat));
    if (strcmp(path, "/") == 0) {
        stbuf->st_mode = S_IFDIR | 0755;
        stbuf->st_nlink = 2;
        return 0;
    }

    // Currently only support simulate c2h
    if (strcmp(path, "/xdma0_c2h_0") == 0) {
        stbuf->st_mode = S_IFREG | 0666;
        stbuf->st_nlink = 1;
        stbuf->st_size = 4096;
        return 0;
    }

    return -ENOENT;
}

int xdma_readdir(const char* path, void* buf, fuse_fill_dir_t filler,
                 off_t offset, struct fuse_file_info* fi,
                 enum fuse_readdir_flags flags) {
    if (strcmp(path, "/") != 0)
        return -ENOENT;

    filler(buf, ".", NULL, 0, FUSE_FILL_DIR_PLUS);
    filler(buf, "..", NULL, 0, FUSE_FILL_DIR_PLUS);
    filler(buf, "xdma0_c2h_0", NULL, 0, FUSE_FILL_DIR_PLUS);

    return 0;
}

int xdma_open(const char* path, struct fuse_file_info* fi) {
    return 0;
}

int xdma_read(const char* path, char* buf, size_t size, off_t offset,
              struct fuse_file_info* fi) {
    printf("[XDMA_SIM] Reading %ld\n", size);
    auto& dev = XDMADevice::instance();

    std::unique_lock<std::mutex> lock(dev.mutex);
    dev.read_waiting = true;
    dev.buffer.clear();

    // wait until xdma_write fill buffer
    dev.write_cond.wait(lock, [&]() {
        return dev.buffer.size() >= BUFFER_SIZE;
    });

    size_t to_copy = std::min(size, dev.buffer.size());
    memcpy(buf, dev.buffer.data(), to_copy);
    dev.buffer.clear();
    dev.read_waiting = false;
    return to_copy;
}

int xdma_write(const char* path, const char* buf, size_t size, off_t offset,
               struct fuse_file_info* fi) {
    printf("[XDMA_SIM] Writing %ld\n", size);
    auto& dev = XDMADevice::instance();
    std::unique_lock<std::mutex> lock(dev.mutex);

    // return size of writing, 0 when writing failed
    if (!dev.read_waiting) return 0;

    size_t space = BUFFER_SIZE - dev.buffer.size();
    size_t to_write = std::min(space, size);
    dev.buffer.insert(dev.buffer.end(), buf, buf + to_write);

    if (dev.buffer.size() >= BUFFER_SIZE)
        dev.write_cond.notify_one();

    return to_write;
}

int xdma_poll(const char* path, struct fuse_file_info* fi,
              struct fuse_pollhandle* ph, unsigned* reventsp) {
    // printf("enter poll\n");
    auto& dev = XDMADevice::instance();
    // std::lock_guard<std::mutex> lock(dev.mutex);
    // printf("pass lock\n");
    if (dev.is_ready()) {
        *reventsp = POLLIN;
    } else {
        *reventsp = 0;
    }
    // printf("[XDMA_SIM]: Poll %d\n", *reventsp);
    return 0;
}

static struct fuse_operations xdma_ops = {
    .getattr = xdma_getattr,
    .open = xdma_open,
    .read = xdma_read,
    .write = xdma_write,
    .readdir = xdma_readdir,
    .poll = xdma_poll,
};

int main(int argc, char* argv[]) {
      int fd = shm_open(SHM_NAME, O_CREAT | O_RDWR, 0666);
    if (fd == -1) {
        perror("shm_open");
        return 1;
    }
    ftruncate(fd, sizeof(struct shared_data));
    struct shared_data *ptr = (struct shared_data *)mmap(NULL, SHM_SIZE,
                                  PROT_READ | PROT_WRITE,
                                  MAP_SHARED, fd, 0);

    ptr->ready = 666;
    return fuse_main(argc, argv, &xdma_ops, nullptr);
}