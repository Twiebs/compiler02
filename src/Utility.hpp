
#define LiteralAndLength(string) string, sizeof(string) - 1

struct MemoryBlockHeader {
  size_t used;
  MemoryBlockHeader *next;
} __attribute((packed));

//This class is used to allocated persistant memory.  It
//does not implement any "free memory" procedures and is only
//intended for data that will persist throughout the durration
//of the compiler's runtime.  Does not attempt to be particulary
//effeciant with space wastage.  Only intended for realtiivly small
//and similarly sized allocations.
struct PersistantBlockAllocator {
  MemoryBlockHeader *firstBlock;
  MemoryBlockHeader *currentBlock;
  size_t blockSize;
};

struct Allocator_Stack {
  size_t memory_size;
  size_t memory_used;
  uint8_t *memory;
};

void allocator_stack_initalize(Allocator_Stack *allocator, size_t size) {
  allocator->memory_size = size;
  allocator->memory_used = 0;
  allocator->memory = (uint8_t *)malloc(size);
  if (allocator->memory == nullptr) {
    printf("fatal error: out of memory");
    abort();
  }
}

uint8_t *allocator_stack_push_size(Allocator_Stack *allocator, void *data, size_t size) {
  if (allocator->memory_used + size > allocator->memory_size) {
    assert(false);
  }

  uint8_t *result = allocator->memory + allocator->memory_used;
  memcpy(result, data, size);
  allocator->memory_used += size;
  return result;
}

uint8_t *allocator_stack_push_string_fmt(Allocator_Stack *allocator, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  uint8_t *result = allocator->memory + allocator->memory_used;
  size_t size_remaining = allocator->memory_size - allocator->memory_used;
  int bytes_written = vsnprintf((char *)result, size_remaining, fmt, args);
  if (bytes_written < 0) assert(false); //Encoding error
  if (bytes_written > size_remaining) {
    assert(false);
  }

  //The null terminator is not included
  allocator->memory_used += bytes_written;
  return result;
}

uint8_t *allocator_stack_push_string_literal(Allocator_Stack *allocator, const char *string_literal);
#define allocator_stack_push_size(allocator, data, size) allocator_stack_push_size(allocator, (void *)(data), size)
#define allocator_stack_push_string_literal(allocator, string_literal) allocator_stack_push_size(allocator, string_literal, sizeof(string_literal) - 1)

struct StringReference {
  size_t length;
  char *string;
};


class TemporaryString {
public:
  TemporaryString(const char *fmt, ...);
  ~TemporaryString();

  void set(const char *fmt, ...);
  const char *getString();

private:
  char buffer[4096];
  int used = 0;
  char *dynamicString;
};

//TODO Make a better robust version of this
class TempStringBuilder {
  char buffer[256];
  int used;

public:
  TempStringBuilder() : used(0) {}

  TempStringBuilder& append(const char *s) {
    while (*s != 0) {
      buffer[used++] = *s;
      s++;
    }
    return *this;
  }

  void clear() {
    used = 0;
  }

  char *get() {
    buffer[used] = 0;
    return buffer;
  }
};

MemoryBlockHeader *AllocateMemoryBlock(MemoryBlockHeader *parent, size_t blockSize);
void InitalizeAllocator(PersistantBlockAllocator *allocator, size_t blockSize);
uint8_t *Allocate(PersistantBlockAllocator *allocator, size_t size, size_t alignment = 8);
StringReference AllocateString(PersistantBlockAllocator *allocator, const char *string, size_t length);

bool Equals(StringReference a, const char *b, size_t length);
bool Equals(StringReference a, StringReference b);
bool MatchesCString(StringReference a, const char *b);

bool string_index_of_char_from_back(char c, const char *s, int64_t l, size_t *result);