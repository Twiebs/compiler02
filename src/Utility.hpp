
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