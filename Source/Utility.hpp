
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

MemoryBlockHeader *AllocateMemoryBlock(MemoryBlockHeader *parent, size_t blockSize);
void InitalizeAllocator(PersistantBlockAllocator *allocator, size_t blockSize);
uint8_t *Allocate(PersistantBlockAllocator *allocator, size_t size, size_t alignment = 8);
StringReference AllocateString(PersistantBlockAllocator *allocator, const char *string, size_t length);

int StringToInt(const char *s, size_t l);
bool Equals(StringReference a, const char *b, size_t length);