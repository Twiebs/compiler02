
MemoryBlockHeader *AllocateMemoryBlock(MemoryBlockHeader *parent, size_t blockSize) {
  assert(blockSize > sizeof(MemoryBlockHeader));
  MemoryBlockHeader *block = (MemoryBlockHeader *)malloc(blockSize);
  memset(block, 0x00, blockSize);
  block->used = sizeof(MemoryBlockHeader);
  if (parent != nullptr) parent->next = block;
  return block;
}

void InitalizeAllocator(PersistantBlockAllocator *allocator, size_t blockSize) {
  assert(blockSize > sizeof(MemoryBlockHeader));
  allocator->blockSize = blockSize;
  allocator->firstBlock = AllocateMemoryBlock(nullptr, blockSize);
  allocator->currentBlock = allocator->firstBlock;
}

uint8_t *Allocate(PersistantBlockAllocator *allocator, size_t size, size_t alignment) {
  assert(size <= (allocator->blockSize - sizeof(MemoryBlockHeader)));
  allocator->currentBlock->used = (allocator->currentBlock->used + alignment) & ~alignment;
  if (allocator->currentBlock->used + size > allocator->blockSize) {
    allocator->currentBlock = AllocateMemoryBlock(allocator->currentBlock, allocator->blockSize);
  }

  uintptr_t baseAddress = (uintptr_t)allocator->currentBlock;
  uint8_t *result = (uint8_t *)(baseAddress + allocator->currentBlock->used);
  assert(((uintptr_t)result & ~alignment) == (uintptr_t)result);
  allocator->currentBlock->used += size;
  return result;
}

StringReference AllocateString(PersistantBlockAllocator *allocator, const char *string, size_t length) {
  uint8_t *memory = Allocate(allocator, length + 1, 4);
  memcpy(memory, string, length);
  memory[length] = 0;
  StringReference result = {};
  result.length = length;
  result.string = (char *)memory;
  return result;
}

int StringToInt(const char *s, size_t l) {
  int result = 0;
  int power = 1;
  for (int i = l - 1; i >= 0; i--) {
    int value = s[i] - '0';
    result += value * power;
    power *= 10;
  }
  return result;
}

bool Equals(StringReference a, const char *b, size_t length) {
  if (a.length != length) return false;
  for (size_t i = 0; i < length; i++) {
    if (a.string[i] != b[i]) return false;
  }
  return true;
}