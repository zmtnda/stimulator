#include "thumbsim.hpp"
#include <stdint.h>
#include <stdio.h>

template<>
void Memory<Data8, Data32>::write(const unsigned int addr, const Data32 data) {
  // data is in native format
  //   if little-endian, 3210 (MSB=3)
  // memory is in big-endian format (0123, MSB=0)
  int i;
  unsigned int myAddr = addr - base;
  if (size() < myAddr + 4) {
    m.resize(myAddr + 4, 0);
  }
  if (myAddr < lowest) lowest = myAddr;
  if (myAddr > highest) highest = myAddr;
  if (opts.writes) {
    cout << hex << addr << ": " << data << " ( ";
    for (i = 0 ; i < 4 ; i++) {
      cout << static_cast<unsigned int>(data.data_ubyte4(i)) << ' ';
    }
    cout << ")" << endl;
  }
  for (i = 0 ; i < 4 ; i++) {
    m[myAddr+i] = Data8(data.data_ubyte4(i));
  }
}

template<>
void Memory<Data16, Data16>::write(const unsigned int addr, const Data16 data) {
  // data is in native format
  int i;
  unsigned short myAddr = addr - base;
  if (size() < myAddr + 2) {
    m.resize(myAddr + 2, 0);
  }
  if (myAddr < lowest) lowest = myAddr;
  if (myAddr > highest) highest = myAddr;
  if (opts.writes) {
    cout << hex << addr << ": " << data << " ( ";
    for (i = 0 ; i < 2 ; i++) {
      cout << static_cast<unsigned int>(data.data_ubyte2(i)) << ' ';
    }
    cout << ")" << endl;
  }

  m[myAddr] = data;
}

template<>
void Memory<Data32, Data32>::write(const unsigned int addr, const Data32 data) {
  unsigned int myAddr = addr - base;
  // cout << hex << addr << ": " << data << endl;
  m[myAddr] = data;
}

template<>
const Data32 Memory<Data8, Data32>::operator[](const unsigned int addr) const {
  unsigned int myAddr = addr - base;
  return Data32(m[myAddr], m[myAddr+1], m[myAddr+2], m[myAddr+3]);
}

template<>
const Data16 Memory<Data16, Data16>::operator[](const unsigned int addr) const {
  unsigned short myAddr = addr - base;
  return m[myAddr];
}

template<>
const Data32 Memory<Data32, Data32>::operator[](const unsigned int addr) const {
  unsigned int myAddr = addr - base;
  return m[myAddr];
}

template<>
void Memory<Data8, Data32>::dump(DataType dt) const {
  Data32 d(0);
  unsigned int addr;
  for (addr = lowest ; addr <= highest + 3 ; addr++) {
    unsigned int byte = addr & 0x3;
    d.set_data_ubyte4(byte, m[addr]);
    if ((byte == 0x3) && (dt == INSTRUCTIONS)) {
      cout << hex << d << endl;
    } else if ((byte == 0x3) && (dt == DATA) && (d.data_uint() != 0)) {
      cout << hex << addr - 3 + getBase() << ": " << d << endl;
    }
  }
}

template<>
void Memory<Data16, Data16>::dump(DataType dt) const {
  if (dt == DATA) {
    for_each(m.begin(), m.end(), Data16::printD);
  }
}

template<>
void Memory<Data32, Data32>::dump(DataType dt) const {
  if (dt == DATA) {
    for_each(m.begin(), m.end(), Data32::printD);
  }
}

unsigned int makeMask(int numBits) {
   unsigned int mask = 0;
   for(int i = 0; i < numBits; i++)
      mask |= (1 << i);
   //return (1 << numBits) - 1;
   return mask;
}

unsigned int getTag(unsigned int address, int tagSize) {
   int shift = 32 - tagSize;
   
   return (address >> shift) & makeMask(tagSize);
}

unsigned int getIndex(unsigned int address, int size, int blockSize) {
   int shift = log2(blockSize);  // Shift past the byte index
   unsigned int indexMask = makeMask((unsigned int)log2(size / blockSize));
   return (address >> shift) & indexMask;
}

// CPE 315: You must implement and call this function for each 
// memory address (dmem only) accessed by the program. It should return 
// true for a cache hit and false for a cache miss, and on a cache miss, 
// should update the cache tags. The "ntries" vector contains the cache
// tags, so if you want to put the tag "t" into cache block "b", then
// evaluate "entries[b] = t;". The locals you have available to help
// you make this decision are "blocksize" (in bytes) and "size" (total
// cache size in blocks). You should also update the "hits" and
// "misses" counters.
bool Cache::access(unsigned int address) {
   int tagSize = 32 - ((int) log2(size/blocksize) + (int) log2(blocksize));
   unsigned int tagMask = makeMask(tagSize);
   
   uint16_t index = getIndex(address, size, blocksize);
   uint16_t tag = getTag(address, tagSize);
   bool hitBool = false;
   if (entries[index] == tag) {
      hits++;
      hitBool = true;
   }
   else {
      misses++;
      entries[index] = tag;
   }
   return hitBool;
}

void Stats::print() {
  cout << dec
       << "Total number of dynamic instructions: " << instrs << endl
       << "Number of Memory Reads: " << numMemReads << endl
       << "Number of Memory Writes: " << numMemWrites << endl
       << "Number of Register Reads: " << numRegReads << endl
       << "Number of Register Writes: " << numRegWrites << endl
       << "Branches: " << endl
       << "  Forward:" << endl
       << "    Taken: " << numForwardBranchesTaken << endl
       << "    Not taken: " << numForwardBranchesNotTaken << endl
       << "  Backward:" << endl
       << "    Taken: " << numBackwardBranchesTaken << endl
       << "    Not taken: " << numBackwardBranchesNotTaken << endl;
}

unsigned int swizzle(unsigned int d) {
  return (((d >> 24) & 0xff) |
          ((d >> 8)  & 0xff00) |
          ((d << 8)  & 0xff0000) |
          ((d << 24) & 0xff000000));
}
