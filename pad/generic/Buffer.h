#ifndef Buffer_h
#define Buffer_h

#include <stdlib.h>

class Buffer {
public:
  Buffer();
  ~Buffer();

  void allocate(int bytes);	// Make some space
  void append(unsigned char c);	// Add an element at the end
  unsigned char get(int n);	// Get an element
  void put(int index, unsigned char c);	// Replace an element
  int count();			// Return the element count
  void setcount(int n);		// Set the element count
  unsigned char pop();		// Get last element and decrease count
  unsigned char *data();	// Return a pointer to element 0

  void fopen(unsigned char *buf, int count); // Set buffer data and count
  void gettoken(char *str);
  int fgets(char *buf, int max);
  unsigned char *fread(int n);

  void seek(int n);
  void rseek(int n);
  unsigned char get();
  void put(unsigned char c);

private:
  unsigned char *data_;
  int count_;
  int max_;
  int pos_;
};

inline Buffer::Buffer()
  : data_(0), count_(0), max_(0), pos_(0)
{
}

inline Buffer::~Buffer()
{
  if (max_) free(data_);	// If max_ is zero we didn't malloc data_
}

inline void 
Buffer::allocate(int bytes)
{
  max_ = bytes;
  if (data_) free(data_); 
  data_ = (unsigned char *)malloc(bytes);
}

//inline void 
//Buffer::deallocate() {if (max_) {free(data_); max_ = 0; data_ = 0;}}

inline void 
Buffer::append(unsigned char c) {data_[count_++] = c;}

inline unsigned char 
Buffer::get(int n) {return data_[n];}

inline void 
Buffer::put(int index, unsigned char c) {data_[index] = c;}

inline unsigned char 
Buffer::pop() {return data_[--count_];}

inline int 
Buffer::count() {return count_;}

inline void 
Buffer::setcount(int n) {count_ = n;}

inline unsigned char *
Buffer::data() {return data_;}

inline void 
Buffer::seek(int n) {pos_ = n;}

inline void 
Buffer::rseek(int n) {pos_ += n;}

inline unsigned char 
Buffer::get() {return data_[pos_];}

inline void
Buffer::put(unsigned char c) {data_[pos_] = c;}

#endif
