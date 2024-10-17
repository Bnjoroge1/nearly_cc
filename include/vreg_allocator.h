#ifndef VREG_ALLOCATOR_H
#define VREG_ALLOCATOR_H

// A VRegAllocator helps the high-level code generator manage
// virtual registers, including
//   - assignment of vregs as storage locations for parameters
//     and local variables
//   - allocation and deallocation of virtual registers for use
//     as temporaries in expression evaluation
class VregAllocator {
private:
  int m_num_params;      // how many parameter there are
  int m_top;             // next vreg to allocate
  int m_first_temp;      // first vreg in current block that is a temporary
  bool m_params_ended;   // true if all parameters have been allocated
  bool m_temps_active;   // true if temporaries have been allocated in the current block

  // don't allow copy ctor and assignment operator
  VregAllocator(const VregAllocator &);
  VregAllocator &operator=(const VregAllocator &);

public:
  VregAllocator();
  ~VregAllocator();

  // Reset to initial state
  void reset();

  // Add a parameter
  int alloc_param();

  // Call when all parameters have been added
  void end_params();

  // Enter block scope.
  // Returns the current "high water mark" representing where allocation
  // should resume upon leaving the block scope.
  int enter_block();

  // Leave a block. Parameter is the value returned by the previous
  // call to enter_block()
  void leave_block(int mark);

  // Allocate a local variable in current block.
  // The vreg will not be used as a temporary in subsequent statements,
  // but will be deallocated upon leaving the block.
  int alloc_local();

  // Call this to reset allocation of temporaries after a statement.
  // The idea is that temporaries used in a statement are dead at the end
  // of the statement, so there is no reason not to reuse the virtual
  //int registers allocated to them when beginning the next statement.
  void begin_statement();

  // Allocate a temporary vreg
  int alloc_temp();
};

#endif // VREG_ALLOCATOR_H
