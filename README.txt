I implemented different helper functions for different tasks. 
the 
a)create_implicit_conversion: This helper function creates a node representing an implicit type conversion. It is used when the return type of a function does not exactly match the type of the return expression, allowing for necessary type promotions.

b)is_assignable:this method checks if one type can be assigned to another, considering implicit conversions and type compatibility rules. It handles basic type promotions and array-to-pointer conversions.

c)promote_arithmetic_rypes: 
This function promotes arithmetic types to ensure that operations are performed with the correct precision. It follows C's rules for type promotion, such as promoting smaller integer types to int.

I did not implement support for union_types, and have an issue with variable redefinitions.

I also added a visit_for_statement method to handle the semantic analysis of for loops, and visit_if_statement to handle semantic analysis of if statements.


** Assignment 4 Milestone 1: High-Level Code Generation Implementation ** 
In this milestone, I implemented high-level code generation focusing on array access and function calls. The key challenge was properly handling array arguments in function calls, where I needed to fix issues with array indexing and parameter passing. I modified the LocalStorageAllocation visitor to properly allocate virtual registers (vr10+ for locals) and memory storage for arrays. The main bug fixes involved correcting array access in the sum function by using the proper base address (parameter 'a' in vr10) rather than an invalid negative offset, and fixing function calls to properly pass array addresses as arguments. In LocalStorageAllocation, I added tracking for address-taken variables and a helper function to find address-taken variables in the AST to avoid double allocation.  

Milestone 2: Low-Level Code Generation Implementation
I implemented low-level code generation, focusing on the assignment of virtual registers to machine registers. I created a helper function to select the appropriate machine register based on the size of the value being assigned. I also implemented the LocalStorageAllocation visitor to allocate memory for local variables and parameters, including handling array parameters. The main challenge was correctly mapping virtual registers to machine registers, especially for parameters, where I needed to map rdi, rsi, rdx, rcx, r8, r9 to vr1, vr2, vr3, vr4, vr5, vr6 respectively. I also added support for local variables by allocating space on the stack and translating high-level instructions to their corresponding low-level instructions.
 
 I couldnt get arrays and pointers to work unfortunatley. I tried a few things but I think the biggest issue was my offset calculations for arrays.




