I implemented different helper functions for different tasks. 
the 
a)create_implicit_conversion: This helper function creates a node representing an implicit type conversion. It is used when the return type of a function does not exactly match the type of the return expression, allowing for necessary type promotions.

b)is_assignable:this method checks if one type can be assigned to another, considering implicit conversions and type compatibility rules. It handles basic type promotions and array-to-pointer conversions.

c)promote_arithmetic_rypes: 
This function promotes arithmetic types to ensure that operations are performed with the correct precision. It follows C's rules for type promotion, such as promoting smaller integer types to int.

I did not implement support for union_types, and have an issue with variable redefinitions.

I also added a visit_for_statement method to handle the semantic analysis of for loops, and visit_if_statement to handle semantic analysis of if statements.

