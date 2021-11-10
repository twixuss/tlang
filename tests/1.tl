
main :: fn () {

    // This is a constant
    a :: 10;
    
    
    // This is a variable
    b := 22;
    
    // You can explicitly specify the type
    c : s32 = 12;
    
    d : u8 = 257;

    return global_constant_with_explicit_type;
}

global_constant :: 14;

global_variable := 65;

global_constant_with_explicit_type : u8 : 34;
global_variable_with_explicit_type : s16 = 156;
