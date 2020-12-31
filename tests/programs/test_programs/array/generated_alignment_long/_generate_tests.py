
for i in range(1, 100):

    out = f'''
program: |
  extern fn long_printer(value: long) : void;
  extern fn int_printer(value: int) : void;

  // testing with different array lengths, in case stack/array length alignment affects codegen
  fn main() : int {{
    let canary_1: long = 9223372036854775807;
    let a: long[{i}] = 0;
    let canary_2: long = 9223372036854775807;

    long_printer(canary_1);
    int_printer(a.length);
    long_printer(canary_2);

    return 0;
     
  }}
link_with:
  - tests/files/support/support.c 
expect_stdout: |
  9223372036854775807
  {i}
  9223372036854775807
'''
    with open(f"can_read_array_length_of_{i}.yaml", "w") as f:
        f.write(out)




