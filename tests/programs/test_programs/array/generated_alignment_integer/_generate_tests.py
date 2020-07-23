
for i in range(1, 100):

    out = f'''
program: |
  extern fn int_printer(value: int) : void;

  // testing with different array lengths, in case stack/array length alignment affects codegen
  fn main() : int {{
    let canary_1: int = 2147483647;
    let a: int[{i}] = 0;
    let canary_2: int = 2147483647;

    int_printer(canary_1);
    int_printer(a.length);
    int_printer(canary_2);

    return 0;
     
  }}
link_with:
  - tests/files/support/support.c 
expect_stdout: |
  2147483647
  {i}
  2147483647
'''
    with open(f"can_read_array_length_of_{i}.yaml", "w") as f:
        f.write(out)




