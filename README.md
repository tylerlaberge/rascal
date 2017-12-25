# rascal
A simple Pascal interpreter written in rust.

## Usage
Download the latest rascal executable from the [release page](https://github.com/tylerlaberge/rascal/releases).

Run the executable.

    rascal.exe <name-of-pascal-file>.pas

## Features

### Types and Variable Declarations
```pascal
program exampleVariables;
var
    intOne, intTwo: integer;
    realOne, realTwo, realThree: real;
    stringOne, stringTwo: string;
    boolOne: boolean;
begin
    intOne := 5;
    realOne := 5.5;
    stringOne := 'foobar';
    boolOne := true;
end.
```

### Functions and Procedures
```pascal
program exampleProcedure;
    procedure printSum(a, b: integer);
    var
        sum: integer;
    begin
        sum := a + b;
        writeln(IntToString(a) + ' + ' + IntToString(b) + ' = ' + IntToString(sum));
    end
begin
    printSum(5, 10);
end.
```
```pascal
program exampleFunction;
var
    mySum: integer;
    
    function sum(a, b: integer): integer;
    var
        sum: integer;
    begin
        sum := a + b;
    end
begin
    mySum := sum(5, 10);
end.
```

### Control Flow
```pascal
program exampleControlFlow;
begin
    if 20 = 5 then
        begin
            writeln('unreachable');
        end
    else if 5 + 7 < 30 then
        begin
            writeln('this will print');
        end
    else if not true then
        begin
            writeln('this will not print');
        end
    else if 20 <> 5 then
        begin
            writeln('<> means not equal');
        end
    else
        begin
            writeln('this will not print');
        end
end.
```

### Expressions
```pascal
program exampleExpressions;
var
    foo: integer;
    bar: real;
    baz: boolean;
begin
    foo := 5 * ( 7 - -2) div 5;
    bar := 5.5 * (7.0 - -2.5) / 10.0;
    baz := true and (true or false) and (10 < foo or 9 = foo); 
end.
```

### BuiltIn Functions
```pascal
program exampleBuiltIns;
var
    my_int: integer;
    my_real: real;
    my_string: string;
begin
    my_int := 5;
    my_string := IntToString(my_int);
    
    my_real := 5.5;
    my_string := RealToString(my_real);
    
    my_string := '5';
    my_int := StringToInt(my_string);
    
    my_string := '5.5';
    my_real := StringToReal(my_string);
    
    write('print without a newline');
    writeln('print with a newline');
    
    my_string := readln();
end.
```
## Example Programs

### hello world
```pascal
program helloworld;
begin
    writeln('hello world!');
end.
```
### fibonacci

note: This program is not very efficient. You should probably stick to integers less than 20.

```pascal
program fibonacci;
var
    input: integer;
    
    function fib(n:integer): integer;
    var
        val: integer;
        return: integer;
    begin
        if (n <= 2) then
            begin
                val := 1;
            end
        else
            begin
                val := fib(n-1) + fib(n-2);
            end
        return := val;
    end
begin
    writeln('Welcome to fibonacci!');
    write('Please enter an integer: ');
    input := StringToInt(readln());
    writeln('fib of ' + IntToString(input) + ' is ' + IntToString(fib(input)));
end.
```

## Resources
Pascal basic syntax can be read about [here](https://www.tutorialspoint.com/pascal/pascal_basic_syntax.htm).

Bear in mind that this interpreter does not implement every feature of pascal.
    



