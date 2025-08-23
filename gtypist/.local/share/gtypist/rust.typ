I: Basic Rust function with println
S: fn main() {
 :     println!("Hello, world!");
 : }

I: Variable declarations and mutability
S: let x = 5;
 : let mut y = 10;
 : y += x;

I: Function with parameters and return type
S: fn add(a: i32, b: i32) -> i32 {
 :     a + b
 : }

I: Match expression pattern matching
S: let number = 7;
 : match number {
 :     1 => println!("One"),
 :     2 | 3 | 5 | 7 => println!("Prime"),
 :     _ => println!("Other"),
 : }

I: Vector creation and iteration
S: let vec = vec![1, 2, 3];
 : for element in vec {
 :     println!("{}", element);
 : }

I: Struct definition and implementation
S: struct Point {
 :     x: f64,
 :     y: f64,
 : }
 :
 : impl Point {
 :     fn new(x: f64, y: f64) -> Self {
 :         Point { x, y }
 :     }
 : }

I: Error handling with Result
S: fn divide(a: f64, b: f64) -> Result<f64, String> {
 :     if b == 0.0 {
 :         Err("Division by zero".to_string())
 :     } else {
 :         Ok(a / b)
 :     }
 : }

I: HashMap usage
S: use std::collections::HashMap;
 : let mut scores = HashMap::new();
 : scores.insert("Alice", 100);
 : scores.insert("Bob", 85);

I: Iterator methods
S: let numbers = vec![1, 2, 3, 4, 5];
 : let sum: i32 = numbers.iter().sum();
 : let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();

I: Option type handling
S: let maybe_number: Option<i32> = Some(42);
 : if let Some(value) = maybe_number {
 :     println!("The number is: {}", value);
 : }

I: Closure syntax
S: let adder = |x, y| x + y;
 : let result = adder(5, 3);
 : let numbers = vec![1, 2, 3];
 : let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();

I: Trait implementation
S: trait Drawable {
 :     fn draw(&self);
 : }
 :
 : impl Drawable for Point {
 :     fn draw(&self) {
 :         println!("Drawing at ({}, {})", self.x, self.y);
 :     }
 : }

I: Module system
S: mod utils {
 :     pub fn helper_function() {
 :         println!("Helping!");
 :     }
 : }
 :
 : utils::helper_function();

I: Lifetime annotations
S: fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
 :     if x.len() > y.len() {
 :         x
 :     } else {
 :         y
 :     }
 : }

I: Async/await syntax
S: async fn fetch_data() -> Result<String, reqwest::Error> {
 :     let response = reqwest::get("https://example.com").await?;
 :     response.text().await
 : }

I: Pattern matching with structs
S: struct Color {
 :     r: u8,
 :     g: u8,
 :     b: u8,
 : }
 :
 : let color = Color { r: 255, g: 0, b: 0 };
 : match color {
 :     Color { r: 255, g: 0, b: 0 } => println!("Red!"),
 :     _ => println!("Other color"),
 : }

I: Macro usage
S: println!("Formatted: {}", 42);
 : vec![1, 2, 3];
 : assert_eq!(2 + 2, 4);

I: Generic functions
S: fn identity<T>(x: T) -> T {
 :     x
 : }
 :
 : let number = identity(42);
 : let text = identity("hello");