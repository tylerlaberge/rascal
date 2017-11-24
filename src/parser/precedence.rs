#[allow(non_camel_case_types)]
pub enum Precedence {
    CALL         = 7,
    UNARY_NUM    = 6,
    PRODUCT      = 5,
    SUM          = 4,
    COMPARISON   = 3,
    UNARY_BOOL   = 2,
    BINARY_BOOL  = 1
}