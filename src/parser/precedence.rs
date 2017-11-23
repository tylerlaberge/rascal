#[allow(non_camel_case_types)]
pub enum Precedence {
    CALL         = 6,
    UNARY_NUM    = 5,
    PRODUCT      = 4,
    SUM          = 3,
    UNARY_BOOL   = 2,
    BINARY_BOOL  = 1
}