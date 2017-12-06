macro_rules! matches {
    ($expression:expr, $($pattern:pat)|*) => {
        match $expression {
            $($pattern)|* => true,
            _             => false
        }
    };
}

macro_rules! assert_matches {
    ($expression:expr, $($pattern:pat)|*) => {
        match $expression {
            $($pattern)|* => (),
            ref actual    => panic!("\nExpected: {}\nActual: {:?}\n", stringify!($($pattern)|*), actual)
        }
    };
}