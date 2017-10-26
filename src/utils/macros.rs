macro_rules! matches {
    ($expression:expr, $($pattern:pat)|*) => {
        match $expression {
            $($pattern)|* => true,
            _             => false
        }
    };
}