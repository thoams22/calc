use super::numeral::Numeral;

#[derive(PartialEq, Eq, Debug, Clone, Hash, Copy)]
pub struct Number {
    pub(crate) num: i64,
}

impl Number {
    pub fn equal(&self, other: &Self) -> bool {
        self == other
    }

    pub fn simplify(self) -> Numeral {
        Numeral::Number(self)
    }
}
